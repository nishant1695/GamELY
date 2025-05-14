# src/GameLY/core.py
import re
import requests
from typing import Dict, List, Optional, Tuple
import openai
from anthropic import Anthropic
import pandas as pd
from .provider_mapper import get_provider
import concurrent.futures
from functools import partial
import numpy as np

class AuthenticationError(Exception):
    """Raised when API key is invalid"""
    def __init__(self, provider: str):
        super().__init__(f"Invalid API key for {provider.upper()}")
        self.provider = provider

class APIRequestError(Exception):
    """Raised for general API failures"""
    pass

class EvaluationError(Exception):
    """Custom exception for evaluation errors"""
    pass

class GameLY:

    DEFAULT_CRITERIA = [
        'Is the LLM generated response accurate?',
        'Is the response correct in comprehension?',
        'Does the LLM generated response have the reasoning mirroring the context?',
        'Is the LLM generated response helpful to the user?',
        'Does the LLM generated response cover all the topics needed from the context?',
        'Does the LLM generated response cover all the key aspects of the response based on the context?',
        'Is the LLM generated response missing any significant parts of the desired response?',
        'Is the LLM generated response fluent?',
        'Is the LLM generated response grammatically correct?',
        'Is the LLM generated response organized well?',
        'Does the LLM generated response have any amount of biasness?',
        'Does the LLM generated response have any amount of toxicity?',
        'Does the LLM generated response violate any privacy?',
        'Does the LLM generated response have any amount of hallucinations?',
        'Is the generated response distinguishable from human response?',
        'How does the generated response compare with human response?',
        'How does the generated response compare to other LLM responses?'
    ]


    def __init__(self, model_name: str, api_key: str, max_workers: int = None):
        self.model_name = model_name
        self.api_key = api_key
        self.provider = get_provider(model_name)
        self.max_workers = max_workers  # Number of workers for parallel processing
        self.valid_api_key = self.validate_api_key(self.provider, self.api_key)
        if not self.valid_api_key:
            raise AuthenticationError(self.provider)
        self._setup_client()

    def _setup_client(self):
        """Initialize the API client based on provider"""
        if self.provider == 'openai':
            self.client = openai.OpenAI(api_key=self.api_key)
        elif self.provider == 'anthropic':
            self.client = Anthropic(api_key=self.api_key)
        elif self.provider == 'deepseek':
            self.client = openai.OpenAI(
                api_key=self.api_key,
                base_url="https://api.deepseek.com/v1"
            )
        else:
            raise ValueError(f"Unsupported provider: {self.provider}")
        

    @staticmethod
    def validate_api_key(provider: str, api_key: str) -> bool:
        """Validate the API key for the selected provider."""
        try:
            if provider == "openai":
                client = openai.OpenAI(api_key=api_key)
                client.models.list()
                return True
            elif provider == "anthropic":
                client = Anthropic(api_key=api_key)
                client.models.list()
                return True
            elif provider == "deepseek":
                url = "https://api.deepseek.com/v1/models"  # Deepseek endpoint
                headers = {
                    "Authorization": f"Bearer {api_key}",
                    "Content-Type": "application/json"
                }

                response = requests.get(url, headers=headers, timeout=10)
                # Successful response (2xx status code)
                if response.status_code // 100 == 2:
                    return True
                # Handle authentication errors (401, 403) or other issues
                else:
                    return False
        except AuthenticationError:
            return False
        except Exception as e:
            return False
        return False

    def evaluate_batch(
        self,
        dataframe: pd.DataFrame,
        criteria: Optional[List[str]] = None
    ) -> pd.DataFrame:
        """
        Evaluate a batch of responses in parallel.
        
        Args:
            dataframe: DataFrame with columns ['reference', 'generated']
            criteria: List of evaluation criteria
            
        Returns:
            DataFrame with evaluation results added
        """
        # Validate input format
        if len(dataframe.columns) != 2:
            raise EvaluationError("DataFrame must have exactly 2 columns: reference answer and the generated answer")
 
        dataframe.columns = ['reference', 'generated']
        results = dataframe.copy()
        
        # Use default criteria if none provided
        eval_criteria = criteria or self.DEFAULT_CRITERIA
        if criteria is None:
            print("No criteria provided. Using default criteria:", self.DEFAULT_CRITERIA)
        
        # Create evaluation tasks
        evaluation_tasks = []
        system_prompt = self._get_system_prompt()
        
        for idx, row in dataframe.iterrows():
            ref = row['reference']
            gen = row['generated']
            for criterion in eval_criteria:
                # Each task is (row_index, criterion, reference, generated)
                evaluation_tasks.append((idx, criterion, ref, gen))
        
        # Execute tasks in parallel
        with concurrent.futures.ThreadPoolExecutor(max_workers=self.max_workers) as executor:
            # Create a partial function with system_prompt already defined
            eval_func = partial(self._parallel_evaluate_task, system_prompt=system_prompt)
            # Map the function to all tasks
            evaluation_results = list(executor.map(eval_func, evaluation_tasks))
        
        # Initialize columns with the right dtype (object) to handle mixed types
        for criterion in eval_criteria:
            results[criterion] = pd.Series(dtype='object')

        # Process results and update DataFrame
        for idx, criterion, score in evaluation_results:
            results.at[idx, criterion] = score
        
        return results
    
    def _parallel_evaluate_task(self, task: Tuple, system_prompt: str) -> Tuple:
        """Process a single evaluation task for parallel execution"""
        idx, criterion, reference, generated = task
        score = self._evaluate_single(reference, generated, criterion, system_prompt)
        return (idx, criterion, score)
    
    def _evaluate_single(
        self,
        reference: str,
        generated: str,
        criterion: str,
        system_prompt: str
    ) -> float:
        """Evaluate a single response pair"""
        prompt = self._build_prompt(reference, generated, criterion)
        response = self._call_provider(prompt, system_prompt)
        return self._parse_response(response)
    
    def _get_system_prompt(self) -> str:
        """Get the system prompt for evaluations"""
        return (
            "You are an expert evaluator comparing AI-generated responses to human-written references. "
            "Respond strictly with the requested numerical rating or 'NaN' when irrelevant."
        )

    def _build_prompt(self, reference: str, generated: str, criterion: str) -> str:
        """Construct the evaluation prompt for a single criterion."""
        return f"""Evaluate this generated text against the reference text:

[Reference Text]
{reference}

[Generated Text]
{generated}

Evaluation Criterion: {criterion}

Respond with ONLY one of these numerical options:
1 = Strongly disagree
2 = Disagree
3 = Neutral
4 = Agree
5 = Strongly agree
NaN = Criterion is irrelevant

ONLY respond with the number/NaN, no other text."""

    def _call_provider(self, prompt: str, system_prompt: str) -> str:
        """Make API call to the configured provider."""
        try:
            if self.provider == 'openai':
                response = self.client.chat.completions.create(
                    model=self.model_name,
                    messages=[
                        {"role": "system", "content": system_prompt},
                        {"role": "user", "content": prompt}
                    ],
                    temperature=0.0
                )
                return response.choices[0].message.content.strip()
            
            elif self.provider == 'anthropic':
                response = self.client.messages.create(
                    model=self.model_name,
                    max_tokens=10,
                    messages=[{"role": "user", "content": prompt}],
                    system=system_prompt,
                    temperature=0.0
                )
                return response.content[0].text.strip()
            
            elif self.provider == 'deepseek':
                response = self.client.chat.completions.create(
                    model=self.model_name,
                    messages=[
                        {"role": "system", "content": system_prompt},
                        {"role": "user", "content": prompt}
                    ],
                    stream=False
                )
                return response.choices[0].message.content
            
        except Exception as e:
            return f"API Error: {str(e)}"

    def _parse_response(self, response: str) -> float:
        """Parse the LLM response into a numerical score or NaN."""
        response = response.strip().lower()
        
        # Check for numerical responses
        match = re.search(r'^\D*(\d+)', response)
        if match:
            return float(match.group(1))
        
        # Check for textual responses
        text_scores = {
            'strongly disagree': 1,
            'disagree': 2,
            'neutral': 3,
            'agree': 4,
            'strongly agree': 5
        }
        for text, score in text_scores.items():
            if text in response:
                return float(score)
        
        # Check for NaN responses
        if 'NaN' in response:
            return np.nan
        
        # Return NaN for unparseable responses
        return float('nan')
