# src/GamELY/__init__.py
from .core import GamELY
from .provider_mapper import MODEL_PROVIDER_MAP
import pandas as pd

def evaluate_responses(
    dataframe: pd.DataFrame,
    model_name: str,
    api_key: str,
    criteria: list = None
) -> pd.DataFrame:
    """
    Main function for package users.
    
    Args:
        dataframe: Input DataFrame with reference/generated columns
        model_name: Name of LLM model to use for evaluation
        api_key: API key for the LLM provider
        criteria: Optional list of evaluation criteria
    
    Returns:
        DataFrame with evaluation results
    """
    evaluator = GamELY(model_name, api_key)
    return evaluator.evaluate_batch(dataframe, criteria)



def get_available_models() -> dict:
    """Return a dictionary of available models grouped by provider.
    
    Returns:
        Dictionary with providers as keys and lists of models as values
    """
    models_by_provider = {}
    for model, provider in MODEL_PROVIDER_MAP.items():
        models_by_provider.setdefault(provider, []).append(model)
    return models_by_provider