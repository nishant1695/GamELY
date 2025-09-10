# src/GamELY/provider_mapper.py
MODEL_PROVIDER_MAP = {
    # OpenAI models
    'gpt-3.5-turbo': 'openai',
    'gpt-4': 'openai',
    'gpt-4-turbo': 'openai',
    "gpt-4o-mini": 'openai',
    "gpt-4o": 'openai',
    "o1-mini": 'openai',
    "o1": 'openai',
    
    # Anthropic models
    'claude-2': 'anthropic',
    "claude-3-5-sonnet-latest": 'anthropic',
    "claude-3-5-haiku-latest": 'anthropic',
    "claude-3-opus-latest": 'anthropic',
    "claude-3-sonnet-20240229": 'anthropic',
    "claude-3-haiku-20240307": 'anthropic',
    
    # DeepSeek models
    'deepseek-chat': 'deepseek',
    'deepseek-reasoner': 'deepseek'
}

def get_provider(model_name: str) -> str:
    """Determine provider from model name."""
    for pattern, provider in MODEL_PROVIDER_MAP.items():
        if model_name.startswith(pattern):
            return provider
    raise ValueError(f"Unknown model: {model_name}. Unsupported Model")