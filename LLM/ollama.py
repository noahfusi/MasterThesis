from __future__ import annotations

import json
import logging
from typing import Iterable, List
from urllib import error, parse, request

import config

__all__ = ["OllamaError", "request_embedding"]

logger = logging.getLogger('uvicorn.error')

DEFAULT_OLLAMA_BASE_URL = config.DEFAULT_OLLAMA_BASE_URL
DEFAULT_EMBED_MODEL = config.DEFAULT_OLLAMA_EMBED_MODEL


class OllamaError(RuntimeError):
    """Raised when the Ollama service cannot fulfill a request."""


def _build_url(base_url: str, path: str) -> str:
    """
    @brief Join a base URL and relative path safely.
    @param base_url Ollama base URL.
    @param path Relative API path.
    @return Fully qualified URL.
    """
    base = base_url.rstrip("/") + "/"
    rel = path.lstrip("/")
    return parse.urljoin(base, rel)


def _ensure_float_list(values: Iterable[object]) -> List[float]:
    """
    @brief Coerce an iterable of values to a list of floats.
    @param values Iterable containing numeric-like values.
    @return List of floats.
    @throws OllamaError When conversion fails.
    """
    try:
        return [float(value) for value in values]
    except (TypeError, ValueError) as exc:
        raise OllamaError("Embedding payload contains non-numeric values.") from exc


def request_embedding(
    text: str,
    model: str | None = None,
    *,
    base_url: str | None = None,
    timeout: float = config.DEFAULT_OLLAMA_TIMEOUT,
) -> list[float]:
    """
    Request an embedding vector for the provided text from Ollama.

    Args:
        text: Source text to embed.
        model: Optional model name (defaults to ``DEFAULT_EMBED_MODEL``).
        base_url: Ollama server base URL (defaults to ``DEFAULT_OLLAMA_BASE_URL``).
        timeout: HTTP timeout in seconds.

    Returns:
        The embedding vector returned by Ollama.

    Raises:
        OllamaError: If the HTTP request fails or the response is invalid.
    """

    if not text:
        raise ValueError("text must be a non-empty string.")

    payload = {
        "model": model or DEFAULT_EMBED_MODEL,
        "input": text,
    }
    url = _build_url(base_url or DEFAULT_OLLAMA_BASE_URL, "/api/embed")

    data = json.dumps(payload).encode("utf-8")
    req = request.Request(url, data=data, headers={"Content-Type": "application/json"}, method="POST")

    try:
        with request.urlopen(req, timeout=timeout) as resp:
            response_data = resp.read()
    except error.HTTPError as exc:
        detail = exc.read().decode("utf-8", errors="replace")
        message = f"Ollama returned HTTP {exc.code}: {detail or exc.reason}"
        raise OllamaError(message) from exc
    except error.URLError as exc:
        raise OllamaError(f"Unable to reach Ollama at {url}: {exc.reason}") from exc

    try:
        decoded = json.loads(response_data.decode("utf-8"))
    except json.JSONDecodeError as exc:
        raise OllamaError("Invalid JSON response from Ollama.") from exc

    logger.debug("Ollama response payload: %s", decoded)

    embeddings = decoded.get("embeddings")
    if embeddings is None:
        raise OllamaError("Response missing 'embeddings' field.")
    if not isinstance(embeddings, list):
        raise OllamaError("'embeddings' field is not a list.")

    embedding_vector = embeddings
    if embeddings and isinstance(embeddings[0], list):
        # Some Ollama versions wrap embeddings in an outer list (one entry per input).
        embedding_vector = embeddings[0]

    return _ensure_float_list(embedding_vector)


def generate_completion(
    prompt: str,
    *,
    model: str | None = None,
    base_url: str | None = None,
    timeout: float = config.DEFAULT_OLLAMA_TIMEOUT,
) -> str:
    """
    @brief Send a prompt to Ollama's generate endpoint and return the full completion.
    @param prompt Prompt text to send.
    @param model Optional model override (defaults to config.FEEDBACK_MODEL).
    @param base_url Ollama server base URL.
    @param timeout HTTP timeout in seconds.
    @return Generated completion text.
    @throws OllamaError On HTTP or decoding failures.
    """
    target_model = model or config.FEEDBACK_MODEL
    if not prompt.strip():
        raise ValueError("prompt must be a non-empty string.")

    payload = {
        "model": target_model,
        "prompt": prompt,
        "stream": False,
    }
    url = _build_url(base_url or DEFAULT_OLLAMA_BASE_URL, "/api/generate")
    data = json.dumps(payload).encode("utf-8")
    req = request.Request(url, data=data, headers={"Content-Type": "application/json"}, method="POST")

    try:
        with request.urlopen(req, timeout=timeout) as resp:
            response_data = resp.read()
    except error.HTTPError as exc:
        detail = exc.read().decode("utf-8", errors="replace")
        message = f"Ollama returned HTTP {exc.code}: {detail or exc.reason}"
        raise OllamaError(message) from exc
    except error.URLError as exc:
        raise OllamaError(f"Unable to reach Ollama at {url}: {exc.reason}") from exc

    try:
        decoded = json.loads(response_data.decode("utf-8"))
    except json.JSONDecodeError as exc:
        raise OllamaError("Invalid JSON response from Ollama.") from exc

    text = decoded.get("response") or decoded.get("text") or ""
    if not isinstance(text, str):
        raise OllamaError("Response missing 'response' text.")
    return text


def generate_completion_llmlingua(
    prompt: str,
    *,
    model: str | None = None,
    base_url: str | None = None,
    timeout: float = config.DEFAULT_OLLAMA_TIMEOUT,
) -> str:
    """
    @brief Send a prompt to Ollama's generate endpoint and return the full completion.
    @param prompt Prompt text to send.
    @param model Optional model override (defaults to config.FEEDBACK_MODEL).
    @param base_url Ollama server base URL.
    @param timeout HTTP timeout in seconds.
    @return Generated completion text.
    @throws OllamaError On HTTP or decoding failures.
    """
    target_model = model or config.FEEDBACK_MODEL
    if not prompt.strip():
        raise ValueError("prompt must be a non-empty string.")

    payload = {
        "model": target_model,
        "prompt": prompt,
        "stream": False,
    }
    url = _build_url(base_url or DEFAULT_OLLAMA_BASE_URL, "/api/generate")
    data = json.dumps(payload).encode("utf-8")
    req = request.Request(url, data=data, headers={"Content-Type": "application/json"}, method="POST")

    try:
        with request.urlopen(req, timeout=timeout) as resp:
            response_data = resp.read()
    except error.HTTPError as exc:
        detail = exc.read().decode("utf-8", errors="replace")
        message = f"Ollama returned HTTP {exc.code}: {detail or exc.reason}"
        raise OllamaError(message) from exc
    except error.URLError as exc:
        raise OllamaError(f"Unable to reach Ollama at {url}: {exc.reason}") from exc

    try:
        decoded = json.loads(response_data.decode("utf-8"))
    except json.JSONDecodeError as exc:
        raise OllamaError("Invalid JSON response from Ollama.") from exc

    text = decoded.get("response") or decoded.get("text") or ""
    if not isinstance(text, str):
        raise OllamaError("Response missing 'response' text.")
    return text
