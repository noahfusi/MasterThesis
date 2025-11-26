from __future__ import annotations

import logging
from typing import Iterable, List
import json
from urllib import error, parse, request

import config

try:
    from openai import OpenAI  # type: ignore
except Exception:  # pragma: no cover - optional dependency
    OpenAI = None

logger = logging.getLogger("uvicorn.error")


class LLMError(RuntimeError):
    """Raised when an LLM provider cannot fulfill a request."""


def _ensure_float_list(values: Iterable[object]) -> List[float]:
    try:
        return [float(value) for value in values]
    except (TypeError, ValueError) as exc:
        raise LLMError("Embedding payload contains non-numeric values.") from exc


def _build_url(base_url: str, path: str) -> str:
    base = base_url.rstrip("/") + "/"
    rel = path.lstrip("/")
    return parse.urljoin(base, rel)


# ---------- Ollama backend ----------
def _ollama_generate(prompt: str, model: str | None, timeout: float) -> str:
    payload = {"model": model or config.FEEDBACK_MODEL, "prompt": prompt, "stream": False}
    url = _build_url(config.DEFAULT_OLLAMA_BASE_URL, "/api/generate")
    data = json.dumps(payload).encode("utf-8")
    req = request.Request(url, data=data, headers={"Content-Type": "application/json"}, method="POST")
    try:
        with request.urlopen(req, timeout=timeout) as resp:
            response_data = resp.read()
    except error.HTTPError as exc:
        detail = exc.read().decode("utf-8", errors="replace")
        raise LLMError(f"Ollama HTTP {exc.code}: {detail or exc.reason}") from exc
    except error.URLError as exc:
        raise LLMError(f"Ollama unreachable at {url}: {exc.reason}") from exc
    try:
        decoded = json.loads(response_data.decode("utf-8"))
    except json.JSONDecodeError as exc:
        raise LLMError("Invalid JSON response from Ollama.") from exc
    text = decoded.get("response") or decoded.get("text") or ""
    if not isinstance(text, str):
        raise LLMError("Response missing 'response' text.")
    return text


def _ollama_embed(text: str, model: str | None, timeout: float) -> list[float]:
    payload = {"model": model or config.DEFAULT_OLLAMA_EMBED_MODEL, "input": text}
    url = _build_url(config.DEFAULT_OLLAMA_BASE_URL, "/api/embed")
    data = json.dumps(payload).encode("utf-8")
    req = request.Request(url, data=data, headers={"Content-Type": "application/json"}, method="POST")
    try:
        with request.urlopen(req, timeout=timeout) as resp:
            response_data = resp.read()
    except error.HTTPError as exc:
        detail = exc.read().decode("utf-8", errors="replace")
        raise LLMError(f"Ollama HTTP {exc.code}: {detail or exc.reason}") from exc
    except error.URLError as exc:
        raise LLMError(f"Ollama unreachable at {url}: {exc.reason}") from exc
    try:
        decoded = json.loads(response_data.decode("utf-8"))
    except json.JSONDecodeError as exc:
        raise LLMError("Invalid JSON response from Ollama.") from exc
    embeddings = decoded.get("embeddings")
    if embeddings is None:
        raise LLMError("Response missing 'embeddings' field.")
    if not isinstance(embeddings, list):
        raise LLMError("'embeddings' field is not a list.")
    vector = embeddings[0] if embeddings and isinstance(embeddings[0], list) else embeddings
    return _ensure_float_list(vector)


# ---------- OpenAI backend ----------
def _openai_client() -> object:
    if OpenAI is None:
        raise LLMError("openai package is not installed.")
    if not config.OPENAI_API_KEY:
        raise LLMError("OPENAI_API_KEY is required for OpenAI provider.")
    return OpenAI(api_key=config.OPENAI_API_KEY, base_url=config.OPENAI_BASE_URL)


def _openai_generate(prompt: str, model: str | None, timeout: float) -> str:
    client = _openai_client()
    target_model = model or config.OPENAI_MODEL
    try:
        completion = client.chat.completions.create(
            model=target_model,
            messages=[{"role": "user", "content": prompt}],
            temperature=0,
            timeout=timeout,
        )
        return completion.choices[0].message.content or ""
    except Exception as exc:  # noqa: BLE001
        raise LLMError(f"OpenAI completion failed: {exc}") from exc


def _openai_embed(text: str, model: str | None, timeout: float) -> list[float]:
    client = _openai_client()
    target_model = model or config.OPENAI_EMBED_MODEL
    try:
        response = client.embeddings.create(model=target_model, input=text, timeout=timeout)
        vector = response.data[0].embedding
        return _ensure_float_list(vector)
    except Exception as exc:  # noqa: BLE001
        raise LLMError(f"OpenAI embedding failed: {exc}") from exc


def generate_completion(prompt: str, *, model: str | None = None, timeout: float = config.DEFAULT_OLLAMA_TIMEOUT) -> str:
    if not prompt or not prompt.strip():
        raise ValueError("prompt must be a non-empty string.")
    provider = config.LLM_PROVIDER.lower()
    if provider == "openai":
        return _openai_generate(prompt, model, timeout)
    return _ollama_generate(prompt, model, timeout)


def request_embedding(text: str, model: str | None = None, *, timeout: float = config.DEFAULT_OLLAMA_TIMEOUT) -> list[float]:
    if not text:
        raise ValueError("text must be a non-empty string.")
    provider = config.LLM_PROVIDER.lower()
    if provider == "openai":
        return _openai_embed(text, model, timeout)
    return _ollama_embed(text, model, timeout)
