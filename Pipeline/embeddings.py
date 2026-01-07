from __future__ import annotations

from typing import List


def chunk_for_embedding(text: str, max_tokens: int) -> List[str]:
    """
    Split text into token-sized chunks based on a simple whitespace approximation.
    """
    if max_tokens <= 0:
        return [text]

    chunks: list[str] = []
    current_lines: list[str] = []
    current_tokens = 0

    def _count_tokens(line: str) -> int:
        return len(line.split())

    for line in text.splitlines(keepends=True):
        line_tokens = _count_tokens(line)
        if current_tokens and current_tokens + line_tokens > max_tokens:
            chunks.append("".join(current_lines))
            current_lines = []
            current_tokens = 0
        if line_tokens > max_tokens:
            max_chars = max_tokens * 4
            for start in range(0, len(line), max_chars):
                chunks.append(line[start : start + max_chars])
            continue
        current_lines.append(line)
        current_tokens += line_tokens

    if current_lines:
        chunks.append("".join(current_lines))

    return chunks or [text]
