from __future__ import annotations

from Tree_Sitter.structural_ast import parse_code

NESTING_NODE_TYPES = {
    "block",
    "if_expression",
    "else_clause",
    "else_if_clause",
    "for_expression",
    "while_expression",
    "match_expression",
    "case_clause",
    "try_expression",
    "catch_clause",
    "finally_clause",
    "function_definition",
}


def _max_depth(node, depth: int = 0) -> int:
    max_depth = depth
    for child in getattr(node, "children", []) or []:
        next_depth = depth + 1 if child.type in NESTING_NODE_TYPES else depth
        child_depth = _max_depth(child, next_depth)
        if child_depth > max_depth:
            max_depth = child_depth
    return max_depth


def compute_max_nesting_depth(code: str) -> int | None:
    """Return the maximum nesting depth for the provided Scala source code."""
    try:
        root = parse_code(code)
    except Exception:
        return None
    return _max_depth(root, 0)
