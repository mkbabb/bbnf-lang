"""Optional RFC 8785 projection over a validated private parse document."""

from __future__ import annotations

import math
from typing import Any

from .parser import (
    Budgets,
    _Document,
    _Node,
    _output_cap,
    _raw,
    _scalar_facts,
    parse,
)


MAX_PROJECTION_BYTES = 1024 * 1024


class ProjectionError(ValueError):
    def __init__(self, code: str, node: int):
        self.category, self.code, self.node = "typed_projection", code, node
        super().__init__(f"typed_projection:{code}@node:{node}")

    def observation(self) -> dict[str, Any]:
        return {
            "status": "rejected",
            "category": self.category,
            "code": self.code,
            "node": self.node,
        }


class ProjectionIntegrityError(ProjectionError):
    def __init__(self, node: int = -1):
        super().__init__("projection_integrity", node)


class ScalarValidityError(ValueError):
    def __init__(self, node: int):
        self.category, self.code, self.node = "scalar_validity", "lone_surrogate", node
        super().__init__(f"scalar_validity:lone_surrogate@node:{node}")

    def observation(self) -> dict[str, Any]:
        return {
            "status": "rejected",
            "category": self.category,
            "code": self.code,
            "node": self.node,
        }


def _integrity(document: object) -> _Document:
    try:
        if type(document) is not _Document or type(document._source) is not bytes:
            raise ProjectionIntegrityError()
        # Reparse the one owned source with source-sized ceilings.  Equality with
        # this fresh immutable document binds every spelling, decoded fact,
        # relation, span, counter and root without maintaining a shadow validator.
        capacity = len(document._source) + 1
        expected = parse(
            document._source,
            Budgets(len(document._source), capacity, capacity, capacity, capacity),
        )
        if document != expected:
            raise ProjectionIntegrityError()
        return document
    except ProjectionIntegrityError:
        raise
    except Exception:
        raise ProjectionIntegrityError() from None


def _require_unicode_scalars(document: _Document) -> None:
    for node in document._nodes:
        if node.kind == "string" and not node.scalar_valid:
            raise ScalarValidityError(node.id)


def require_unicode_scalars(document: object) -> None:
    _require_unicode_scalars(_integrity(document))


def _number(raw: str, node: int) -> str:
    value = float(raw)
    if not math.isfinite(value):
        raise ProjectionError("number_not_finite_binary64", node)
    if value == 0:
        return "0"
    rendered = repr(value).lower()
    sign = ""
    if rendered[0] == "-":
        sign, rendered = "-", rendered[1:]
    if "e" in rendered:
        mantissa, exponent_text = rendered.split("e")
        exponent = int(exponent_text)
        digits = mantissa.replace(".", "").rstrip("0")
        if 1e-6 <= abs(value) < 1e21:
            point = 1 + exponent
            if point <= 0:
                return sign + "0." + "0" * (-point) + digits
            if point >= len(digits):
                return sign + digits + "0" * (point - len(digits))
            return sign + digits[:point] + "." + digits[point:]
        tail = digits[1:]
        mantissa = digits[0] + ("." + tail if tail else "")
        return sign + mantissa + "e" + ("+" if exponent >= 0 else "") + str(exponent)
    if rendered.endswith(".0"):
        rendered = rendered[:-2]
    return sign + rendered


def _string(node: _Node) -> str:
    if not node.scalar_valid:
        raise ProjectionError("non_unicode_scalar_string", node.id)
    escaped: list[str] = ['"']
    short = {8: "\\b", 9: "\\t", 10: "\\n", 12: "\\f", 13: "\\r"}
    for scalar in _scalar_facts(node.utf16):
        if not isinstance(scalar, int):
            raise ProjectionIntegrityError(node.id)
        if scalar in short:
            escaped.append(short[scalar])
        elif scalar < 0x20:
            escaped.append(f"\\u{scalar:04x}")
        elif scalar == 0x22:
            escaped.append('\\"')
        elif scalar == 0x5C:
            escaped.append("\\\\")
        else:
            escaped.append(chr(scalar))
    escaped.append('"')
    return "".join(escaped)


def _check(document: _Document) -> None:
    for node in document._nodes:
        if node.kind == "string" and not node.scalar_valid:
            raise ProjectionError("non_unicode_scalar_string", node.id)
        if node.kind == "number":
            _number(_raw(document, node), node.id)
        if node.kind == "object":
            seen: set[bytes] = set()
            for key_id, _ in node.members:
                key = document._nodes[key_id]
                if key.utf16 in seen:
                    raise ProjectionError("duplicate_object_name", key.id)
                seen.add(key.utf16)


def _canonicalize(
    document: _Document, max_output_bytes: int = MAX_PROJECTION_BYTES
) -> bytes:
    _output_cap("max_projection_bytes", max_output_bytes)
    try:
        _check(document)
        output = bytearray()

        def append(text: str, node: int = -1) -> None:
            encoded = text.encode("utf-8")
            if len(output) + len(encoded) > max_output_bytes:
                raise ProjectionError("projection_byte_budget", node)
            output.extend(encoded)

        work: list[tuple[str, Any]] = [("node", document._root)]
        while work:
            kind, value = work.pop()
            if kind == "text":
                append(value)
                continue
            node = document._nodes[value]
            if node.kind in ("null", "true", "false"):
                append(node.kind, node.id)
            elif node.kind == "number":
                append(_number(_raw(document, node), node.id), node.id)
            elif node.kind == "string":
                append(_string(node), node.id)
            elif node.kind == "array":
                work.append(("text", "]"))
                for index, child in reversed(list(enumerate(node.children))):
                    work.append(("node", child))
                    if index:
                        work.append(("text", ","))
                work.append(("text", "["))
            else:
                members = sorted(
                    node.members,
                    key=lambda member: document._nodes[member[0]].utf16,
                )
                work.append(("text", "}"))
                for index, (key, child) in reversed(list(enumerate(members))):
                    work.append(("node", child))
                    work.append(("text", ":"))
                    work.append(("node", key))
                    if index:
                        work.append(("text", ","))
                work.append(("text", "{"))
        return bytes(output)
    except ProjectionError:
        raise
    except (AttributeError, IndexError, KeyError, TypeError, ValueError, AssertionError):
        raise ProjectionIntegrityError() from None


def canonicalize(
    document: object, max_output_bytes: int = MAX_PROJECTION_BYTES
) -> bytes:
    _output_cap("max_projection_bytes", max_output_bytes)
    return _canonicalize(_integrity(document), max_output_bytes)
