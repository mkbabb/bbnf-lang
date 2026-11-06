"""RFC 8259 byte parser with immutable normalized observations."""

from __future__ import annotations

import base64
from dataclasses import dataclass
import hashlib
import json
from typing import Any


MAX_OBSERVATION_BYTES = 16 * 1024 * 1024


class ConfigurationError(ValueError):
    """A budget is outside the one supported configuration domain."""

    def __init__(self, field: str, value: object):
        self.category = "configuration"
        self.code = "exact_non_negative_integer_required"
        self.field = field
        self.value_type = type(value).__name__
        super().__init__(f"configuration:{self.code}@{field}:{self.value_type}")

    def observation(self) -> dict[str, Any]:
        return {
            "status": "rejected",
            "category": self.category,
            "code": self.code,
            "field": self.field,
            "value_type": self.value_type,
        }


@dataclass(frozen=True)
class Budgets:
    # These active ceilings bound distinct shapes below the byte ceiling: dense
    # objects can hit nodes, dense arrays can hit entries, and one long string can
    # hit decoded units. The measured envelope is recorded in AUTHOR.md.
    max_input_bytes: int = 1_048_576
    max_depth: int = 500
    max_nodes: int = 60_000
    max_aggregate_entries: int = 40_000
    max_decoded_utf16_units: int = 131_072

    def __post_init__(self) -> None:
        for field in self.__dataclass_fields__:
            value = getattr(self, field)
            if type(value) is not int or value < 0:
                raise ConfigurationError(field, value)


class OracleError(ValueError):
    def __init__(
        self,
        category: str,
        code: str,
        offset: int,
        counters: dict[str, int] | None = None,
    ):
        self.category, self.code, self.offset = category, code, offset
        self.counters = dict(counters) if counters is not None else None
        super().__init__(f"{category}:{code}@{offset}")

    def observation(self) -> dict[str, Any]:
        result: dict[str, Any] = {
            "status": "rejected",
            "category": self.category,
            "code": self.code,
            "offset": self.offset,
        }
        if self.counters is not None:
            result["counters"] = self.counters
        return result


@dataclass(frozen=True, slots=True)
class _Node:
    id: int
    kind: str
    span: tuple[int, int]
    # Unsigned big-endian words preserve UTF-16 equality and sort order without
    # retaining one Python integer (and, for lone surrogates, one nested tuple)
    # per decoded unit. Raw spelling remains in the owned source and span.
    utf16: bytes = b""
    scalar_valid: bool | None = None
    children: tuple[int, ...] = ()
    members: tuple[tuple[int, int], ...] = ()


@dataclass(frozen=True, slots=True)
class _Document:
    _source: bytes
    _nodes: tuple[_Node, ...]
    _root: int
    _counters: tuple[tuple[str, int], ...]
    _source_sha256: bytes

    def source_preserving_bytes(self) -> bytes:
        return self._source


_WS = frozenset(b" \t\r\n")
_HEX = frozenset(b"0123456789abcdefABCDEF")
_ESCAPES = {
    0x22: 0x22,
    0x5C: 0x5C,
    0x2F: 0x2F,
    0x62: 0x08,
    0x66: 0x0C,
    0x6E: 0x0A,
    0x72: 0x0D,
    0x74: 0x09,
}


def _utf16_units(encoded: bytes):
    for index in range(0, len(encoded), 2):
        yield (encoded[index] << 8) | encoded[index + 1]


def _scalar_facts(encoded: bytes):
    units = _utf16_units(encoded)
    pending = next(units, None)
    while pending is not None:
        unit = pending
        pending = next(units, None)
        if 0xD800 <= unit <= 0xDBFF and pending is not None:
            if 0xDC00 <= pending <= 0xDFFF:
                yield 0x10000 + ((unit - 0xD800) << 10) + pending - 0xDC00
                pending = next(units, None)
                continue
        if 0xD800 <= unit <= 0xDFFF:
            yield ("lone_surrogate", unit)
        else:
            yield unit


def _scalar_valid(encoded: bytes) -> bool:
    units = _utf16_units(encoded)
    pending = next(units, None)
    valid = True
    while pending is not None:
        unit = pending
        pending = next(units, None)
        if 0xD800 <= unit <= 0xDBFF and pending is not None:
            if 0xDC00 <= pending <= 0xDFFF:
                pending = next(units, None)
                continue
        if 0xD800 <= unit <= 0xDFFF:
            valid = False
    return valid


class _BuilderNode:
    __slots__ = (
        "id",
        "kind",
        "start",
        "end",
        "utf16",
        "scalar_valid",
        "children",
        "members",
    )

    def __init__(self, node_id: int, kind: str, start: int):
        self.id = node_id
        self.kind = kind
        self.start = start
        self.end: int | None = None
        self.utf16 = b""
        self.scalar_valid: bool | None = None
        self.children: list[int] | None = None
        self.members: list[tuple[int, int]] | None = None


class _Parser:
    """Private mutable builder; only a frozen document can escape."""

    def __init__(self, source: bytes, budgets: Budgets):
        self.source, self.budgets = source, budgets
        self.nodes: list[_BuilderNode | _Node] = []
        self.aggregate_entries = 0
        self.decoded_units = 0
        self.max_depth = 0

    def counters(self) -> dict[str, int]:
        return {
            "input_bytes": len(self.source),
            "nodes": len(self.nodes),
            "aggregate_entries": self.aggregate_entries,
            "decoded_utf16_units": self.decoded_units,
            "max_depth": self.max_depth,
        }

    def fail(self, code: str, offset: int, category: str = "syntax") -> None:
        counters = self.counters() if category == "resource" else None
        raise OracleError(category, code, offset, counters)

    def skip_ws(self, offset: int) -> int:
        while offset < len(self.source) and self.source[offset] in _WS:
            offset += 1
        return offset

    def reserve_node(self, kind: str, offset: int) -> int:
        if len(self.nodes) >= self.budgets.max_nodes:
            self.fail("node_budget", offset, "resource")
        node = len(self.nodes)
        self.nodes.append(_BuilderNode(node, kind, offset))
        return node

    def reserve_entry(self, offset: int) -> None:
        if self.aggregate_entries >= self.budgets.max_aggregate_entries:
            self.fail("aggregate_entry_budget", offset, "resource")
        self.aggregate_entries += 1

    def append_unit(self, units: bytearray, unit: int, offset: int) -> None:
        if self.decoded_units >= self.budgets.max_decoded_utf16_units:
            self.fail("decoded_utf16_budget", offset, "resource")
        self.decoded_units += 1
        units.extend((unit >> 8, unit & 0xFF))

    def string(self, start: int) -> tuple[int, int]:
        node_id = self.reserve_node("string", start)
        source = self.source
        offset = start + 1
        units = bytearray()
        while offset < len(source):
            byte = source[offset]
            if byte == 0x22:
                end = offset + 1
                node = self.nodes[node_id]
                assert type(node) is _BuilderNode
                node.end = end
                node.utf16 = bytes(units)
                node.scalar_valid = _scalar_valid(node.utf16)
                return node_id, end
            if byte < 0x20:
                self.fail("unescaped_control", offset)
            if byte == 0x5C:
                if offset + 1 >= len(source):
                    self.fail("unterminated_escape", offset)
                escape = source[offset + 1]
                if escape in _ESCAPES:
                    self.append_unit(units, _ESCAPES[escape], offset)
                    offset += 2
                    continue
                if escape != 0x75:
                    self.fail("invalid_escape", offset)
                if offset + 6 > len(source) or any(
                    digit not in _HEX for digit in source[offset + 2 : offset + 6]
                ):
                    self.fail("invalid_unicode_escape", offset)
                self.append_unit(units, int(source[offset + 2 : offset + 6], 16), offset)
                offset += 6
                continue
            width = 1
            if byte >= 0x80:
                width = 2 if byte < 0xE0 else 3 if byte < 0xF0 else 4
            scalar = ord(source[offset : offset + width].decode("utf-8"))
            if scalar <= 0xFFFF:
                self.append_unit(units, scalar, offset)
            else:
                scalar -= 0x10000
                self.append_unit(units, 0xD800 + (scalar >> 10), offset)
                self.append_unit(units, 0xDC00 + (scalar & 0x3FF), offset)
            offset += width
        self.fail("unterminated_string", start)
        raise AssertionError("unreachable")

    def number(self, start: int) -> tuple[int, int]:
        node_id = self.reserve_node("number", start)
        source = self.source
        offset = start
        if source[offset] == 0x2D:
            offset += 1
            if offset == len(source):
                self.fail("number_missing_integer", offset)
        if offset >= len(source) or not 0x30 <= source[offset] <= 0x39:
            self.fail("number_missing_integer", offset)
        if source[offset] == 0x30:
            offset += 1
            if offset < len(source) and 0x30 <= source[offset] <= 0x39:
                self.fail("number_leading_zero", offset)
        else:
            while offset < len(source) and 0x30 <= source[offset] <= 0x39:
                offset += 1
        if offset < len(source) and source[offset] == 0x2E:
            offset += 1
            first = offset
            while offset < len(source) and 0x30 <= source[offset] <= 0x39:
                offset += 1
            if offset == first:
                self.fail("number_missing_fraction", offset)
        if offset < len(source) and source[offset] in (0x45, 0x65):
            offset += 1
            if offset < len(source) and source[offset] in (0x2B, 0x2D):
                offset += 1
            first = offset
            while offset < len(source) and 0x30 <= source[offset] <= 0x39:
                offset += 1
            if offset == first:
                self.fail("number_missing_exponent", offset)
        node = self.nodes[node_id]
        assert type(node) is _BuilderNode
        node.end = offset
        return node_id, offset

    def literal(self, start: int, literal: bytes, kind: str) -> tuple[int, int]:
        node_id = self.reserve_node(kind, start)
        if not self.source.startswith(literal, start):
            self.fail("expected_value", start)
        end = start + len(literal)
        node = self.nodes[node_id]
        assert type(node) is _BuilderNode
        node.end = end
        return node_id, end

    def value(self, offset: int, depth: int) -> tuple[int, int, dict[str, Any] | None]:
        if offset >= len(self.source):
            self.fail("expected_value", offset)
        byte = self.source[offset]
        if byte == 0x22:
            node, end = self.string(offset)
            return node, end, None
        if byte == 0x2D or 0x30 <= byte <= 0x39:
            node, end = self.number(offset)
            return node, end, None
        literals = {0x74: (b"true", "true"), 0x66: (b"false", "false"), 0x6E: (b"null", "null")}
        if byte in literals:
            node, end = self.literal(offset, *literals[byte])
            return node, end, None
        if byte not in (0x5B, 0x7B):
            self.fail("expected_value", offset)
        if depth > self.budgets.max_depth:
            self.fail("depth_budget", offset, "depth")
        self.max_depth = max(self.max_depth, depth)
        kind = "array" if byte == 0x5B else "object"
        node = self.reserve_node(kind, offset)
        builder = self.nodes[node]
        assert type(builder) is _BuilderNode
        if kind == "array":
            builder.children = []
        else:
            builder.members = []
        state = "value_or_end" if kind == "array" else "key_or_end"
        return node, offset + 1, {"kind": kind, "node": node, "state": state}

    def parse(self) -> _Document:
        offset = self.skip_ws(0)
        root, offset, opened = self.value(offset, 1)
        stack: list[dict[str, Any]] = []
        if opened:
            stack.append(opened)
        while stack:
            frame = stack[-1]
            node = self.nodes[frame["node"]]
            assert type(node) is _BuilderNode
            offset = self.skip_ws(offset)
            if frame["kind"] == "array":
                state = frame["state"]
                if state in ("value_or_end", "value"):
                    if offset < len(self.source) and self.source[offset] == 0x5D:
                        if state == "value":
                            self.fail("expected_value", offset)
                        node.end = offset + 1
                        stack.pop()
                        offset += 1
                        continue
                    self.reserve_entry(offset)
                    child, offset, opened = self.value(offset, len(stack) + 1)
                    assert node.children is not None
                    node.children.append(child)
                    frame["state"] = "comma_or_end"
                    if opened:
                        stack.append(opened)
                    continue
                if offset < len(self.source) and self.source[offset] == 0x2C:
                    frame["state"] = "value"
                    offset += 1
                    continue
                if offset < len(self.source) and self.source[offset] == 0x5D:
                    node.end = offset + 1
                    stack.pop()
                    offset += 1
                    continue
                self.fail("expected_array_delimiter", offset)
            else:
                state = frame["state"]
                if state in ("key_or_end", "key"):
                    if offset < len(self.source) and self.source[offset] == 0x7D:
                        if state == "key":
                            self.fail("expected_object_key", offset)
                        node.end = offset + 1
                        stack.pop()
                        offset += 1
                        continue
                    if offset >= len(self.source) or self.source[offset] != 0x22:
                        self.fail("expected_object_key", offset)
                    self.reserve_entry(offset)
                    frame["key"], offset = self.string(offset)
                    frame["state"] = "colon"
                    continue
                if state == "colon":
                    if offset >= len(self.source) or self.source[offset] != 0x3A:
                        self.fail("expected_colon", offset)
                    frame["state"] = "value"
                    offset += 1
                    continue
                if state == "value":
                    offset = self.skip_ws(offset)
                    child, offset, opened = self.value(offset, len(stack) + 1)
                    assert node.members is not None
                    node.members.append((frame.pop("key"), child))
                    frame["state"] = "comma_or_end"
                    if opened:
                        stack.append(opened)
                    continue
                if offset < len(self.source) and self.source[offset] == 0x2C:
                    frame["state"] = "key"
                    offset += 1
                    continue
                if offset < len(self.source) and self.source[offset] == 0x7D:
                    node.end = offset + 1
                    stack.pop()
                    offset += 1
                    continue
                self.fail("expected_object_delimiter", offset)
        offset = self.skip_ws(offset)
        if offset != len(self.source):
            self.fail("trailing_content", offset)
        # Replace compact builders in place so construction state is released as
        # immutable nodes are formed; the parse never retains both complete forms.
        for index in range(len(self.nodes)):
            node = self.nodes[index]
            assert type(node) is _BuilderNode and node.end is not None
            self.nodes[index] = _Node(
                id=node.id,
                kind=node.kind,
                span=(node.start, node.end),
                utf16=node.utf16,
                scalar_valid=node.scalar_valid,
                children=tuple(node.children or ()),
                members=tuple(node.members or ()),
            )
        return _Document(
            self.source,
            tuple(self.nodes),
            root,
            tuple(self.counters().items()),
            hashlib.sha256(self.source).digest(),
        )


def parse(source: bytes, budgets: Budgets | None = None) -> _Document:
    if budgets is None:
        budgets = Budgets()
    elif type(budgets) is not Budgets:
        raise ConfigurationError("budgets", budgets)
    if type(source) is not bytes:
        raise ConfigurationError("source", source)
    if len(source) > budgets.max_input_bytes:
        counters = {
            "input_bytes": budgets.max_input_bytes,
            "nodes": 0,
            "aggregate_entries": 0,
            "decoded_utf16_units": 0,
            "max_depth": 0,
        }
        raise OracleError(
            "resource", "input_byte_budget", budgets.max_input_bytes, counters
        )
    # The deliberate BOM policy owns byte zero even if the suffix is malformed.
    if source.startswith(b"\xef\xbb\xbf"):
        raise OracleError("encoding", "bom_forbidden", 0)
    try:
        source.decode("utf-8")
    except UnicodeDecodeError as error:
        raise OracleError("encoding", "invalid_utf8", error.start) from None
    return _Parser(source, budgets).parse()


def _json_bytes(value: Any) -> bytes:
    return json.dumps(
        value, ensure_ascii=True, sort_keys=True, separators=(",", ":")
    ).encode("ascii")


def _output_cap(field: str, value: object) -> int:
    if type(value) is not int or value < 0:
        raise ConfigurationError(field, value)
    return value


def _raw(document: _Document, node: _Node) -> str:
    return document._source[slice(*node.span)].decode("utf-8")


def _node_observation(document: _Document, node: _Node) -> dict[str, Any]:
    item: dict[str, Any] = {
        "id": node.id,
        "kind": node.kind,
        "span": list(node.span),
    }
    if node.kind not in ("array", "object"):
        item["raw"] = _raw(document, node)
    if node.kind == "string":
        item.update(
            {
                "literal_kind": "string",
                "utf16_units": list(_utf16_units(node.utf16)),
                "scalars": [
                    {scalar[0]: scalar[1]} if isinstance(scalar, tuple) else scalar
                    for scalar in _scalar_facts(node.utf16)
                ],
                "scalar_valid": node.scalar_valid,
            }
        )
    elif node.kind == "number":
        item.update({"literal_kind": "number", "number_lexeme": _raw(document, node)})
    elif node.kind in ("true", "false", "null"):
        item.update(
            {
                "literal_kind": node.kind,
                "literal": True if node.kind == "true" else False if node.kind == "false" else None,
            }
        )
    elif node.kind == "array":
        item["children"] = list(node.children)
    else:
        first: dict[bytes, int] = {}
        members = []
        for position, (key, value) in enumerate(node.members):
            units = document._nodes[key].utf16
            duplicate_of = first.get(units)
            if duplicate_of is None:
                first[units] = key
            members.append(
                {
                    "position": position,
                    "key": key,
                    "value": value,
                    "duplicate_of": duplicate_of,
                }
            )
        item["members"] = members
    return item


def _observation_chunks(
    document: _Document, jcs: dict[str, Any] | None
):
    yield b'{"counters":'
    yield _json_bytes(dict(document._counters))
    if jcs is not None:
        yield b',"jcs":'
        yield _json_bytes(jcs)
    yield b',"nodes":['
    for index, node in enumerate(document._nodes):
        if index:
            yield b","
        yield _json_bytes(_node_observation(document, node))
    yield b'],"root":'
    yield str(document._root).encode("ascii")
    yield b',"source_base64":'
    yield _json_bytes(base64.b64encode(document._source).decode("ascii"))
    yield b',"source_bytes":'
    yield str(len(document._source)).encode("ascii")
    yield b',"source_preserving_roundtrip":true,"source_sha256":'
    yield _json_bytes(hashlib.sha256(document._source).hexdigest())
    yield b',"status":"accepted"}'


def _observe(
    document: _Document,
    max_output_bytes: int = MAX_OBSERVATION_BYTES,
    jcs: dict[str, Any] | None = None,
) -> bytes:
    _output_cap("max_output_bytes", max_output_bytes)
    output = bytearray()
    for chunk in _observation_chunks(document, jcs):
        if len(output) + len(chunk) > max_output_bytes:
            raise OracleError(
                "resource",
                "output_byte_budget",
                max_output_bytes,
                dict(document._counters),
            )
        output.extend(chunk)
    return bytes(output)


def observe(
    document: _Document, max_output_bytes: int = MAX_OBSERVATION_BYTES
) -> bytes:
    """Return bounded canonical immutable bytes with all accepted facts."""

    _output_cap("max_output_bytes", max_output_bytes)
    # A local import avoids a module cycle while routing every public consumer
    # through the same source-derived typed integrity boundary.
    from .projection import _integrity

    return _observe(_integrity(document), max_output_bytes)
