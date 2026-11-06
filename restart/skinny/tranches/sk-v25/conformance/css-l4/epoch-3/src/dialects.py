"""Strict, separate CSS VDS, Bikeshed railroad, legacy, WebIDL, and algorithm dialects."""

from __future__ import annotations

import re
from dataclasses import dataclass
from typing import Any

from common import fail, require_clean, sha256


class DialectError(ValueError):
    pass


@dataclass(frozen=True)
class Lexeme:
    kind: str
    value: str
    start: int
    end: int


MULTIPLIER = re.compile(r"\{(?:0|[1-9]\d*)(?:,(?:[1-9]\d*)?)?\}")
ANGLE_REFERENCE = r"<[^<>\n]*(?:<[^<>\n]+>[^<>\n]*)*>"
LHS_TEXT = rf"(?:<<[^<>]+>>|{ANGLE_REFERENCE}|@?[-*A-Za-z][\w-]*(?:\(\))?)"
LHS = re.compile(LHS_TEXT)


def value_lex(text: str) -> list[Lexeme]:
    result: list[Lexeme] = []
    cursor = 0
    while cursor < len(text):
        if text[cursor].isspace():
            cursor += 1
            continue
        start = cursor
        if text.startswith("<<", cursor):
            depth = 1
            end = cursor + 2
            while end < len(text) and depth:
                if text.startswith("<<", end):
                    depth += 1
                    end += 2
                    continue
                if text.startswith(">>", end):
                    depth -= 1
                    end += 2
                    continue
                end += 1
            if depth or not text[cursor + 2 : end - 2].strip():
                raise DialectError(f"invalid <<reference>> at {cursor}")
            result.append(Lexeme("REF", text[cursor + 2 : end - 2], cursor, end))
            cursor = end
            continue
        if text[cursor] == "<":
            depth = 1
            end = cursor + 1
            while end < len(text) and depth:
                if text[end] == "<":
                    depth += 1
                elif text[end] == ">":
                    depth -= 1
                end += 1
            if depth or not text[cursor + 1 : end - 1].strip():
                raise DialectError(f"invalid <reference> at {cursor}")
            result.append(Lexeme("REF", text[cursor + 1 : end - 1], cursor, end))
            cursor = end
            continue
        if text[cursor] in "'\"":
            quote = text[cursor]
            cursor += 1
            escaped = False
            while cursor < len(text):
                char = text[cursor]
                if char == quote and not escaped:
                    cursor += 1
                    break
                escaped = char == "\\" and not escaped
                if char != "\\":
                    escaped = False
                cursor += 1
            else:
                raise DialectError(f"unterminated quoted literal at {start}")
            result.append(Lexeme("LITERAL", text[start:cursor], start, cursor))
            continue
        operator = next((value for value in ("||", "&&") if text.startswith(value, cursor)), None)
        if operator:
            result.append(Lexeme(operator, operator, cursor, cursor + 2))
            cursor += 2
            continue
        if text[cursor] == "*" and cursor + 1 < len(text) and re.match(r"[-A-Za-z]", text[cursor + 1]):
            cursor += 2
            while cursor < len(text) and re.match(r"[\w-]", text[cursor]):
                cursor += 1
            result.append(Lexeme("ATOM", text[start:cursor], start, cursor))
            continue
        single = {"|": "|", "[": "[", "]": "]", "(": "(", ")": ")", "?": "MULT", "*": "MULT", "+": "MULT", "#": "MULT", "!": "MULT", ",": "DELIM", "/": "DELIM", ":": "DELIM", ";": "DELIM", "}": "DELIM"}
        if text[cursor] in single:
            result.append(Lexeme(single[text[cursor]], text[cursor], cursor, cursor + 1))
            cursor += 1
            continue
        if text[cursor] == "{":
            end = text.find("}", cursor + 1)
            raw = text[cursor : end + 1] if end >= 0 else text[cursor:]
            if end < 0:
                raise DialectError(f"illegal range multiplier {raw!r} at {cursor}")
            if MULTIPLIER.fullmatch(raw):
                left, _, right = raw[1:-1].partition(",")
                if right and int(right) < int(left):
                    raise DialectError(f"descending range multiplier {raw!r} at {cursor}")
                result.append(Lexeme("MULT", raw, cursor, end + 1))
                cursor = end + 1
                continue
            inner = raw[1:-1]
            if not any(char.isspace() for char in inner) and "<" not in inner:
                raise DialectError(f"illegal range multiplier {raw!r} at {cursor}")
            result.append(Lexeme("DELIM", "{", cursor, cursor + 1))
            cursor += 1
            continue
        if text[cursor] in "=":
            raise DialectError(f"illegal CSS value-definition delimiter {text[cursor]!r} at {cursor}")
        while cursor < len(text) and not text[cursor].isspace() and text[cursor] not in "|[]()?*+#!,/{'\"<=:;}":
            cursor += 1
        if cursor == start:
            raise DialectError(f"unrecognized CSS value-definition token at {cursor}")
        result.append(Lexeme("ATOM", text[start:cursor], start, cursor))
    for index, token in enumerate(result):
        before = result[index - 1] if index else None
        after = result[index + 1] if index + 1 < len(result) else None
        if token.kind == "DELIM" and token.value == ":":
            adjacent = (before is not None and before.end == token.start) or (after is not None and token.end == after.start)
            referenced = (before is not None and before.kind == "REF") or (after is not None and after.kind in {"REF", "["})
            if not adjacent and not referenced:
                raise DialectError(f"prose-shaped colon delimiter at {token.start}")
        if token.kind == "DELIM" and token.value == ";" and after is not None and after.kind not in {"]", ")", "MULT", "|", "||", "&&"}:
            raise DialectError(f"nonterminal semicolon delimiter at {token.start}")
    return result


def _node(kind: str, start: int, end: int, **values: Any) -> dict[str, Any]:
    return {"kind": kind, "text_start": start, "text_end": end, **values}


class ValueParser:
    def __init__(self, tokens: list[Lexeme]):
        self.tokens = tokens
        self.cursor = 0

    def peek(self, *kinds: str) -> bool:
        return self.cursor < len(self.tokens) and self.tokens[self.cursor].kind in kinds

    def take(self, kind: str | None = None) -> Lexeme:
        if self.cursor >= len(self.tokens):
            raise DialectError("unexpected end of value definition")
        token = self.tokens[self.cursor]
        if kind is not None and token.kind != kind:
            raise DialectError(f"expected {kind}, got {token.kind} at {token.start}")
        self.cursor += 1
        return token

    def expression(self, stops: set[str] | None = None) -> dict[str, Any]:
        return self.alternative(stops or set())

    def _combine(self, lower: Any, operator: str, kind: str, stops: set[str]) -> dict[str, Any]:
        first = lower(stops | {operator})
        items = [first]
        while self.peek(operator) and operator not in stops:
            separator = self.take(operator)
            if self.cursor >= len(self.tokens) or self.tokens[self.cursor].kind in stops | {operator, "]", ")"}:
                raise DialectError(f"empty operand after {operator} at {separator.start}")
            items.append(lower(stops | {operator}))
        return first if len(items) == 1 else _node(kind, items[0]["text_start"], items[-1]["text_end"], items=items)

    def alternative(self, stops: set[str]) -> dict[str, Any]:
        return self._combine(self.any_order, "|", "alternative", stops)

    def any_order(self, stops: set[str]) -> dict[str, Any]:
        return self._combine(self.all_order, "||", "any-order", stops)

    def all_order(self, stops: set[str]) -> dict[str, Any]:
        return self._combine(self.sequence, "&&", "all-order", stops)

    def sequence(self, stops: set[str]) -> dict[str, Any]:
        items: list[dict[str, Any]] = []
        while self.cursor < len(self.tokens) and self.tokens[self.cursor].kind not in stops:
            if self.tokens[self.cursor].kind in {"|", "||", "&&"}:
                break
            items.append(self.postfix())
        if not items:
            at = self.tokens[self.cursor].start if self.cursor < len(self.tokens) else -1
            raise DialectError(f"empty CSS value-definition operand at {at}")
        return items[0] if len(items) == 1 else _node("sequence", items[0]["text_start"], items[-1]["text_end"], items=items)

    def postfix(self) -> dict[str, Any]:
        value = self.atom()
        multipliers: list[Lexeme] = []
        while self.peek("MULT"):
            multipliers.append(self.take())
        if not multipliers:
            return value
        spellings = [item.value for item in multipliers]
        legal_pair = len(spellings) == 2 and (
            (spellings[0] == "#" and MULTIPLIER.fullmatch(spellings[1]) is not None)
            or (MULTIPLIER.fullmatch(spellings[0]) is not None and spellings[1] == "?")
            or tuple(spellings) == ("+", "#")
            or tuple(spellings) == ("#", "?")
        )
        legal = len(spellings) == 1 or legal_pair
        if not legal:
            raise DialectError(f"illegal multiplier combination {' '.join(spellings)!r} at {multipliers[0].start}")
        if "!" in spellings and value["kind"] != "group":
            raise DialectError(f"required-value ! multiplier must follow a group at {multipliers[0].start}")
        return _node("repeat", value["text_start"], multipliers[-1].end, value=value, multipliers=spellings)

    def atom(self) -> dict[str, Any]:
        token = self.take()
        if token.kind == "REF":
            raw = " ".join(token.value.split())
            name = re.sub(r"\s*\[[^]]*\]\s*$", "", raw).strip()
            if not name:
                raise DialectError(f"empty reference at {token.start}")
            return _node("reference", token.start, token.end, name=name, raw=raw)
        if token.kind in {"LITERAL", "DELIM"}:
            return _node(token.kind.lower(), token.start, token.end, value=token.value)
        if token.kind in {")", "]"}:
            return _node("delim", token.start, token.end, value=token.value)
        if token.kind == "ATOM":
            if self.peek("("):
                opening = self.take("(")
                if self.peek(")"):
                    closing = self.take(")")
                    return _node("function-token", token.start, closing.end, name=token.value)
                body = self.expression({")"})
                closing = self.take(")")
                return _node("function", token.start, closing.end, name=token.value, value=body)
            return _node("atom", token.start, token.end, value=token.value)
        if token.kind == "[":
            if self.peek("]"):
                raise DialectError(f"empty [] group at {token.start}")
            body = self.expression({"]"})
            closing = self.take("]")
            return _node("group", token.start, closing.end, delimiter="[]", value=body)
        if token.kind == "(":
            if self.peek(")"):
                raise DialectError(f"empty () group at {token.start}")
            body = self.expression({")"})
            closing = self.take(")")
            return _node("group", token.start, closing.end, delimiter="()", value=body)
        raise DialectError(f"unexpected {token.kind} at {token.start}")


def parse_value(text: str) -> dict[str, Any]:
    tokens = value_lex(text)
    if not tokens:
        raise DialectError("empty CSS value definition")
    parser = ValueParser(tokens)
    value = parser.expression()
    if parser.cursor != len(tokens):
        raise DialectError(f"unconsumed {tokens[parser.cursor].kind} at {tokens[parser.cursor].start}")
    return value


def value_definitions(text: str) -> tuple[list[dict[str, Any]], dict[str, Any] | None]:
    definition = re.compile(rf"(?m)^[ \t]*(?P<lhs>{LHS_TEXT})[ \t]*(?:\r?\n[ \t]*)?=[ \t]*")
    boundaries = list(definition.finditer(text))
    if boundaries:
        result = []
        prefix_raw = text[: boundaries[0].start()]
        prefix = prefix_raw.strip()
        expression = None
        if prefix:
            prefix_start = len(prefix_raw) - len(prefix_raw.lstrip())
            expression = {"text": prefix, "text_start": prefix_start, "text_end": len(prefix_raw.rstrip()), "ast": parse_value(prefix)}
        for index, match in enumerate(boundaries):
            rhs_end = boundaries[index + 1].start() if index + 1 < len(boundaries) else len(text)
            raw_rhs = text[match.end() : rhs_end]
            leading = len(raw_rhs) - len(raw_rhs.lstrip())
            trailing = len(raw_rhs.rstrip())
            rhs = raw_rhs.strip()
            if not rhs:
                raise DialectError(f"CSS value definition {match.group('lhs')} has empty RHS")
            rhs_start = match.end() + leading
            result.append(
                {
                    "lhs": match.group("lhs"),
                    "lhs_start": match.start("lhs"),
                    "lhs_end": match.end("lhs"),
                    "rhs": rhs,
                    "rhs_source_parts": [{"text": rhs, "start": rhs_start, "end": match.end() + trailing}],
                    "ast": parse_value(rhs),
                }
            )
        return result, expression
    stripped = text.strip()
    leading = len(text) - len(text.lstrip())
    return [], ({"text": stripped, "text_start": leading, "text_end": len(text.rstrip()), "ast": parse_value(stripped)} if stripped else None)


RAILROAD = {
    "N": ("LEAF_PAYLOAD", 0, 0), "T": ("LEAF_PAYLOAD", 0, 0),
    "C": ("LEAF_PAYLOAD", 0, 0), "S": ("LEAF_EMPTY", 0, 0), "Skip": ("LEAF_EMPTY", 0, 0),
    "Seq": ("NARY_EMPTY", 1, None), "Sequence": ("NARY_EMPTY", 2, None), "Stack": ("NARY_EMPTY", 2, None), "And": ("NARY_EMPTY", 2, None),
    "Choice": ("CHOICE", 2, None), "Or": ("CHOICE", 2, None),
    "Plus": ("WRAPPER", 1, None), "OneOrMore": ("WRAPPER", 1, None), "Star": ("WRAPPER", 1, None), "ZeroOrMore": ("WRAPPER", 1, None),
    "Opt": ("OPTIONAL", 1, 1), "Optional": ("OPTIONAL", 1, 1),
}


def parse_railroad(text: str) -> dict[str, Any]:
    roots: list[dict[str, Any]] = []
    stack: list[tuple[int, list[dict[str, Any]]]] = [(-1, roots)]
    offset = 0
    for number, raw_line in enumerate(text.splitlines(keepends=True), start=1):
        raw = raw_line.rstrip("\r\n")
        if not raw.strip():
            offset += len(raw_line)
            continue
        indent = len(raw.expandtabs(8)) - len(raw.lstrip().expandtabs(8))
        match = re.fullmatch(r"\s*([A-Za-z][A-Za-z0-9-]*):\s*(.*)", raw)
        if match is None or match.group(1) not in RAILROAD:
            raise DialectError(f"unknown or malformed railroad instruction on line {number}")
        while len(stack) > 1 and indent <= stack[-1][0]:
            stack.pop()
        operator, payload = match.groups()
        node = {"kind": "railroad-node", "operator": operator, "payload": payload, "children": [], "text_start": offset + match.start(1), "text_end": offset + len(raw)}
        stack[-1][1].append(node)
        stack.append((indent, node["children"]))
        offset += len(raw_line)
    if not roots:
        raise DialectError("empty railroad expression")

    def validate(node: dict[str, Any]) -> None:
        rule, minimum, maximum = RAILROAD[node["operator"]]
        arity = len(node["children"])
        payload = node["payload"].strip()
        if arity < minimum or (maximum is not None and arity > maximum):
            raise DialectError(f"railroad {node['operator']} has illegal arity {arity}")
        if rule == "LEAF_PAYLOAD" and not payload:
            raise DialectError(f"railroad {node['operator']} requires payload")
        if rule in {"LEAF_EMPTY", "NARY_EMPTY", "WRAPPER"} and payload:
            raise DialectError(f"railroad {node['operator']} forbids payload")
        if rule == "OPTIONAL" and payload not in {"", "skip"}:
            raise DialectError(f"railroad {node['operator']} payload must be empty or skip")
        if rule == "CHOICE" and payload:
            if not payload.isdigit() or not 1 <= int(payload) <= arity:
                raise DialectError(f"railroad {node['operator']} default disagrees with arity")
        for child in node["children"]:
            validate(child)

    for root in roots:
        validate(root)
    return {"kind": "railroad", "items": roots, "text_start": roots[0]["text_start"], "text_end": roots[-1]["text_end"]}


YACC_NAME = re.compile(r"[-A-Za-z_][\w-]*")


def parse_yacc(text: str) -> dict[str, Any]:
    productions = []
    cursor = 0

    def trivia(position: int) -> int:
        while position < len(text):
            whitespace = re.match(r"\s*", text[position:]).end()
            position += whitespace
            if text.startswith("/*", position):
                end = text.find("*/", position + 2)
                if end < 0:
                    raise DialectError(f"unterminated legacy Yacc comment at {position}")
                position = end + 2
                continue
            return position
        return position

    while cursor < len(text):
        cursor = trivia(cursor)
        if cursor >= len(text):
            break
        match = YACC_NAME.match(text, cursor)
        if match is None:
            raise DialectError(f"legacy Yacc production name expected at {cursor}")
        name = match.group()
        colon_start = trivia(match.end())
        if colon_start >= len(text) or text[colon_start] != ":":
            raise DialectError(f"legacy Yacc colon expected after {name}")
        rhs_start = colon_start + 1
        position = rhs_start
        quote: str | None = None
        escaped = False
        end = -1
        while position < len(text):
            if quote is not None:
                char = text[position]
                if char == quote and not escaped:
                    quote = None
                escaped = char == "\\" and not escaped
                if char != "\\":
                    escaped = False
                position += 1
                continue
            if text.startswith("/*", position):
                comment_end = text.find("*/", position + 2)
                if comment_end < 0:
                    raise DialectError(f"unterminated legacy Yacc comment at {position}")
                position = comment_end + 2
                continue
            if text[position] in "'\"":
                quote = text[position]
                position += 1
                continue
            if text[position] == ";":
                end = position
                break
            position += 1
        if end < 0:
            raise DialectError(f"legacy Yacc production {name} lacks semicolon")
        rhs = text[rhs_start:end].strip()
        semantic_rhs = re.sub(r"/\*.*?\*/", "", rhs, flags=re.S).strip()
        if not rhs:
            raise DialectError(f"legacy Yacc production {name} has empty RHS")
        refs = sorted(set(re.findall(r"\b[-A-Za-z_][\w-]*\b", re.sub(r"'[^']*'|\"[^\"]*\"", "", semantic_rhs))))
        productions.append({"kind": "legacy-production", "name": name, "rhs": rhs, "references": refs, "text_start": match.start(), "text_end": end + 1})
        cursor = end + 1
    if not productions:
        raise DialectError("legacy Yacc carrier has no productions")
    return {"kind": "legacy-yacc", "productions": productions}


def parse_lex(text: str) -> dict[str, Any]:
    lines = []
    offset = 0
    for number, raw in enumerate(text.splitlines(keepends=True), start=1):
        line = raw.rstrip("\r\n")
        if line.strip():
            lines.append((number, line, offset, offset + len(line)))
        offset += len(raw)
    if not any(line.lstrip().startswith("%option") for _number, line, _start, _end in lines):
        raise DialectError("legacy lex carrier lacks %option declaration")
    macros = []
    for number, line, start, end in lines:
        match = re.match(r"\s*([-A-Za-z_][\w-]*)\s+(.+)$", line)
        if match and not line.lstrip().startswith(("%", "/*")):
            macros.append({"name": match.group(1), "pattern": match.group(2), "line": number, "text_start": start + match.start(1), "text_end": end})
    if not macros:
        raise DialectError("legacy lex carrier lacks named scanner definitions")
    return {"kind": "legacy-lex", "options": [line.strip() for _number, line, _start, _end in lines if line.lstrip().startswith("%option")], "macros": macros}


IDL_DECLARATION = re.compile(
    r"(?m)^\s*(?P<prefix>(?:\[[^]]+\]\s*)?(?:partial\s+)?)(?P<kind>interface\s+mixin|interface|dictionary|namespace|enum|typedef|callback)\s+(?P<name>[A-Za-z_][A-Za-z0-9_]*)"
)
IDL_INCLUDES = re.compile(r"(?m)^\s*(?P<name>[A-Za-z_][A-Za-z0-9_]*)\s+includes\s+(?P<target>[A-Za-z_][A-Za-z0-9_]*)\s*;")


def parse_webidl(text: str) -> list[dict[str, Any]]:
    declarations = []
    for match in IDL_DECLARATION.finditer(text):
        terminator = ";"
        body_start = text.find("{", match.end())
        if match.group("kind") in {"typedef", "callback"} or body_start < 0:
            end = text.find(";", match.end())
        else:
            depth = 0
            end = -1
            for index in range(body_start, len(text)):
                if text[index] == "{": depth += 1
                elif text[index] == "}":
                    depth -= 1
                    if depth == 0:
                        semicolon = text.find(";", index)
                        end = semicolon if semicolon >= 0 else index
                        break
        if end < 0:
            raise DialectError(f"WebIDL declaration {match.group('name')} lacks terminator")
        declarations.append({"kind": "webidl-declaration", "declaration_kind": " ".join(match.group("kind").split()), "name": match.group("name"), "partial": "partial" in match.group("prefix"), "text_start": match.start(), "text_end": end + 1, "sha256": sha256(text[match.start():end+1].encode())})
    for match in IDL_INCLUDES.finditer(text):
        declarations.append({"kind": "webidl-includes", "name": match.group("name"), "target": match.group("target"), "text_start": match.start(), "text_end": match.end(), "sha256": sha256(match.group().encode())})
    declarations.sort(key=lambda row: row["text_start"])
    if not declarations:
        raise DialectError("normative WebIDL carrier has no declaration")
    return declarations


def references(node: Any) -> list[dict[str, Any]]:
    found: list[dict[str, Any]] = []
    if isinstance(node, dict):
        if node.get("kind") == "reference":
            found.append({"name": node["name"], "text_start": node["text_start"], "text_end": node["text_end"]})
        for value in node.values():
            found.extend(references(value))
    elif isinstance(node, list):
        for value in node:
            found.extend(references(value))
    return found


def capture(extraction: dict[str, Any]) -> dict[str, Any]:
    require_clean(extraction, "strict-dialects")
    units: list[dict[str, Any]] = []
    root_blockers: list[dict[str, Any]] = []
    for carrier in extraction["records"]:
        if carrier["normativity"] != "NORMATIVE":
            continue
        try:
            kind = carrier["kind"]
            parsed: Any
            if kind == "prod" or carrier["dialect"] == "LEGACY_CSS_VDS":
                definitions, expression = value_definitions(carrier["text"])
                parsed = {"definitions": definitions, "expression": expression}
            elif kind == "railroad":
                parsed = parse_railroad(carrier["text"])
            elif carrier["dialect"] == "LEGACY_YACC":
                parsed = parse_yacc(carrier["text"])
            elif carrier["dialect"] == "LEGACY_LEX":
                parsed = parse_lex(carrier["text"])
            elif kind == "idl":
                parsed = parse_webidl(carrier["text"])
            elif kind == "algorithm":
                blocks = carrier.get("semantic_blocks", [])
                if not blocks:
                    raise DialectError("algorithm carrier has no explicit structural step/outcome units")
                parsed = {"kind": "algorithm", "steps": blocks}
            elif kind in {"propdef", "descdef", "legacy-row"}:
                values = []
                by_label: dict[str, list[dict[str, Any]]] = {}
                for field in carrier.get("fields", []):
                    if field["label"] in {"value", "new_values"} and field["value"]:
                        by_label.setdefault(field["label"], []).append(field)
                for label, value_fields in by_label.items():
                    parts = []
                    for field in value_fields:
                        raw_value = field.get("raw_value", field["value"])
                        leading = len(raw_value) - len(raw_value.lstrip())
                        exact = raw_value.strip()
                        parts.append({"text": exact, "carrier_text_start": (field.get("value_text_start") or 0) + leading, "source": field["source"]})
                    joined = " ".join(part["text"] for part in parts)
                    values.append({"label": label, "text": joined, "source_parts": parts, "tokens": [token.__dict__ for token in value_lex(joined)], "ast": parse_value(joined)})
                parsed = {"kind": "entity-fields", "fields": carrier.get("fields", []), "value_asts": values}
            else:
                continue
            units.append({"id": f"unit::{carrier['id']}", "carrier_id": carrier["id"], "spec_id": carrier["spec_id"], "dialect": carrier["dialect"], "kind": kind, "parsed": parsed, "status": "PARSED"})
        except DialectError as error:
            root_blockers.append({"code": "STRICT_DIALECT_FAILED", "carrier_id": carrier["id"], "dialect": carrier["dialect"], "detail": str(error)})
    return {"schema_version": 3, "stage": "strict-dialects", "units": units, "root_blockers": root_blockers}
