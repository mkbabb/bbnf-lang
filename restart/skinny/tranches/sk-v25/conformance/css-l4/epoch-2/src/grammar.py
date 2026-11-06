"""Stage 3: CSS value-definition/railroad/legacy parsing and reference closure."""

from __future__ import annotations

import re
from dataclasses import dataclass
from typing import Any

from common import collapse, encoded_json, reject_candidate_schema_use, sha256


FIELD_LABELS = (
    "Logical property group",
    "Animation type",
    "Computed value",
    "Canonical order",
    "New values",
    "Applies to",
    "Percentages",
    "Used value",
    "Animatable",
    "Inherited",
    "Initial",
    "Media",
    "For",
    "Type",
    "On",
    "Name",
    "Value",
)
FIELD = re.compile(rf"(?:^|\s)(?P<label>{'|'.join(re.escape(label) for label in FIELD_LABELS)}):", re.I)
LHS_ATOM = re.compile(r"^(?:<<[^<>]+>>|<[^<>]+>|@?[-*A-Za-z][\w-]*(?:\s*\(\))?)$")
LEGACY_NAME = re.compile(r"^[-A-Za-z_][\w-]*$")
PRELUDE = {
    "angle",
    "angle-percentage",
    "any-value",
    "at-keyword-token",
    "custom-ident",
    "custom-property-name",
    "dashed-ident",
    "declaration",
    "declaration-list",
    "declaration-value",
    "dimension",
    "frequency",
    "function-token",
    "hash-token",
    "ident",
    "ident-token",
    "integer",
    "length",
    "length-percentage",
    "number",
    "number-percentage",
    "percentage",
    "resolution",
    "string",
    "string-token",
    "time",
    "unicode-range-token",
    "url-token",
    "zero",
}


class GrammarSyntaxError(ValueError):
    pass


@dataclass(frozen=True)
class Token:
    kind: str
    value: str
    start: int
    end: int


def lex(text: str) -> list[Token]:
    tokens: list[Token] = []
    cursor = 0
    while cursor < len(text):
        char = text[cursor]
        if char.isspace():
            cursor += 1
            continue
        start = cursor
        if text.startswith("<<", cursor):
            end = text.find(">>", cursor + 2)
            if end < 0:
                raise GrammarSyntaxError(f"unterminated <<reference>> at {cursor}")
            tokens.append(Token("REF", text[cursor + 2 : end], cursor, end + 2))
            cursor = end + 2
            continue
        if char == "<":
            end = text.find(">", cursor + 1)
            if end > cursor + 1 and "<" not in text[cursor + 1 : end]:
                tokens.append(Token("REF", text[cursor + 1 : end], cursor, end + 1))
                cursor = end + 1
                continue
        if char in "'\"":
            quote = char
            cursor += 1
            escaped = False
            while cursor < len(text):
                current = text[cursor]
                if current == quote and not escaped:
                    cursor += 1
                    break
                escaped = current == "\\" and not escaped
                if current != "\\":
                    escaped = False
                cursor += 1
            else:
                raise GrammarSyntaxError(f"unterminated literal at {start}")
            tokens.append(Token("LITERAL", text[start:cursor], start, cursor))
            continue
        matched = next((operator for operator in ("||", "&&") if text.startswith(operator, cursor)), None)
        if matched:
            tokens.append(Token(matched, matched, start, start + 2))
            cursor += 2
            continue
        single = {
            "|": "|",
            "[": "[",
            "]": "]",
            "(": "(",
            ")": ")",
            "?": "MULT",
            "*": "MULT",
            "+": "MULT",
            "#": "MULT",
            "!": "MULT",
            ",": "DELIM",
            "/": "DELIM",
        }
        if char in single:
            tokens.append(Token(single[char], char, start, start + 1))
            cursor += 1
            continue
        if char == "{":
            end = text.find("}", cursor + 1)
            if end < 0:
                raise GrammarSyntaxError(f"unterminated multiplier at {cursor}")
            tokens.append(Token("MULT", text[cursor : end + 1], cursor, end + 1))
            cursor = end + 1
            continue
        while cursor < len(text) and not text[cursor].isspace() and text[cursor] not in "|[]()?*+#!,/{'\"<":
            cursor += 1
        if cursor == start:
            cursor += 1
        tokens.append(Token("ATOM", text[start:cursor], start, cursor))
    return tokens


class Parser:
    def __init__(self, tokens: list[Token]):
        self.tokens = tokens
        self.cursor = 0

    def peek(self, *kinds: str) -> bool:
        return self.cursor < len(self.tokens) and self.tokens[self.cursor].kind in kinds

    def take(self, kind: str | None = None) -> Token:
        if self.cursor >= len(self.tokens):
            raise GrammarSyntaxError("unexpected end of expression")
        token = self.tokens[self.cursor]
        if kind is not None and token.kind != kind:
            raise GrammarSyntaxError(f"expected {kind}, got {token.kind} at {token.start}")
        self.cursor += 1
        return token

    def expression(self, stops: set[str] | None = None) -> dict[str, Any]:
        return self.alternation(stops or set())

    def alternation(self, stops: set[str]) -> dict[str, Any]:
        items = [self.double_bar(stops | {"|"})]
        while self.peek("|") and "|" not in stops:
            self.take("|")
            items.append(self.double_bar(stops | {"|"}))
        return items[0] if len(items) == 1 else {"kind": "alternative", "items": items}

    def double_bar(self, stops: set[str]) -> dict[str, Any]:
        items = [self.double_amp(stops | {"||"})]
        while self.peek("||") and "||" not in stops:
            self.take("||")
            items.append(self.double_amp(stops | {"||"}))
        return items[0] if len(items) == 1 else {"kind": "any-order", "items": items}

    def double_amp(self, stops: set[str]) -> dict[str, Any]:
        items = [self.sequence(stops | {"&&"})]
        while self.peek("&&") and "&&" not in stops:
            self.take("&&")
            items.append(self.sequence(stops | {"&&"}))
        return items[0] if len(items) == 1 else {"kind": "all-order", "items": items}

    def sequence(self, stops: set[str]) -> dict[str, Any]:
        items = []
        while self.cursor < len(self.tokens) and self.tokens[self.cursor].kind not in stops:
            items.append(self.postfix())
        if not items:
            return {"kind": "empty"}
        return items[0] if len(items) == 1 else {"kind": "sequence", "items": items}

    def postfix(self) -> dict[str, Any]:
        node = self.atom()
        multipliers = []
        while self.peek("MULT"):
            multipliers.append(self.take().value)
        return node if not multipliers else {"kind": "repeat", "value": node, "multipliers": multipliers}

    def atom(self) -> dict[str, Any]:
        token = self.take()
        if token.kind == "REF":
            value = collapse(re.sub(r"\s*\[[^]]*\]\s*$", "", token.value))
            return {"kind": "reference", "name": value, "raw": collapse(token.value)}
        if token.kind in {"ATOM", "LITERAL", "DELIM"}:
            if token.kind == "ATOM" and self.peek("("):
                self.take("(")
                body = self.expression({")"})
                self.take(")")
                return {"kind": "function", "name": token.value, "value": body}
            return {"kind": token.kind.lower(), "value": token.value}
        if token.kind in {"[", "("}:
            closing = "]" if token.kind == "[" else ")"
            body = self.expression({closing})
            self.take(closing)
            return {"kind": "group", "delimiter": token.kind + closing, "value": body}
        raise GrammarSyntaxError(f"unexpected token {token.kind} at {token.start}")


def parse_expression(text: str) -> dict[str, Any]:
    parser = Parser(lex(text))
    value = parser.expression()
    if parser.cursor != len(parser.tokens):
        token = parser.tokens[parser.cursor]
        raise GrammarSyntaxError(f"unconsumed {token.kind} at {token.start}")
    return value


def name_of(lhs: str) -> str:
    value = collapse(lhs)
    if value.startswith("<<") and value.endswith(">>"):
        return value[2:-2].strip()
    if value.startswith("<") and value.endswith(">"):
        return value[1:-1].strip()
    return value


def equals_at(line: str) -> int | None:
    quote: str | None = None
    square = paren = 0
    for index, char in enumerate(line):
        if quote:
            if char == quote and (index == 0 or line[index - 1] != "\\"):
                quote = None
            continue
        if char in "'\"":
            quote = char
        elif char == "[":
            square += 1
        elif char == "]":
            square = max(0, square - 1)
        elif char == "(":
            paren += 1
        elif char == ")":
            paren = max(0, paren - 1)
        elif char == "=" and square == 0 and paren == 0:
            return index
    return None


def value_definitions(text: str) -> tuple[list[tuple[str, str]], str | None]:
    chunks: list[tuple[str, list[str]]] = []
    expression_lines: list[str] = []
    for raw in text.splitlines():
        line = raw.strip()
        if not line:
            continue
        boundary = equals_at(line)
        if boundary is not None:
            lhs = collapse(line[:boundary])
            if LHS_ATOM.fullmatch(lhs):
                chunks.append((lhs, [line[boundary + 1 :].strip()]))
                continue
        if chunks:
            chunks[-1][1].append(line)
        else:
            expression_lines.append(line)
    if chunks:
        return [(lhs, collapse(" ".join(rhs))) for lhs, rhs in chunks], None
    expression = collapse(" ".join(expression_lines))
    return [], expression or None


def legacy_definitions(text: str) -> tuple[list[tuple[str, str]], str | None]:
    lines = [line.rstrip() for line in text.splitlines()]
    result: list[tuple[str, str]] = []
    index = 0
    while index < len(lines):
        current = lines[index].strip()
        if not current:
            index += 1
            continue
        same = current.split(":", 1)
        if len(same) == 2 and LEGACY_NAME.fullmatch(same[0].strip()):
            name, body = same[0].strip(), [same[1].strip()]
        elif LEGACY_NAME.fullmatch(current) and index + 1 < len(lines) and lines[index + 1].lstrip().startswith(":"):
            name, body = current, [lines[index + 1].lstrip()[1:].strip()]
            index += 1
        else:
            index += 1
            continue
        index += 1
        if ";" in body[-1]:
            body[-1] = body[-1].split(";", 1)[0]
            result.append((name, collapse(" ".join(body))))
            continue
        while index < len(lines):
            line = lines[index].strip()
            if ";" in line:
                body.append(line.split(";", 1)[0])
                break
            body.append(line)
            index += 1
        result.append((name, collapse(" ".join(body))))
        index += 1
    return result, None if result else collapse(text)


def railroad_definitions(text: str) -> tuple[list[tuple[str, str]], str | None]:
    result = []
    for line in text.splitlines():
        if ":" not in line:
            continue
        name, rhs = line.split(":", 1)
        if collapse(name):
            result.append((collapse(name), collapse(rhs)))
    return result, None if result else collapse(text)


def railroad_expression(text: str) -> dict[str, Any]:
    """Parse Bikeshed's indentation-based railroad source into a real tree."""
    roots: list[dict[str, Any]] = []
    stack: list[tuple[int, list[dict[str, Any]]]] = [(-1, roots)]
    for number, raw in enumerate(text.splitlines(), start=1):
        if not raw.strip():
            continue
        indent = len(raw.expandtabs(8)) - len(raw.lstrip().expandtabs(8))
        match = re.fullmatch(r"\s*([A-Za-z][A-Za-z0-9-]*):\s*(.*)", raw)
        if match is None:
            raise GrammarSyntaxError(f"invalid railroad instruction on line {number}")
        operator, payload = match.groups()
        while len(stack) > 1 and indent <= stack[-1][0]:
            stack.pop()
        node: dict[str, Any] = {
            "kind": "railroad-node",
            "operator": operator,
            "payload": payload,
            "children": [],
        }
        if operator == "N" and payload:
            name = collapse(payload)
            if name.startswith("<") and name.endswith(")"):
                name = name[1:].split(None, 1)[0].rstrip(">")
            elif name.startswith("<") and name.endswith(">"):
                name = name[1:-1]
            node["value"] = {"kind": "reference", "name": name, "raw": collapse(payload)}
        elif operator == "T":
            node["value"] = {"kind": "literal", "value": payload}
        stack[-1][1].append(node)
        stack.append((indent, node["children"]))
    if not roots:
        raise GrammarSyntaxError("empty railroad expression")
    return {"kind": "railroad", "items": roots}


def field_occurrences(text: str) -> list[dict[str, Any]]:
    matches = list(FIELD.finditer(text))
    result = []
    for index, match in enumerate(matches):
        end = matches[index + 1].start() if index + 1 < len(matches) else len(text)
        result.append(
            {
                "label": collapse(match.group("label")).lower().replace(" ", "_"),
                "value": collapse(text[match.end() : end]),
                "text_start": match.start(),
                "text_end": end,
            }
        )
    return result


def priority(spec: dict[str, Any]) -> int:
    scopes = set(spec["scopes"])
    base = 0
    if any(scope.startswith("current-work:") for scope in scopes):
        base = 500
    elif "snapshot:pre-cr-exception" in scopes:
        base = 450
    elif "snapshot:official" in scopes:
        base = 400
    elif "snapshot:reliable" in scopes:
        base = 350
    elif "snapshot:fairly-stable" in scopes:
        base = 300
    elif "snapshot:rough-interoperability" in scopes:
        base = 250
    elif "normative-dependency" in scopes:
        base = 200
    level = spec.get("metadata", {}).get("level", "0")
    return base + (int(level) if str(level).isdigit() else 0)


def references(node: Any) -> set[str]:
    found: set[str] = set()
    if isinstance(node, dict):
        if node.get("kind") == "reference":
            found.add(node["name"])
        for value in node.values():
            found.update(references(value))
    elif isinstance(node, list):
        for value in node:
            found.update(references(value))
    return found


def capture(extraction: dict[str, Any], scope: dict[str, Any]) -> dict[str, Any]:
    reject_candidate_schema_use(extraction)
    specs = {spec["id"]: spec for spec in scope["specs"]}
    definitions = []
    expressions = []
    entities = []
    parse_errors = []
    for carrier in extraction["records"]:
        if carrier["normativity"] != "NORMATIVE":
            continue
        kind = carrier["kind"]
        if kind in {"propdef", "descdef", "legacy-row"}:
            occurrences = field_occurrences(carrier["text"])
            by_label: dict[str, list[str]] = {}
            for occurrence in occurrences:
                by_label.setdefault(occurrence["label"], []).append(occurrence["value"])
            names = [collapse(name) for value in by_label.get("name", []) for name in value.split(",") if collapse(name)]
            if not names and kind == "legacy-row":
                names = [f"legacy-media-feature@{carrier['line']}"]
            for name in names:
                entity = {
                    "id": f"{carrier['id']}:{name}",
                    "carrier_id": carrier["id"],
                    "spec_id": carrier["spec_id"],
                    "kind": kind,
                    "name": name,
                    "qualifier": by_label.get("for", [None])[-1],
                    "occurrences": occurrences,
                    "value_asts": [],
                    "terminal_state": "STRUCTURED",
                }
                for label in ("value", "new_values"):
                    for value in by_label.get(label, []):
                        try:
                            entity["value_asts"].append({"label": label, "text": value, "ast": parse_expression(value)})
                        except GrammarSyntaxError as error:
                            entity["terminal_state"] = "UNPROVEN"
                            parse_errors.append(
                                {
                                    "code": "GRAMMAR_PARSE_FAILED",
                                    "carrier_id": carrier["id"],
                                    "plane": "entity-value",
                                    "detail": str(error),
                                }
                            )
                entities.append(entity)
            continue
        if kind not in {"prod", "railroad", "legacy-grammar"}:
            continue
        if kind == "railroad":
            expression_id = f"{carrier['id']}:expression"
            try:
                ast = railroad_expression(carrier["text"])
                state = "PARSED_RAILROAD"
            except GrammarSyntaxError as error:
                ast = None
                state = "UNPROVEN"
                parse_errors.append(
                    {
                        "code": "GRAMMAR_PARSE_FAILED",
                        "carrier_id": carrier["id"],
                        "plane": "railroad",
                        "detail": str(error),
                    }
                )
            expressions.append(
                {
                    "id": expression_id,
                    "carrier_id": carrier["id"],
                    "spec_id": carrier["spec_id"],
                    "dialect": "railroad",
                    "text": carrier["text"],
                    "ast": ast,
                    "ast_sha256": sha256(encoded_json(ast)) if ast is not None else None,
                    "terminal_state": state,
                }
            )
            continue
        if kind == "prod":
            chunks, expression = value_definitions(carrier["text"])
        else:
            chunks, expression = legacy_definitions(carrier["text"])
            if not chunks:
                chunks, expression = value_definitions(carrier["text"])
        for ordinal, (lhs, rhs) in enumerate(chunks, start=1):
            try:
                ast = parse_expression(rhs)
                state = "PARSED"
            except GrammarSyntaxError as error:
                ast = None
                state = "UNPROVEN"
                parse_errors.append(
                    {
                        "code": "GRAMMAR_PARSE_FAILED",
                        "carrier_id": carrier["id"],
                        "definition": lhs,
                        "detail": str(error),
                    }
                )
            definitions.append(
                {
                    "id": f"{carrier['id']}:definition:{ordinal}",
                    "carrier_id": carrier["id"],
                    "spec_id": carrier["spec_id"],
                    "dialect": kind,
                    "lhs": lhs,
                    "name": name_of(lhs),
                    "rhs": rhs,
                    "ast": ast,
                    "ast_sha256": sha256(encoded_json(ast)) if ast is not None else None,
                    "profile": "CSS-SNAPSHOT-2026-L4-UNION",
                    "precedence": priority(specs[carrier["spec_id"]]),
                    "terminal_state": state,
                }
            )
        if expression:
            expression_id = f"{carrier['id']}:expression"
            try:
                ast = parse_expression(expression)
                state = "PARSED_EXPRESSION"
            except GrammarSyntaxError as error:
                ast = None
                state = "UNPROVEN"
                parse_errors.append(
                    {
                        "code": "GRAMMAR_PARSE_FAILED",
                        "carrier_id": carrier["id"],
                        "plane": "expression",
                        "detail": str(error),
                    }
                )
            expressions.append(
                {
                    "id": expression_id,
                    "carrier_id": carrier["id"],
                    "spec_id": carrier["spec_id"],
                    "dialect": kind,
                    "text": expression,
                    "ast": ast,
                    "ast_sha256": sha256(encoded_json(ast)) if ast is not None else None,
                    "terminal_state": state,
                }
            )

    grouped: dict[str, list[dict[str, Any]]] = {}
    for definition in definitions:
        grouped.setdefault(definition["name"], []).append(definition)
    profiles = []
    selected: dict[str, list[dict[str, Any]]] = {}
    for name, choices in sorted(grouped.items()):
        maximum = max(choice["precedence"] for choice in choices)
        active = [choice for choice in choices if choice["precedence"] == maximum]
        hashes = {choice["ast_sha256"] for choice in active}
        if len(active) == 1:
            state = "RESOLVED_BY_PROFILE"
        elif len(hashes) == 1 and None not in hashes:
            state = "EQUIVALENT_AT_PRECEDENCE"
        else:
            state = "CONFLICT"
        profiles.append(
            {
                "name": name,
                "active_definition_ids": [choice["id"] for choice in active],
                "all_definition_ids": [choice["id"] for choice in choices],
                "precedence": maximum,
                "terminal_state": state,
            }
        )
        selected[name] = active if state != "CONFLICT" else []

    closure = []
    for definition in definitions:
        if definition["ast"] is None:
            continue
        for name in sorted(references(definition["ast"])):
            if name.startswith("'") and name.endswith("'"):
                state = "PROPERTY_REFERENCE"
                targets: list[str] = []
            elif name in selected and selected[name]:
                state = "RESOLVED"
                targets = [target["id"] for target in selected[name]]
            elif name in grouped:
                state = "CONFLICT"
                targets = [target["id"] for target in grouped[name]]
            elif name in PRELUDE:
                state = "PRELUDE_IMPORTED"
                targets = [f"css-syntax-prelude::{name}"]
            else:
                state = "UNMAPPED"
                targets = []
            closure.append(
                {
                    "from_definition": definition["id"],
                    "source_kind": "definition",
                    "reference": name,
                    "target_definition_ids": targets,
                    "terminal_state": state,
                }
            )
    for expression in expressions:
        if expression["ast"] is None:
            continue
        for name in sorted(references(expression["ast"])):
            if name in selected and selected[name]:
                state = "RESOLVED"
                targets = [target["id"] for target in selected[name]]
            elif name in grouped:
                state = "CONFLICT"
                targets = [target["id"] for target in grouped[name]]
            elif name in PRELUDE:
                state = "PRELUDE_IMPORTED"
                targets = [f"css-syntax-prelude::{name}"]
            else:
                state = "UNMAPPED"
                targets = []
            closure.append(
                {
                    "from_expression": expression["id"],
                    "source_kind": "expression",
                    "reference": name,
                    "target_definition_ids": targets,
                    "terminal_state": state,
                }
            )

    blockers = list(parse_errors)
    blockers.extend({"code": "PROFILE_CONFLICT", "name": item["name"]} for item in profiles if item["terminal_state"] == "CONFLICT")
    blockers.extend(
        {
            "code": "REFERENCE_UNMAPPED",
            "source_id": item.get("from_definition", item.get("from_expression")),
            "reference": item["reference"],
        }
        for item in closure
        if item["terminal_state"] == "UNMAPPED"
    )
    return {
        "schema_version": 2,
        "stage": "grammar-closure",
        "candidate_input_policy": "OBSERVATION_ONLY_FORBID_SCHEMA_OR_TYPE_INPUT",
        "extraction_sha256": sha256(encoded_json(extraction)),
        "definitions": definitions,
        "expressions": expressions,
        "entities": entities,
        "profiles": profiles,
        "reference_closure": closure,
        "blockers": blockers,
    }
