"""Small consuming parsers. Every accepted unit consumes its complete carrier."""

from __future__ import annotations

import re
from dataclasses import dataclass
from typing import Any


class DialectError(ValueError):
    pass


@dataclass(frozen=True)
class Lexeme:
    kind: str
    value: str
    start: int
    end: int


MULTIPLIER = re.compile(r"\{(?:0|[1-9]\d*)(?:,(?:[1-9]\d*)?)?\}")


def css_lex(text: str) -> list[Lexeme]:
    result: list[Lexeme] = []
    cursor = 0
    while cursor < len(text):
        if text[cursor].isspace():
            cursor += 1
            continue
        start = cursor
        if text.startswith("<<", cursor):
            end = text.find(">>", cursor + 2)
            if end < 0 or not text[cursor + 2 : end].strip():
                raise DialectError(f"invalid <<reference>> at {cursor}")
            end += 2
            result.append(Lexeme("REF", text[cursor + 2 : end - 2], cursor, end))
            cursor = end
            continue
        if text[cursor] == "<":
            depth, end = 1, cursor + 1
            while end < len(text) and depth:
                depth += text[end] == "<"
                depth -= text[end] == ">"
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
                raise DialectError(f"unterminated literal at {start}")
            result.append(Lexeme("LITERAL", text[start:cursor], start, cursor))
            continue
        pair = next((value for value in ("||", "&&") if text.startswith(value, cursor)), None)
        if pair:
            result.append(Lexeme(pair, pair, cursor, cursor + 2))
            cursor += 2
            continue
        if text[cursor] == "{" and (found := re.match(r"\{[^}]*\}", text[cursor:])):
            raw = found.group()
            if MULTIPLIER.fullmatch(raw):
                left, separator, right = raw[1:-1].partition(",")
                if separator and right and int(right) < int(left):
                    raise DialectError(f"descending multiplier {raw!r} at {cursor}")
                result.append(Lexeme("MULT", raw, cursor, cursor + len(raw)))
                cursor += len(raw)
                continue
        singles = {
            "|": "|", "[": "[", "]": "]", "(": "(", ")": ")",
            "?": "MULT", "*": "MULT", "+": "MULT", "#": "MULT", "!": "MULT",
            ",": "DELIM", "/": "DELIM", ":": "DELIM", ";": "DELIM",
            "{": "DELIM", "}": "}",
        }
        if text[cursor] in singles:
            result.append(Lexeme(singles[text[cursor]], text[cursor], cursor, cursor + 1))
            cursor += 1
            continue
        if text[cursor] == "=":
            raise DialectError(f"illegal equals at {cursor}")
        while cursor < len(text) and not text[cursor].isspace() and text[cursor] not in "|[]()?*+#!,/{'\"<=:;}":
            cursor += 1
        if cursor == start:
            raise DialectError(f"unrecognized CSS VDS token at {cursor}")
        result.append(Lexeme("ATOM", text[start:cursor], start, cursor))
    return result


def _node(kind: str, start: int, end: int, **extra: Any) -> dict[str, Any]:
    return {"kind": kind, "text_start": start, "text_end": end, **extra}


class CssParser:
    def __init__(self, tokens: list[Lexeme]):
        self.tokens = tokens
        self.cursor = 0

    def peek(self, *kinds: str) -> bool:
        return self.cursor < len(self.tokens) and self.tokens[self.cursor].kind in kinds

    def take(self, kind: str | None = None) -> Lexeme:
        if self.cursor >= len(self.tokens):
            raise DialectError("unexpected end of CSS VDS")
        token = self.tokens[self.cursor]
        if kind is not None and token.kind != kind:
            raise DialectError(f"expected {kind}, got {token.kind} at {token.start}")
        self.cursor += 1
        return token

    def expression(self, stops: set[str] | None = None) -> dict[str, Any]:
        return self.combine(self.any_order, "|", "alternative", stops or set())

    def combine(self, lower: Any, operator: str, kind: str, stops: set[str]) -> dict[str, Any]:
        first = lower(stops | {operator})
        items = [first]
        while self.peek(operator) and operator not in stops:
            separator = self.take()
            if self.cursor >= len(self.tokens) or self.tokens[self.cursor].kind in stops | {operator, "]", ")", "}"}:
                raise DialectError(f"empty operand after {operator} at {separator.start}")
            items.append(lower(stops | {operator}))
        return first if len(items) == 1 else _node(kind, items[0]["text_start"], items[-1]["text_end"], items=items)

    def any_order(self, stops: set[str]) -> dict[str, Any]:
        return self.combine(self.all_order, "||", "any-order", stops)

    def all_order(self, stops: set[str]) -> dict[str, Any]:
        return self.combine(self.sequence, "&&", "all-order", stops)

    def sequence(self, stops: set[str]) -> dict[str, Any]:
        items: list[dict[str, Any]] = []
        while self.cursor < len(self.tokens) and self.tokens[self.cursor].kind not in stops | {"|", "||", "&&"}:
            if self.tokens[self.cursor].kind in {"]", ")", "}"}:
                token = self.tokens[self.cursor]
                raise DialectError(f"unmatched closer {token.value!r} at {token.start}")
            items.append(self.postfix())
        if not items:
            at = self.tokens[self.cursor].start if self.cursor < len(self.tokens) else -1
            raise DialectError(f"empty CSS VDS operand at {at}")
        return items[0] if len(items) == 1 else _node("sequence", items[0]["text_start"], items[-1]["text_end"], items=items)

    def postfix(self) -> dict[str, Any]:
        value = self.atom()
        multipliers: list[Lexeme] = []
        while self.peek("MULT"):
            multipliers.append(self.take())
        if not multipliers:
            return value
        values = [item.value for item in multipliers]
        legal_pair = len(values) == 2 and (
            (values[0] == "#" and MULTIPLIER.fullmatch(values[1]) is not None)
            or (MULTIPLIER.fullmatch(values[0]) is not None and values[1] == "?")
            or tuple(values) in {("+", "#"), ("#", "?")}
        )
        if len(values) != 1 and not legal_pair:
            raise DialectError(f"illegal multiplier combination at {multipliers[0].start}")
        if "!" in values and value["kind"] != "group":
            raise DialectError(f"! multiplier must follow a group at {multipliers[0].start}")
        return _node("repeat", value["text_start"], multipliers[-1].end, value=value, multipliers=values)

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
        if token.kind == "ATOM":
            if self.peek("("):
                self.take("(")
                if self.peek(")"):
                    closing = self.take(")")
                    return _node("function-token", token.start, closing.end, name=token.value)
                body = self.expression({")"})
                closing = self.take(")")
                return _node("function", token.start, closing.end, name=token.value, value=body)
            return _node("atom", token.start, token.end, value=token.value)
        if token.kind in {"[", "("}:
            closer = "]" if token.kind == "[" else ")"
            if self.peek(closer):
                raise DialectError(f"empty group at {token.start}")
            body = self.expression({closer})
            closing = self.take(closer)
            return _node("group", token.start, closing.end, delimiter=token.value + closing.value, value=body)
        raise DialectError(f"unexpected {token.kind} at {token.start}")


def parse_css_vds(text: str) -> dict[str, Any]:
    tokens = css_lex(text)
    if not tokens:
        raise DialectError("empty CSS VDS")
    parser = CssParser(tokens)
    ast = parser.expression()
    if parser.cursor != len(tokens):
        token = tokens[parser.cursor]
        raise DialectError(f"unconsumed {token.kind} at {token.start}")
    return ast


RAILROAD = {
    "N": ("LEAF_PAYLOAD", 0, 0), "T": ("LEAF_PAYLOAD", 0, 0), "C": ("LEAF_PAYLOAD", 0, 0),
    "S": ("LEAF_EMPTY", 0, 0), "Skip": ("LEAF_EMPTY", 0, 0),
    "Seq": ("NARY", 1, None), "Sequence": ("NARY", 2, None), "Stack": ("NARY", 2, None), "And": ("NARY", 2, None),
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
            raise DialectError(f"unknown railroad instruction on line {number}")
        while len(stack) > 1 and indent <= stack[-1][0]:
            stack.pop()
        node = {
            "kind": "railroad-node", "operator": match.group(1), "payload": match.group(2), "children": [],
            "text_start": offset + match.start(1), "line_end": offset + len(raw), "text_end": offset + len(raw),
        }
        stack[-1][1].append(node)
        stack.append((indent, node["children"]))
        offset += len(raw_line)
    if not roots:
        raise DialectError("empty railroad")

    def validate(node: dict[str, Any]) -> int:
        rule, minimum, maximum = RAILROAD[node["operator"]]
        arity = len(node["children"])
        payload = node["payload"].strip()
        if arity < minimum or (maximum is not None and arity > maximum):
            raise DialectError(f"railroad {node['operator']} has illegal arity {arity}")
        if rule == "LEAF_PAYLOAD" and not payload:
            raise DialectError(f"railroad {node['operator']} requires payload")
        if rule in {"LEAF_EMPTY", "NARY", "WRAPPER"} and payload:
            raise DialectError(f"railroad {node['operator']} forbids payload")
        if rule == "OPTIONAL" and payload not in {"", "skip"}:
            raise DialectError(f"railroad {node['operator']} payload is invalid")
        if rule == "CHOICE" and payload and (not payload.isdigit() or not 1 <= int(payload) <= arity):
            raise DialectError(f"railroad {node['operator']} default disagrees with arity")
        descendant_end = max((validate(child) for child in node["children"]), default=node["line_end"])
        node["text_end"] = max(node["line_end"], descendant_end)
        return node["text_end"]

    envelope = max(validate(root) for root in roots)
    return {"kind": "railroad", "items": roots, "text_start": roots[0]["text_start"], "text_end": envelope}


YACC_NAME = re.compile(r"[-A-Za-z_][\w-]*")


def _balanced(text: str, *, allow_macro_braces: bool = False) -> None:
    pairs = {"]": "[", ")": "(", "}": "{"}
    stack: list[tuple[str, int]] = []
    quote: str | None = None
    escaped = False
    cursor = 0
    while cursor < len(text):
        if quote is not None:
            char = text[cursor]
            if char == quote and not escaped:
                quote = None
            escaped = char == "\\" and not escaped
            if char != "\\": escaped = False
            cursor += 1
            continue
        if text.startswith("/*", cursor):
            end = text.find("*/", cursor + 2)
            if end < 0: raise DialectError(f"unterminated comment at {cursor}")
            cursor = end + 2
            continue
        char = text[cursor]
        if char in "'\"": quote = char
        elif char in "[({":
            if char == "{" and allow_macro_braces:
                end = text.find("}", cursor + 1)
                if end < 0: raise DialectError(f"unterminated macro/action at {cursor}")
                cursor = end + 1
                continue
            stack.append((char, cursor))
        elif char in pairs:
            if not stack or stack[-1][0] != pairs[char]: raise DialectError(f"unmatched {char} at {cursor}")
            stack.pop()
        cursor += 1
    if quote is not None: raise DialectError("unterminated quote")
    if stack: raise DialectError(f"unclosed {stack[-1][0]} at {stack[-1][1]}")


def parse_yacc(text: str) -> dict[str, Any]:
    productions: list[dict[str, Any]] = []
    cursor = 0

    def trivia(position: int) -> int:
        while position < len(text):
            match = re.match(r"\s*", text[position:])
            position += match.end()
            if text.startswith("/*", position):
                end = text.find("*/", position + 2)
                if end < 0: raise DialectError(f"unterminated Yacc comment at {position}")
                position = end + 2
                continue
            return position
        return position

    while (cursor := trivia(cursor)) < len(text):
        match = YACC_NAME.match(text, cursor)
        if match is None: raise DialectError(f"Yacc production name expected at {cursor}")
        name = match.group()
        position = trivia(match.end())
        if position >= len(text) or text[position] != ":": raise DialectError(f"Yacc colon expected after {name}")
        rhs_start, position = position + 1, position + 1
        quote: str | None = None
        escaped = False
        levels: list[str] = []
        while position < len(text):
            if quote is not None:
                char = text[position]
                if char == quote and not escaped: quote = None
                escaped = char == "\\" and not escaped
                if char != "\\": escaped = False
            elif text.startswith("/*", position):
                end = text.find("*/", position + 2)
                if end < 0: raise DialectError(f"unterminated Yacc comment at {position}")
                position = end + 2
                continue
            elif text[position] in "'\"": quote = text[position]
            elif text[position] in "[({": levels.append(text[position])
            elif text[position] in "]) }".replace(" ", ""):
                expected = {"]": "[", ")": "(", "}": "{"}[text[position]]
                if not levels or levels[-1] != expected: raise DialectError(f"unmatched Yacc closer at {position}")
                levels.pop()
            elif text[position] == ";" and not levels:
                break
            position += 1
        if position >= len(text): raise DialectError(f"Yacc production {name} lacks semicolon")
        rhs = text[rhs_start:position].strip()
        if not rhs: raise DialectError(f"Yacc production {name} has empty RHS")
        _balanced(rhs)
        semantic = re.sub(r"/\*.*?\*/|'(?:\\.|[^'])*'|\"(?:\\.|[^\"])*\"", " ", rhs, flags=re.S)
        if "???" in semantic or re.search(r"%(?!prec\b)[A-Za-z_-]+", semantic):
            raise DialectError(f"Yacc production {name} has unknown syntax")
        alternatives = [part.strip() for part in semantic.split("|")]
        if any(not part for part in alternatives): raise DialectError(f"Yacc production {name} has empty alternative")
        references = [word for word in re.findall(r"\b[-A-Za-z_][\w-]*\b", semantic) if word != "prec"]
        productions.append({"kind": "yacc-production", "name": name, "rhs": rhs, "references": references, "text_start": match.start(), "text_end": position + 1})
        cursor = position + 1
    if not productions: raise DialectError("Yacc carrier has no productions")
    return {"kind": "yacc", "productions": productions, "text_end": len(text)}


def _lex_pattern(pattern: str, macros: set[str]) -> list[str]:
    _balanced(pattern, allow_macro_braces=True)
    if "???" in pattern: raise DialectError("Lex pattern contains unknown syntax")
    references = re.findall(r"\{([-A-Za-z_][\w-]*)\}", pattern)
    # Numeric braces are quantifiers; named braces are macro edges and must bind.
    missing = sorted(set(references) - macros)
    if missing: raise DialectError(f"Lex pattern references undefined macros: {missing}")
    return references


def _lex_rule(line: str, offset: int, macros: set[str]) -> dict[str, Any]:
    quote: str | None = None
    escaped = False
    square = 0
    split = None
    for index, char in enumerate(line):
        if quote is not None:
            if char == quote and not escaped: quote = None
            escaped = char == "\\" and not escaped
            if char != "\\": escaped = False
            continue
        if char in "'\"": quote = char
        elif char == "[": square += 1
        elif char == "]":
            if square == 0: raise DialectError("Lex rule has unmatched ]")
            square -= 1
        elif char.isspace() and square == 0:
            split = index
            break
    if quote is not None or square: raise DialectError("Lex rule pattern is unterminated")
    if split is None: raise DialectError("Lex rule lacks an action")
    pattern, action = line[:split], line[split:].strip()
    if not pattern or not action: raise DialectError("Lex rule lacks pattern or action")
    if not (action.startswith("{") and action.endswith("}") or action.startswith("return ") or action.startswith("/*") and action.endswith("*/")):
        raise DialectError(f"Lex rule has invalid action {action!r}")
    references = _lex_pattern(pattern, macros)
    return {"kind": "lex-rule", "pattern": pattern, "action": action, "macro_references": references, "text_start": offset, "text_end": offset + len(line)}


def parse_lex(text: str) -> dict[str, Any]:
    sections = text.split("%%")
    if len(sections) not in {2, 3}: raise DialectError("Lex carrier must contain a definitions/rules %% boundary and at most one user-code boundary")
    definitions_text, rules_text = sections[0], sections[1]
    if len(sections) == 3 and sections[2].strip(): raise DialectError("Lex user-code section is unsupported and must be a typed blocker")
    options: list[str] = []
    macros: list[dict[str, Any]] = []
    offset = 0
    for number, raw in enumerate(definitions_text.splitlines(keepends=True), start=1):
        line = raw.rstrip("\r\n")
        stripped = line.strip()
        if not stripped or stripped.startswith("/*"):
            offset += len(raw); continue
        if stripped.startswith("%option"):
            values = stripped.split()[1:]
            if not values or any(not re.fullmatch(r"[-A-Za-z_][\w-]*", value) for value in values): raise DialectError(f"invalid Lex option on line {number}")
            options.extend(values); offset += len(raw); continue
        match = re.match(r"\s*([-A-Za-z_][\w-]*)\s+(.+)$", line)
        if match is None: raise DialectError(f"invalid Lex definition on line {number}")
        macros.append({"kind": "lex-macro", "name": match.group(1), "pattern": match.group(2), "text_start": offset + match.start(1), "text_end": offset + len(line)})
        offset += len(raw)
    if not options: raise DialectError("Lex carrier lacks %option")
    if not macros: raise DialectError("Lex carrier lacks macros")
    names = {row["name"] for row in macros}
    if len(names) != len(macros): raise DialectError("Lex macro name is duplicated")
    for row in macros: row["macro_references"] = _lex_pattern(row["pattern"], names)
    rules: list[dict[str, Any]] = []
    rule_offset = len(definitions_text) + 2
    for raw in rules_text.splitlines(keepends=True):
        line = raw.rstrip("\r\n")
        if line.strip(): rules.append(_lex_rule(line.strip(), rule_offset + len(line) - len(line.lstrip()), names))
        rule_offset += len(raw)
    if not rules: raise DialectError("Lex rules section is empty")
    return {"kind": "lex", "options": options, "macros": macros, "rules": rules, "text_end": len(text)}


IDL_HEAD = re.compile(r"(?:(?:\[[^]]+\])\s*)?(?:partial\s+)?(?:callback\s+interface|interface\s+mixin|interface|dictionary|namespace|enum|typedef|callback)\s+[A-Za-z_][A-Za-z0-9_]*")
IDL_INCLUDES = re.compile(r"[A-Za-z_][A-Za-z0-9_]*\s+includes\s+[A-Za-z_][A-Za-z0-9_]*\s*;")


def _skip_space_comments(text: str, cursor: int) -> int:
    while cursor < len(text):
        found = re.match(r"\s+", text[cursor:])
        if found: cursor += found.end(); continue
        if text.startswith("//", cursor):
            end = text.find("\n", cursor + 2); cursor = len(text) if end < 0 else end + 1; continue
        if text.startswith("/*", cursor):
            end = text.find("*/", cursor + 2)
            if end < 0: raise DialectError(f"unterminated WebIDL comment at {cursor}")
            cursor = end + 2; continue
        break
    return cursor


def _idl_declaration_end(text: str, start: int, head_end: int) -> tuple[int, list[dict[str, Any]]]:
    opening = text.find("{", head_end)
    semicolon = text.find(";", head_end)
    if semicolon < 0: raise DialectError(f"WebIDL declaration at {start} lacks semicolon")
    if opening < 0 or semicolon < opening:
        return semicolon + 1, []
    depth, cursor = 0, opening
    quote: str | None = None
    escaped = False
    while cursor < len(text):
        char = text[cursor]
        if quote is not None:
            if char == quote and not escaped: quote = None
            escaped = char == "\\" and not escaped
            if char != "\\": escaped = False
        elif char in "'\"": quote = char
        elif text.startswith("//", cursor):
            end = text.find("\n", cursor + 2); cursor = len(text) if end < 0 else end; continue
        elif text.startswith("/*", cursor):
            end = text.find("*/", cursor + 2)
            if end < 0: raise DialectError(f"unterminated WebIDL comment at {cursor}")
            cursor = end + 2; continue
        elif char == "{": depth += 1
        elif char == "}":
            depth -= 1
            if depth == 0:
                end = _skip_space_comments(text, cursor + 1)
                if end >= len(text) or text[end] != ";": raise DialectError(f"WebIDL body at {start} lacks trailing semicolon")
                body = text[opening + 1 : cursor]
                members = []
                member_start = 0
                nested = 0
                member_quote: str | None = None
                for index, item in enumerate(body):
                    if member_quote:
                        if item == member_quote and (index == 0 or body[index - 1] != "\\"): member_quote = None
                    elif item in "'\"": member_quote = item
                    elif item in "([{<": nested += 1
                    elif item in ")]}>": nested -= 1
                    elif item == ";" and nested == 0:
                        raw = body[member_start : index + 1]
                        if raw.strip(): members.append({"text": raw.strip(), "text_start": opening + 1 + member_start, "text_end": opening + 2 + index})
                        member_start = index + 1
                if body[member_start:].strip(): raise DialectError(f"WebIDL member lacks semicolon at {opening + 1 + member_start}")
                return end + 1, members
        cursor += 1
    raise DialectError(f"WebIDL body at {start} is unclosed")


def parse_webidl(text: str) -> dict[str, Any]:
    declarations: list[dict[str, Any]] = []
    cursor = 0
    while (cursor := _skip_space_comments(text, cursor)) < len(text):
        includes = IDL_INCLUDES.match(text, cursor)
        if includes:
            declarations.append({"kind": "includes", "text": includes.group(), "text_start": cursor, "text_end": includes.end(), "members": []})
            cursor = includes.end(); continue
        head = IDL_HEAD.match(text, cursor)
        if head is None: raise DialectError(f"unconsumed WebIDL input at {cursor}")
        end, members = _idl_declaration_end(text, cursor, head.end())
        declarations.append({"kind": "declaration", "head": head.group(), "text_start": cursor, "text_end": end, "members": members})
        cursor = end
    if not declarations: raise DialectError("WebIDL carrier has no declarations")
    return {"kind": "webidl", "declarations": declarations, "text_end": len(text)}
