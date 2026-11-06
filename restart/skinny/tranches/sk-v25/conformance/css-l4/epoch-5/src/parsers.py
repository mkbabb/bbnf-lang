"""Source-preserving projection and consuming supported-dialect parsers."""

from __future__ import annotations

import html
import re
from dataclasses import dataclass
from typing import Any


class UnsupportedDialect(ValueError):
    def __init__(self, dialect: str, code: str, detail: str, offset: int = 0):
        super().__init__(detail)
        self.dialect = dialect
        self.code = code
        self.detail = detail
        self.offset = offset


MARKUP_NAMES = {
    "a", "abbr", "algorithm", "aside", "b", "blockquote", "br", "code", "dd", "del", "dfn", "div", "dl",
    "dt", "em", "figcaption", "figure", "h1", "h2", "h3", "h4", "h5", "h6", "i", "img", "ins", "li",
    "link", "meta", "ol", "p", "pre", "section", "span", "strong", "sup", "table", "tbody", "td", "th",
    "thead", "tr", "ul", "var", "wpt", "wpt-rest", "xmp",
}
TAG_START = re.compile(rb"</?([A-Za-z][A-Za-z0-9:_-]*)\b")
ENTITY = re.compile(rb"&(?:#[0-9]+|#[xX][0-9A-Fa-f]+|[A-Za-z][A-Za-z0-9]+);")


def _tag_end(data: bytes, start: int) -> int | None:
    found = TAG_START.match(data, start)
    if found is None or found.group(1).decode("ascii").lower() not in MARKUP_NAMES:
        return None
    cursor = found.end()
    quote: int | None = None
    while cursor < len(data):
        byte = data[cursor]
        if quote is not None:
            if byte == quote:
                quote = None
        elif byte in (34, 39):
            quote = byte
        elif byte == 62:
            return cursor + 1
        cursor += 1
    return None


@dataclass(frozen=True)
class Projection:
    text: str
    byte_map: tuple[tuple[int, int], ...]

    def fragments(self, start: int, end: int) -> list[list[int]]:
        if start < 0 or end > len(self.byte_map) or start >= end:
            raise UnsupportedDialect("span-map", "SPAN_RANGE_INVALID", f"invalid projected range {start}:{end}", start)
        fragments: list[list[int]] = []
        for left, right in self.byte_map[start:end]:
            if fragments and fragments[-1][1] == left:
                fragments[-1][1] = right
            elif not fragments or fragments[-1] != [left, right]:
                fragments.append([left, right])
        return fragments


def project_markup(data: bytes, *, base: int = 0) -> Projection:
    """Remove only known markup while preserving every dialect byte mapping."""
    output: list[str] = []
    mapping: list[tuple[int, int]] = []
    cursor = 0
    while cursor < len(data):
        if data.startswith(b"<!--", cursor):
            end = data.find(b"-->", cursor + 4)
            if end < 0:
                raise UnsupportedDialect("markup", "COMMENT_UNTERMINATED", "source comment is unclosed", cursor)
            cursor = end + 3
            continue
        if data[cursor : cursor + 1] == b"<" and not data.startswith(b"<<", cursor):
            end = _tag_end(data, cursor)
            if end is not None:
                cursor = end
                continue
        entity = ENTITY.match(data, cursor)
        if entity:
            raw = entity.group().decode("ascii")
            decoded = html.unescape(raw)
            if decoded == raw:
                decoded = raw
            output.extend(decoded)
            mapping.extend([(base + cursor, base + entity.end())] * len(decoded))
            cursor = entity.end()
            continue
        lead = data[cursor]
        width = 1
        if lead >= 0x80:
            width = 2 if lead < 0xE0 else 3 if lead < 0xF0 else 4
        raw = data[cursor : cursor + width]
        try:
            char = raw.decode("utf-8")
        except UnicodeDecodeError as error:
            raise UnsupportedDialect("markup", "UTF8_INVALID", str(error), cursor) from error
        output.append(char)
        mapping.append((base + cursor, base + cursor + width))
        cursor += width
    return Projection("".join(output), tuple(mapping))


@dataclass(frozen=True)
class Lexeme:
    kind: str
    value: str
    start: int
    end: int


CSS_WORD = re.compile(r"[-_A-Za-z0-9.%#]+")
CSS_MULTIPLIER = re.compile(r"\{(?:0|[1-9][0-9]*)(?:,(?:[1-9][0-9]*)?)?\}")


def _css_tokens(text: str) -> list[Lexeme]:
    tokens: list[Lexeme] = []
    cursor = 0
    while cursor < len(text):
        if text[cursor].isspace():
            cursor += 1
            continue
        start = cursor
        if text.startswith("<<", cursor):
            end = text.find(">>", cursor + 2)
            if end < 0 or not text[cursor + 2 : end].strip():
                raise UnsupportedDialect("css-vds", "CSS_REFERENCE_INVALID", "invalid <<reference>>", cursor)
            cursor = end + 2
            tokens.append(Lexeme("OPERAND", text[start:cursor], start, cursor))
            continue
        if text[cursor] == "<":
            end = text.find(">", cursor + 1)
            if end < 0 or not text[cursor + 1 : end].strip() or "<" in text[cursor + 1 : end]:
                raise UnsupportedDialect("css-vds", "CSS_REFERENCE_INVALID", "invalid <reference>", cursor)
            cursor = end + 1
            tokens.append(Lexeme("OPERAND", text[start:cursor], start, cursor))
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
                raise UnsupportedDialect("css-vds", "CSS_LITERAL_UNTERMINATED", "unterminated string", start)
            tokens.append(Lexeme("OPERAND", text[start:cursor], start, cursor))
            continue
        pair = next((value for value in ("||", "&&") if text.startswith(value, cursor)), None)
        if pair:
            tokens.append(Lexeme("OP", pair, cursor, cursor + 2))
            cursor += 2
            continue
        multiplier = CSS_MULTIPLIER.match(text, cursor)
        if multiplier:
            raw = multiplier.group()
            left, comma, right = raw[1:-1].partition(",")
            if comma and right and int(right) < int(left):
                raise UnsupportedDialect("css-vds", "CSS_MULTIPLIER_DESCENDING", raw, cursor)
            tokens.append(Lexeme("POSTFIX", raw, cursor, multiplier.end()))
            cursor = multiplier.end()
            continue
        if text[cursor] in "[]()":
            kind = "OPEN" if text[cursor] in "[(" else "CLOSE"
            tokens.append(Lexeme(kind, text[cursor], cursor, cursor + 1))
            cursor += 1
            continue
        if text[cursor] in "?*+#!":
            tokens.append(Lexeme("POSTFIX", text[cursor], cursor, cursor + 1))
            cursor += 1
            continue
        if text[cursor] == "|":
            tokens.append(Lexeme("OP", "|", cursor, cursor + 1))
            cursor += 1
            continue
        if text[cursor] in ",/:;":
            tokens.append(Lexeme("OPERAND", text[cursor], cursor, cursor + 1))
            cursor += 1
            continue
        if text[cursor] in "{}=@":
            raise UnsupportedDialect("css-vds", "CSS_TOKEN_UNSUPPORTED", f"unsupported token {text[cursor]!r}", cursor)
        word = CSS_WORD.match(text, cursor)
        if not word:
            raise UnsupportedDialect("css-vds", "CSS_TOKEN_UNSUPPORTED", f"unrecognized token {text[cursor]!r}", cursor)
        tokens.append(Lexeme("OPERAND", word.group(), cursor, word.end()))
        cursor = word.end()
    return tokens


def parse_css_vds(text: str) -> list[dict[str, Any]]:
    tokens = _css_tokens(text)
    if not tokens:
        raise UnsupportedDialect("css-vds", "CSS_EMPTY", "empty CSS value definition")
    stack: list[tuple[str, int]] = []
    need_operand = True
    prior_postfix = False
    for token in tokens:
        if token.kind == "OPEN":
            if not need_operand:
                # Function-like grouping is supported, but juxtaposed bare groups
                # remain a sequence and therefore do not need an operator.
                pass
            stack.append((token.value, token.start))
            need_operand = True
            prior_postfix = False
        elif token.kind == "CLOSE":
            expected = ")" if stack and stack[-1][0] == "(" else "]"
            if not stack or token.value != expected or need_operand:
                raise UnsupportedDialect("css-vds", "CSS_GROUP_INVALID", "unmatched or empty group", token.start)
            stack.pop()
            need_operand = False
            prior_postfix = False
        elif token.kind == "OP":
            if need_operand:
                raise UnsupportedDialect("css-vds", "CSS_OPERATOR_EMPTY", "operator lacks left operand", token.start)
            need_operand = True
            prior_postfix = False
        elif token.kind == "POSTFIX":
            if need_operand or prior_postfix:
                raise UnsupportedDialect("css-vds", "CSS_POSTFIX_INVALID", "postfix lacks one operand or is doubled", token.start)
            prior_postfix = True
        else:
            need_operand = False
            prior_postfix = False
    if stack:
        raise UnsupportedDialect("css-vds", "CSS_GROUP_UNCLOSED", "group is unclosed", stack[-1][1])
    if need_operand:
        raise UnsupportedDialect("css-vds", "CSS_OPERAND_MISSING", "expression ends after operator", len(text))
    return [{"family": "css-expression", "start": tokens[0].start, "end": tokens[-1].end}]


YACC_NAME = re.compile(r"[-A-Za-z_][A-Za-z0-9_-]*")
YACC_RHS_TOKEN = re.compile(r"\s+|/\*.*?\*/|'(?:\\.|[^'])*'|\"(?:\\.|[^\"])*\"|%prec\b|[-A-Za-z_][A-Za-z0-9_-]*|[|()[\]{}+*?,.:]", re.S)


def parse_yacc(text: str) -> list[dict[str, Any]]:
    units: list[dict[str, Any]] = []
    cursor = 0
    while cursor < len(text):
        trivia = re.match(r"(?:\s+|/\*.*?\*/)*", text[cursor:], re.S)
        cursor += trivia.end()
        if cursor == len(text):
            break
        name = YACC_NAME.match(text, cursor)
        if not name:
            raise UnsupportedDialect("yacc", "YACC_NAME_EXPECTED", "production name expected", cursor)
        start = cursor
        cursor = name.end()
        cursor += re.match(r"\s*", text[cursor:]).end()
        if cursor >= len(text) or text[cursor] != ":":
            raise UnsupportedDialect("yacc", "YACC_COLON_EXPECTED", "colon expected", cursor)
        rhs_start = cursor = cursor + 1
        quote: str | None = None
        escaped = False
        depth = 0
        while cursor < len(text):
            char = text[cursor]
            if quote:
                if char == quote and not escaped:
                    quote = None
                escaped = char == "\\" and not escaped
                if char != "\\":
                    escaped = False
            elif text.startswith("/*", cursor):
                end = text.find("*/", cursor + 2)
                if end < 0:
                    raise UnsupportedDialect("yacc", "YACC_COMMENT_UNTERMINATED", "comment is unclosed", cursor)
                cursor = end + 2
                continue
            elif char in "'\"":
                quote = char
            elif char in "([{":
                depth += 1
            elif char in ")]}":
                depth -= 1
                if depth < 0:
                    raise UnsupportedDialect("yacc", "YACC_GROUP_INVALID", "unmatched closer", cursor)
            elif char == ";" and depth == 0:
                break
            cursor += 1
        if cursor >= len(text) or quote or depth:
            raise UnsupportedDialect("yacc", "YACC_PRODUCTION_UNCLOSED", "production is not fully closed", start)
        rhs = text[rhs_start:cursor]
        if not rhs.strip():
            raise UnsupportedDialect("yacc", "YACC_RHS_EMPTY", "empty production RHS", rhs_start)
        at = 0
        while at < len(rhs):
            token = YACC_RHS_TOKEN.match(rhs, at)
            if not token:
                raise UnsupportedDialect("yacc", "YACC_TOKEN_UNSUPPORTED", f"unsupported RHS token {rhs[at]!r}", rhs_start + at)
            at = token.end()
        semantic = re.sub(r"/\*.*?\*/|'(?:\\.|[^'])*'|\"(?:\\.|[^\"])*\"", "x", rhs, flags=re.S)
        if any(not part.strip() for part in semantic.split("|")):
            raise UnsupportedDialect("yacc", "YACC_ALTERNATIVE_EMPTY", "empty alternative", rhs_start)
        units.append({"family": "yacc-production", "name": name.group(), "start": start, "end": cursor + 1})
        cursor += 1
    if not units:
        raise UnsupportedDialect("yacc", "YACC_EMPTY", "no productions")
    return units


LEX_OPTIONS = {"case-insensitive", "8bit", "batch", "ecs", "fast", "full", "never-interactive", "no-default", "nodefault", "noinput", "nounistd", "nounput", "unicode", "yylineno"}
LEX_NAME = re.compile(r"[-A-Za-z_][A-Za-z0-9_-]*")


def _lex_pattern(pattern: str, names: set[str], offset: int) -> list[str]:
    if re.search(r"(?<!\\)[*+?]{2,}", pattern):
        raise UnsupportedDialect("lex", "LEX_POSTFIX_DOUBLED", "doubled repetition operator", offset)
    if "???" in pattern or "@@@" in pattern:
        raise UnsupportedDialect("lex", "LEX_TOKEN_UNSUPPORTED", "unknown pattern token", offset)
    refs = re.findall(r"\{([-A-Za-z_][A-Za-z0-9_-]*)\}", pattern)
    missing = sorted(set(refs) - names)
    if missing:
        raise UnsupportedDialect("lex", "LEX_MACRO_UNRESOLVED", f"undefined macros: {missing}", offset)
    square = 0
    escaped = False
    for index, char in enumerate(pattern):
        if escaped:
            escaped = False
        elif char == "\\":
            escaped = True
        elif char == "[":
            square += 1
        elif char == "]":
            square -= 1
            if square < 0:
                raise UnsupportedDialect("lex", "LEX_CLASS_INVALID", "unmatched ]", offset + index)
    if square:
        raise UnsupportedDialect("lex", "LEX_CLASS_UNCLOSED", "character class is unclosed", offset)
    return refs


def parse_lex(text: str) -> list[dict[str, Any]]:
    sections = text.split("%%")
    if len(sections) not in (2, 3):
        raise UnsupportedDialect("lex", "LEX_SECTION_COUNT", "definitions/rules boundary is required")
    if len(sections) == 3 and sections[2].strip():
        raise UnsupportedDialect("lex", "LEX_USER_CODE_UNSUPPORTED", "user-code section is unsupported")
    definitions, rules_text = sections[:2]
    options: list[str] = []
    macros: list[dict[str, Any]] = []
    offset = 0
    for raw in definitions.splitlines(keepends=True):
        line = raw.rstrip("\r\n")
        stripped = line.strip()
        if not stripped or stripped.startswith("/*"):
            offset += len(raw)
            continue
        if stripped.startswith("%option"):
            values = stripped.split()[1:]
            unknown = sorted(set(values) - LEX_OPTIONS)
            if not values or unknown:
                raise UnsupportedDialect("lex", "LEX_OPTION_UNSUPPORTED", f"unsupported options: {unknown}", offset)
            options.extend(values)
        else:
            match = re.match(r"\s*([-A-Za-z_][A-Za-z0-9_-]*)\s+(.+)$", line)
            if not match:
                raise UnsupportedDialect("lex", "LEX_DEFINITION_INVALID", "invalid macro definition", offset)
            macros.append({"family": "lex-macro", "name": match.group(1), "pattern": match.group(2), "start": offset + match.start(1), "end": offset + len(line)})
        offset += len(raw)
    names = {row["name"] for row in macros}
    if len(names) != len(macros) or not options or not macros:
        raise UnsupportedDialect("lex", "LEX_DEFINITIONS_INCOMPLETE", "options and unique macros are required")
    for row in macros:
        row["references"] = _lex_pattern(row["pattern"], names, row["start"])
    rules: list[dict[str, Any]] = []
    offset = len(definitions) + 2
    for raw in rules_text.splitlines(keepends=True):
        line = raw.rstrip("\r\n")
        stripped = line.strip()
        if stripped:
            split = re.search(r"\s+", stripped)
            if not split:
                raise UnsupportedDialect("lex", "LEX_ACTION_MISSING", "rule action is missing", offset)
            pattern, action = stripped[: split.start()], stripped[split.end() :].strip()
            if not action or not (action.startswith("{") and action.endswith("}") or action.startswith("return ") or action.startswith("/*") and action.endswith("*/")):
                raise UnsupportedDialect("lex", "LEX_ACTION_UNSUPPORTED", "rule action is not in supported grammar", offset)
            refs = _lex_pattern(pattern, names, offset)
            rules.append({"family": "lex-rule", "start": offset + len(line) - len(line.lstrip()), "end": offset + len(line), "references": refs})
        offset += len(raw)
    if not rules:
        raise UnsupportedDialect("lex", "LEX_RULES_EMPTY", "rules section is empty")
    return macros + rules


IDL_HEAD = re.compile(r"(?:(?:\[[^]]+\])\s*)?(?:partial\s+)?(?:callback\s+interface|interface\s+mixin|interface|dictionary|namespace|enum|typedef|callback)\s+[A-Za-z_][A-Za-z0-9_]*")
IDL_INCLUDES = re.compile(r"[A-Za-z_][A-Za-z0-9_]*\s+includes\s+[A-Za-z_][A-Za-z0-9_]*\s*;")


def _idl_trivia(text: str, cursor: int) -> int:
    while cursor < len(text):
        whitespace = re.match(r"\s+", text[cursor:])
        if whitespace:
            cursor += whitespace.end()
        elif text.startswith("//", cursor):
            end = text.find("\n", cursor + 2)
            cursor = len(text) if end < 0 else end + 1
        elif text.startswith("/*", cursor):
            end = text.find("*/", cursor + 2)
            if end < 0:
                raise UnsupportedDialect("webidl", "WEBIDL_COMMENT_UNTERMINATED", "comment is unclosed", cursor)
            cursor = end + 2
        else:
            break
    return cursor


def parse_webidl(text: str) -> list[dict[str, Any]]:
    units: list[dict[str, Any]] = []
    cursor = 0
    while (cursor := _idl_trivia(text, cursor)) < len(text):
        includes = IDL_INCLUDES.match(text, cursor)
        if includes:
            units.append({"family": "webidl-includes", "start": cursor, "end": includes.end()})
            cursor = includes.end()
            continue
        head = IDL_HEAD.match(text, cursor)
        if not head:
            raise UnsupportedDialect("webidl", "WEBIDL_INPUT_UNCONSUMED", "declaration head expected", cursor)
        start = cursor
        after_head = _idl_trivia(text, head.end())
        if after_head < len(text) and text[after_head] == ";":
            units.append({"family": "webidl-declaration", "start": start, "end": after_head + 1})
            cursor = after_head + 1
            continue
        if after_head >= len(text) or text[after_head] != "{":
            raise UnsupportedDialect("webidl", "WEBIDL_HEAD_GARBAGE", "only trivia may occur between head and body", head.end())
        opening = after_head
        depth = 1
        quote: str | None = None
        index = opening + 1
        while index < len(text) and depth:
            char = text[index]
            if quote:
                if char == quote and text[index - 1] != "\\":
                    quote = None
            elif char in "'\"":
                quote = char
            elif char == "{":
                depth += 1
            elif char == "}":
                depth -= 1
            index += 1
        if depth:
            raise UnsupportedDialect("webidl", "WEBIDL_BODY_UNCLOSED", "body is unclosed", opening)
        close = index - 1
        end = _idl_trivia(text, index)
        if end >= len(text) or text[end] != ";":
            raise UnsupportedDialect("webidl", "WEBIDL_SEMICOLON_MISSING", "declaration lacks trailing semicolon", end)
        body = text[opening + 1 : close]
        member_start = 0
        nested = 0
        member_quote: str | None = None
        members: list[dict[str, Any]] = []
        for local, char in enumerate(body):
            if member_quote:
                if char == member_quote and (local == 0 or body[local - 1] != "\\"):
                    member_quote = None
            elif char in "'\"":
                member_quote = char
            elif char in "([<":
                nested += 1
            elif char in ")]>":
                nested -= 1
                if nested < 0:
                    raise UnsupportedDialect("webidl", "WEBIDL_MEMBER_GROUP_INVALID", "unmatched member closer", opening + 1 + local)
            elif char == ";" and nested == 0:
                raw = body[member_start : local + 1]
                if raw.strip():
                    left = member_start + len(raw) - len(raw.lstrip())
                    right = local + 1
                    if "???" in raw or "@@@" in raw or re.search(r"[^\w\s,;:.?=<>()[\]{}\"'/*+&|!-]", raw):
                        raise UnsupportedDialect("webidl", "WEBIDL_MEMBER_UNSUPPORTED", "member contains unsupported syntax", opening + 1 + left)
                    members.append({"family": "webidl-member", "start": opening + 1 + left, "end": opening + 1 + right})
                member_start = local + 1
        if body[member_start:].strip() or nested or member_quote:
            raise UnsupportedDialect("webidl", "WEBIDL_MEMBER_UNCLOSED", "member is not fully consumed", opening + 1 + member_start)
        units.append({"family": "webidl-declaration", "start": start, "end": end + 1})
        units.extend(members)
        cursor = end + 1
    if not units:
        raise UnsupportedDialect("webidl", "WEBIDL_EMPTY", "no declarations")
    return units
