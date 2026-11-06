"""One byte-state tokenizer for Bikeshed/HTML source and every occurrence edge."""

from __future__ import annotations

import html
import re
from dataclasses import dataclass
from typing import Any, Iterator

from common import fail, sha256


NAME = re.compile(rb"[A-Za-z][A-Za-z0-9:_-]*")
ATTRIBUTE = re.compile(rb"([A-Za-z_:][-A-Za-z0-9_:.]*)\s*=\s*(?:\"([^\"]*)\"|'([^']*)'|([^\s>]+))")
RAW_TEXT = {"script", "style", "xmp"}
VOID = {"area", "base", "br", "col", "embed", "hr", "img", "input", "link", "meta", "param", "source", "track", "wbr"}
OPTIONAL_SAME_CLOSE = {"li", "dt", "dd", "p", "rt", "rp", "option", "thead", "tbody", "tfoot", "tr", "td", "th"}
P_CLOSE_START = {"address", "article", "aside", "blockquote", "div", "dl", "fieldset", "footer", "form", "h1", "h2", "h3", "h4", "h5", "h6", "header", "hr", "menu", "nav", "ol", "p", "pre", "section", "table", "ul"}
DEPENDENCY = re.compile(rb"\[\[(!?)([^\]|\s#]+)(?:#[^\]|]+)?(?:\|[^\]]+)?\]\]")


@dataclass(frozen=True)
class Element:
    serial: int
    name: str
    start: int
    open_end: int
    attributes: dict[str, str]


@dataclass(frozen=True)
class Token:
    kind: str
    start: int
    end: int
    raw: bytes
    ancestors: tuple[Element, ...]
    name: str | None = None
    attributes: dict[str, str] | None = None
    edit: str = "CURRENT"

    def receipt(self) -> dict[str, Any]:
        return {
            "kind": self.kind,
            "start": self.start,
            "end": self.end,
            "sha256": sha256(self.raw),
            "ancestors": [element.serial for element in self.ancestors],
            "edit": self.edit,
        }


def _edit(stack: list[Element]) -> str:
    if any(element.name == "del" for element in stack):
        return "DELETED"
    return "CURRENT"  # real <ins> is current source, not history


def _implicit_start(stack: list[Element], name: str) -> None:
    groups = []
    if name == "li": groups.append({"li"})
    if name in {"dt", "dd"}: groups.append({"dt", "dd"})
    if name in {"thead", "tbody", "tfoot"}: groups.append({"thead", "tbody", "tfoot"})
    if name == "tr": groups.append({"tr"})
    if name in {"td", "th"}: groups.append({"td", "th"})
    if name in {"h1", "h2", "h3", "h4", "h5", "h6"}: groups.append({"h1", "h2", "h3", "h4", "h5", "h6"})
    if name in P_CLOSE_START: groups.append({"p"})
    for group in groups:
        index = next((i for i in range(len(stack) - 1, -1, -1) if stack[i].name in group), None)
        if index is not None:
            del stack[index:]


def _tag(data: bytes, start: int) -> tuple[str, bool, bool, dict[str, str], int] | None:
    if data[start : start + 1] != b"<" or data.startswith((b"<!--", b"<<"), start):
        return None
    cursor = start + 1
    closing = cursor < len(data) and data[cursor : cursor + 1] == b"/"
    cursor += int(closing)
    while cursor < len(data) and data[cursor : cursor + 1].isspace():
        cursor += 1
    match = NAME.match(data, cursor)
    if match is None:
        return None
    name = match.group().decode("ascii").lower()
    quote: int | None = None
    cursor = match.end()
    while cursor < len(data):
        byte = data[cursor]
        if quote is not None:
            if byte == quote:
                quote = None
        elif byte in (34, 39):
            quote = byte
        elif byte == 62:
            body = data[match.end() : cursor]
            attrs: dict[str, str] = {}
            for found in ATTRIBUTE.finditer(body):
                raw = next(group for group in found.groups()[1:] if group is not None)
                attrs[found.group(1).decode("ascii").lower()] = html.unescape(raw.decode("utf-8"))
            # Bikeshed's <wpt pathprefix=.../> uses the trailing slash as part of
            # the unquoted path value and still has a closing </wpt>.
            return name, closing, (body.rstrip().endswith(b"/") and name != "wpt") or name in VOID, attrs, cursor + 1
        cursor += 1
    fail("TAG_UNTERMINATED", "source tag has no quote-aware terminator", start=start)


def scan(data: bytes) -> list[Token]:
    tokens: list[Token] = []
    stack: list[Element] = []
    cursor = 0
    serial = 0
    while cursor < len(data):
        railroad = next(
            (
                element
                for element in reversed(stack)
                if "railroad" in element.attributes.get("class", "").lower().split()
            ),
            None,
        )
        if railroad is not None and data[cursor : cursor + 1] == b"<":
            closing = re.match(rb"</" + re.escape(railroad.name.encode()) + rb"\s*>", data[cursor:], re.I)
            if closing is None:
                terminator = re.compile(rb"</" + re.escape(railroad.name.encode()) + rb"\s*>", re.I).search(data, cursor + 1)
                if terminator is None:
                    fail("RAILROAD_UNTERMINATED", "railroad source carrier lacks its closing tag", start=railroad.start)
                tokens.append(Token("text", cursor, terminator.start(), data[cursor : terminator.start()], tuple(stack), edit=_edit(stack)))
                cursor = terminator.start()
                continue
        if stack and stack[-1].name in RAW_TEXT and not re.match(rb"</" + re.escape(stack[-1].name.encode()) + rb"\s*>", data[cursor:], re.I):
            pattern = re.compile(rb"</" + re.escape(stack[-1].name.encode()) + rb"\s*>", re.I)
            found = pattern.search(data, cursor)
            if found is None:
                fail("RAW_TEXT_UNTERMINATED", "raw-text element lacks its closing tag", tag=stack[-1].name)
            if found.start() > cursor:
                tokens.append(Token("raw_text", cursor, found.start(), data[cursor : found.start()], tuple(stack), edit=_edit(stack)))
            cursor = found.start()
            continue
        if data.startswith(b"<!--", cursor):
            end = data.find(b"-->", cursor + 4)
            if end < 0:
                fail("COMMENT_UNTERMINATED", "source comment has no terminator", start=cursor)
            end += 3
            tokens.append(Token("comment", cursor, end, data[cursor:end], tuple(stack), edit=_edit(stack)))
            cursor = end
            continue
        if data.startswith(b"<<", cursor):
            end = data.find(b">>", cursor + 2)
            if end < 0:
                fail("REFERENCE_UNTERMINATED", "Bikeshed reference has no terminator", start=cursor)
            end += 2
            tokens.append(Token("bikeshed_reference", cursor, end, data[cursor:end], tuple(stack), edit=_edit(stack)))
            cursor = end
            continue
        if data[cursor : cursor + 1] == b"<":
            parsed = _tag(data, cursor)
            if parsed is not None:
                name, closing, self_closing, attrs, end = parsed
                if closing:
                    index = next((i for i in range(len(stack) - 1, -1, -1) if stack[i].name == name), None)
                    ancestors = tuple(stack)
                    tokens.append(Token("end_tag", cursor, end, data[cursor:end], ancestors, name, attrs, _edit(stack)))
                    if index is not None:
                        del stack[index:]
                else:
                    _implicit_start(stack, name)
                    ancestors = tuple(stack)
                    tokens.append(Token("start_tag", cursor, end, data[cursor:end], ancestors, name, attrs, _edit(stack)))
                    if not self_closing:
                        serial += 1
                        stack.append(Element(serial, name, cursor, end, attrs))
                cursor = end
                continue
        next_special = data.find(b"<", cursor + 1)
        end = len(data) if next_special < 0 else next_special
        tokens.append(Token("text", cursor, end, data[cursor:end], tuple(stack), edit=_edit(stack)))
        cursor = end
    # Bikeshed sources use HTML optional end tags; remaining ancestry is source
    # state, not an XML-well-formedness claim.
    return tokens


def occurrences(data: bytes, tokens: list[Token]) -> list[dict[str, Any]]:
    result: list[dict[str, Any]] = []
    for token in tokens:
        for match in DEPENDENCY.finditer(token.raw):
            start, end = token.start + match.start(), token.start + match.end()
            result.append(
                {
                    "key": match.group(2).decode("ascii").upper(),
                    "normative": match.group(1) == b"!",
                    "start": start,
                    "end": end,
                    "sha256": sha256(data[start:end]),
                    "source_state": "COMMENT" if token.kind == "comment" else token.edit,
                    "ancestors": [element.serial for element in token.ancestors],
                }
            )
    return result


def inner_tokens(tokens: list[Token], element: Element) -> list[Token]:
    return [token for token in tokens if any(parent.serial == element.serial for parent in token.ancestors)]


def decoded_map(
    tokens: Iterator[Token],
    *,
    markup_literals: bool = False,
    comment_literals: bool = False,
) -> tuple[str, list[dict[str, Any]]]:
    text: list[str] = []
    mapping: list[dict[str, Any]] = []
    cursor = 0

    def append(value: str, source_start: int, source_end: int, raw: bytes, transform: str) -> None:
        nonlocal cursor
        if not value:
            return
        text.append(value)
        mapping.append(
            {
                "text_start": cursor,
                "text_end": cursor + len(value),
                "source_start": source_start,
                "source_end": source_end,
                "source_sha256": sha256(raw),
                "transform": transform,
            }
        )
        cursor += len(value)

    for token in tokens:
        allowed = {"text", "raw_text", "bikeshed_reference"}
        if markup_literals:
            allowed.update({"start_tag", "end_tag"})
        if comment_literals:
            allowed.add("comment")
        if token.kind not in allowed or token.edit == "DELETED":
            continue
        raw_text = token.raw.decode("utf-8")
        if token.kind != "text":
            append(raw_text, token.start, token.end, token.raw, "IDENTITY_UTF8")
            continue
        source_cursor = 0
        for entity in re.finditer(r"&(?:#[xX][0-9A-Fa-f]+|#\d+|[A-Za-z][A-Za-z0-9]+);?", raw_text):
            decoded = html.unescape(entity.group())
            if decoded == entity.group():
                continue
            if entity.start() > source_cursor:
                raw = raw_text[source_cursor : entity.start()].encode("utf-8")
                start = token.start + len(raw_text[:source_cursor].encode("utf-8"))
                append(raw_text[source_cursor : entity.start()], start, start + len(raw), raw, "IDENTITY_UTF8")
            raw = entity.group().encode("utf-8")
            start = token.start + len(raw_text[: entity.start()].encode("utf-8"))
            append(decoded, start, start + len(raw), raw, "HTML_ENTITY_DECODE")
            source_cursor = entity.end()
        if source_cursor < len(raw_text):
            raw = raw_text[source_cursor:].encode("utf-8")
            start = token.start + len(raw_text[:source_cursor].encode("utf-8"))
            append(raw_text[source_cursor:], start, token.end, raw, "IDENTITY_UTF8")
    return "".join(text), mapping


def validate_map(data: bytes, text: str, mapping: list[dict[str, Any]]) -> None:
    pieces: list[str] = []
    expected = 0
    for item in mapping:
        if item["text_start"] != expected:
            fail("SOURCE_MAP_GAP", "decoded source map is not contiguous", expected=expected, actual=item["text_start"])
        raw = data[item["source_start"] : item["source_end"]]
        if sha256(raw) != item["source_sha256"]:
            fail("SOURCE_MAP_DRIFT", "source map span digest differs")
        decoded = raw.decode("utf-8")
        if item["transform"] == "HTML_ENTITY_DECODE":
            decoded = html.unescape(decoded)
        elif item["transform"] != "IDENTITY_UTF8":
            fail("SOURCE_MAP_TRANSFORM_UNKNOWN", "unknown reversible source-map transform", transform=item["transform"])
        pieces.append(decoded)
        expected = item["text_end"]
    if "".join(pieces) != text:
        fail("SOURCE_MAP_TEXT_MISMATCH", "source map does not reproduce decoded carrier text")
