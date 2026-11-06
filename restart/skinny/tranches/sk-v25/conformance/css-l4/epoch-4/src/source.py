"""One byte tokenizer with inherited edit/normativity state and source occurrences."""

from __future__ import annotations

import html
import re
from dataclasses import dataclass
from typing import Any

from common import encode, fail, sha256


NAME = re.compile(rb"[A-Za-z][A-Za-z0-9:_-]*")
ATTRIBUTE = re.compile(rb"([A-Za-z_:][-A-Za-z0-9_:.]*)\s*=\s*(?:\"([^\"]*)\"|'([^']*)'|([^\s>]+))")
BIKESHED_REF = re.compile(rb"\[\[(!?)([^\]|\s#]+)(?:#[^\]|]+)?(?:\|[^\]]+)?\]\]")
RENDERED_REF = re.compile(rb"\[([^\]\s]+)\]\s*<!--\{\{(!?)([^}\s#|]+)(?:#[^}|]+)?(?:\|[^}]+)?\}\}-->")
RAW_TEXT = {"script", "style", "xmp"}
VOID = {"area", "base", "br", "col", "embed", "hr", "img", "input", "link", "meta", "param", "source", "track", "wbr"}
P_CLOSE_START = {"address", "article", "aside", "blockquote", "div", "dl", "fieldset", "footer", "form", "h1", "h2", "h3", "h4", "h5", "h6", "header", "hr", "menu", "nav", "ol", "p", "pre", "section", "table", "ul"}
NON_NORMATIVE_CLASSES = {"note", "example", "informative", "non-normative"}


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


def classes(item: Token | Element) -> set[str]:
    attributes = item.attributes or {}
    return {part.lower() for part in attributes.get("class", "").split()}


def inherited_state(token: Token, *, comment_is_comment: bool = True) -> tuple[str, str]:
    lineage = (*token.ancestors, token)
    if comment_is_comment and token.kind == "comment":
        return "COMMENT", "source comment"
    if any(item.name == "del" for item in lineage):
        return "DELETED", "inside current <del> ancestry"
    # <ins> is current content. Own classes and every ancestor use the same rule.
    found = sorted(set().union(*(classes(item) for item in lineage)) & NON_NORMATIVE_CLASSES)
    if found:
        return "NON_NORMATIVE", "inherited source class: " + ",".join(found)
    return "NORMATIVE", "current source outside inherited non-normative state"


def _implicit_start(stack: list[Element], name: str) -> None:
    groups: list[set[str]] = []
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


def parse_attributes(raw: bytes) -> dict[str, str]:
    result: dict[str, str] = {}
    for found in ATTRIBUTE.finditer(raw):
        value = next(group for group in found.groups()[1:] if group is not None)
        result[found.group(1).decode("ascii").lower()] = html.unescape(value.decode("utf-8"))
    return result


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
            # Bikeshed WPT directives use slash-shaped attribute endings while
            # still having source bodies, so only ordinary elements self-close.
            self_closing = (body.rstrip().endswith(b"/") and name not in {"wpt", "wpt-rest"}) or name in VOID
            return name, closing, self_closing, parse_attributes(body), cursor + 1
        cursor += 1
    fail("TAG_UNTERMINATED", "source tag has no quote-aware terminator", start=start)


def scan(data: bytes) -> list[Token]:
    tokens: list[Token] = []
    stack: list[Element] = []
    cursor = 0
    serial = 0
    while cursor < len(data):
        railroad = next((item for item in reversed(stack) if "railroad" in classes(item)), None)
        if railroad is not None and data[cursor : cursor + 1] == b"<":
            closing = re.match(rb"</" + re.escape(railroad.name.encode()) + rb"\s*>", data[cursor:], re.I)
            if closing is None:
                terminator = re.compile(rb"</" + re.escape(railroad.name.encode()) + rb"\s*>", re.I).search(data, cursor + 1)
                if terminator is None:
                    fail("RAILROAD_UNTERMINATED", "railroad carrier has no closing tag", start=railroad.start)
                tokens.append(Token("text", cursor, terminator.start(), data[cursor : terminator.start()], tuple(stack)))
                cursor = terminator.start()
                continue
        if stack and stack[-1].name in RAW_TEXT and not re.match(rb"</" + re.escape(stack[-1].name.encode()) + rb"\s*>", data[cursor:], re.I):
            found = re.compile(rb"</" + re.escape(stack[-1].name.encode()) + rb"\s*>", re.I).search(data, cursor)
            if found is None:
                fail("RAW_TEXT_UNTERMINATED", "raw-text element lacks closing tag", tag=stack[-1].name)
            if found.start() > cursor:
                tokens.append(Token("raw_text", cursor, found.start(), data[cursor : found.start()], tuple(stack)))
            cursor = found.start()
            continue
        if data.startswith(b"<!--", cursor):
            end = data.find(b"-->", cursor + 4)
            if end < 0:
                fail("COMMENT_UNTERMINATED", "source comment lacks terminator", start=cursor)
            end += 3
            tokens.append(Token("comment", cursor, end, data[cursor:end], tuple(stack)))
            cursor = end
            continue
        if data.startswith(b"<<", cursor):
            end = data.find(b">>", cursor + 2)
            if end < 0:
                fail("REFERENCE_UNTERMINATED", "Bikeshed reference lacks terminator", start=cursor)
            end += 2
            tokens.append(Token("bikeshed_reference", cursor, end, data[cursor:end], tuple(stack)))
            cursor = end
            continue
        if data[cursor : cursor + 1] == b"<":
            parsed = _tag(data, cursor)
            if parsed is not None:
                name, closing, self_closing, attributes, end = parsed
                if closing:
                    ancestors = tuple(stack)
                    tokens.append(Token("end_tag", cursor, end, data[cursor:end], ancestors, name, attributes))
                    index = next((i for i in range(len(stack) - 1, -1, -1) if stack[i].name == name), None)
                    if index is not None:
                        del stack[index:]
                else:
                    _implicit_start(stack, name)
                    tokens.append(Token("start_tag", cursor, end, data[cursor:end], tuple(stack), name, attributes))
                    if not self_closing:
                        serial += 1
                        stack.append(Element(serial, name, cursor, end, attributes))
                cursor = end
                continue
        next_special = data.find(b"<", cursor + 1)
        end = len(data) if next_special < 0 else next_special
        tokens.append(Token("text", cursor, end, data[cursor:end], tuple(stack)))
        cursor = end
    return tokens


def token_at(tokens: list[Token], offset: int) -> Token:
    return next((token for token in tokens if token.start <= offset < token.end), tokens[-1])


def dependency_occurrences(spec_id: str, path: str, data: bytes, tokens: list[Token]) -> list[dict[str, Any]]:
    result: list[dict[str, Any]] = []
    rendered_spans: list[tuple[int, int]] = []
    for match in RENDERED_REF.finditer(data):
        rendered_spans.append(match.span())
        token = token_at(tokens, match.start())
        state, reason = inherited_state(token, comment_is_comment=False)
        key = match.group(3).decode("ascii").upper()
        row = {
            "spec_id": spec_id,
            "path": path,
            "syntax": "RENDERED_BIBLIOGRAPHY",
            "key": key,
            "normative": match.group(2) == b"!",
            "start": match.start(),
            "end": match.end(),
            "sha256": sha256(match.group()),
            "source_state": state,
            "state_reason": reason,
        }
        row["id"] = "dependency::" + sha256(encode(row))
        result.append(row)
    for token in tokens:
        for match in BIKESHED_REF.finditer(token.raw):
            start, end = token.start + match.start(), token.start + match.end()
            if any(a <= start and end <= b for a, b in rendered_spans):
                continue
            state, reason = inherited_state(token)
            row = {
                "spec_id": spec_id,
                "path": path,
                "syntax": "BIKESHED",
                "key": match.group(2).decode("ascii").upper(),
                "normative": match.group(1) == b"!",
                "start": start,
                "end": end,
                "sha256": sha256(data[start:end]),
                "source_state": state,
                "state_reason": reason,
            }
            row["id"] = "dependency::" + sha256(encode(row))
            result.append(row)
    identities = [row["id"] for row in result]
    if len(identities) != len(set(identities)):
        fail("DEPENDENCY_DUPLICATE", "dependency occurrence identity is duplicated", spec_id=spec_id)
    return sorted(result, key=lambda row: (row["start"], row["end"], row["key"]))


def element_body(opening: Token, tokens: list[Token], data: bytes) -> tuple[bytes, int]:
    element = next(
        (
            ancestor
            for token in tokens
            if token.start >= opening.end
            for ancestor in token.ancestors
            if ancestor.start == opening.start
        ),
        None,
    )
    if element is None:
        fail("ELEMENT_BODY_MISSING", "source element has no body ancestry", start=opening.start, name=opening.name)
    closing = next(
        (
            token
            for token in tokens
            if token.kind == "end_tag"
            and token.name == element.name
            and any(parent.serial == element.serial for parent in token.ancestors)
        ),
        None,
    )
    if closing is None:
        fail("ELEMENT_CLOSE_MISSING", "source element has no exact closing tag", start=opening.start, name=opening.name)
    return data[opening.end : closing.start], closing.end


def anchor_for(opening: Token, tokens: list[Token]) -> str | None:
    for ancestor in reversed(opening.ancestors):
        if ancestor.attributes.get("id"):
            return ancestor.attributes["id"]
    prior = [
        token
        for token in tokens
        if token.kind == "start_tag"
        and token.start < opening.start
        and token.name in {"h1", "h2", "h3", "h4", "h5", "h6"}
        and token.attributes
        and token.attributes.get("id")
    ]
    return prior[-1].attributes["id"] if prior else None


def wpt_directives(spec: dict[str, Any], data: bytes, tokens: list[Token]) -> list[dict[str, Any]]:
    directives: list[dict[str, Any]] = []
    known = {"wpt", "wpt-rest"}
    for opening in [token for token in tokens if token.kind == "start_tag" and token.name and token.name.startswith("wpt")]:
        state, reason = inherited_state(opening)
        if opening.name not in known:
            directives.append({"kind": "UNKNOWN", "tag": opening.name, "start": opening.start, "source_state": state, "status": "BLOCKED_UNKNOWN_DIRECTIVE"})
            continue
        if state != "NORMATIVE":
            directives.append({"kind": opening.name.upper().replace("-", "_"), "start": opening.start, "source_state": state, "status": "EXCLUDED", "reason": reason})
            continue
        attributes = opening.attributes or {}
        prefix = attributes.get("pathprefix") or spec.get("wpt_path_prefix") or f"css/{spec['id']}"
        prefix = prefix.strip("/") + "/"
        anchor = anchor_for(opening, tokens)
        if opening.name == "wpt-rest":
            directives.append({"kind": "WPT_REST", "start": opening.start, "anchor": anchor, "prefix": prefix, "status": "DECLARED"})
            continue
        body, _end = element_body(opening, tokens, data)
        text = re.sub(rb"<[^>]*>", b"", body).decode("utf-8", errors="strict")
        names = [" ".join(line.split()) for line in text.splitlines() if line.split() and not line.lstrip().startswith("#")]
        if not names:
            directives.append({"kind": "WPT", "start": opening.start, "anchor": anchor, "prefix": prefix, "status": "NO_DIRECT_TESTS", "reason": attributes.get("title", "empty WPT block")})
            continue
        directives.append({"kind": "WPT", "start": opening.start, "anchor": anchor, "prefix": prefix, "status": "DECLARED", "names": names})
    return directives
