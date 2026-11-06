"""Stage 2: source-aware carrier and algorithm extraction with exact byte spans."""

from __future__ import annotations

import html
import re
from dataclasses import dataclass
from pathlib import Path
from typing import Any

from common import collapse, line_of, reject_candidate_schema_use, sha256


CLASS_KINDS = ("propdef", "descdef", "prod", "railroad", "idl")
RAW_TEXT = {"xmp"}
MARKUP_NAMES = {
    "a", "abbr", "aside", "b", "blockquote", "br", "code", "dd", "del",
    "dfn", "div", "dl", "dt", "em", "figcaption", "figure", "h1", "h2",
    "h3", "h4", "h5", "h6", "i", "img", "ins", "li", "mark", "ol", "p",
    "pre", "prop", "q", "section", "small", "span", "strong", "sub", "sup", "table",
    "tbody", "td", "tfoot", "th", "thead", "tr", "ul", "var", "wpt", "xmp",
    "css", "issue", "l", "production",
}
GRAMMAR_CONTEXT = (
    "formal grammar",
    "the productions are",
    "defines the following productions",
    "defined as follows",
    "syntax for the",
    "syntax is:",
    "syntax is described",
    "production defined below",
    "following new definitions",
    "lexical scanner",
    "tokenizer",
)
NAME = re.compile(rb"[A-Za-z][A-Za-z0-9:-]*")
ATTRIBUTE = re.compile(rb"([A-Za-z_:][-A-Za-z0-9_:.]*)\s*=\s*(?:\"([^\"]*)\"|'([^']*)'|([^\s>]+))")
COMMENT_END = b"-->"

NO_GRAMMAR_DECISIONS = {
    "css-2026": (
        "ADJUDICATED_SCOPE_ONLY",
        "annual CSS snapshot defines the scope of CSS; it owns no grammar carrier",
    ),
    "css-print": (
        "ADJUDICATED_OBSOLETE_PROFILE_NO_OWN_GRAMMAR",
        "abandoned print profile constrains CSS2/page behavior and owns no grammar carrier",
    ),
}


@dataclass(frozen=True)
class Tag:
    start: int
    end: int
    name: str
    closing: bool
    self_closing: bool
    attributes: dict[str, str]


def parse_tag(data: bytes, start: int) -> Tag | None:
    if start >= len(data) or data[start : start + 1] != b"<" or data.startswith(b"<!--", start):
        return None
    cursor = start + 1
    closing = False
    if cursor < len(data) and data[cursor : cursor + 1] == b"/":
        closing = True
        cursor += 1
    while cursor < len(data) and data[cursor : cursor + 1].isspace():
        cursor += 1
    match = NAME.match(data, cursor)
    if match is None:
        return None
    name = match.group(0).decode("ascii").lower()
    quote: int | None = None
    cursor = match.end()
    while cursor < len(data):
        byte = data[cursor]
        if quote is not None:
            if byte == quote:
                quote = None
        elif byte in (ord("'"), ord('"')):
            quote = byte
        elif byte == ord(">"):
            raw = data[match.end() : cursor]
            attrs: dict[str, str] = {}
            for attribute in ATTRIBUTE.finditer(raw):
                value = next(group for group in attribute.groups()[1:] if group is not None)
                attrs[attribute.group(1).decode().lower()] = html.unescape(value.decode("utf-8", errors="replace"))
            return Tag(
                start=start,
                end=cursor + 1,
                name=name,
                closing=closing,
                self_closing=raw.rstrip().endswith(b"/"),
                attributes=attrs,
            )
        cursor += 1
    raise ValueError(f"unterminated tag at byte {start}")


def closing_tag(data: bytes, opening: Tag) -> Tag:
    # Carrier bodies legitimately contain comparison text such as ``x < row``.
    # Scanning arbitrary '<' as markup would consume through the real closing tag.
    # The supported carrier families are non-nesting in the pinned source dialects,
    # so their first exact closing tag is authoritative.
    match = re.search(rb"</" + re.escape(opening.name.encode()) + rb"\s*>", data[opening.end :], re.I)
    if match is not None:
        start = opening.end + match.start()
        tag = parse_tag(data, start)
        assert tag is not None
        return tag
    raise ValueError(f"unclosed <{opening.name}> at byte {opening.start}")


def class_tokens(tag: Tag) -> set[str]:
    return {token.lower() for token in tag.attributes.get("class", "").split()}


def text_segments(
    data: bytes,
    start: int,
    end: int,
    raw_text: bool,
    preserve_comment_literals: bool = False,
) -> tuple[str, list[dict[str, Any]]]:
    if raw_text:
        value = html.unescape(data[start:end].decode("utf-8", errors="strict"))
        return value, [{"start": start, "end": end, "text": value}]
    pieces: list[str] = []
    segments: list[dict[str, Any]] = []
    cursor = start
    while cursor < end:
        opening = data.find(b"<", cursor, end)
        if opening < 0:
            opening = end
        if opening > cursor:
            value = html.unescape(data[cursor:opening].decode("utf-8", errors="strict"))
            pieces.append(value)
            segments.append({"start": cursor, "end": opening, "text": value})
        if opening == end:
            break
        if data.startswith(b"<!--", opening) and not preserve_comment_literals:
            comment_end = data.find(COMMENT_END, opening + 4, end)
            if comment_end < 0:
                raise ValueError(f"unclosed comment in carrier body at byte {opening}")
            cursor = comment_end + len(COMMENT_END)
            continue
        if data.startswith(b"<<", opening):
            reference_end = data.find(b">>", opening + 2, end)
            if reference_end >= 0:
                reference_end += 2
                value = data[opening:reference_end].decode("utf-8", errors="strict")
                pieces.append(value)
                segments.append({"start": opening, "end": reference_end, "text": value})
                cursor = reference_end
                continue
        tag = parse_tag(data, opening)
        if tag is None or tag.end > end or tag.name not in MARKUP_NAMES:
            value = "<"
            pieces.append(value)
            segments.append({"start": opening, "end": opening + 1, "text": value})
            cursor = opening + 1
        else:
            cursor = tag.end
    return "".join(pieces), segments


def grammar_like(text: str, context: str) -> bool:
    lowered = context.lower()
    contextual = any(marker in lowered for marker in GRAMMAR_CONTEXT)
    legacy = bool(re.search(r"(?m)^\s*[-A-Za-z_][\w-]*\s*\n?\s*:\s*", text))
    equals = bool(re.search(r"(?m)^\s*(?:@?[-A-Za-z][\w-]*|<[^>]+>|<<[^>]+>>)\s*=", text))
    scanner = "%%" in text or "%option" in text
    return equals or (contextual and (legacy or scanner))


def disposition(classes: set[str], ancestors: list[tuple[str, set[str]]], context: str, body: bytes) -> tuple[str, str]:
    ancestor_classes = set().union(*(item[1] for item in ancestors)) if ancestors else set()
    lowered = context.lower()
    local_example = max(lowered.rfind("class=example"), lowered.rfind("class='example'"), lowered.rfind('class="example"'))
    if "example" in ancestor_classes and local_example > lowered.rfind("</div"):
        return "EXAMPLE", "ancestor class=example"
    if "non-normative" in lowered[-800:]:
        return "NON_NORMATIVE", "nearby source explicitly says non-normative"
    if b"<del" in body or b"<ins" in body:
        return "HISTORICAL", "change-history markup"
    if "extract" in classes and "non-normative" in lowered[-1200:]:
        return "NON_NORMATIVE", "extract marked by non-normative context"
    return "NORMATIVE", "recognized normative source carrier"


def carrier_kind(tag: Tag, body_text: str, context: str) -> tuple[str | None, str | None]:
    classes = class_tokens(tag)
    for kind in CLASS_KINDS:
        if kind in classes:
            return kind, "bikeshed-class"
    if "lang-webidl" in classes and "def" in classes:
        return "idl", "webidl-definition"
    if "media-feature" in classes:
        return "legacy-row", "legacy-media-feature"
    if "algorithm" in classes or "algorithm" in tag.attributes:
        return "algorithm", "algorithm-class"
    if tag.name == "pre" and grammar_like(body_text, context):
        return "legacy-grammar", "source-context-adapter"
    return None, None


def scan_document(spec: dict[str, Any], root: Path) -> list[dict[str, Any]]:
    path = root / spec["path"]
    data = path.read_bytes()
    if len(data) != spec["bytes"] or sha256(data) != spec["sha256"]:
        raise ValueError(f"source pin drifted: {spec['path']}")
    records: list[dict[str, Any]] = []
    cursor = 0
    ancestors: list[tuple[str, set[str]]] = []
    ordinals: dict[str, int] = {}
    while cursor < len(data):
        start = data.find(b"<", cursor)
        if start < 0:
            break
        if data.startswith(b"<!--", start):
            end = data.find(COMMENT_END, start + 4)
            if end < 0:
                raise ValueError(f"unclosed comment in {spec['path']} at byte {start}")
            cursor = end + len(COMMENT_END)
            continue
        tag = parse_tag(data, start)
        if tag is None:
            cursor = start + 1
            continue
        if tag.closing:
            for index in range(len(ancestors) - 1, -1, -1):
                if ancestors[index][0] == tag.name:
                    del ancestors[index:]
                    break
            cursor = tag.end
            continue
        if tag.self_closing:
            cursor = tag.end
            continue
        classes = class_tokens(tag)
        potential = (
            tag.name in {"pre", "xmp"}
            or bool(classes.intersection(CLASS_KINDS))
            or ("lang-webidl" in classes and "def" in classes)
            or "media-feature" in classes
            or "algorithm" in classes
            or "algorithm" in tag.attributes
        )
        if not potential:
            ancestors.append((tag.name, classes))
            cursor = tag.end
            continue
        end_tag = closing_tag(data, tag)
        body_start, body_end = tag.end, end_tag.start
        text, segments = text_segments(
            data,
            body_start,
            body_end,
            tag.name in RAW_TEXT,
            "railroad" in classes,
        )
        context_start = max(0, tag.start - 1600)
        context = html.unescape(data[context_start:tag.start].decode("utf-8", errors="replace"))
        kind, dialect = carrier_kind(tag, text, context)
        if kind is not None:
            state, reason = disposition(classes, ancestors, context, data[body_start:body_end])
            ordinals[kind] = ordinals.get(kind, 0) + 1
            raw = data[tag.start : end_tag.end]
            records.append(
                {
                    "id": f"{spec['id']}:{kind}:{ordinals[kind]}",
                    "spec_id": spec["id"],
                    "path": spec["path"],
                    "kind": kind,
                    "dialect": dialect,
                    "tag": tag.name,
                    "classes": sorted(classes),
                    "ordinal": ordinals[kind],
                    "start": tag.start,
                    "end": end_tag.end,
                    "body_start": body_start,
                    "body_end": body_end,
                    "line": line_of(data, tag.start),
                    "raw_bytes": len(raw),
                    "raw_sha256": sha256(raw),
                    "body_sha256": sha256(data[body_start:body_end]),
                    "text": text,
                    "normalized_text": collapse(text),
                    "text_segments": segments,
                    "normativity": state,
                    "classification_reason": reason,
                    "terminal_state": "CLASSIFIED" if state != "NORMATIVE" else "EXTRACTED",
                }
            )
            cursor = end_tag.end
            continue
        ancestors.append((tag.name, class_tokens(tag)))
        cursor = tag.end
    return records


def capture(scope: dict[str, Any], csswg: Path) -> dict[str, Any]:
    reject_candidate_schema_use(scope)
    records = []
    for spec in scope["specs"]:
        records.extend(scan_document(spec, csswg))
    counts: dict[str, int] = {}
    dispositions: dict[str, int] = {}
    for record in records:
        counts[record["kind"]] = counts.get(record["kind"], 0) + 1
        dispositions[record["normativity"]] = dispositions.get(record["normativity"], 0) + 1
    mapped = {record["spec_id"] for record in records if record["normativity"] == "NORMATIVE"}
    no_grammar = []
    blockers = []
    for spec in scope["specs"]:
        if spec["id"] in mapped:
            continue
        decision = NO_GRAMMAR_DECISIONS.get(spec["id"])
        if decision:
            no_grammar.append(
                {
                    "spec_id": spec["id"],
                    "terminal_state": decision[0],
                    "reason": decision[1],
                }
            )
        else:
            no_grammar.append({"spec_id": spec["id"], "terminal_state": "UNPROVEN"})
            blockers.append({"code": "SPEC_WITHOUT_NORMATIVE_CARRIER_DECISION", "spec_id": spec["id"]})
    return {
        "schema_version": 2,
        "stage": "source-extraction",
        "candidate_input_policy": "OBSERVATION_ONLY_FORBID_SCHEMA_OR_TYPE_INPUT",
        "scope_sha256": sha256(__import__("common").encoded_json(scope)),
        "counts": dict(sorted(counts.items())),
        "normativity_counts": dict(sorted(dispositions.items())),
        "records": records,
        "no_grammar_decisions": no_grammar,
        "blockers": blockers,
    }
