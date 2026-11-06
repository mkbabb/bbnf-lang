"""Structural carrier classification with reversible field and carrier source maps."""

from __future__ import annotations

import re
from pathlib import Path
from typing import Any

from common import INPUTS, fail, line_of, load, require_clean, sha256
from scanner import Element, Token, decoded_map, scan, validate_map


SOURCE_KINDS = ("propdef", "descdef", "prod", "railroad", "idl")
FIELD_LABELS = (
    "logical property group", "animation type", "computed value", "canonical order",
    "new values", "applies to", "percentages", "used value", "animatable",
    "inherited", "initial", "media", "for", "type", "on", "name", "value",
)
FIELD = re.compile(rf"(?:^|\s)(?P<label>{'|'.join(re.escape(label) for label in FIELD_LABELS)}):", re.I)


def classes(token: Token | Element) -> set[str]:
    attrs = token.attributes or {}
    return {value.lower() for value in attrs.get("class", "").split()}


def _element(opening: Token, tokens: list[Token]) -> Element | None:
    return next(
        (
            ancestor
            for token in tokens
            if token.start >= opening.end
            for ancestor in token.ancestors
            if ancestor.start == opening.start
        ),
        None,
    )


def _body(opening: Token, tokens: list[Token]) -> tuple[Element, list[Token], int]:
    element = _element(opening, tokens)
    if element is None:
        fail("CARRIER_STRUCTURE_INVALID", "candidate source carrier has no structural body", start=opening.start, tag=opening.name)
    body = [token for token in tokens if any(parent.serial == element.serial for parent in token.ancestors)]
    closing = next((token for token in body if token.kind == "end_tag" and token.name == element.name and token.ancestors[-1].serial == element.serial), None)
    if closing is None:
        # The end tag's ancestors include the element, but it is not part of inner text.
        closing = next((token for token in tokens if token.kind == "end_tag" and token.name == element.name and any(parent.serial == element.serial for parent in token.ancestors)), None)
    if closing is None:
        fail("CARRIER_CLOSE_MISSING", "candidate source carrier lacks an exact closing token", start=opening.start, tag=opening.name)
    return element, [token for token in body if token.start < closing.start], closing.end


def _source_range(data: bytes, mapping: list[dict[str, Any]], start: int, end: int) -> dict[str, Any]:
    touched = []
    for original in mapping:
        text_start = max(start, original["text_start"])
        text_end = min(end, original["text_end"])
        if text_end <= text_start:
            continue
        item = dict(original)
        if original["transform"] == "IDENTITY_UTF8":
            raw = data[original["source_start"] : original["source_end"]].decode("utf-8")
            local_start = text_start - original["text_start"]
            local_end = text_end - original["text_start"]
            item["source_start"] = original["source_start"] + len(raw[:local_start].encode("utf-8"))
            item["source_end"] = original["source_start"] + len(raw[:local_end].encode("utf-8"))
            item["source_sha256"] = sha256(data[item["source_start"] : item["source_end"]])
        item["text_start"] = text_start
        item["text_end"] = text_end
        touched.append(item)
    if not touched:
        return {"start": None, "end": None, "sha256": None, "map": []}
    source_start, source_end = touched[0]["source_start"], touched[-1]["source_end"]
    return {"start": source_start, "end": source_end, "sha256": sha256(data[source_start:source_end]), "map": touched}


def source_range(data: bytes, mapping: list[dict[str, Any]], start: int, end: int) -> dict[str, Any]:
    """Public exact-span projection used by downstream semantic row receipts."""
    return _source_range(data, mapping, start, end)


def fields(data: bytes, text: str, mapping: list[dict[str, Any]], body_tokens: list[Token]) -> list[dict[str, Any]]:
    matches = list(FIELD.finditer(text))
    result: list[dict[str, Any]] = []
    for index, match in enumerate(matches):
        end = matches[index + 1].start() if index + 1 < len(matches) else len(text)
        result.append(
            {
                "label": "_".join(match.group("label").lower().split()),
                "value": " ".join(text[match.end() : end].split()),
                "raw_value": text[match.end() : end],
                "value_text_start": match.end(),
                "value_text_end": end,
                "text_start": match.start(),
                "text_end": end,
                "source": _source_range(data, mapping, match.start(), end),
            }
        )
    # Compositing's typographic WebIDL property title has no Name: table row.
    if not any(row["label"] == "name" for row in result):
        for token in body_tokens:
            if token.kind == "start_tag" and "prop-name" in classes(token):
                element = _element(token, body_tokens)
                if element is None:
                    continue
                inner = [item for item in body_tokens if any(parent.serial == element.serial for parent in item.ancestors)]
                value, local_map = decoded_map(iter(inner))
                normalized = " ".join(value.strip("‘’'\" ").split())
                if normalized:
                    result.insert(0, {"label": "name", "value": normalized, "raw_value": value, "value_text_start": None, "value_text_end": None, "text_start": None, "text_end": None, "source": _source_range(data, local_map, 0, len(value)), "source_form": "TYPOGRAPHIC_PROP_NAME"})
                    break
    return result


def semantic_blocks(data: bytes, body_tokens: list[Token], fallback_text: str, fallback_map: list[dict[str, Any]]) -> list[dict[str, Any]]:
    """Turn list-item structure into exact algorithm step units."""
    result: list[dict[str, Any]] = []
    seen: set[int] = set()
    for opening in [token for token in body_tokens if token.kind == "start_tag" and token.name in {"li", "dt", "dd"}]:
        element = _element(opening, body_tokens)
        if element is None or element.serial in seen:
            continue
        seen.add(element.serial)
        inner = [token for token in body_tokens if any(parent.serial == element.serial for parent in token.ancestors)]
        value, mapping = decoded_map(iter(inner))
        normalized = " ".join(value.split())
        if not normalized:
            continue
        validate_map(data, value, mapping)
        span = _source_range(data, mapping, 0, len(value))
        result.append(
            {
                "ordinal": len(result) + 1,
                "role": "STEP" if opening.name == "li" else ("TERM" if opening.name == "dt" else "OUTCOME"),
                "text": normalized,
                "source": span,
            }
        )
    if not result and fallback_text.strip():
        # Some source-native algorithm blocks are a single imperative statement.
        result.append({"ordinal": 1, "role": "STATEMENT", "text": " ".join(fallback_text.split()), "source": _source_range(data, fallback_map, 0, len(fallback_text))})
    return result


def _kind(opening: Token) -> tuple[str | None, str | None]:
    own = classes(opening)
    direct = next((kind for kind in SOURCE_KINDS if kind in own), None)
    if direct:
        return direct, "BIKESHED_CLASS"
    if "lang-webidl" in own and "def" in own:
        return "idl", "BIKESHED_WEBIDL_CLASS"
    if "media-feature" in own:
        return "legacy-row", "BIKESHED_MEDIA_FEATURE_CLASS"
    if "algorithm" in own or (opening.attributes and "algorithm" in opening.attributes):
        return "algorithm", "BIKESHED_ALGORITHM_CLASS"
    return None, None


def _normativity(opening: Token) -> tuple[str, str]:
    if opening.edit == "DELETED":
        return "DELETED_HISTORY", "structurally inside current <del>"
    ancestor_classes = set().union(*(classes(item) for item in opening.ancestors)) if opening.ancestors else set()
    if "example" in ancestor_classes:
        return "EXAMPLE", "structural ancestor class=example"
    if "non-normative" in ancestor_classes or "informative" in ancestor_classes:
        return "NON_NORMATIVE", "structural ancestor has a source-native non-normative class"
    return "NORMATIVE", "source-native carrier outside comments, <del>, examples, and non-normative ancestors"


def capture(scope: dict[str, Any], csswg: Path) -> dict[str, Any]:
    require_clean(scope, "structural-extraction")
    decisions = load(INPUTS / "decisions.json")
    pre_sites = {(row["path"], row["start"], row["sha256"]): row for row in decisions["pre_sites"]}
    records: list[dict[str, Any]] = []
    negatives: list[dict[str, Any]] = []
    root_blockers: list[dict[str, Any]] = []
    ordinals: dict[tuple[str, str], int] = {}
    for spec in scope["specs"]:
        path = csswg / spec["path"]
        data = path.read_bytes()
        if len(data) != spec["bytes"] or sha256(data) != spec["sha256"]:
            fail("SOURCE_PIN_DRIFT", "CSSWG member bytes differ from the sealed membership", path=spec["path"])
        tokens = scan(data)
        for opening in [token for token in tokens if token.kind == "start_tag"]:
            kind, dialect = _kind(opening)
            element: Element | None = None
            body_tokens: list[Token] = []
            raw_end: int | None = None
            if kind is None and opening.name == "pre":
                element, body_tokens, raw_end = _body(opening, tokens)
                raw = data[opening.start:raw_end]
                site = pre_sites.get((spec["path"], opening.start, sha256(raw)))
                if site is not None and site["decision"].startswith("LEGACY_"):
                    kind, dialect = "legacy-grammar", site["decision"]
                elif site is not None:
                    negatives.append({"spec_id": spec["id"], "path": spec["path"], "start": opening.start, "end": raw_end, "sha256": sha256(raw), "classification": site["decision"], "status": "EXCLUDED_NON_GRAMMAR"})
                    continue
                elif classes(opening):
                    negatives.append({"spec_id": spec["id"], "path": spec["path"], "start": opening.start, "end": raw_end, "sha256": sha256(raw), "classification": "SOURCE_NATIVE_NON_CARRIER_CLASS", "classes": sorted(classes(opening)), "status": "EXCLUDED_NON_GRAMMAR"})
                    continue
                else:
                    plain, _plain_map = decoded_map(iter(body_tokens))
                    signature = re.search(r"(?m)^\s*(?:<<[^>]+>>|<[^>]+>|@[-A-Za-z_][\w-]*)\s*=", plain) or "%option" in plain
                    if re.search(r"<[!?/]?[A-Za-z][^>]*>", plain):
                        decision = "NEGATIVE_MARKUP_EXAMPLE"
                    elif re.search(r"(?m)^\s*[^\n=]{1,80}=", plain) and not re.search(r"(?m)^\s*(?:<<|<|@)", plain):
                        decision = "NEGATIVE_EQUATION_OR_MATH"
                    elif any(marker in plain for marker in ("{", "}", ";", "=>", "function ", "//", "/*")):
                        decision = "NEGATIVE_PROGRAM_OR_STYLESHEET"
                    elif signature:
                        root_blockers.append({"code": "UNCLASSIFIED_PRE", "spec_id": spec["id"], "path": spec["path"], "start": opening.start, "sha256": sha256(raw)})
                        continue
                    else:
                        decision = "NEGATIVE_PREFORMATTED_DATA"
                    negatives.append({"spec_id": spec["id"], "path": spec["path"], "start": opening.start, "end": raw_end, "sha256": sha256(raw), "classification": decision, "status": "EXCLUDED_NON_GRAMMAR"})
                    continue
            if kind is None:
                continue
            if element is None or raw_end is None:
                element, body_tokens, raw_end = _body(opening, tokens)
            # Railroad DSL payloads deliberately use literal ``<name>`` terminals
            # and, in a few diagrams, literal comment delimiters.  They are source
            # tokens in this carrier dialect even though the outer source scanner
            # must continue to recognize the same byte shapes as markup/comments.
            text, mapping = decoded_map(
                iter(body_tokens),
                markup_literals=kind == "railroad",
                comment_literals=kind == "railroad",
            )
            validate_map(data, text, mapping)
            state, reason = _normativity(opening)
            key = (spec["id"], kind)
            ordinals[key] = ordinals.get(key, 0) + 1
            raw = data[opening.start:raw_end]
            record = {
                "id": f"{spec['id']}::{kind}::{ordinals[key]}",
                "spec_id": spec["id"],
                "path": spec["path"],
                "kind": kind,
                "dialect": dialect,
                "tag": opening.name,
                "classes": sorted(classes(opening)),
                "start": opening.start,
                "end": raw_end,
                "body_start": opening.end,
                "body_end": max((token.end for token in body_tokens), default=opening.end),
                "line": line_of(data, opening.start),
                "anchor": next((ancestor.attributes["id"] for ancestor in reversed(opening.ancestors) if ancestor.attributes.get("id")), None),
                "raw_bytes": len(raw),
                "raw_sha256": sha256(raw),
                "text": text,
                "text_sha256": sha256(text.encode()),
                "source_map": mapping,
                "normativity": state,
                "classification_reason": reason,
            }
            if kind in {"propdef", "descdef", "legacy-row"}:
                record["fields"] = fields(data, text, mapping, body_tokens)
                names = [row["value"] for row in record["fields"] if row["label"] == "name"]
                values = [row["value"] for row in record["fields"] if row["label"] == "value"]
                if names == ["property-name"] and values in (["legal values & syntax"], ["legal values &amp; syntax"]):
                    negatives.append(
                        {
                            "spec_id": spec["id"],
                            "path": spec["path"],
                            "start": opening.start,
                            "end": raw_end,
                            "sha256": sha256(raw),
                            "classification": "SOURCE_NATIVE_SCHEMA_TEMPLATE",
                            "status": "EXCLUDED_NON_GRAMMAR",
                        }
                    )
                    continue
            if kind == "algorithm":
                record["semantic_blocks"] = semantic_blocks(data, body_tokens, text, mapping)
            records.append(record)
    return {
        "schema_version": 3,
        "stage": "structural-extraction",
        "records": records,
        "negative_pre_fixtures": negatives,
        "counts": {kind: sum(row["kind"] == kind for row in records) for kind in sorted({row["kind"] for row in records})},
        "root_blockers": root_blockers,
    }
