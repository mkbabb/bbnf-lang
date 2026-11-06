"""Failure-total source discovery with exact byte-qualified atomic units."""

from __future__ import annotations

import re
from collections import Counter, defaultdict
from dataclasses import dataclass, field
from typing import Any

from common import encode, sha256
from parsers import MARKUP_NAMES, Projection, UnsupportedDialect, parse_css_vds, parse_lex, parse_webidl, parse_yacc, project_markup


ATTRIBUTE = re.compile(rb"([A-Za-z_:][-A-Za-z0-9_:.]*)\s*=\s*(?:\"([^\"]*)\"|'([^']*)'|([^\s>]+))")
TAG_HEAD = re.compile(rb"<\s*(/?)\s*([A-Za-z][A-Za-z0-9:_-]*)\b")
NON_NORMATIVE = {"example", "informative", "non-normative", "note"}
SOURCE_CLASSES = {"propdef", "descdef", "prod", "railroad"}
P_CLOSE_START = {"address", "article", "aside", "blockquote", "div", "dl", "fieldset", "footer", "form", "h1", "h2", "h3", "h4", "h5", "h6", "header", "hr", "menu", "nav", "ol", "p", "pre", "section", "table", "ul"}
FIELD = re.compile(r"(?im)(?:^|\n)\s*(name|value|new values)\s*:\s*(.*?)(?=\n\s*[A-Za-z][A-Za-z ]{0,30}\s*:|\Z)", re.S)
DEFINITION = re.compile(r"(?m)^\s*(?:<<[^<>]+>>|<[^<>]+>|@?[-*A-Za-z][\w-]*(?:\(\))?)\s*=\s*")


@dataclass
class Tag:
    serial: int
    name: str
    start: int
    open_end: int
    attributes: dict[str, str]
    ancestors: tuple[int, ...]
    close_start: int | None = None
    close_end: int | None = None

    @property
    def classes(self) -> set[str]:
        return set(self.attributes.get("class", "").lower().split())


@dataclass
class Scan:
    tags: list[Tag] = field(default_factory=list)
    failures: list[dict[str, Any]] = field(default_factory=list)


def _attributes(raw: bytes) -> dict[str, str]:
    result: dict[str, str] = {}
    for match in ATTRIBUTE.finditer(raw):
        value = next(group for group in match.groups()[1:] if group is not None)
        result[match.group(1).decode("ascii").lower()] = value.decode("utf-8", errors="strict")
    return result


def _end(data: bytes, start: int) -> int | None:
    cursor = start
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


def _implicit(stack: list[Tag], name: str) -> None:
    groups: list[set[str]] = []
    if name == "li": groups.append({"li"})
    if name in {"dt", "dd"}: groups.append({"dt", "dd"})
    if name in {"thead", "tbody"}: groups.append({"thead", "tbody"})
    if name in {"tr"}: groups.append({"tr"})
    if name in {"td", "th"}: groups.append({"td", "th"})
    if name in {"h1", "h2", "h3", "h4", "h5", "h6"}: groups.append({"h1", "h2", "h3", "h4", "h5", "h6"})
    if name in P_CLOSE_START: groups.append({"p"})
    for group in groups:
        found = next((index for index in range(len(stack) - 1, -1, -1) if stack[index].name in group), None)
        if found is not None:
            del stack[found:]


def scan_markup(data: bytes) -> Scan:
    result = Scan()
    stack: list[Tag] = []
    cursor = 0
    serial = 0
    while cursor < len(data):
        if data.startswith(b"<!--", cursor):
            end = data.find(b"-->", cursor + 4)
            if end < 0:
                result.failures.append({"code": "COMMENT_UNTERMINATED", "start": cursor, "end": len(data)})
                break
            cursor = end + 3
            continue
        if data.startswith(b"<<", cursor) or data[cursor : cursor + 1] != b"<":
            cursor += 1
            continue
        head = TAG_HEAD.match(data, cursor)
        if head is None or head.group(2).decode("ascii").lower() not in MARKUP_NAMES:
            cursor += 1
            continue
        end = _end(data, head.end())
        if end is None:
            result.failures.append({"code": "TAG_UNTERMINATED", "start": cursor, "end": len(data)})
            break
        closing = bool(head.group(1))
        name = head.group(2).decode("ascii").lower()
        if closing:
            found = next((index for index in range(len(stack) - 1, -1, -1) if stack[index].name == name), None)
            if found is not None:
                target = stack[found]
                target.close_start = cursor
                target.close_end = end
                del stack[found:]
        else:
            _implicit(stack, name)
            serial += 1
            raw = data[head.end() : end - 1]
            try:
                attributes = _attributes(raw)
            except UnicodeDecodeError as error:
                result.failures.append({"code": "ATTRIBUTE_UTF8_INVALID", "start": cursor, "end": end, "detail": str(error)})
                attributes = {}
            tag = Tag(serial, name, cursor, end, attributes, tuple(item.serial for item in stack))
            result.tags.append(tag)
            self_closing = (raw.rstrip().endswith(b"/") and name not in {"wpt", "wpt-rest"}) or name in {"br", "img", "link", "meta"}
            if not self_closing:
                stack.append(tag)
        cursor = end
    candidate_serials = {
        tag.serial for tag in result.tags
        if tag.classes & SOURCE_CLASSES or "lang-webidl" in tag.classes or tag.name in {"pre", "table", "xmp", "ol"}
    }
    for tag in stack:
        if tag.serial in candidate_serials:
            result.failures.append({"code": "CARRIER_UNCLOSED", "start": tag.start, "end": len(data), "name": tag.name})
    return result


def _state(tag: Tag, by_serial: dict[int, Tag]) -> tuple[str, str]:
    lineage = [by_serial[index] for index in tag.ancestors] + [tag]
    if any(item.name == "del" for item in lineage):
        return "EXCLUDED_DELETED", "del ancestry"
    classes = set().union(*(item.classes for item in lineage))
    found = sorted(classes & NON_NORMATIVE)
    if found:
        return "EXCLUDED_NON_NORMATIVE", "inherited class: " + ",".join(found)
    return "NORMATIVE", "current source"


def _legacy(tag: Tag, path: str, data: bytes, policy: dict[str, Any]) -> str | None:
    if tag.name != "pre" or tag.close_end is None:
        return None
    raw = data[tag.start : tag.close_end]
    return next((row["decision"] for row in policy["legacy_pre_sites"] if row["path"] == path and row["start"] == tag.start and row["end"] == tag.close_end and row["sha256"] == sha256(raw)), None)


def _kind(tag: Tag, path: str, data: bytes, policy: dict[str, Any]) -> tuple[str | None, str | None]:
    direct = next((kind for kind in SOURCE_CLASSES if kind in tag.classes), None)
    if direct:
        return direct, direct
    if "lang-webidl" in tag.classes and "def" in tag.classes:
        return "webidl", "WEBIDL"
    if "media-feature" in tag.classes:
        return "entity-table", "MEDIA_FEATURE"
    legacy = _legacy(tag, path, data, policy)
    if legacy:
        return "legacy", legacy
    return None, None


def _links(by_path: dict[str, list[tuple[int, int, int]]], path: str, fragments: list[list[int]]) -> list[int]:
    result: set[int] = set()
    for left, right in fragments:
        result.update(index for index, start, end in by_path.get(path, []) if start < right and left < end)
    return sorted(result)


def _unit(path: str, family: str, fragments: list[list[int]], data: bytes, disposition: str, links: list[int], **extra: Any) -> dict[str, Any]:
    payload = b"".join(data[left:right] for left, right in fragments)
    identity_basis = [path, family, fragments, sha256(payload)]
    return {
        "id": "unit::" + sha256(encode(identity_basis)),
        "path": path,
        "family": family,
        "fragments": fragments,
        "source_bytes": len(payload),
        "source_sha256": sha256(payload),
        "disposition": disposition,
        "reverse_links": links,
        **extra,
    }


def _projected_units(kind: str, dialect: str, projection: Projection) -> list[dict[str, Any]]:
    text = projection.text
    if dialect == "LEGACY_YACC":
        return parse_yacc(text)
    if dialect == "LEGACY_LEX":
        return parse_lex(text)
    if kind == "webidl":
        return parse_webidl(text)
    if kind == "railroad":
        rows = []
        offset = 0
        for raw in text.splitlines(keepends=True):
            line = raw.rstrip("\r\n")
            if line.strip():
                match = re.fullmatch(r"\s*(N|T|C|S|Skip|Seq|Sequence|Stack|And|Choice|Or|Plus|OneOrMore|Star|ZeroOrMore|Opt|Optional):\s*(.*)", line)
                if not match:
                    raise UnsupportedDialect("railroad", "RAILROAD_OPERATOR_UNSUPPORTED", "unknown railroad instruction", offset)
                rows.append({"family": "railroad-instruction", "start": offset + match.start(1), "end": offset + len(line)})
            offset += len(raw)
        if not rows:
            raise UnsupportedDialect("railroad", "RAILROAD_EMPTY", "no instructions")
        return rows
    if kind == "prod" or dialect == "LEGACY_GRAMMAR":
        boundaries = list(DEFINITION.finditer(text))
        rows: list[dict[str, Any]] = []
        if boundaries:
            for index, match in enumerate(boundaries):
                end = boundaries[index + 1].start() if index + 1 < len(boundaries) else len(text)
                left = match.end()
                while left < end and text[left].isspace(): left += 1
                right = end
                while right > left and text[right - 1].isspace(): right -= 1
                parse_css_vds(text[left:right])
                rows.append({"family": "css-production", "start": match.start(), "end": right})
            return rows
        left, right = 0, len(text)
        while left < right and text[left].isspace(): left += 1
        while right > left and text[right - 1].isspace(): right -= 1
        parse_css_vds(text[left:right])
        return [{"family": "css-expression", "start": left, "end": right}]
    if kind in {"propdef", "descdef", "entity-table"}:
        rows = []
        for match in FIELD.finditer(text):
            value_start, value_end = match.span(2)
            while value_start < value_end and text[value_start].isspace(): value_start += 1
            while value_end > value_start and text[value_end - 1].isspace(): value_end -= 1
            family = "entity-value-field" if match.group(1).lower() in {"value", "new values"} else "entity-name-field"
            if family == "entity-value-field" and value_start < value_end:
                parse_css_vds(text[value_start:value_end])
            rows.append({"family": family, "start": match.start(1), "end": value_end})
        if not rows:
            raise UnsupportedDialect("entity-table", "ENTITY_FIELDS_MISSING", "no Name/Value fields")
        return rows
    raise UnsupportedDialect(dialect, "DIALECT_UNSUPPORTED", "carrier dialect is unsupported")


def denominator(source_bytes: dict[str, bytes], policy: dict[str, Any], obligations: dict[str, Any]) -> dict[str, Any]:
    by_path: dict[str, list[tuple[int, int, int]]] = defaultdict(list)
    for index, row in enumerate(obligations["rows"]):
        source = obligations["sources"][row[3]]
        by_path[source[2]].append((index, source[3], source[4]))
    units: list[dict[str, Any]] = []
    for spec_id, path in sorted(policy["source_paths"].items()):
        data = source_bytes[path]
        scan = scan_markup(data)
        tags = {tag.serial: tag for tag in scan.tags}
        for failure in scan.failures:
            fragments = [[failure["start"], failure["end"]]]
            units.append(_unit(path, "detector-failure", fragments, data, "BLOCKED_DETECTOR_FAILURE", _links(by_path, path, fragments), blocker=failure["code"], spec_id=spec_id))
        for tag in scan.tags:
            if tag.close_start is None or tag.close_end is None:
                continue
            kind, dialect = _kind(tag, path, data, policy)
            if kind is None:
                continue
            state, reason = _state(tag, tags)
            body_start, body_end = tag.open_end, tag.close_start
            envelope = [[tag.start, tag.close_end]]
            if state != "NORMATIVE":
                units.append(_unit(path, kind, envelope, data, state, _links(by_path, path, envelope), dialect=dialect, state_reason=reason, spec_id=spec_id))
                continue
            try:
                projection = project_markup(data[body_start:body_end], base=body_start)
                parsed = _projected_units(kind, dialect or kind, projection)
                for row in parsed:
                    fragments = projection.fragments(row["start"], row["end"])
                    family = row.pop("family")
                    links = _links(by_path, path, fragments)
                    disposition = "PARSED_WITH_REVERSE_LINKS_RED" if links else "BLOCKED_NO_REVERSE_LINK"
                    units.append(_unit(path, family, fragments, data, disposition, links, dialect=dialect, spec_id=spec_id, parser_span=[row.pop("start"), row.pop("end")], **row))
            except UnsupportedDialect as error:
                units.append(_unit(path, "typed-unsupported-carrier", envelope, data, "BLOCKED_TYPED_UNSUPPORTED", _links(by_path, path, envelope), dialect=error.dialect, blocker=error.code, detail=error.detail, offset=error.offset, spec_id=spec_id))
        for tag in scan.tags:
            if tag.name != "li" or tag.close_start is None or tag.close_end is None:
                continue
            ancestors = [tags[index] for index in tag.ancestors]
            if not any(item.name == "ol" and "algorithm" in item.classes or "algorithm" in item.classes for item in ancestors):
                continue
            state, reason = _state(tag, tags)
            fragments = [[tag.start, tag.close_end]]
            disposition = "PARSED_WITH_REVERSE_LINKS_RED" if state == "NORMATIVE" and _links(by_path, path, fragments) else state if state != "NORMATIVE" else "BLOCKED_NO_REVERSE_LINK"
            units.append(_unit(path, "algorithm-step", fragments, data, disposition, _links(by_path, path, fragments), state_reason=reason, spec_id=spec_id))
    units.sort(key=lambda row: (row["path"], row["fragments"][0][0], row["fragments"][-1][1], row["family"], row["id"]))
    identities = [row["id"] for row in units]
    if len(identities) != len(set(identities)):
        duplicates = [key for key, value in Counter(identities).items() if value > 1]
        raise UnsupportedDialect("denominator", "UNIT_ID_DUPLICATE", f"duplicate unit identities: {duplicates[:3]}")
    family_counts = dict(sorted(Counter(row["family"] for row in units).items()))
    disposition_counts = dict(sorted(Counter(row["disposition"] for row in units).items()))
    digest_rows = [[row["id"], row["family"], row["fragments"], row["source_sha256"], row["disposition"], row["reverse_links"]] for row in units]
    return {
        "schema_version": 1,
        "role": "FAILURE_TOTAL_UNIT_ATOMIC_SOURCE_DENOMINATOR",
        "unit_count": len(units),
        "unit_digest": sha256(encode(digest_rows)),
        "family_counts": family_counts,
        "disposition_counts": disposition_counts,
        "reverse_link_count": sum(len(row["reverse_links"]) for row in units),
        "units": units,
    }
