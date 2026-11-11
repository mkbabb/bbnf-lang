#!/usr/bin/env python3
"""
LOCK-6 / family 6A / T2 (surface correspondence) — probe t2.

A *syntactic* surface extractor + name-map differ for the parse-that combinator
core (Parser method surface), measuring 6A's "where reasonable ends" clean-map
ratio between the Rust twin (rust/parse_that/src/{parse,combinators/methods/*})
and the TS twin (typescript/src/parse/parser.ts).

This is the T2 tier operating at the granularity the family specifies — "item
set, field names, variant sets, arities" — WITHOUT a full nightly rustdoc-JSON
build. It stands in for the ts-morph TS extractor + a syn-level Rust extractor.
The rustdoc-JSON toolchain question (open-Q ii) is answered separately in
PROTO-2.md by an actual run.

READ-ONLY over parse-that. No mutation, no build.
"""
import re
import sys
import json
from pathlib import Path

PT = Path("/Users/mkbabb/Programming/parse-that")

# --- name-map: the hand-authored idiom residue 6A's manifest carries. -------
# snake_case<->camelCase is MECHANICAL (not an escape hatch). These entries are
# the genuine cross-name / cross-module correspondences a human must assert.
NAME_MAP = {
    # rust_method : ts_method
    "new": "constructor",
    "parse_return_state": "parseState",
    "map_with_state": "mapState",
    "look_ahead": "lookAhead",
}
# one-sided idiom allow-list (risk 4): genuinely single-language, no twin owed.
RUST_ONLY_IDIOM = {
    "many_small", "sep_by_small",          # SmallVec inline-storage alloc idiom
    "sep_by_ws", "sep_by_ws_until",         # ws-folded convenience variants
    "trim_keep", "negate",                  # convenience combinators
    "map_with_ctx", "parse_with_context",   # &Context threading (Rust lifetimes)
    "parse_return_state_with_context",
    "parse_or_error",                       # Result<> vs state-carried error
    "save_state", "trim_whitespace",        # flag-setting builder (TS: ParserContext)
}
TS_ONLY_IDIOM = {
    "debug",       # diagnostics method (Rust: `diagnostics` feature, free fns)
    "toString",    # JS convention
}


def snake_to_camel(s: str) -> str:
    head, *rest = s.split("_")
    return head + "".join(w[:1].upper() + w[1:] for w in rest)


def balanced_params(text: str, open_idx: int) -> str:
    """Return the substring inside the parens starting at text[open_idx]=='('."""
    depth = 0
    for i in range(open_idx, len(text)):
        c = text[i]
        if c == "(":
            depth += 1
        elif c == ")":
            depth -= 1
            if depth == 0:
                return text[open_idx + 1 : i]
    return ""


def top_level_arity(params: str) -> int:
    """Count top-level comma-separated params, dropping a leading self/&self."""
    params = params.strip()
    if not params:
        return 0
    depth = 0
    parts, cur = [], ""
    for c in params:
        if c in "(<[":
            depth += 1
        elif c in ")>]":
            depth -= 1
        if c == "," and depth == 0:
            parts.append(cur)
            cur = ""
        else:
            cur += c
    parts.append(cur)
    parts = [p.strip() for p in parts if p.strip()]
    if parts and re.match(r"(mut\s+)?(&\s*)?(mut\s+)?self\b", parts[0]):
        parts = parts[1:]
    return len(parts)


def extract_rust_parser_methods():
    """pub fn on `impl Parser` blocks across parse.rs + combinators/methods/.

    Self-receiver methods are the combinator mirror surface; the associated
    `new` ctor is captured separately (paired with the TS `constructor`)."""
    files = [PT / "rust/parse_that/src/parse.rs"]
    files += sorted((PT / "rust/parse_that/src/combinators/methods").glob("*.rs"))
    methods = {}
    for f in files:
        src = f.read_text()
        for m in re.finditer(r"\n\s*pub fn (\w+)\s*(?:<[^>]*>)?\s*\(", src):
            name = m.group(1)
            open_idx = src.index("(", m.end() - 1)
            params = balanced_params(src, open_idx)
            if re.match(r"\s*(mut\s+)?(&\s*)?(mut\s+)?self\b", params):
                methods[name] = top_level_arity(params)
            elif name == "new":  # associated ctor -> TS constructor
                methods["new"] = top_level_arity(params)
    return methods


KEYWORDS = {"return", "if", "for", "while", "switch", "catch", "else", "do",
            "const", "let", "var", "throw", "case", "await", "yield", "new"}


def extract_ts_parser_methods():
    """method members of `class Parser` in parser.ts (public only).

    Line-based: a member is a line at EXACTLY 4-space indent whose head is an
    identifier immediately followed by `(` or `<...>(`. Robust to methods that
    abut a preceding `}` with no blank line (the regex-anchor bug)."""
    src = (PT / "typescript/src/parse/parser.ts").read_text()
    methods = {}
    sig = re.compile(r"^    ((?:private |get |set |async )*)(\w+)\s*(?:<[^>]*>)?\s*\(")
    lines = src.split("\n")
    for i, line in enumerate(lines):
        m = sig.match(line)
        if not m:
            continue
        mods, name = m.group(1), m.group(2)
        if name in KEYWORDS or name == "constructor" and False:
            continue
        # rebuild from the '(' and balance across following lines
        rest = "\n".join(lines[i:])
        open_idx = rest.index("(")
        params = balanced_params(rest, open_idx)
        methods[name] = {"arity": top_level_arity(params),
                         "private": "private" in mods}
    return methods


def main():
    rust = extract_rust_parser_methods()
    ts_all = extract_ts_parser_methods()
    ts = {k: v for k, v in ts_all.items() if not v["private"]}

    # build correspondence keyed by the rust name.
    rows = []
    ts_used = set()
    for rname, rarity in sorted(rust.items()):
        tname = NAME_MAP.get(rname, snake_to_camel(rname))
        if tname in ts:
            ts_used.add(tname)
            tarity = ts[tname]["arity"]
            kind = "CLEAN" if tarity == rarity else "ARITY-DRIFT"
            rows.append((rname, tname, rarity, tarity, kind))
        elif rname in RUST_ONLY_IDIOM:
            rows.append((rname, "-", rarity, "-", "RUST-ONLY-IDIOM"))
        else:
            rows.append((rname, "-", rarity, "-", "RUST-ONLY-UNMAPPED"))
    for tname, meta in sorted(ts.items()):
        if tname in ts_used:
            continue
        kind = "TS-ONLY-IDIOM" if tname in TS_ONLY_IDIOM else "TS-ONLY-UNMAPPED"
        rows.append(("-", tname, "-", meta["arity"], kind))

    counts = {}
    for *_, kind in rows:
        counts[kind] = counts.get(kind, 0) + 1

    print(f"{'RUST':<32}{'TS':<18}{'r/t arity':<12}{'class'}")
    print("-" * 74)
    for rname, tname, rarity, tarity, kind in rows:
        print(f"{rname:<32}{tname:<18}{str(rarity)+'/'+str(tarity):<12}{kind}")

    total = len(rows)
    clean = counts.get("CLEAN", 0)
    arity = counts.get("ARITY-DRIFT", 0)
    mapped = clean + arity
    idiom = counts.get("RUST-ONLY-IDIOM", 0) + counts.get("TS-ONLY-IDIOM", 0)
    unmapped = counts.get("RUST-ONLY-UNMAPPED", 0) + counts.get("TS-ONLY-UNMAPPED", 0)

    print("\n=== SUMMARY (Parser combinator-core surface) ===")
    print(f"total distinct mirror items      : {total}")
    print(f"rust methods / ts methods        : {len(rust)} / {len(ts)}")
    for k in sorted(counts):
        print(f"  {k:<22}: {counts[k]}")
    print(f"\nCLEAN 1:1 (name-map + same arity): {clean}/{total} = {clean/total:.0%}")
    print(f"MAPPED (clean + arity-drift)     : {mapped}/{total} = {mapped/total:.0%}")
    print(f"idiom escape-hatch (allow-listed): {idiom}/{total} = {idiom/total:.0%}")
    print(f"UNMAPPED (would be RED)          : {unmapped}/{total} = {unmapped/total:.0%}")


if __name__ == "__main__":
    main()
