#!/usr/bin/env python3
"""Synthesize 1 MB Unicode-stress JSON corpora for sonic-rs profiling.

Generates two corpora:
- unicode_mixed.json: 1 MB of mixed Unicode strings (ASCII + Latin + Greek + CJK + Emoji)
- unicode_escapes.json: 1 MB of strings using \\uXXXX\\uXXXX surrogate-pair escapes for non-BMP codepoints
"""

import json
import random
from pathlib import Path

OUT = Path(__file__).parent
TARGET_BYTES = 1_000_000

random.seed(0xB00B)

# Pools of characters across UTF-8 width classes.
ASCII = list("abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 ,.!?")
LATIN = [chr(c) for c in range(0x00C0, 0x00FF)]  # 2-byte Latin-1 supplement
GREEK = [chr(c) for c in range(0x0370, 0x03FF)]  # 2-byte Greek
CJK = [chr(c) for c in range(0x4E00, 0x4F00)]    # 3-byte CJK
EMOJI = [chr(c) for c in range(0x1F600, 0x1F650)]  # 4-byte Emoji (non-BMP, surrogate pair in \u-escape)

def mixed_string(target_chars: int) -> str:
    out = []
    while sum(len(c) for c in out) < target_chars:
        bucket = random.choices(
            ["ascii", "latin", "greek", "cjk", "emoji"],
            weights=[60, 10, 10, 15, 5],
        )[0]
        if bucket == "ascii":
            out.append("".join(random.choices(ASCII, k=random.randint(4, 24))))
        elif bucket == "latin":
            out.append("".join(random.choices(LATIN, k=random.randint(2, 8))))
        elif bucket == "greek":
            out.append("".join(random.choices(GREEK, k=random.randint(2, 8))))
        elif bucket == "cjk":
            out.append("".join(random.choices(CJK, k=random.randint(2, 8))))
        else:
            out.append("".join(random.choices(EMOJI, k=random.randint(1, 4))))
    return "".join(out)

# ---- Corpus 1: native UTF-8 mixed (forces simdutf8 to validate the full buffer)
docs = []
while True:
    s = mixed_string(random.randint(20, 200))
    docs.append(s)
    blob = json.dumps(docs, ensure_ascii=False).encode("utf-8")
    if len(blob) >= TARGET_BYTES:
        break
(OUT / "unicode_mixed.json").write_bytes(blob)
print(f"unicode_mixed.json: {len(blob)} bytes, {len(docs)} strings")

# ---- Corpus 2: \uXXXX\uXXXX escape-heavy (forces surrogate-pair decode)
docs = []
while True:
    s = "".join(random.choices(EMOJI + CJK, k=random.randint(20, 80)))
    docs.append(s)
    blob = json.dumps(docs, ensure_ascii=True).encode("utf-8")  # ensure_ascii=True forces \uXXXX escapes
    if len(blob) >= TARGET_BYTES:
        break
(OUT / "unicode_escapes.json").write_bytes(blob)
print(f"unicode_escapes.json: {len(blob)} bytes, {len(docs)} strings")
