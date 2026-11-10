#!/usr/bin/env python3
"""Reference oracle for the JSON planes P1 (check) and P4 (DOM) (sk-v27 LW2.CON-1).

It implements `bench/planes/P1.md` and `bench/planes/P4.md` exactly, over bytes:

- the input must be well-formed UTF-8, with no byte-order mark;
- strings are unescaped to Unicode scalar values; an unpaired surrogate escape is rejected;
- an integer lexeme in [-2^63, 2^64 - 1] is an exact integer, any other number is the
  correctly rounded binary64, and a number that rounds to infinity is rejected;
- duplicate object keys are kept by the parser and resolved in the canonical form
  (the last value wins);
- a rejection reports the plane's error offset.

Output, one line per input file: `ACCEPT <sha256 of the canonical form>` or `REJECT <offset>`.
Every arm of a JSON P1/P4 cell prints the same line format (its witness mode), so
`admit.py` can compare arms on the plane's witness inputs.

Usage: oracle_json.py FILE ...
"""
from __future__ import annotations

import hashlib
import struct
import sys

I64_MIN = -(2**63)
U64_MAX = 2**64 - 1
WS = b" \t\n\r"
ESCAPES = {ord('"'): '"', ord("\\"): "\\", ord("/"): "/", ord("b"): "\b",
           ord("f"): "\f", ord("n"): "\n", ord("r"): "\r", ord("t"): "\t"}


class Reject(Exception):
    def __init__(self, offset: int):
        super().__init__(offset)
        self.offset = offset


def unescape(raw: bytes, at: int) -> str:
    """The body of a string token (between the quotes) to its scalar values.

    `at` is the byte offset of `raw[0]` in the input, for error offsets.
    """
    out: list[str] = []
    i = 0
    n = len(raw)
    while i < n:
        b = raw[i]
        if b != 0x5C:  # not a backslash: copy the run up to the next one
            j = raw.find(b"\\", i)
            j = n if j < 0 else j
            out.append(raw[i:j].decode("utf-8"))
            i = j
            continue
        if i + 1 >= n:
            raise Reject(at + i)
        e = raw[i + 1]
        if e in ESCAPES:
            out.append(ESCAPES[e])
            i += 2
            continue
        if e != ord("u"):
            raise Reject(at + i)
        hi = _hex4(raw, i + 2, at)
        if 0xD800 <= hi <= 0xDBFF:
            if raw[i + 6:i + 8] != b"\\u":
                raise Reject(at + i)
            lo = _hex4(raw, i + 8, at)
            if not 0xDC00 <= lo <= 0xDFFF:
                raise Reject(at + i)
            out.append(chr(0x10000 + ((hi - 0xD800) << 10) + (lo - 0xDC00)))
            i += 12
        elif 0xDC00 <= hi <= 0xDFFF:
            raise Reject(at + i)
        else:
            out.append(chr(hi))
            i += 6
    return "".join(out)


def _hex4(raw: bytes, i: int, at: int) -> int:
    digits = raw[i:i + 4]
    if len(digits) != 4 or any(c not in b"0123456789abcdefABCDEF" for c in digits):
        raise Reject(at + i - 2)
    return int(digits, 16)


class _Parser:
    def __init__(self, data: bytes):
        self.d = data
        self.i = 0

    def ws(self) -> None:
        d, i = self.d, self.i
        while i < len(d) and d[i] in WS:
            i += 1
        self.i = i

    def value(self):
        self.ws()
        d, i = self.d, self.i
        if i >= len(d):
            raise Reject(i)
        c = d[i]
        if c == 0x7B:
            return self.obj()
        if c == 0x5B:
            return self.arr()
        if c == 0x22:
            return self.string()
        if c == 0x2D or 0x30 <= c <= 0x39:
            return self.number()
        for word, val in ((b"true", True), (b"false", False), (b"null", None)):
            if c == word[0]:
                for k, w in enumerate(word):
                    if i + k >= len(d) or d[i + k] != w:
                        raise Reject(i + k)
                self.i = i + len(word)
                return val
        raise Reject(i)

    def obj(self):
        self.i += 1
        members = []
        self.ws()
        if self.i < len(self.d) and self.d[self.i] == 0x7D:
            self.i += 1
            return ("obj", members)
        while True:
            self.ws()
            if self.i >= len(self.d) or self.d[self.i] != 0x22:
                raise Reject(self.i)
            k = self.string()
            self.ws()
            if self.i >= len(self.d) or self.d[self.i] != 0x3A:
                raise Reject(self.i)
            self.i += 1
            members.append((k, self.value()))
            self.ws()
            if self.i >= len(self.d):
                raise Reject(self.i)
            c = self.d[self.i]
            self.i += 1
            if c == 0x7D:
                return ("obj", members)
            if c != 0x2C:
                raise Reject(self.i - 1)

    def arr(self):
        self.i += 1
        items = []
        self.ws()
        if self.i < len(self.d) and self.d[self.i] == 0x5D:
            self.i += 1
            return items
        while True:
            items.append(self.value())
            self.ws()
            if self.i >= len(self.d):
                raise Reject(self.i)
            c = self.d[self.i]
            self.i += 1
            if c == 0x5D:
                return items
            if c != 0x2C:
                raise Reject(self.i - 1)

    def string(self) -> str:
        d = self.d
        start = self.i + 1
        i = start
        while True:
            if i >= len(d):
                raise Reject(i)
            c = d[i]
            if c == 0x22:
                break
            if c < 0x20:
                raise Reject(i)
            if c == 0x5C:
                if i + 1 >= len(d):
                    raise Reject(i + 1)
                i += 2
                continue
            i += 1
        self.i = i + 1
        return unescape(d[start:i], start)

    def number(self):
        d = self.d
        s = i = self.i
        if d[i] == 0x2D:
            i += 1
        if i >= len(d) or not 0x30 <= d[i] <= 0x39:
            raise Reject(i)
        if d[i] == 0x30:
            i += 1
        else:
            while i < len(d) and 0x30 <= d[i] <= 0x39:
                i += 1
        integral = True
        if i < len(d) and d[i] == 0x2E:
            integral = False
            i += 1
            if i >= len(d) or not 0x30 <= d[i] <= 0x39:
                raise Reject(i)
            while i < len(d) and 0x30 <= d[i] <= 0x39:
                i += 1
        if i < len(d) and d[i] in b"eE":
            integral = False
            i += 1
            if i < len(d) and d[i] in b"+-":
                i += 1
            if i >= len(d) or not 0x30 <= d[i] <= 0x39:
                raise Reject(i)
            while i < len(d) and 0x30 <= d[i] <= 0x39:
                i += 1
        lexeme = d[s:i].decode("ascii")
        self.i = i
        if integral:
            n = int(lexeme)
            if I64_MIN <= n <= U64_MAX:
                return ("int", n)
        x = float(lexeme)
        if x in (float("inf"), float("-inf")):
            raise Reject(s)
        return ("f64", x)


def parse(data: bytes):
    """The P4 value of `data`, or `Reject(offset)`."""
    if data.startswith(b"\xef\xbb\xbf"):
        raise Reject(0)
    try:
        data.decode("utf-8")
    except UnicodeDecodeError as e:  # first byte of the maximal ill-formed subpart
        bad = e.start
        # A well-formed prefix still has to be valid JSON up to `bad`; the earlier
        # of the two offsets is the error.
        try:
            p = _Parser(data[:bad])
            p.value()
            p.ws()
            if p.i < bad:
                raise Reject(p.i)
        except Reject as r:
            if r.offset < bad:
                raise
        raise Reject(bad) from None
    p = _Parser(data)
    v = p.value()
    p.ws()
    if p.i != len(data):
        raise Reject(p.i)
    return v


def canonical(v) -> bytes:
    """The canonical form of a P4 value (bench/planes/P4.md §Canonical form)."""
    out = bytearray()

    def enc_str(s: str) -> None:
        b = s.encode("utf-8")
        out.extend(b"s%d:" % len(b))
        out.extend(b)

    def enc(x) -> None:
        if x is None:
            out.extend(b"n")
        elif x is True:
            out.extend(b"t")
        elif x is False:
            out.extend(b"f")
        elif isinstance(x, str):
            enc_str(x)
        elif isinstance(x, list):
            out.extend(b"[")
            for y in x:
                enc(y)
            out.extend(b"]")
        elif x[0] == "int":
            out.extend(b"i%d;" % x[1])
        elif x[0] == "f64":
            out.extend(b"d" + struct.pack(">d", x[1]).hex().encode() + b";")
        else:  # ("obj", members): last value wins, keys ordered by UTF-8 bytes
            merged = dict(x[1])
            out.extend(b"{")
            for k in sorted(merged, key=lambda s: s.encode("utf-8")):
                enc_str(k)
                enc(merged[k])
            out.extend(b"}")

    enc(v)
    return bytes(out)


def verdict(data: bytes) -> str:
    try:
        v = parse(data)
    except Reject as r:
        return f"REJECT {r.offset}"
    except RecursionError:
        return "REJECT nesting"
    return "ACCEPT " + hashlib.sha256(canonical(v)).hexdigest()


def main(argv: list[str]) -> int:
    sys.setrecursionlimit(10_000)
    for path in argv:
        with open(path, "rb") as f:
            print(verdict(f.read()))
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
