/** LaTeX accent → Unicode mappings */

export const UMLAUT_MAP: Record<string, string> = {
    a: "\u00e4", o: "\u00f6", u: "\u00fc", e: "\u00eb", i: "\u00ef", y: "\u00ff",
    A: "\u00c4", O: "\u00d6", U: "\u00dc", E: "\u00cb", I: "\u00cf", Y: "\u0178",
};

export const ACUTE_MAP: Record<string, string> = {
    a: "\u00e1", e: "\u00e9", i: "\u00ed", o: "\u00f3", u: "\u00fa",
    A: "\u00c1", E: "\u00c9", I: "\u00cd", O: "\u00d3", U: "\u00da",
};

export const GRAVE_MAP: Record<string, string> = {
    a: "\u00e0", e: "\u00e8", i: "\u00ec", o: "\u00f2", u: "\u00f9",
    A: "\u00c0", E: "\u00c8", I: "\u00cc", O: "\u00d2", U: "\u00d9",
};

export const CIRCUMFLEX_MAP: Record<string, string> = {
    a: "\u00e2", e: "\u00ea", i: "\u00ee", o: "\u00f4", u: "\u00fb",
    A: "\u00c2", E: "\u00ca", I: "\u00ce", O: "\u00d4", U: "\u00db",
};

export const TILDE_MAP: Record<string, string> = {
    a: "\u00e3", n: "\u00f1", o: "\u00f5",
    A: "\u00c3", N: "\u00d1", O: "\u00d5",
};

export const CEDILLA_MAP: Record<string, string> = {
    c: "\u00e7", C: "\u00c7",
};

/** All accent maps keyed by the LaTeX command character */
export const ACCENT_MAPS: Record<string, Record<string, string>> = {
    '"': UMLAUT_MAP,
    "'": ACUTE_MAP,
    "`": GRAVE_MAP,
    "^": CIRCUMFLEX_MAP,
    "~": TILDE_MAP,
    c: CEDILLA_MAP,
    H: {},  // Hungarian umlaut (rare)
    u: {},  // Breve (rare)
    v: {},  // Háček (rare)
};

/** Named symbol commands → Unicode */
export const SYMBOL_MAP: Record<string, string> = {
    "implies": "\u21d2",
    "iff": "\u21d4",
    "Rightarrow": "\u21d2",
    "Leftarrow": "\u21d0",
    "rightarrow": "\u2192",
    "leftarrow": "\u2190",
    "leftrightarrow": "\u2194",
    "to": "\u2192",
    "infty": "\u221e",
    "ldots": "\u2026",
    "cdots": "\u22ef",
    "dots": "\u2026",
    "S": "\u00a7",
    "aa": "\u00e5",
    "AA": "\u00c5",
    "ae": "\u00e6",
    "AE": "\u00c6",
    "oe": "\u0153",
    "OE": "\u0152",
    "ss": "\u00df",
    "i": "\u0131",
    "j": "\u0237",
    "o": "\u00f8",
    "O": "\u00d8",
    "l": "\u0142",
    "L": "\u0141",
};
