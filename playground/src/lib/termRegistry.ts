// Term registry for doc enrichment — definitions, file links, and tooltip data.

export type TermCategory = "directive" | "operator" | "concept" | "type";

export interface TermEntry {
    /** Short explanation, 1-2 sentences. Plain text only. */
    description: string;
    /** Doc page slug for "Learn more" link. */
    docSlug?: string;
    /** Anchor within the doc page. */
    docAnchor?: string;
    /** Visual category for badge coloring. */
    category: TermCategory;
}

export const TERM_REGISTRY: Record<string, TermEntry> = {
    // ── Directives ──────────────────────────────────────────────
    "@ws": {
        description:
            "Overrides the default whitespace pattern that ?w compiles to. Accepts any regex.",
        docSlug: "bbnf/grammar-syntax",
        docAnchor: "ws",
        category: "directive",
    },
    "@inline": {
        description:
            "Force-inlines a rule at every call site. No enum variant or function is generated.",
        docSlug: "bbnf/grammar-syntax",
        docAnchor: "inline",
        category: "directive",
    },
    "@pretty": {
        description:
            "Attaches formatting hints to a rule for the pretty-printer. Hint vocabulary includes group, indent, block, sep, split, and more.",
        docSlug: "bbnf/pretty-directives",
        category: "directive",
    },
    "@recover": {
        description:
            "Associates an error-recovery sync expression with a rule. Enables multi-error parsing.",
        docSlug: "bbnf/recover-directives",
        category: "directive",
    },
    "@no_collapse": {
        description:
            "Prevents a rule from being inlined or fused, preserving its identity in the generated AST.",
        docSlug: "bbnf/grammar-syntax",
        category: "directive",
    },
    "@import": {
        description:
            "Imports rules from another .bbnf file. Supports glob and selective imports with automatic transitive dependency resolution.",
        docSlug: "bbnf/grammar-syntax",
        docAnchor: "imports",
        category: "directive",
    },
    "@arena": {
        description:
            "Triggers monolithic arena codegen — direct recursive functions using BumpSlab instead of combinator chains.",
        docSlug: "performance/arena-handoff",
        category: "directive",
    },
    "skip_recover": {
        description:
            "Parser attribute that suppresses @recover codegen and the Recovered enum variant. Used for formatters that assume well-formed input.",
        docSlug: "bbnf/api-reference",
        category: "directive",
    },

    // ── Operators ───────────────────────────────────────────────
    "?w": {
        description:
            "Optional whitespace operator. Trims whitespace between elements. Overridable via @ws.",
        docSlug: "bbnf/operators",
        docAnchor: "optional-whitespace-w",
        category: "operator",
    },
    ">>": {
        description:
            "Value projection (next). Parses both sides but keeps only the right-hand value.",
        docSlug: "bbnf/operators",
        docAnchor: "value-projection",
        category: "operator",
    },
    "<<": {
        description:
            "Value projection (skip). Parses both sides but keeps only the left-hand value.",
        docSlug: "bbnf/operators",
        docAnchor: "value-projection",
        category: "operator",
    },

    // ── Concepts ────────────────────────────────────────────────
    "dispatch tables": {
        description:
            "128-byte O(1) lookup tables that select alternation branches by leading character, computed from FIRST sets.",
        docSlug: "performance/parsing",
        category: "concept",
    },
    "dispatch table": {
        description:
            "128-byte O(1) lookup table that selects alternation branches by leading character, computed from FIRST sets.",
        docSlug: "performance/parsing",
        category: "concept",
    },
    "FIRST sets": {
        description:
            "The set of characters that can begin a rule. Computed iteratively to fixed point over a 128-bit CharSet.",
        docSlug: "bbnf/lsp",
        docAnchor: "analysis-pipeline",
        category: "concept",
    },
    "FIRST-set": {
        description:
            "The set of characters that can begin a rule. Computed iteratively to fixed point over a 128-bit CharSet.",
        docSlug: "bbnf/lsp",
        docAnchor: "analysis-pipeline",
        category: "concept",
    },
    "FOLLOW sets": {
        description:
            "Characters that can appear immediately after a rule. Used for dispatch and memoization decisions.",
        category: "concept",
    },
    "Tarjan SCC": {
        description:
            "Tarjan's strongly-connected-component algorithm, used to detect recursive rule cycles and guide inlining decisions.",
        docSlug: "bbnf/lsp",
        docAnchor: "analysis-pipeline",
        category: "concept",
    },
    "monolithic codegen": {
        description:
            "Code generation mode that emits direct recursive functions instead of combinator chains. Zero vtable dispatches.",
        docSlug: "performance/arena-handoff",
        docAnchor: "monolithic-codegen",
        category: "concept",
    },
    "arena allocation": {
        description:
            "Bump allocation strategy where objects are allocated contiguously and freed all at once. Used via BumpSlab.",
        docSlug: "performance/arena-handoff",
        docAnchor: "bumpslab",
        category: "concept",
    },
    "span eligibility": {
        description:
            "IR pass that marks rules whose entire body produces a Span without semantic transforms, enabling zero-copy parsing.",
        category: "concept",
    },
    "memoization": {
        description:
            "Caching of parse results to avoid redundant work. Strategies: Full, None, or Selective per rule.",
        category: "concept",
    },
    "left-recursion elimination": {
        description:
            "Transforms left-recursive rules into iterative form via Paull's algorithm. Indirect recursion handled by substitution.",
        docSlug: "bbnf/api-reference",
        category: "concept",
    },
    "error recovery": {
        description:
            "Mechanism that skips to a sync expression on parse failure, wrapping bad input in a Recovered node to continue parsing.",
        docSlug: "bbnf/recover-directives",
        category: "concept",
    },

    // ── Types ───────────────────────────────────────────────────
    "BumpSlab": {
        description:
            "Byte-based bump allocator in parse_that. Generic alloc<T>/alloc_slice_clone<T> methods — faster than typed_arena.",
        docSlug: "performance/arena-handoff",
        docAnchor: "bumpslab",
        category: "type",
    },
    "GrammarIR": {
        description:
            "The canonical intermediate representation of a grammar. Shared by AOT codegen, VM bytecode compilation, and analysis.",
        docSlug: "codegen-paths",
        category: "type",
    },
    "CharSet": {
        description:
            "128-bit bitset covering ASCII characters. Used for FIRST/FOLLOW set computation and dispatch tables.",
        category: "type",
    },
    "IrNode": {
        description:
            "A node in the grammar IR tree. Variants: Literal, Regex, Seq, Alt, Repeat, Ref, Epsilon, and more.",
        docSlug: "codegen-paths",
        category: "type",
    },
    "TypeDesc": {
        description:
            "Type descriptor inferred for each IR node. Variants: Span, Vec, Option, Tuple, BoxedEnum, Enum, Named.",
        category: "type",
    },
    "BytecodeProgram": {
        description:
            "Serialized bytecode for the VM interpreter. Compiled from GrammarIR, serialized via MessagePack.",
        docSlug: "codegen-paths",
        category: "type",
    },
    "ParserState": {
        description:
            "Mutable parsing state threaded through combinators. Holds offset, source bytes, and memoization cache.",
        docSlug: "parse-that/overview",
        category: "type",
    },
    "SpanParser": {
        description:
            "Zero-copy parser that returns Span references into the input. Generated for span-eligible rules.",
        docSlug: "parse-that/span-combinators",
        category: "type",
    },
};

// ── File path mapping ───────────────────────────────────────────

const GITHUB_BASE = "https://github.com/mkbabb/bbnf-lang/blob/master";

const FILE_PATH_MAP: Record<string, string> = {
    // grammar/json/
    "json.bbnf": "grammar/json/json.bbnf",
    "json-pretty.bbnf": "grammar/json/json.bbnf",
    // grammar/bbnf/
    "bbnf.bbnf": "grammar/bbnf/bbnf.bbnf",
    // grammar/bnf/
    "bnf.bbnf": "grammar/bnf/bnf.bbnf",
    // grammar/ebnf/
    "ebnf.bbnf": "grammar/ebnf/ebnf.bbnf",
    // grammar/google-sheets/
    "google-sheets.bbnf": "grammar/google-sheets/google-sheets.bbnf",
    // grammar/misc/
    "json-commented.bbnf": "grammar/misc/json-commented.bbnf",
    "csv.bbnf": "grammar/misc/csv.bbnf",
    "emoji.bbnf": "grammar/misc/emoji.bbnf",
    "g4.bbnf": "grammar/misc/g4.bbnf",
    "math.bbnf": "grammar/misc/math.bbnf",
    "math-ambiguous.bbnf": "grammar/misc/math-ambiguous.bbnf",
    "regex.bbnf": "grammar/misc/regex.bbnf",
    // grammar/css/
    "css-pretty.bbnf": "grammar/css/pretty.bbnf",
    // grammar/css/l4/
    "value-unit.bbnf": "grammar/css/l4/value-unit.bbnf",
    "color.bbnf": "grammar/css/l4/color.bbnf",
    "values.bbnf": "grammar/css/l4/values.bbnf",
    "selectors.bbnf": "grammar/css/l4/selectors.bbnf",
    "keyframes.bbnf": "grammar/css/l4/keyframes.bbnf",
    "stylesheet.bbnf": "grammar/css/l4/stylesheet.bbnf",
    "tokens.bbnf": "grammar/css/l4/tokens.bbnf",
    "media.bbnf": "grammar/css/l4/media.bbnf",
    "properties.bbnf": "grammar/css/l4/properties.bbnf",
    "easing.bbnf": "grammar/css/l4/easing.bbnf",
    "filters.bbnf": "grammar/css/l4/filters.bbnf",
    "gradients.bbnf": "grammar/css/l4/gradients.bbnf",
    "transforms.bbnf": "grammar/css/l4/transforms.bbnf",
    "keywords.bbnf": "grammar/css/l4/keywords.bbnf",
    "func-body.bbnf": "grammar/css/l4/func-body.bbnf",
};

// ── Lookup helpers ──────────────────────────────────────────────

function unescapeHtml(s: string): string {
    return s
        .replace(/&amp;/g, "&")
        .replace(/&lt;/g, "<")
        .replace(/&gt;/g, ">")
        .replace(/&quot;/g, '"')
        .replace(/&#39;/g, "'");
}

/** Look up a term by its inline-code text (after HTML entity unescaping). */
export function lookupTerm(rawText: string): (TermEntry & { key: string }) | undefined {
    const text = unescapeHtml(rawText).trim();
    // Exact match first
    if (text in TERM_REGISTRY) return { ...TERM_REGISTRY[text]!, key: text };
    // Case-insensitive fallback for concepts
    const lower = text.toLowerCase();
    for (const [key, entry] of Object.entries(TERM_REGISTRY)) {
        if (key.toLowerCase() === lower) return { ...entry, key };
    }
    return undefined;
}

/** Look up a file's GitHub URL by its basename. */
export function lookupFileUrl(filename: string): string | undefined {
    const name = filename.trim();
    const path = FILE_PATH_MAP[name];
    if (!path) return undefined;
    return `${GITHUB_BASE}/${path}`;
}

/** Category → CSS color token name. */
export const CATEGORY_COLORS: Record<TermCategory, string> = {
    directive: "pastel-purple",
    operator: "pastel-amber",
    concept: "pastel-cyan",
    type: "pastel-green",
};
