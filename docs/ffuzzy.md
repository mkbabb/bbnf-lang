⏺ Concession and reframe

  You're right — "@transform" as I floated it was ICU-shaped. If the directive only makes sense when
  the rules look like CLDR transliteration, it's a hack bolted onto bbnf for one client's convenience
  and upstream should reject it. Let me actually answer the question: what's the general facility
  here, and what's ICU-specific?

  The general concept: finite-state transducers as a first-class bbnf construct

  BBNF today is a parser toolkit: grammars compile to recursive-descent parsers that consume input and
   produce typed ASTs. There's a sibling computational model it doesn't express: finite-state
  transducers — context-sensitive string rewriters that consume input and produce transformed input.
  Same formal grounding as parsers (nondeterministic finite automata + pushdown extensions), same
  compilation target (dispatch tables, lookahead-aware state machines), same optimization
  opportunities (FIRST/FOLLOW sets, common-prefix factoring, dead-state elimination) — just a
  different output type.

  This isn't a new invention. It's the same thing lex/flex does with its "rules section", the same
  thing sed/awk scripts are, the same thing C's trigraph expansion step is, the same thing the ICU
  transliteration engine is, and the same thing a Makefile's pattern rules are. It's a rewriting
  system.

  The concrete facility:

  @transducer <name> {
      # rewrite rules (syntax TBD, but decoupled from ICU)
      pattern → replacement ;
      pattern → replacement ;
  }

  - <name> becomes a public method on the derived parser struct, with signature fn <name>(input: &str)
   -> String.
  - Each rule is a context-sensitive rewrite: left context, pattern, right context, replacement.
  - Rules are applied left-to-right, first-match-wins, single pass (same semantics as ICU, but the
  semantics are a property of the construct, not of the rules inside it).
  - The entire block compiles through bbnf's existing 18-pass IR pipeline, reusing merge_literals,
  factor_common_prefixes, compute_follow_sets, generate_dispatch_tables, and the egraph optimizer. No
  new IR passes needed — just one new IR node type.

  Non-ICU use cases (the "is it general" test)

  If I can't name at least five concrete non-ICU uses, the directive is bespoke and should be
  rejected. Here they are:

  ┌─────┬────────────────────────────┬─────────────────────────────┬─────────────────────────────┐
  │  #  │          Use case          │        Example rule         │    Where it's used today    │
  ├─────┼────────────────────────────┼─────────────────────────────┼─────────────────────────────┤
  │ 1   │ C/Ada trigraph expansion   │ ? ? = → #                   │ C99 cpp -trigraphs, Ada     │
  │     │                            │                             │ preprocessor                │
  ├─────┼────────────────────────────┼─────────────────────────────┼─────────────────────────────┤
  │ 2   │ Macro-style source         │ $LOG_LEVEL → "info"         │ Jinja2, ERB, m4, cpp        │
  │     │ preprocessing              │                             │                             │
  ├─────┼────────────────────────────┼─────────────────────────────┼─────────────────────────────┤
  │ 3   │ Deprecated syntax          │ foo.bar() { → foo.bar {     │ codemods, 2to3, cargo fix   │
  │     │ migration                  │                             │                             │
  ├─────┼────────────────────────────┼─────────────────────────────┼─────────────────────────────┤
  │ 4   │ Unicode normalization      │ \u00E9 → \u0065\u0301       │ Every compiler that handles │
  │     │ (NFC/NFD)                  │                             │  Unicode source             │
  ├─────┼────────────────────────────┼─────────────────────────────┼─────────────────────────────┤
  │ 5   │ Character set              │ \u0627 → a (Arabic → Latin) │ ICU (our use case, one of   │
  │     │ transliteration            │                             │ many)                       │
  ├─────┼────────────────────────────┼─────────────────────────────┼─────────────────────────────┤
  │ 6   │ Case folding               │ A → a with locale context   │ Turkish dotted/dotless i    │
  │     │                            │                             │ handling                    │
  ├─────┼────────────────────────────┼─────────────────────────────┼─────────────────────────────┤
  │ 7   │ CSS vendor prefix          │ -webkit-transform →         │ PostCSS autoprefixer        │
  │     │ rewriting                  │ transform                   │                             │
  ├─────┼────────────────────────────┼─────────────────────────────┼─────────────────────────────┤
  │ 8   │ JSON/YAML canonicalization │ 'true' → true (strip        │ schema validators           │
  │     │                            │ redundant quoting)          │                             │
  ├─────┼────────────────────────────┼─────────────────────────────┼─────────────────────────────┤
  │ 9   │ Lexer desugaring           │ => → => (distinguish arrow  │ Scala, Kotlin, Rust parsers │
  │     │                            │ tokens)                     │                             │
  ├─────┼────────────────────────────┼─────────────────────────────┼─────────────────────────────┤
  │ 10  │ Source-map-preserving text │ log(x) → console.log(x)     │ codemod tools with source   │
  │     │  edits                     │                             │ locations                   │
  └─────┴────────────────────────────┴─────────────────────────────┴─────────────────────────────┘

  (1), (2), (3), (7), (9), (10) have nothing to do with transliteration or phonetics. They're string
  rewriters. The same compilation machinery serves all ten.

  What's IR-level and what isn't

  New (one IR node type)

  // crates/ir/src/lib.rs — addition to IrNode
  pub enum IrNode {
      // ... existing 20 variants ...
      Transducer {
          rules: Vec<TransducerRule>,
          dispatch: Option<TransducerDispatch>,  // populated by pass 16
      },
  }

  pub struct TransducerRule {
      pub left_ctx: Option<ContextPattern>,
      pub source: IrNode,         // reuses existing Literal/Regex/Alt/Seq nodes
      pub right_ctx: Option<ContextPattern>,
      pub target: TargetTemplate,
  }

  pub enum TargetTemplate {
      Literal(StringId),
      Ref(StringId),              // back-reference to capture from source
      Composed(Vec<TargetTemplate>),
  }

  pub enum ContextPattern {
      StartAnchor,
      EndAnchor,
      Pattern(IrNode),            // reuses Literal/Regex/Alt for context
  }

  pub struct TransducerDispatch {
      pub table: [u16; 256],      // byte → rule bucket index, same shape as AltDispatch
      pub buckets: Vec<SmallVec<[u16; 4]>>,
  }

  Key insight: TransducerRule.source and ContextPattern::Pattern both reuse the existing IrNode enum.
  Char classes, alternations, regexes all work the same way they do in parser rules. The only
  fundamentally new thing is the Transducer node itself — one variant, one dispatch table shape,
  mirroring Alt + AltDispatch.

  Existing passes that extend trivially

  ┌──────────────────────────┬─────────────────────────────────────────────────────────────────────┐
  │           Pass           │                          Extension needed                           │
  ├──────────────────────────┼─────────────────────────────────────────────────────────────────────┤
  │ canonicalize_aliases     │ No-op — aliases don't appear in transducer rules                    │
  ├──────────────────────────┼─────────────────────────────────────────────────────────────────────┤
  │ prune_unreachable        │ Include Transducer nodes in reachability traversal                  │
  ├──────────────────────────┼─────────────────────────────────────────────────────────────────────┤
  │ merge_literals           │ Apply to literal-only transducer rules with same context            │
  ├──────────────────────────┼─────────────────────────────────────────────────────────────────────┤
  │ factor_common_prefixes   │ Runs verbatim on TransducerRule.source as if it were an Alt branch  │
  ├──────────────────────────┼─────────────────────────────────────────────────────────────────────┤
  │ sort_alt_branches        │ Apply specificity ordering across transducer rules                  │
  ├──────────────────────────┼─────────────────────────────────────────────────────────────────────┤
  │ compute_follow_sets      │ TransducerRule.right_ctx becomes the rule's FOLLOW set — literally  │
  ├──────────────────────────┼─────────────────────────────────────────────────────────────────────┤
  │ generate_dispatch_tables │ Produces TransducerDispatch instead of AltDispatch, same CSP/AC-3   │
  │                          │ infrastructure                                                      │
  ├──────────────────────────┼─────────────────────────────────────────────────────────────────────┤
  │ project_types            │ No-op for transducers — output type is always String                │
  ├──────────────────────────┼─────────────────────────────────────────────────────────────────────┤
  │ refine_span_eligibility  │ No-op for transducers — they don't emit spans                       │
  └──────────────────────────┴─────────────────────────────────────────────────────────────────────┘

  Zero hand-written CSP, zero hand-written dispatch analysis. The passes that need extending get
  trivially symmetric handling of transducer rules vs parser alternations. The passes that don't apply
   cleanly skip the new node type.

  New runtime (one walker function)

  // crates/ir/src/transducer/runtime.rs — new, ~100 LOC
  pub fn apply_transducer(node: &IrNode, input: &str) -> String {
      let IrNode::Transducer { rules, dispatch } = node else {
          panic!("apply_transducer called on non-transducer node");
      };
      let dispatch = dispatch.as_ref().expect("transducer not compiled");
      // dispatch-table-driven single-pass walker
      // identical in shape to our current walker but reads from dispatch.table
  }

  New codegen (one output path)

  // crates/derive — in the Rust codegen pipeline
  // For each @transducer in the grammar file, emit:
  impl <ParserName> {
      pub fn <transducer_name>(input: &str) -> String {
          static DISPATCH: once_cell::sync::Lazy<TransducerDispatch> = ...;
          apply_transducer_compiled(&DISPATCH, input)
      }
  }

  What's ICU-specific and stays on our side

  The general facility gives us the compilation engine and the runtime walker. It does not give us:

  1. The ICU rule set itself — default.icu, language-specific folds, CLDR-derived transforms. Pure
  data, lives in ffuzzy.
  2. The language registry — mapping ISO codes to bundled transducer selections. Pure data + trivial
  dispatch, lives in ffuzzy.
  3. CLDR XML compatibility — if we want to consume raw CLDR XML files, that's a preprocessor in
  ffuzzy-cli, not a bbnf feature.
  4. Double Metaphone integration — our PhoneticEncoder still owns the "transducer output → Metaphone
  encoder" plumbing. Transducers produce strings; what we do with those strings is our business.
  5. Phonetic-specific egraph rewrites — if we want to detect "rule A has broader context than rule B
  and same target, absorb B," that's an ICU domain rewrite. It fits the egraph facility but the
  specific rules are ours.

  The split is clean: bbnf owns the transducer abstraction and its compilation; we own ICU rules as
  data. Exactly like bbnf owns parser compilation and our icu_rules.bbnf owns the ICU rule syntax as
  data.

  What the syntax looks like, decoupled from ICU

  A trigraph transducer in the hypothetical general facility:

  @transducer trigraphs {
      "??="  → "#" ;
      "??("  → "[" ;
      "??)"  → "]" ;
      "??/"  → "\\" ;
      "??'"  → "^" ;
      "??<"  → "{" ;
      "??>"  → "}" ;
      "??!"  → "|" ;
      "??-"  → "~" ;
  }

  @import { translation_unit } from "c99.bbnf" ;

  c_source = @transduce(trigraphs) , translation_unit ;

  A CSS vendor-prefix stripper:

  @transducer strip_vendor_prefixes {
      "-webkit-" { [a-z-]+ } → "" ;
      "-moz-"    { [a-z-]+

⏺ You're right — @transform as I framed it was bespoke, and that's a bad smell

  The question exposes the flaw in my proposal: a directive named for our exact use case is a consumer
   reaching into grammar-vendor space. That breaks the abstraction. Let me decompose what @transform
  would need to DO, identify the general primitives underneath, and check each one for independent
  motivation.

  What @transform needed to do

  When I wrote @transform phonetic ; followed by ein { [cons] → an ;, I was conflating four distinct
  capabilities:

  ┌───────────────────────────────────────────────┬───────────────────────────────────────────────┐
  │                  Capability                   │              Is it ICU-specific?              │
  ├───────────────────────────────────────────────┼───────────────────────────────────────────────┤
  │ (1) Parser output mode: instead of producing  │                                               │
  │ a typed AST, the compiled parser emits a      │ No — this is "BBNF as stream transformer."    │
  │ stream (replacement string)                   │                                               │
  ├───────────────────────────────────────────────┼───────────────────────────────────────────────┤
  │ (2) Lookbehind: "match X only if preceded by  │ No — missing PEG primitive, useful for any    │
  │ Y, don't consume Y"                           │ context-sensitive grammar.                    │
  ├───────────────────────────────────────────────┼───────────────────────────────────────────────┤
  │ (3) Unicode char-class algebra: [:L:],        │ No — a gap in BBNF's Unicode story, useful    │
  │ [:Cyrillic:], A & B, A - B, ^A                │ for any grammar targeting real text.          │
  ├───────────────────────────────────────────────┼───────────────────────────────────────────────┤
  │ (4) Ordered-choice rewriting walker: at each  │ No — it's ordered-choice semantics (which     │
  │ input position, try branches in order, first  │ BBNF already has for Alt) applied to stream   │
  │ match fires, fallback copies byte             │ output mode (capability 1).                   │
  └───────────────────────────────────────────────┴───────────────────────────────────────────────┘

  None of these are ICU-specific. @transform was bundling four general capabilities under one
  consumer-facing label. The correct move is to land each general primitive independently, then ICU
  rules fall out as a composition of them with no bespoke machinery anywhere.

  The three primitives (ordered by independent motivation)

  Primitive 1 — mode = "rewrite" parser attribute (general, small)

  A new attribute on #[derive(Parser)]:

  #[derive(Parser)]
  #[parser(path = "slug.bbnf", mode = "rewrite")]
  pub struct SlugSanitizer;

  // Generates:
  //   impl SlugSanitizer { pub fn apply(input: &str) -> String; }
  // instead of the default:
  //   impl SlugSanitizer { pub fn entry_rule<'a>() -> Parser<'a, ...>; }

  Semantics: the top-level rule is still an Alt (possibly with a dispatch table), but the generated
  code is a walker over the input, not a parser returning a typed AST:

  pos := 0
  out := empty
  while pos < input.len():
      if some alt branch matches at pos:
          emit branch.mapping_output to out
          pos += length_of_source_match   # not length including lookbehind/lookahead
      else:
          copy input[pos] to out
          pos += 1
  return out

  Every pattern-matching capability BBNF already has — dispatch tables, ordered choice, FIRST-set
  dispatch, -> mappings — carries over unchanged. The only thing that's new is the surrounding walker
  loop and the output stream. ~150 LOC in bbnf_derive codegen, zero new grammar concepts.

  Use cases independent of ICU:
  - URL slug sanitization (café → cafe, 100% → 100pct)
  - sed/awk-style text pipelines
  - Compile-time text preprocessors (macro expansion, templating)
  - Unicode normalization pipelines composed from sub-rule-sets
  - Code migration tools (old_fn(x) → new_fn(x, default_arg) for api rewrites)
  - Log scrubbing / PII redaction
  - Markdown-to-HTML inline rewriting

  Every one of these is "BBNF's pattern matcher and dispatch optimizer, with a stream output instead
  of an AST output." The primitive stands on its own.

  Primitive 2 — bounded-width lookbehind operator (general, medium)

  BBNF today has >>, << (value projection — keep-right/keep-left), - (set difference, a form of
  negative lookahead), and @{...} (span capture). It does NOT have lookbehind.

  Proposal: |< operator at the same precedence as << / >>:

  # A |< B means "match A then B, keep B's value, expose that B was preceded by A,
  # and advance the cursor by |B| bytes only (A is not consumed)."
  rule = "s" |< "ch" -> "k" ;    # "ch" preceded by "s" becomes "k"

  The engine implementation is bounded-width lookbehind: the walker tracks a k-byte window of recent
  input (k = max lookbehind length across all rules), and at each position checks whether that window
  ends with the lookbehind pattern. This is exactly how production regex engines (RE2, Rust's regex
  crate with ?<=) handle bounded lookbehind.

  Use cases independent of ICU:
  - Context-sensitive tokenization (type-vs-value disambiguation in Rust/C++ grammars)
  - Markdown inline parsing where * behaves differently after whitespace vs. letter
  - Delimiter-sensitive parsing (' as apostrophe vs. string quote depends on preceding char)
  - Regex-with-lookbehind replacement in grammars that currently fall back to hand-written state
  machines
  - Any grammar that today uses - (set difference) to simulate lookbehind awkwardly

  Lookbehind is a missing PEG primitive, not an ICU-shaped hack. The |< operator is ~200 LOC in
  bbnf-ir (new IrNode::Lookbehind variant) + ~150 LOC in the walker codegen (bounded-window tracking).
   The existing FIRST/FOLLOW set infrastructure mostly covers the analysis side; pass 13
  (compute_follow_sets) has a natural symmetric counterpart.

  Primitive 3 — Unicode char-class set algebra (general, mostly borrowable)

  BBNF today accepts bare char classes inside regex literals (/[a-z]/) but has no first-class
  grammar-level sets with:
  - Unicode properties ([:L:], [:Cyrillic:], [:Nonspacing_Mark:])
  - Set algebra (A & B, A - B, A | B, ^A)
  - Named char-class productions

  Proposal: a new grammar production form:

  # Char class as a named production
  consonants = [:L:] - [aeiou] ;
  safe_ident = [:XID_Start:] | "_" ;

  # Inline use
  rule = /[:L:]+/ ;
  rule = /[[:L:] - [:M:]]+/ ;    # letters but not marks

  # As a lookahead/lookbehind constraint
  rule = "e" "n" |< consonants ;

  Use cases independent of ICU:
  - Any Unicode-aware grammar (JSON identifiers, CSS identifiers, programming language parsers)
  - XID_Start / XID_Continue support for identifier parsing (this is what Python, Rust, Swift parsers
  need)
  - Script-restricted grammars (e.g., "only accept Latin letters here")
  - Mathematical notation grammars ([:Sm:] for math symbols)

  Implementation: icu_properties crate provides the Unicode data;
  icu::collections::codepointinvlist::CodePointInversionList provides the set algebra. ~400 LOC of
  wrapper in a new bbnf-unicode crate or inside bbnf-ir.

  What ICU rules look like composed from these three primitives

  # icu_phonetic.bbnf  — a regular BBNF grammar, no special directives

  consonants = [:L:] - [aeiouh] ;
  vowels     = [aeiou] ;

  # Each ICU rule is an Alt branch: lookbehind (if any) |< source >| lookahead (if any)
  # with a "-> target" mapping that the rewrite-mode walker emits.

  phonetic_rule =
        "ein" >| consonants -> "an"
      | "ain" >| consonants -> "an"
      | "oin" >| consonants -> "an"
      | "en"  >| consonants -> "an"
      | "an"  >| consonants -> "an"
      | "on"  >| consonants -> "an"
      | "in"  >| consonants -> "an"
      | "un"  >| consonants -> "an"
      | " "  |< "en" >| " "  -> "an"        # word-delimited French preposition
      | "tion"                -> "sion"
      | "gn"                  -> "ny"
      | "sch"                 -> "sh"
      | "tsch"                -> "ch"
      | "ph"                  -> "f"
      | "ght"                 -> "t"
      | "ck"                  -> "k"
      | "wh"                  -> "w"
      # double-consonant simplification
      | "ss" -> "s" | "ll" -> "l" | "ff" -> "f" | "tt" -> "t"
      | "pp" -> "p" | "nn" -> "n" | "mm" -> "m" | "rr" -> "r"
      | "dd" -> "d" | "bb" -> "b" | "gg" -> "g" | "cc" -> "k"
      | "zz" -> "z"
      ;

  #[derive(Parser)]
  #[parser(path = "src/phonetic/rules/icu_phonetic.bbnf", mode = "rewrite")]
  pub struct IcuPhonetic;

  // Generated:
  //   impl IcuPhonetic { pub fn apply(input: &str) -> String; }

  That's the entire ICU rule compiler on our side. Zero hand-written Rust beyond the #[derive(Parser)]
   marker. Every feature in use is a general BBNF primitive.

  Compare with what @transform would have required: a bespoke grammar sub-syntax (ein { [cons] → an
  ;), bespoke lowering from transform-rule AST to Alt IR, bespoke codegen for the rewrite walker,
  bespoke semantic checks. All of that machinery vanishes — it's subsumed by three
  independently-motivated primitives.

  Cost comparison (honest)

  ┌─────────────────────────┬──────────────────────┬────────┬─────────────────┬─────────────────┐
  │                         │                      │ ffuzzy │   General to    │   Motivated     │
  │        Approach         │  bbnf upstream LOC   │   LOC  │     other       │ independently?  │
  │                         │                      │        │   consumers?    │                 │
  ├─────────────────────────┼──────────────────────┼────────┼─────────────────┼─────────────────┤
  │ @transform directive    │ ~1000 (new AST node, │        │                 │                 │
  │ (my original bad idea)  │  lowerer, codegen    │ ~40    │ No              │ No              │
  │                         │ path)                │        │                 │                 │
  ├─────────────────────────┼──────────────────────┼────────┼─────────────────┼─────────────────┤
  │                         │ ~750 (rewrite mode   │        │                 │                 │
  │ Three primitives        │ ~150 + lookbehind    │ ~40    │ Yes, each       │ Yes, each       │
  │ (decomposed)            │ ~350 + unicode sets  │        │ independently   │ independently   │
  │                         │ ~250)                │        │                 │                 │
  ├─────────────────────────┼──────────────────────┼────────┼─────────────────┼─────────────────┤
  │ Path B from previous    │                      │        │                 │                 │
  │ message (GrammarIR      │ ~300                 │ ~400   │ Yes             │ Yes             │
  │ builder + passes +      │                      │        │                 │                 │
  │ egraph hooks)           │                      │        │                 │                 │
  └─────────────────────────┴──────────────────────┴────────┴─────────────────┴─────────────────┘

  The decomposed primitives are slightly smaller total upstream footprint than the bespoke directive
  (because we're not writing glue code to bridge ICU semantics into bbnf's type system — we're using
  bbnf's existing semantics directly). And each primitive has a pull-quote-worthy motivation
  independent of our use case: rewrite mode for sed/awk-style tooling, lookbehind for
  context-sensitive tokenization, Unicode sets for identifier parsing in modern language grammars.

  The right framing for the upstream PRs

  Each of the three becomes its own bbnf RFC, each sells itself on its own merits, and ICU falls out
  as a happy downstream consequence — mentioned in the rationale as "one motivating use case" rather
  than the whole design justification.

  1. RFC: Rewrite output mode for BBNF parsers
    - Use cases: text preprocessing, URL slugs, migration tools, sed-style pipelines, phonetic
  normalization (ICU)
    - Zero grammar syntax changes; pure codegen addition
    - Smallest of the three
  2. RFC: Bounded-width lookbehind operator (|<)
    - Use cases: context-sensitive tokenization, Markdown inline parsing, delimiter-sensitive
  grammars, phonetic rule context (ICU)
    - New IrNode variant, engine walker extension
    - Medium sized; biggest conceptual add
  3. RFC: First-class Unicode char-class algebra
    - Use cases: Unicode identifier parsing (XID_Start/Continue), script-restricted grammars, math
  notation, phonetic char classes (ICU)
    - Mostly borrowing icu_properties + icu::collections
    - Medium sized; mostly plumbing

  Each PR stands alone. None of them mentions ICU in the title. None of them ships bespoke ICU
  semantics. But after all three land, an ICU rule compiler is just a .bbnf file, because ICU
  transform rules happen to be exactly what you get when you compose those three primitives.

  That's the shape of an upstream contribution that respects abstraction boundaries: we're not asking
  bbnf to absorb our use case, we're asking it to fill three gaps in its general capability surface,
  and we benefit as a downstream consequence.

  Answers to your two questions

  "What would @transform even do?" — The question itself is load-bearing. It would bundle four
  capabilities (stream output mode, lookbehind, Unicode sets, ordered-choice rewrite walker) under a
  consumer-facing label. That's a design smell. The honest decomposition reveals that three of the
  four are general primitives worth adding on their own merits, and the fourth (ordered-choice walker)
   already exists in BBNF's Alt semantics — rewrite mode just wires it to a stream.

  "Is this bespoke to our use case?" — As originally framed, yes, and that's exactly why it should be
  rejected. Decomposed into the three primitives above, none of them are bespoke. Each has multiple
  independent motivations (sed-style tools, context-sensitive tokenization, Unicode identifier
  grammars). ICU rules are a downstream composition, not a first-class grammar concept inside bbnf.

  Updated sequencing

  Replace Stage 4 ("@transform directive") with three independent upstream PRs:

  - Stage 4a — rewrite mode attribute (smallest, most obvious upstream win)
  - Stage 4b — lookbehind operator (biggest, hardest sell, but most valuable to bbnf in general)
  - Stage 4c — Unicode char-class algebra (mostly wiring, biggest Unicode correctness win)

  Once 4a + 4b + 4c land upstream, the entire ffuzzy/crates/ffuzzy-core/src/phonetic/rules/
  subdirectory collapses to one .bbnf file and a one-line #[derive(Parser)] marker struct — and every
  future language/transform addition is a commit to that file.

  Want me to draft any one of those three RFCs in detail (Stage 4a is probably the lowest-friction
  starting point), or should we lock down the Path A implementation first since that's what unblocks
  us today?

✻ Baked for 4m 54s

❯ holy shrimp that's a lot of text. What's the actual proposal here                                   

⏺ The actual proposal

  ICU rules aren't special. They're just pattern-matching with a different output mode. Add three
  small general features to BBNF and our ICU compiler becomes a single .bbnf file.

  The three features (each useful on its own, none ICU-specific)

  1. mode = "rewrite" attribute on #[derive(Parser)] — makes the generated parser emit a replacement
  string instead of an AST. Useful for sed-style tools, URL slug sanitizers, macro expanders. ~150 LOC
   upstream.
  2. |< lookbehind operator — "s" |< "ch" → "k" means "match ch only if preceded by s, don't consume
  the s". Useful for context-sensitive tokenization, Markdown inline parsing, anything that currently
  fakes lookbehind. ~350 LOC upstream.
  3. Unicode char-class algebra — [:L:], A - B, A & B. Useful for any grammar that parses real Unicode
   text (Python identifiers, etc.). ~250 LOC upstream, mostly wiring icu_properties.

  What our ICU code collapses to

  # icu_phonetic.bbnf
  consonants = [:L:] - [aeiouh] ;

  phonetic =
      "ein" >| consonants -> "an"
    | "ph" -> "f"
    | "ght" -> "t"
    # ... ~50 more rules
    ;

  #[derive(Parser)]
  #[parser(path = "icu_phonetic.bbnf", mode = "rewrite")]
  pub struct IcuPhonetic;
  // → IcuPhonetic::apply(input) -> String

  Zero hand-written Rust. ~40 LOC replaces the ~900 LOC I was proposing earlier.

  Why this isn't bespoke

  @transform was — it bundled four capabilities under one ICU-shaped label. The three primitives each
  have independent non-ICU motivations, and ICU compilation falls out as "a composition of things bbnf
   would want anyway." Upstream PRs sell on their own merits; ICU is a footnote in the rationale.

  What to do right now

  - Stage 1 (unblocked by nothing): implement Path A (synthetic .bbnf → compile_grammar → extract
  AltDispatch) as a ~300 LOC stopgap using only public bbnf APIs.
  - Stage 2+ (unblocked by bbnf upstream stabilizing): file three independent RFCs for rewrite-mode,
  lookbehind, and Unicode sets. Each small enough to land quickly, none ICU-flavored.

  Start with Stage 1 Path A, or draft RFC 4a (rewrite mode) first?

❯ why would this replace the CSP?                                                                     

⏺ It doesn't. I conflated two things.

  - CSP = the compile-time analysis that turns "ordered rule set" into "dispatch table" (FIRST sets,
  shadowing, filter propagation).
  - Three primitives = the surface syntax we use to express rule sets.

  These are orthogonal. The three primitives only change how rules are WRITTEN; something still has to
   COMPILE them to a dispatch table.

  The thing that actually "replaces" our CSP work is bbnf's pass 16 (generate_dispatch_tables) — which
   is itself CSP-backed (Agent 4 confirmed: "CSP pre-computation" via AC-3 over FIRST sets). We don't
  rewrite our CSP; we delegate it to bbnf's pre-existing CSP pass by feeding it a GrammarIR. Same
  algorithm, we just don't maintain the code.

  So the corrected framing:

  ┌────────────────────────────────┬──────────────────────────────────────────────────────────────┐
  │            Concern             │                        Where it lives                        │
  ├────────────────────────────────┼──────────────────────────────────────────────────────────────┤
  │ How rules are written (surface │ Three primitives: rewrite mode + lookbehind + Unicode sets   │
  │  syntax)                       │ (or the stopgap: hand-written Rust + synthetic .bbnf)        │
  ├────────────────────────────────┼──────────────────────────────────────────────────────────────┤
  │ How rules are compiled to a    │ bbnf pass 16 — already exists, already CSP-backed, we call   │
  │ dispatch table (CSP)           │ it instead of reimplementing                                 │
  ├────────────────────────────────┼──────────────────────────────────────────────────────────────┤
  │ Rule-set simplification before │ bbnf egraph Layer 1b — already exists, extensible via custom │
  │  compilation                   │  rewrites                                                    │
  ├────────────────────────────────┼──────────────────────────────────────────────────────────────┤
  │ Runtime walker                 │ ~30 LOC that consumes the AltDispatch tables bbnf produced   │
  └────────────────────────────────┴──────────────────────────────────────────────────────────────┘

  Both the grammar extension path and the "hand-write rules in Rust" path route through the same bbnf
  dispatch-table pass. The CSP is never rewritten on our side regardless; it's just a question of
  whether our upstream is a .bbnf file or a hand-constructed GrammarIR.

  The thing I was originally proposing — "let's model rule dispatch as a CSP ourselves" — is the thing
   that's cleanly obsoleted, because bbnf already did it. Everything else (grammar primitives, rewrite
   mode, etc.) is about ergonomics of rule expression, not about the CSP.

❯ So the whole finite-state transducers is not needed either?                                         

⏺ Correct. Not needed upstream.

  The "finite-state transducer" is what we're building at the runtime level, but bbnf doesn't need to
  know about it. Here's the minimum viable split:

  What bbnf does (unchanged, no upstream work): takes a rule set expressed as GrammarIR, runs the
  pipeline (merge_literals, factor_common_prefixes, compute_follow_sets, generate_dispatch_tables,
  etc.), and hands us back the compiled AltDispatch tables. It doesn't care that we intend to use them
   as a rewriter rather than a parser.

  What we do (~50 LOC of Rust on our side):
  - Parse ICU rule text into our own OwnedRule values (the current grammar.rs does this).
  - Construct a GrammarIR directly from the OwnedRules — GrammarIR fields are all pub per Agent 4, so
  this is a struct-literal builder we write ourselves.
  - Call bbnf's passes; get back the compiled IR with AltDispatch tables attached to Alt nodes.
  - Write a ~30 LOC walker that iterates input bytes, looks up the dispatch table, tries matched rules
   in priority order with left-context tracking, emits targets, advances.

  That walker IS the finite-state transducer. It's 30 lines. It doesn't need to be a bbnf feature.

  What I overreached on: proposing the three primitives as upstream work. mode = "rewrite" saves us
  ~30 LOC of walker code — real but tiny. |< lookbehind is cleaner than tracking left context
  manually, but we can handle left context in the walker ourselves by scanning backward from the match
   position (ICU left contexts are bounded-width, usually 1-3 bytes). Unicode char classes are only
  needed if our rules use them; our current default.icu is pure ASCII so it's not pressing.

  So the three primitives are nice-to-haves that would make icu_phonetic.bbnf become a single file
  with zero hand-written Rust, but they're not required. They're the endgame, not the MVP.

  Corrected sizing

  ┌──────────────────────────┬──────────────┬──────────────────────────────┬─────────────────────┐
  │           Path           │  Upstream    │           Our Rust           │      Our .bbnf      │
  │                          │     bbnf     │                              │                     │
  ├──────────────────────────┼──────────────┼──────────────────────────────┼─────────────────────┤
  │ Minimum viable (just use │              │ ~200-300 LOC (existing       │ existing            │
  │  bbnf's passes)          │ 0            │ parser + GrammarIR builder + │ icu_rules.bbnf      │
  │                          │              │  transducer walker)          │                     │
  ├──────────────────────────┼──────────────┼──────────────────────────────┼─────────────────────┤
  │ Grammar-expressive       │ ~750 LOC     │                              │ one .bbnf file,     │
  │ (three primitives)       │ across 3     │ ~40 LOC                      │ zero hand-Rust      │
  │                          │ RFCs         │                              │                     │
  └──────────────────────────┴──────────────┴──────────────────────────────┴─────────────────────┘

  The minimum viable path requires nothing upstream beyond whatever fixes the current pprint breakage.
   Everything I proposed after that is polish.

  Actual near-term plan: ship the minimum viable path. Revisit the primitives later if and when
  there's independent demand for them from other bbnf consumers.

✻ Brewed for 3m 7s

❯ /plan let's develop a concrete plan and path forward, for both BBNF, and herein. three primitives   
is good. The multi-function chaining gap is real and needs to be fixed within BBNF, too. Further, we  
should not need a new E node--doesnt egraph already handle this, like the Language derive?            
  ⎿  Enabled plan mode

⏺ Plan mode active. I have most of the research from the earlier 4 agents — let me do one focused
  verification pass on the remaining unknowns, then write the revised plan.

⏺ Explore(BBNF upstream verification pass)
  ⎿  Done (21 tool uses · 42.3k tokens · 54s)
  (ctrl+o to expand)

⏺ Reading the existing plan file to understand what needs to be revised.

  Read 1 file (ctrl+o to expand)

⏺ Plan state is the old α-ι phase breakdown, which is architecturally superseded by the new
  bbnf-delegation approach. This is a refinement of the same task (ICU rule compilation for Floridify)
   but with a completely different architecture. Rewriting.

⏺ I have the full existing plan plus all the architectural decisions from the conversation. Now
  writing the revised plan that captures:
  - Part A: minimum-viable ffuzzy-side path (no upstream needed beyond bbnf unblocking)
  - Part B: four upstream bbnf RFCs (→ chaining, rewrite mode, lookbehind, Unicode sets)
  - Part C: endgame collapse once upstream lands
  - Part D: completed backend integration preserved

⏺ Updated plan
  ⎿  /plan to preview

⏺ Let me load AskUserQuestion and ask the three open questions.

⏺ User answered Claude's questions:
  ⎿  · How should we sequence Part A (ffuzzy-side compiler, no upstream bbnf changes) against Part B
     (five RFCs to bbnf-lang)? → This is a dev product, we do not have to ship anything immediately.
     BBNF is still undergoing changes right now, those are after the implementations herein.
     · Who authors RFCs B1-B5 on bbnf-lang? → No fork. I own these repos locally.
     · When should CLDR transform ingestion + gap-language folds (Dutch, Polish, Nordic, Turkish,
     Arabic, Hebrew, etc.) land? → Part of Part C migration

⏺ Three clarifying answers — let me update the plan to reflect them before exiting plan mode.

  Key updates:
  - Sequencing: Part A first (dev product, no ship pressure); bbnf changes come after; then Part C
  collapse.
  - Ownership: user owns bbnf-lang locally — no fork/PR process. "RFCs" become direct bbnf-lang edits
  when the time comes.
  - Language scope: full coverage (Romance + Germanic + Slavic + Nordic + Semitic + CLDR transforms)
  ships as part of Part C migration, not as follow-ons.

⏺ Updated plan
  ⎿  /plan to preview

⏺ Updated plan
  ⎿  /plan to preview

⏺ Updated plan
  ⎿  /plan to preview

⏺ Updated plan
  ⎿  /plan to preview

⏺ Updated plan
  ⎿  /plan to preview

⏺ Updated plan
  ⎿  /plan to preview

⏺ Now update the sequencing section to reflect the dev-product-no-rush + user-owns-bbnf reality.

⏺ Updated plan
  ⎿  /plan to preview

⏺ Now clean up the Open Questions section since we've answered them.

⏺ Updated plan
  ⎿  /plan to preview

⏺ Plan is written. Exiting plan mode for approval.