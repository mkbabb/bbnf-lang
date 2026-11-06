# LOCK-5 — THE REGEX CONTINUUM — PASS-3 RESEARCH DIRECTIVES

Settled ground (XL-C, RATIFIED — fact, do not re-open). Two domain-fit types: grammar tier keeps
`Regex(StringId)` opaque; engine tier keeps a rich `Hir` grown with `Class`/`Look`; a cheap
`string`/`format!` bridge reusing the engine's `simplify_hir` joins them. Canonical engine home is
parse-that (one-regex-home); the vendored parse-that-regex inside skinny is the MEASUREMENT SUBSTRATE
(base-pin every number to HEAD, XL-H). Charter: HARDEN this substrate with run evidence — not
re-argue which type-plane wins. SPEC-ONLY(grammar-driven) rides on every finding until LOCK-7/7A.

## LEAF-GROWTH (Class/Look on the engine Hir)
- Q1. WRITE the per-pass dispatch census: enumerate + build every arm across the IR passes that must
  handle the grown `Class`/`Look` leaves (first_sets, regex_info/collect_regex_ids, types/generate,
  scalar_routing, materialization/classify, vm/compiler/node) — don't estimate. Evidence: exact LOC
  before/after per arm; does classify re-home SHRINK or merely RELOCATE (LOC delta, not a one-liner)?
- Q2. Route `Class`/`Look` typing through LOCK-1's real HM (row-poly + widths), not a stub
  `BuiltinTy`. Where does `[α-ω]*` byte-expansion live — confined to automata, or does it leak into
  the shared type? Evidence: the typing derivation + the confinement boundary.
- Q3. Automata parity re-derived FROM the narrowed type: `from_expr` NFA byte-identical to vendored
  `from_hir` (Group-elision, `Optional↔Repeat{0,1}`). Evidence: byte-identity harness + freshly-run
  throughput (derive numbers from the type; carry NO inherited band).
- Q4. Build a `Look`-consumer test that can FAIL — e.g. `/foo$/` rejects `"foobar"`, `/foo/` accepts.
  Evidence: the failing/passing assertion actually exercised, `Look::Start`/`End` consulted.

## AUTOMATA-AS-EXTRACTION
- Q5. Build a REFERENCE O(n·k·log n) Hopcroft control and measure minimization-as-congruence against
  IT (not the incumbent). On a GROWN corpus (≥50, tagged acyclic/cyclic) with `is_acyclic` dispatch
  folded across ALL patterns: does the acyclic fast path beat a correct Hopcroft, or does deleting the
  restart erase its reason to exist? Evidence: corpus manifest, both timings, congruence-rebuild
  complexity measured against the O(n·k·log n) claim.
- Q6. Tier selection from `(cardinality, literal-prefix, byte-rarity)`: earn table-vs-CSP. Evidence:
  exhibit a concrete global codegen/LUT-budget coupling witness, or show none exists (then it is local).

## GRAMMAR-FRAGMENT LOWERING
- Q7. Measure the FOLLOW-sound one-pass fraction over the engine corpus with real leaves (a sound
  predicate that checks the class against its own FOLLOW). Evidence: the fraction, both under a
  straight-line lowering and under `Seq[Repeat,suffix]` + grammar FOLLOW — measure BOTH fresh.
- Q8. Prototype: cold-paired class-scan vs prefiltered `Dfa::find`; a `ByteClassRecognizer` that
  actually fires (not the masked wildcard); `BackendExpr::ByteClass` reaching the tape; capture-leaf
  → `Project` byte-identical to a hand-authored grammar capture. Evidence: cold pairs + tape output.

## GRAMMAR-DRIVEN PATH + CROSS-LOCK EVIDENCE
- Q9. Exercise ONE grammar-driven regex path: promote `bootstrap/regex.bbnf`, make `parse_generated()`
  return its AST instead of `bool` (break-the-discard), de-dup `grammar/misc/regex.bbnf`. Evidence:
  the AST actually threaded through, grammar-totality picture; gated on LOCK-7/7A, SPEC-ONLY.
- Q10. Census the grammar-tier regex e-graph rewrite rules: over the corpus, how often does each rule
  FIRE on real branches (both bare and projection-wrapped)? Evidence: per-rule fire counts.
- Q11. Confirm with LOCK-1 the concrete representation of `->` (inner-class `Map` barrier vs typed
  edge). Evidence: the ratified representation — it is binding on the bridge's projection soundness.
