# LOCK-5 — THE REGEX CONTINUUM — PASS-2 RESEARCH DIRECTIVES

Every number must name the tree it was produced in and be re-measured in the reconciled tree of record. Two live snapshots hold incompatible substrate: skinny `crates/ir` (regex leaf = opaque `ExprKind::Regex{pattern:String}`, no grammar-tier regex e-graph) and top-level `crates/ir` (interned `IrNode::Regex(StringId)` + a 434-LOC grammar-tier regex e-graph). A result in one tree is not a result in the other — state which tree each measurement ran in and re-run in the reconciled tree.

Grammar-driven regex is spec-only today. Two `regex.bbnf` already exist — `parse-that/rust/bootstrap/regex.bbnf` (whose `parse_generated()` discards its AST) and `grammar/misc/regex.bbnf` (divergent) — and skinny hand-scans regex to an opaque `String` (`grammar/src/lib.rs:1561`). Treat any grammar path as promote + de-dup + break-the-discard, never greenfield authoring.

## Formulation A — regex as a fragment of the one HIR (add `Class{set,negated}`, `Look{kind}` to `ExprKind`)
- Re-run the per-arm leak census on the *current* HEAD enum (10 variants incl. `Project`) AND the LOCK-1/LOCK-3 target enum: with real `->`/`Project` typing present, which arms carry regex-specific concerns? Evidence: per-arm census on both, not the 8-variant snapshot.
- Route `Class`/`Look` typing through LOCK-1's real type system (HM + row-poly + widths), not skinny's stub `BuiltinTy`. Does `Class` reuse the literal byte type, or does a scalar-byte/UTF-8 concern leak into the shared enum? Where does `[α-ω]*` expansion live — confined to automata, or in the shared type? Evidence: inferred types under real typing.
- Name a grammar-side consumer that *reads* `Look` (e.g. an anchored/unanchored distinction) and build a test that FAILS if `Look` is carried-but-never-read — or show `Look` belongs as an annotation, not an enum leaf. Evidence: a consumer test that can fail.
- Build automata directly from the merged fragment (`from_expr`) byte-identical to the vendored `Hir`→NFA path, then re-derive find/scan throughput *from this type*. Evidence: byte-identical NFA/DFA + freshly-run throughput, no inherited band.
- Can one type serve the saturation cache (`Eq+Hash`) and the IR (arena/`ExprId`/span/`Serialize`) without a god-type? Does re-homing the ~1148-LOC classify pass as an IR analysis shrink it or merely relocate it? Evidence: single type meeting both; classifier LOC before/after a prototype that touches it.

## Formulation D — grammar-fragment / transition-action byte-recognizer
- Over the engine corpus with real leaves, what fraction of regex sites pass `check_one_pass_eligible`? Evidence: the measured fraction — report it whatever it is.
- Cold-paired, does a straight-line byte-class scan beat a prefiltered `Dfa::find` (start-state memchr), `count_a==count_b`? Evidence: cold-paired head-to-head; a tie reported as a tie.
- Does that eligibility predicate correctly reject a repetition whose class overlaps its own FOLLOW (`[a-z]+z` on `"abz"`, greedy give-back)? Evidence: soundness test on the overlap case.
- Does `derive_recognizers` actually *select* on `ByteClass`, or does its `_ => {}` wildcard silently swallow it? Evidence: proof the chooser fires on `ByteClass`.
- Can a recognizer carry `->` projections on its transitions (Ragel transition-action style) and still compile to one DFA+prefilter per site? Do zero `ExprKind::Regex` survive `regex_desugar`, and does `BackendExpr::ByteClass` reach the tape emitter? Evidence: projection-carrying lowering + survivor count.

## Formulation B — two HIRs joined by a functor / joint coproduct e-graph (run in the tree that holds the 434-LOC grammar-tier regex e-graph)
- Build a cross-domain rewrite that fuses regex+literal grammar leaves (`FuseAltRegexBranches`) and the functor `F ⊣ G`. When the regex algebra is written once, does the duplicate grammar-tier rule copy die — by how many rules? Evidence: rule-death count in the named tree that holds the copy.
- Build `insert_bbnf`/`extract_bbnf` + a round-trip oracle (strict off, canonical on, DFA-equivalence): do span and projection survive `F∘G`? What is the generate-axis cost vs `analyze()`? Evidence: oracle result + span/projection survival + cost.
- Does the reconciled tree keep a grammar-tier regex e-graph at all? If so, model the two as a joint coproduct (`Left(HirENode)|Right(GrammarENode)`, bridge rewrites folded pre-emission, MLIR `unrealized_conversion_cast` style) — does it need the cross-domain vehicle? Does the fleet even carry a rich regex leaf (LOCK-2 Island A)? Evidence: coproduct prototype + leaf-richness census.

## Formulation C — automata as e-graph extraction
- Does an acyclic-congruence minimization fast path beat a *correct* reference O(n·k·log n) Hopcroft — not the incumbent naive Moore + global restart at `dfa.rs:358` — with the `is_acyclic` dispatch cost folded across ALL patterns? Evidence: reference-Hopcroft control + crossover point.
- Price the null: if you simply fix/delete the `dfa.rs:358` restart, does the minimizer's advantage vanish? Evidence: the delete-the-restart baseline priced.
- Can derivative saturation over the same derived `Language` as the HIR rewrite theory serve as determinizer, minimizer, or both? Measure the saturation ratio ρ across the corpus, exhibit any equivalence the finite rewrite set fails to reach, and characterize its completeness ceiling. Fix the gate/corpus mismatch (gate cites 50 patterns, corpus holds 26): re-cut or grow, and state which.
- Build the prefilter/tier stage: is there a *global* constraint (shared SIMD-LUT / codegen budget across sites) that makes tier-selection a genuine CSP, or does a pluggable cost-sorted table suffice? Exercise determinism/tie-break for bit-identical regen. Evidence: a global-constraint witness or a table-suffices demonstration — earn whichever holds.

## Cross-lock interlocks (settled, shareable)
- The Axis-1 type-plane choice gates LOCK-4-4D, LOCK-9-9C, LOCK-3-3C; every `ExprKind`/`BackendExpr` edit is LOCK-3-governed and the typed-materialization invariant binds the new leaves — `ByteClass` must reach the tape emitter.
- The prefilter/tier CSP is double-homed with LOCK-2's scan-plan CSP — a single home must be decided. LOCK-5 owns the HIR→DFA→classify substrate; LOCK-9-9C consumes it or retires.
- The rust/ts face of any merged/functored regex HIR (`Class`/`Look`/`ByteClass` + automata crossing to TS, PROVE-14 tape) is a named gap for LOCK-6.
