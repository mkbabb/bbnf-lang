# Regex Adjudication: rulings on both critics and the final design

## Grounding

I read origin/master of both repos on 2026-11-05 and changed nothing.

- **Local bbnf-lang master has diverged from origin; it is not just behind.** `master...origin/master` gives 71 commits only on the local side and 38 only on origin. The local branch also no longer resolves as `master` under rev-parse. A fast-forward is therefore impossible. R0's "push to a branch, then make an ordinary merge" is the only lawful route, and the LAW line "fetch and fast-forward" cannot be carried out as written.
- **The simd-scan claim (C1-7) is confirmed.** `crates/simd-scan/src/lib.rs` lines 58, 61, 93 and 100 gate the `avx2` and `avx512` modules behind `cfg(target_feature=…)`. On a default build the `is_x86_feature_detected!` calls inside them are never compiled.
- **The duplicate code (C1-5) is confirmed.** These files exist on bbnf-lang origin/master:
  - `crates/ir/src/passes/regex_info.rs`
  - `crates/core/src/generate/regex/{emit/dfa/{table,accel,mod}.rs, emit/scanner_plan.rs, emit/simd.rs, cost_model.rs, phf.rs, byte_class.rs, last_byte_set.rs}`

  parse-that has its own `rust/regex/src/info/{classify,dfa_size,literal_prefix,one_pass,width}.rs`.
- **The SupersetAbsorbAlt code (C2-1) is confirmed.** In `crates/ir/src/egraph/rules/regex.rs` (around lines 141–158), the rule drops branch j whenever any other branch i contains it, in either order, using `pattern_is_superset` on pattern strings.

## Rulings

Two critic messages arrived cut off: Critic 1 at item 9 ("G-P1…") and Critic 2 at item 6 ("Changing semantics breaks e…"). I rule on everything that arrived. The two cut-off objections are recorded as UNREAD and must be asked again before wave 3 is funded. I have inferred their likely content in ADJ-1 and ADJ-2.

| # | Objection | Ruling | Reason |
|---|---|---|---|
| C1-1 | A post-match guard for a trailing lookahead gives the wrong answer | **ACCEPT** | `/\d+(?!\.\d)/y` on `"12.5"` gives `"1"` in V8, because V8 backtracks into the core. A guard checked only after the match ends rejects the whole match, so the design was incorrect as written. RegexInfo gains an `end_unambiguous` fact: no accepting prefix exists before the leftmost-first end. The guard lowering applies only when that fact holds. Otherwise the regular lookahead is folded into the automaton by a product construction, and a non-regular one goes to the linear-lookaround tier. The coupling to FOLLOW is **removed**: FOLLOW is a static set of next tokens, whereas a lookahead changes which match is selected. |
| C1-2 / C2-2 | UTF-8 vs UTF-16 leaves the meaning undefined | **ACCEPT (merged)** | OR-1 is restated: the canonical meaning is u/v-mode semantics over code points. The Rust backend reads a leaf without u as u, with a lint (`BBNF-RX-NOU`). A leaf whose behaviour depends on astral input or on non-u case folding is an error. The TS emitter adding `u` is deferred until wf_d837f96c closes, and then applies only to leaves that parse as valid u patterns; adding u blindly changes escape validity. The harness generates only valid Unicode scalar values and maps offsets UTF-16↔UTF-8 by code-point index. Each divergence is classified ENGINE, ENCODING-EXCLUDED or DIALECT-EXCLUDED. The exclusion classes are a finite named list in the gate, and "0 divergences" applies to ENGINE only. The Kelvin probe runs twice (`/i` and `/iu`), plus the `/iu` `\b`/`\w` set (U+017F, U+212A). |
| C1-3 | regex-automata is not a correct oracle | **ACCEPT** | Its `(?m)$`, `\b`/`\w`, case folding and `\s` all differ from ECMAScript. **V8 is the authority.** regex-automata, or whatever replaces it under OR-7, is an oracle only under a committed applicability predicate, and that predicate is itself tested on both sides. **JavaScriptCore via bun is the second JS oracle**, because value.js ships to Safari. A disagreement between V8 and JSC is a SPEC-AMBIGUITY flag, never a pass. Each oracle runs as one guarded subprocess per run. |
| C1-4 / C2-3 | Build vs adopt for the regular fragment was never evaluated | **ACCEPT, as new unit R1.5 and owner ruling OR-7** | The BBNF law governs grammars, not automaton engines, so adopting regex-automata as the determinizer and minimizer is lawful. The spike is a decision, not an assumption. On the 121 value.js leaves plus the sk-v24 56, it measures fidelity, compile time, state counts and emitted-code speed for both routes. **Route A (adopt):** lower our Hir into `regex_syntax::hir::Hir`, pre-expand the ECMAScript classes and Canonicalize, and read the dense DFA's transitions for codegen. **Route B:** keep the in-house engine. Two known risks for A are measured explicitly. First, JS line terminators (`\r`, U+2028, U+2029) under the m flag have no equivalent among regex-automata's look kinds; the workaround is a pre-split or a rewrite to lookaround, with fidelity measured. Second, ECMAScript `\b` must lower to `(?-u:\b)`, which is only correct with ASCII `\w`; under `/iu` it is not. My recommendation is **A for the regular fragment, unless the spike finds a fidelity gap on the value.js corpus that cannot be closed**. If A is chosen, R2, R3 and R7 shrink to an adapter plus an emitter, and the in-tree regex-automata oracle is replaced by the PCRE2 *interpreter* (not the JIT) in anchored mode, on the shared fragment, so the system under test and its oracle share no code. |
| C1-5 | "One home, one analysis" ignores code that already exists twice | **ACCEPT** | Verified above. R8 and R10 are recast as **convergence**, not construction, with a keep/merge/delete table (§B). Falsifier: a grep shows 0 `RegexInfo` definitions outside bbnf-regex, and 0 class-scan or DFA-tier decisions outside `plan()`. |
| C1-6 | Cross-repo dependency cycle | **ACCEPT** | OR-4 moves into R0. The crate DAG is fixed in §A. `Budget` is a plain struct owned by bbnf-regex; the CSP is a client of the engine. The pprint integration moves to an adapter in the formatter crate, behind a visitor. The skinny shadow `bbnf-regex` crate (a name collision) is deleted in R8. |
| C1-7 | The x86 premise is false in the tree | **ACCEPT** | Verified. OR-2 now also rules on the existing `avx2.rs`, `avx512.rs` and `wasm.rs`. Recorded fact: the owner order of 2026-11-05 asks for `is_x86_feature_detected` explicitly, which conflicts with O033; the owner rules. Any x86 code that survives uses `#[target_feature(enable)]` functions plus runtime detection, with no `cfg(target_feature)` gate. A CI x86-64 job proves each tier is reachable on a default build, using Intel SDE for AVX-512 because hosted runners are not guaranteed to have it. |
| C1-8 | Teddy and G-P4 target unanchored search, not sticky leaves | **ACCEPT IN PART** | Anchored literal sets use `phf.rs` or a trie, which stay. Teddy survives **only** for named unanchored consumers: `find_next_structural_from` (the generated code already calls it), error-recovery resync and comment skip. G-P4 is measured only on those consumers. If R8's audit finds none on a hot path, Teddy and G-P4 are dropped, not built speculatively. R9's effort goes first to shufti/truffle class runs, accel self-loops and the UTF-8 valid-prefix check. |
| C1-9 | (cut off, about G-P1) | **UNREAD** | See ADJ-1 for the provisional repair. |
| C2-1 | The e-graph proof uses the wrong equivalence, and the grammar rules depend on semantics the design changes | **ACCEPT, with one refinement** | Equivalence is defined as equality of the sticky end function `end(x) = max{|p| : p a prefix of x, p ∈ L_cut}`. Here `L_cut` is the language accepted by the leftmost-first DFA after the priority cut. Two leaves are interchangeable exactly when their `L_cut` are equal, so **minimal-DFA isomorphism on the priority-cut DFAs** is the correct certificate; isomorphism on the plain language is not. `a|ab` and `ab?` have equal languages but different `L_cut`, so the check rejects that rewrite, as it should. `FuseAltRegexBranches` is disabled for the Rust backend until R3 lands. Born-RED tests: fused `Alt(/a/,/ab/)` on `"ab"` versus the unfused form, and `SupersetAbsorbAlt` on `[é]` / `[α-ω]` (labelled PROBABLE until it runs). The absorb and union rules are ported from pattern strings onto RegexInfo's `FirstSet` with the precondition `min_len == max_len == 1`. **Refinement (adjudicator):** absorbing is also sound only when branch identity is not observable, meaning no distinct node tag or action per branch. That becomes a precondition of the rule and a test. The grep in R8 is extended to `crates/ir/src/egraph/rules/`. |
| C2-4 | R10 depends on R12, but R12 is scheduled later | **ACCEPT** | The printer is split. `Hir::to_source(flags) -> String` goes to R6 (wave 2) and doubles as the round-trip oracle for the self-hosted parser. `Hir::to_doc` stays in R12. A V8 law is added: the original and printed forms agree under V8 **and JSC** on the same generated inputs. |
| C2-5 | Wave 2 runs in parallel over shared state | **ACCEPT** | New unit **R1.6, ABI freeze**. It freezes the Hir node set, with Look, Lazy, LookAround and BackRef as parse-only placeholders. On Route B it also freezes the state type `State{nfa_set: OrderedSet, look_have, look_need, match_prio}`. R2 then R3 run in sequence: R3 takes master by ordinary merge after R2. R4 and R6 run in parallel. R7 targets the final state type. Under Route A the state type belongs to regex-automata, and only the Hir ABI is frozen. |
| C2-6 | (cut off, "Changing semantics breaks e…") | **UNREAD** | See ADJ-2 for the provisional repair. |

### Additions from the adjudicator

- **ADJ-1, G-P1 repair (provisional, pending C1-9).** G-P1 compares *the same DFA*: our direct-coded or table emission against regex-automata's interpretive dense DFA, with state counts and the `Plan` tier reported for each leaf. Under Route A this measures codegen alone, which is the honest claim. Leaves where `plan()` chooses a non-DFA tier (Literal, ClassRun) are reported in a separate row. Otherwise a planner win would be disguised as a DFA win.
- **ADJ-2, migration of existing grammars (provisional, pending C2-6).** Moving the Rust backend from longest-match to leftmost-first changes accepted parses for existing `.bbnf` grammars. Before R3 merges, R1's harness runs every in-tree grammar's test corpus plus the value.js corpus under both semantics. Every changed parse is listed in a migration ledger, and each one is either a corrected bug (TS already behaved this way) or a grammar edit made at the root. Compatibility shims are banned by standing law.
- **ADJ-3, the R0 fact.** Local and origin master have diverged by 71 and 38 commits, so the R0 merge is a real merge that needs conflict review, not a formality. It runs on a branch, and CI must be green before the ordinary merge.

## Final design (as adjudicated)

### §0 Thesis, amended

A BBNF regex leaf means **the ECMAScript sticky leftmost-first match at a position, under u/v code-point semantics**. A leaf without the u flag is linted and read as u in Rust. The authorities are V8 and JSC; a disagreement between them is SPEC-AMBIGUITY.

The structure:
- **One parser:** BBNF-generated.
- **One analysis:** RegexInfo, including `end_unambiguous`.
- **One dispatch surface:** `plan()`.
- **One kernel home:** `parse_that::scan`.

The regular-fragment determinizer is chosen by OR-7 (adopt or build).

### §A Crate DAG (acyclic, settled in R0 under OR-4)

```
egraph, egraph-derive, csp-solver   (generic leaves; published)
        ▼
bbnf-regex   (parser [generated], Hir, to_source, RegexInfo, Plan [versioned, serde], Budget [plain struct];
              optional dep regex-automata if OR-7 = A)
        ▼
parse_that   (scan kernels + scalar twins; interpretive Plan exec)
        ▼
bbnf-lang crates: ir (interns RegexInfo; Recognizer::Regex/ByteClass), core codegen,
                  csp client (produces Budget), formatter (pprint adapter: Hir::to_doc via visitor)
```

Rules for the DAG:
- `[patch.crates-io]` local overrides are banned in committed config.
- If egraph and csp-solver cannot be published, the fallback is to move bbnf-regex into bbnf-lang, with parse_that consuming `Plan` from it. OR-4 picks one. No unit starts work on the crate location before that ruling.

### §B Convergence table (R8 and R10)

| Existing file | Disposition |
|---|---|
| bbnf `crates/ir/src/passes/regex_info.rs` | **Delete**. Replaced by bbnf-regex RegexInfo, interned by the IR. |
| parse-that `rust/regex/src/info/*` | **Keep** as the RegexInfo home, adding `end_unambiguous`, look_need, guards and rarity. |
| bbnf `generate/regex/emit/dfa/{table,accel,mod}.rs` | **Keep and merge.** These become R10's emitter, reading Plan instead of re-deciding the tier. |
| bbnf `emit/scanner_plan.rs`, `cost_model.rs` | **Merge** into `plan()`, then delete. |
| bbnf `phf.rs` | **Keep.** The anchored LiteralSet lowering. |
| bbnf `byte_class.rs`, `last_byte_set.rs` | **Merge** into RegexInfo facts. |
| bbnf `emit/simd.rs`, `crates/simd-scan`, skinny `bbnf-simd` | **Fold** into `parse_that::scan`, then delete. |
| skinny `crates/bbnf-regex` | **Delete** (the name collision). |
| parse-that `rust/regex/src/algebra/{superset,union}.rs` (string-based) | **Port** onto RegexInfo, then delete the string paths. |
| `emit/hir/*`, `emit/generalized/*`, `patterns/*` | Each is audited in R8 and gets keep or delete with a reason. No unit may leave a file unlisted. |

### §C Goals (amended)

- **G-C1:** 0 **ENGINE** divergences against V8 **and** JSC (full dialect), on at least 10⁷ generated cases (valid scalar values only) plus the fixed corpora, with the exclusion classes listed by name. The second in-process oracle is regex-automata under its applicability predicate (Route B) or the PCRE2 interpreter on the shared fragment (Route A).
- **G-C2, G-C3, G-C4:** as designed. G-C4 adds the CI x86-64 reachability job (C1-7) and a mutant on the cut-language certifier.
- **G-P1:** as repaired in ADJ-1.
- **G-P2, G-P3, G-P5, G-P6:** as designed.
- **G-P4:** restricted to the named unanchored consumers. It is dropped if none exists.
- Benchmark honesty, the quiet window (load < 8, `--cpu-cores 0`, waited for), and the NOISY labelling all stand.

### §D Units (amended)

- **R0. Ground.** Merge the diverged local master (71/38) through a branch with an ordinary merge. **OR-4 crate DAG ruled here.** egraph/csp home, sccache, the `parser_test.rs` root fix, and the `rust/regex/CLAUDE.md` correction. Falsifier unchanged.
- **R1. Semantics plus oracle.**
  - The spec section uses the OR-1 wording from §0.
  - The harness covers V8, JSC, the offset mapper, the divergence classifier and the applicability predicate.
  - Born-RED ledger (9 entries, where the original 5 + 2 count 8 because Kelvin is split):
    - the original 5: `foo$`, `a|ab`, `a*?`, `\b`, and Kelvin split into `/i` and `/iu`;
    - fused `Alt(/a/,/ab/)` on `"ab"`;
    - `SupersetAbsorbAlt` on a non-ASCII class;
    - the `/iu` `\b` set.
  - The ADJ-2 dual-semantics run over every in-tree grammar corpus produces the migration ledger.
- **R1.5. Adopt vs build spike (OR-7).** Both routes on the 121 + 56 leaves: fidelity, compile time, state counts, emitted speed. The m-flag terminators and `/iu` `\b` are measured explicitly. Result is an owner ruling.
- **R1.6. ABI freeze.** The Hir node set with placeholders; the State type under Route B only.
- **R2 → R3 (sequential).** Look as an automaton condition, then the leftmost-first priority cut. Under Route A this is the adapter lowering plus fidelity tests. `FuseAltRegexBranches` is re-enabled for Rust only after R3's gate.
- **R4 ∥ R6.** Unicode: Canonicalize for u/v and the generated tables pinned to node 24's ICU (OR-6), checked against JSC. Self-hosted parser **plus `Hir::to_source`**, plus the V8/JSC print law.
- **R5. Non-regular tier.**
  - Guard lowering only when `end_unambiguous` holds; otherwise the product construction, or the Barrière/Pit-Claudel linear tier. RE# is evaluated.
  - BackRef goes to the bounded Backtrack tier with a typed `BudgetExceeded` error.
  - Falsifier adds `/\d+(?!\.\d)/y` on `"12.5"` → `"1"`.
- **R7. Minimizer.** Valmari–Lehtinen, Route B only. Under Route A it becomes a doubling-fit check on regex-automata's minimizer.
- **R8. Convergence to `plan()`.** Executes §B. The grep falsifier covers skinny, `crates/ir/src/egraph/rules/` and `generate/regex/`. Absorb and union rules move onto RegexInfo, with the branch-identity precondition.
- **R9. SIMD kernels.**
  - First: shufti, truffle, accel skip, the UTF-8 prefix check.
  - Teddy only if R8 named a consumer.
  - NEON is the baseline. x86 per OR-2 using `#[target_feature]` plus runtime detection.
  - Intrinsics first; `asm!` only where a deficit is both inspected and measured.
  - A checkasm-style harness, scalar twins, a differential fuzz of at least 1 h guarded, exhaustive lengths 0..=128 at every alignment, a PROT_NONE tail test, and Miri under `force_scalar`.
- **R10. Codegen convergence.** The kept emitter consumes Plan; Guarded/Backtrack lowering is added. The TS side (pattern text from `to_source`, facts from RegexInfo, `u` addition) waits for wf_d837f96c to close. Gates: G-P1 (per ADJ-1), G-P3, G-P6.
- **R11. Competition.** As designed. The planner de-tier mutant must show a measured regression.
- **R12. pprint.** `Hir::to_doc` and `RegexError::to_doc` in the formatter adapter. Blocked on pprint being pushed (39 unpushed commits, `FmtBuilder`/`FmtOp` missing from origin).
- **R13. Close.** A census re-run must show every RX row resolved. OR-4 has already been ruled in R0.

### §E Wave lock-step (amended)

1. R0 and R1
2. R1.5, then R1.6 (gate: OR-7 ruled)
3. R2 → R3, alongside R4 ∥ R6
4. R5, R7, R8
5. R9 and R10. The TS half of R10 is held until wf_d837f96c closes.
6. R11 and R12
7. R13

Before wave 4 is funded, C1-9 and C2-6 must be received and ruled.

### §F Owner rulings outstanding

- **OR-1:** u/v code-point leftmost-first, with the non-u lint. Needed by R1.
- **OR-2:** the x86 question, now including the existing avx2/avx512/wasm modules and the 2026-11-05 order's explicit `is_x86_feature_detected` request against O033. Needed by R9.
- **OR-4:** the crate DAG. Needed by R0, moved earlier.
- **OR-6:** the UCD/ICU pin. Needed by R4.
- **OR-7 (new):** adopt or build for the regular-fragment determinizer, from R1.5's numbers. Needed by R2, R3 and R7.

Concurrency law stands: nothing in bbnf-lang `typescript/`, parse-that's release workflow or value.js's BBNF pin is touched while wf_d837f96c is open. Scratch output goes in `~/.dev-logs/bbnf-lockstep/` and the fuzz corpus in `~/.dev-logs/bbnf-lockstep/regex-fuzz/`.