# SK-V21 Pass 0 — The PRECEPTS-RECAP Walk (23 directives, re-adjudicated at HEAD)

Verified at `734bfec51` — the SK-V21 charter commit. No code has landed since SK-V20's close
at `05302109d`; report 16 (`16-head-delta-reverify.md:3-4`) establishes this explicitly. What
follows is therefore not a re-derivation of changed code but a precept-by-precept walk of
`restart/skinny/tranches/sk-v20/PRECEPTS-RECAP.md`'s full evidence table
(`sk-v20/research/06-precepts-recap.md`, 23 rows) against the corrections pass-0 actually made
to that table's own factual claims. This fills Gap G1 identified in
`32-completeness-critic.md:100-106` ("No pass-0 report walks the PRECEPTS-RECAP scorecard
precept-by-precept against HEAD").

Three corrections are load-bearing and recur across directives 21, 9, and 5/18 below:
`perf_cost` is a dead literal at **one** production site, not ~8 (`16-head-delta-reverify.md:21-44`,
confirmed `31-verify-now-frontier.md:27-33`); inline-test violations are **7 core + at least 14
skinny (21+)**, not "7 + 2" (`16-head-delta-reverify.md:101-126`); and the live JSON parse hot
path decodes numbers **hand-rolled**, never via `serde_json` (`21-restart-lacuna-profiling.md:84-108`,
confirmed `31-verify-now-frontier.md:110-125`).

---

1. **No quick solutions / workarounds, gestalt transpositions** — PARTIAL, unchanged. No pass-0
   report re-derives G1–G4/W1/W2; carried forward verbatim from `06-precepts-recap.md:14`.

2. **No hardcoded grammar items — everything grammar-derived** — PARTIAL, sharpened. Report 16
   item 1 reconfirms Lock-14's live gate scans exactly `crates/ir/src` + `crates/analysis/src`
   (`lock14_grammar_generalization.rs:83-87`); `crates/core`, `xtask`, all of `skinny/` sit
   outside. Report 31 item 12 corrects the dig's LOCKS.md-vacuity claim: the verification
   command is at `LOCKS.md:349` (not `:279`), and references a 13-crate topology of which **8**
   paths are phantom (one more than previously counted — `parse-that-regex` was omitted) — "near
   vacuous," not fully vacuous, since 5 of 13 (ir, egraph, csp-solver, analysis, lsp) do exist.
   Net: still effectively RED-by-exclusion, with the phantom-path count corrected upward.

3. **No overfit / over-emphasis of JSON or CSS** — PARTIAL, unchanged. No pass-0 report revisits
   the `json_sink_direct`/`css_scan_direct`/`general_descent` triad or the W3 C-fold status.

4. **Beat lightningcss in every metric** — PARTIAL, held but newly qualified in two directions.
   Report 21 reconfirms the cold CSS structural-parse-vs-full-CSSOM ratio (~2.0–3.6×) survives
   even under loadavg ~9–10.66 (noisier than SK-V20's disclosed 4.35–6.79) — directionally
   sound, but the H1 quiet-loadavg (<1.0) capture still has not run. Report 24 (SOTA CSS)
   independently corroborates the existing depth-asymmetry finding and adds two new facts: the
   vendored `lightningcss` (1.0.0-alpha.71) is several `cssparser` releases stale versus
   upstream, and its ~100+-variant typed `Property` CSSOM is far deeper than bbnf's current
   27-unit hand-curated catalog — so even the disclosed "beat" figure compares against a
   shallower typed depth on bbnf's own side, not just a lazy-vs-eager asymmetry.

5. **JSON beats sonic-rs strict / simdjson-class** — PARTIAL, CORRECTED mechanism. SK-V20 carried
   a "scalar scan + serde_json number decode" narrative for the 51/51 gate figure. Pass-0
   (reports 21 and 31, resolving a direct conflict with report 19) establishes the live parse hot
   path decodes numbers hand-rolled (`parse_number_direct → emit_number_direct →
   materialize_i64/u64/f64`, `generated.rs:768-838`); `serde_json` never runs on that path — it
   is confined to the lazy view's `as_f64` accessor and `Display`/`to_string` surfaces
   (`view.rs:130,232`, `value.rs:94`). The gated win is bbnf's own hand-rolled decode beating
   sonic-rs, not a serde_json-mediated one. No native simdjson/yyjson DOM comparator exists in
   the gate — that absence is unchanged.

6. **Rich AST parity — never flatten typed grammar rules for speed** — ADDRESSED (skinny),
   unchanged. Report 16 item 4 reconfirms three disjoint value substrates persist in totality
   (`ir::vm::interpreter::Value`, nine per-grammar `crates/core/.../value.rs` enums, and a leaked
   `serde_json::Value` in skinny's own public API at `bbnf/src/lib.rs:83`) — this bears more on
   directive 18 below, but it means the "typed values are hand-written per grammar in totality"
   caveat on directive 6 still holds without new contradiction.

7. **ONE monolithic codegen path; no combinator fallback; ONE regex HIR** — PARTIAL, unchanged.
   Not revisited by any pass-0 report.

8. **No backward compat — migrate fully** — PARTIAL, unchanged. Report 16 item 9 reconfirms
   `StructLayout` sites still total 960 (`rg -c StructLayout crates/`), `backend_shape|LayoutFacts`
   still 0 — zero progress on the rename, exactly as SK-V20 stated.

9. **No inline tests in src/** — REGRESSED, WORSE than carried. Report 16 item 5 recounts
   repo-wide: 42 files / 84 `#[cfg(test)]` occurrences. `crates/core` unchanged at 7. The skinny
   side, previously cited as "2 violations," is recounted at **at least 14** non-bench,
   non-tooling production files: `bbnf-regex/src/lib.rs:259`, `bbnf/src/lib.rs:91`,
   `codegen/src/lib.rs:290,1161,1245`, `codegen/src/lower/{css_scan,sink_only}.rs`,
   `grammar/src/lib.rs:1680,1728,1760`, `ir/src/lib.rs:602`, `passes/src/decision_csp.rs:226`,
   `passes/src/lib.rs:1674`, `runtime/src/lib.rs:32`, `runtime/src/grammars/json/scan.rs`. One of
   SK-V20's two citations (`grammar/src/lib.rs:602`) was a transcription slip for `ir/src/lib.rs:602`;
   `grammar/src/lib.rs`'s own three violations (1680/1728/1760) were never listed. Corrected
   figure: **7 core + 21+ skinny**, not "7 + 2."

10. **Singular collection strategy (AoS→SoA)** — PARTIAL/CHRONIC, unchanged. No pass-0 code
    recheck; report 32 (G2) flags that no report has yet reconciled this against the
    DEFERRED-LEDGER line by line — that reconciliation remains an open synthesis-stage gap, not a
    status change.

11. **Direct-to-struct, generalized regex-to-value, no hardcoded pattern lists** — PARTIAL,
    unchanged. Not revisited.

12. **aarch64/Apple-M only** — PARTIAL, CONFIRMED unchanged. Report 16 item 6: `crates/simd-scan`
    still ships `avx2.rs`(370)/`avx512.rs`(149)/`wasm.rs`(433) = 952 dead lines; the Rust emitter
    still stamps dead AVX2 branches into all nine generated grammar files (92 `avx2` hits via
    `rg -c`).

13. **Benchmarks cold per-parse only, never warm/cached** — PARTIAL, unchanged in substance,
    meta-flagged. Report 21's own CSS bench ran cold-per-parse as required but at loadavg
    ~9–10.66; report 32 (Part IV) notes 21 itself ran outside the quiet-loadavg discipline the
    precept demands — the audit meant to assess this precept itself violated its thermal-quiet
    clause. JSON Criterion warm-cache mixing with cold CSS is unchanged.

14. **Build infra first when iteration time is the bottleneck** — ADDRESSED, unchanged.

15. **Never defer optimizations to future tranches** — REGRESSED, unchanged and newly
    illustrated: report 27 (SOTA egraph/CSP) independently restates "`perf_cost ≡ 0` at all eight
    assignment sites" (`27-sota-egraph-csp.md:230`) — i.e. a frontier report written *during*
    pass-0 itself inherited the stale "8 sites" framing that reports 16/31 correct to "1
    production + 6 test-fixture." The chronic ledger (C1 AoS→SoA, C3 BackendShape/perf_cost)
    remains open; no report has yet closed the reconciliation gap flagged in item 10 above.

16. **Every `->` projection must reach the tape emitter** — PARTIAL, unchanged. Not rechecked;
    `typed_materialization_invariant` status not revisited by pass-0.

17. **Docs standalone, no meta-language** — PARTIAL, unchanged. No pass-0 report re-scans
    LOCKS.md's embedded hardening-prompt block; not contradicted.

18. **Full type resolution (HM/CSP/egraph)** — PARTIAL, REINFORCED. Report 28 (SOTA type
    inference) independently confirms no HM machinery exists anywhere (no `TyVar`, no `Scheme`,
    no unification) and that the current system is a monotone lattice fold over a closed
    lattice — exactly SK-V20's finding that the README/ARCHITECTURE HM claim is false. Report 28
    recommends rank-1 HM (Algorithm J + union-find) as the forward path, consistent with the
    existing TW5 target; no contradiction, only external corroboration.

19. **dav1d-style hybrid Rust+ASM emitter** — PARTIAL, unchanged. Report 16 item 3 reconfirms the
    JSON NEON scanner is dead on the live path (`CapacityPlan::GrowOnly` default returns a
    constant 256; `neon::scan` reachable only via non-default env override) — identical to
    SK-V20's characterization. Report 25 (SOTA dav1d/NEON) is forward-looking architecture
    research; it does not re-derive bbnf's current dead-kernel state, only confirms the
    discipline is transplantable.

20. **Generated files always fresh regen, never hand-patched** — ADDRESSED, unchanged (with the
    same disclosed exception SK-V20 already carried). Report 16 item 8 reconfirms skinny's
    `json_templates/{parser,value,view,generated}.rs` still carry `@generated ... do not edit by
    hand` headers while being codegen *input* (hand-maintained, spliced in via `include_str!` at
    `runtime_generator.rs:101,107,122,126,130,281`) — a false-generated-provenance courier. This
    is the same N1-ledgered exception SK-V20 already disclosed (`PRECEPTS-RECAP.md:18-19`), not a
    new finding.

21. **Decision points pluggable (cost model, pattern registry), not hardcoded** — PARTIAL,
    CORRECTED severity. SK-V20 and frontier report 27 both describe `perf_cost ≡ 0` as pervasive
    theatre (~8 sites). Report 16, confirmed by 31, recounts 11 total `perf_cost` hits across
    three files, of which only **`skinny/crates/passes/src/lib.rs:590`** is a live production
    dead-literal-zero; the other six literal zeros sit inside `#[cfg(test)]` fixture blocks
    (boundaries at `lib.rs:2012,2052`, `decision_csp.rs:226`), and `backend_egraph.rs`'s four
    occurrences are the field declaration plus genuine passthrough, never a literal. The field is
    plumbed end-to-end; the fix is a one-line change at the single production constructor, not an
    eight-site sweep. Directive stays PARTIAL (the totality `crates/ir` side still has zero
    `BackendShape`/cost model, per `06-precepts-recap.md:34`), but the skinny-side remediation
    size is corrected sharply downward.

22. **`@import` modules, no monolithic grammar files** — ADDRESSED, unchanged.

23. **Self-hosting via language features** — ADDRESSED, unchanged.

---

## Summary of what pass-0 actually moved

Of 23 directives, **20 are unchanged in status** from `PRECEPTS-RECAP.md`'s adjudication (no
pass-0 report re-derived their underlying code). **Three carry corrected evidence** that changes
the shape of the remaining work without flipping the status label: directive 21 (perf_cost: 1
site not ~8, a one-line fix), directive 9 (inline tests: 7 core + 21+ skinny, not 7+2, i.e. worse
than "not trending down"), and directive 5/18 (JSON hot-path number decode is hand-rolled, never
serde_json — the 51/51 gate figure needs re-narrating, not re-measuring). Two directives (4, 2)
gained sharper qualifying detail without changing PARTIAL status: lightningcss's own typed depth
and staleness (4), and the LOCKS.md phantom-path count (2). No directive moved from its SK-V20
bucket (ADDRESSED/PARTIAL/CHRONIC/REGRESSED) to a different bucket — the frontier and
now-state passes corrected specific facts *inside* existing adjudications, they did not
overturn any of them. The DEFERRED-LEDGER line-by-line reconciliation that would validate
directives 10 and 15 more deeply remains an open gap (`32-completeness-critic.md:108-114`,
G2), not yet executed by any pass-0 report.
