# SK-V20 — The Precepts Recap (every standing directive, adjudicated at HEAD)

Date: 2026-06-10. HEAD: `36ca10646`. Full evidence table: `research/06-precepts-recap.md` (23 directives,
each with file:line evidence). This document is the adjudicated summary plus the disposition of every
directive that is not fully addressed. Verifier corrections from `research/29-verify-*.md` are applied.

## Scorecard

23 standing directives: **4 ADDRESSED · 10 PARTIAL · 4 CHRONIC-VIOLATED · 4 honest-but-incomplete ·
1 REGRESSED** (inline tests). No directive is wholesale abandoned; the dominant pattern is
*addressed-in-skinny, open-in-totality* — the prototype absorbed the discipline, the product tree did not.

## Fully addressed (no action)

1. **Grammar extensions via `@import` modules** — CSS L4 and BBNF-self both factored; exercised end-to-end.
2. **Self-hosting via language features** — `bbnf.bbnf` self-hosts with `@import`/`->`/`?w`/regex literals.
3. **Build-infra-first** — `ax-iter` profile, `xtask regen --check`, build-time manifest generation.
4. **Clean-regen discipline (full tree)** — `--check` byte-equality drift gate in place; the residual false
   `@generated` couriers are a *skinny* violation, ledgered as N1.

## Addressed in skinny, open in totality (the cutover carries these)

5. **No workarounds / gestalt transpositions** — skinny G1–G4 landed; core still rides the eager builder
   ladder + AoS tape + per-grammar xtask fork. → TW1/TW2.
6. **aarch64-only** — skinny pruned clean; `crates/simd-scan` still ships avx2/avx512/wasm, and the emitter
   stamps dead AVX2 bodies into all nine generated grammars. → TW0 deletion.
7. **ONE codegen path** — skinny deleted `RuntimeEmitterKind` but still has three emit bodies; totality's
   xtask is itself the per-grammar fork (regen_css vs regen_simple_runtime's four RuntimeStyle arms). → TW2.
8. **Pluggable decision points** — skinny has the shape (BackendShape + egraph + CSP) but the cost model is
   theatre (perf_cost ≡ 0; the CSP re-derives its input); core has nothing. → TW4 builds the real one.

## Chronic structural violations (the never-defer precept is itself the casualty)

9. **Never defer optimizations** — REGRESSED: six chronic items (DEFERRED-LEDGER §1), the worst at five
   cycles. The re-founded SPEC adopts the rule: a new planning cycle may not open while the chronic ledger
   is non-empty.
10. **Singular collection strategy** — AoS TapeRec is the fifth consecutive re-defer of W2. → TW1.
11. **Every `->` reaches the tape emitter** — `typed_materialization_invariant = "not_built:w5"` on every
    bench row; core's typed values are hand-written per grammar, not derived. → TW3.
12. **No inline tests in src/** — REGRESSED: 7 core + 2 skinny violations, flat across cycles. → TW0.

## Honest but incomplete (claims must be requalified until closed)

13. **Beat lightningcss in every metric** — ratios (1.50–3.27×) are real within-plane but: lazy-HEAD vs
    eager-full-CSSOM (disclosed in FRAMING.md, not co-located at claim sites); ALL absolute Mbps captured
    under loadavg 4.35–6.79 (directional only); H1 binding close (quiet re-capture) never ran. → TW0.
14. **JSON beats sonic-rs strict / simdjson-class** — 51/51 A/GO vs sonic-rs strict is gated, BUT: the
    figures are Criterion warm-cache (violating the cold-only precept as a *claim plane*); the widest
    margins are on the validate-and-discard parse_only lane; native simdjson/yyjson DOM are not gated
    comparators; and the skinny NEON JSON scanner is dead on the live path — the claim currently rides
    scalar scanning + serde_json number decode. → TW0 (disclosure) + TW4 (make it true with SIMD live).
15. **Unified Value API** — skinny lazy `ValueRef`/`Cursor` exists; the unified tape, one path API,
    live direct-to-struct do not. Three substrates, four path implementations, dead `ser::Deserializer`. → TW3.
16. **dav1d-style hybrid Rust+ASM emitter** — vocabulary imported (kernels/dispatch/checkasm), the binding
    discipline absent: the emitter is not the specializer, kernels are hand-wired per grammar, checkasm
    passes green over dead kernels, the only two `asm!` blocks are dead. → TW4 (and the asm line holds:
    intrinsics by default; hand asm only for proven mis-schedules, missing intrinsics, or cross-chunk spills).

## Newly adjudicated this pass (from the user's gestalt directive)

17. **No hardcoded grammar items — everything grammar-derived** — Lock-14 is GREEN at HEAD only
    by-exclusion (gate scans 2 of the generic surfaces; core/xtask/skinny-codegen sit outside). The honest
    reading is RED in substance. The skinny firewall (5,283 lines, 47 owner-path slices, 20 subject
    matchers, append-only across 66 commits) is a contrivance ledger, not a converging gate. → TW0 gate
    re-founding; the structural-scan pattern of the live gate replaces the firewall.
18. **No overfitting JSON/CSS** — reduced, not eliminated: `has_rule("value")` JSON gate,
    b"true"/b"false"/b"null" literal tables, JSON-alphabet recognizer filters, CSS rule-name scans in
    css_scan lowering, `^`→Right associativity for Sheets. Five of nine grammars have no oracle at all —
    the fleet beyond JSON/CSS is under-verified, which is the same over-emphasis by omission. → TW2/TW6.
19. **Docs standalone, no meta-language** — largely held; LOCKS.md carries an embedded hardening-prompt
    block (process scaffolding inside a normative doc). → TW0 hygiene.
20. **Full type resolution (HM/CSP/egraph)** — the README/ARCHITECTURE claim of an existing HM core is
    false and must be struck (the honesty strike precedes the build). What exists is a monotone lattice
    fold. → TW5.

## The single most load-bearing correction

The compound ">SOTA" sentence — CSS 1.5–3.3× AND JSON 51/51 — mixes thermal planes (cold N=200 CSS vs
warm Criterion JSON), rides load-depressed absolutes, and stands on a lazy-HEAD depth asymmetry disclosed
only in a wave-level FRAMING doc. Every constituent is defensible *as qualified*; the compound sentence as
currently displayed is not. TW0 makes the qualifiers co-located and mandatory; TW3/TW4 make the
unqualified sentence true.
