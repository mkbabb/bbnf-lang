# SK-V19 PASS-IMPL — Fold-Readiness Verdict (the overfit/inflection audit)

Date: 2026-06-07. Cycle: SK-V19 totality fold. Phase: PASS-IMPL-OVERFIT-AUDIT (the inflection
validation before the fold). Source: the 6-way fold-readiness workflow `wf_674314b7-638` (582K subagent
tokens). HEAD: `a97f5f365`.

## Verdict: READY-TO-DESIGN, but the fold is a GREENFIELD ADOPTION, not a light wiring — and the
## design must be RE-ANCHORED to the SK-V18 close.

The skinny proving tree DELIVERED the architecture-in-prototype (the un-forked dispatch, the SoA tape,
`ValueRef<K>`/`Cursor`, the 5-shape `BackendShape` selection, the value-api `ValueProject` derive, the G6
NEON classifier). But `crates/core` (the 9-grammar fleet) has NOT inherited it — it is at an EARLIER
state. The SK-V19 fold INTRODUCES/PORTS the skinny-proven prototypes into core at fleet scale, preserving
>SOTA via the decision engine. The unanimous per-dimension readiness is `needs-reconcile`.

## The greenfield reality (per receiver cluster)
- **Substrate (W0-W2).** `crates/core` runs THREE eager builder families — OpenFrame (json/css_l4/bbnf),
  Frame (sheets), SimpleStructBuilder (the 5-grammar simple cohort) — PLUS an unwired AoS
  `TapeStructBuilder` (`runtime/tape/record.rs`, 16-byte). NOT on the proven SoA tape. W1's `grep
  JsonStructBuilder|CssStructBuilder = 40` UNDER-scopes: the SimpleStructBuilder cohort + builder_template
  + arena_template + the latent AoS tape must ALSO retire for true exactly-one-substrate (Lock-1). The
  unwired AoS tape is a dual-encoding violation the moment SoA is wired unless D08/REDRESS-53 clears it.
- **Value-plane (W3-W4).** core has NO `ValueRef`, NO single-tape value substrate — it is eager
  per-grammar enums + arena ids (`JsonValue` with `JsonArrayId`/`JsonObjectId`). The value-plane is
  ENTIRELY net-new for core. The skinny value-api Phase 1 (`b7ab7c558`) proved the grammar-derived
  accessor SHAPE (`classify_projection`/`(Kind,projection)` guards, zero rule-name literals) but ONLY for
  Sheets — JSON still rides hand-authored `json_templates/{value,view,visitor}.rs` couriers (the false
  `@generated` header; Phase 3 pending even in skinny).
- **Decision-engine (W5).** core has ZERO `BackendShape` — no enum, no select/finalize, no DecisionCost
  lattice (`rg backend_shape crates/ir` = 0). The 5-shape canon is SKINNY-ONLY; W5 is a 0→N INTRODUCE, not
  a wiring of latent infra. The two structural SIMD kernels — core's `simd-scan` lo6-LUT `scan_structural`
  (→ StructuralIndex) vs skinny's `bbnf-simd` vceqq `structural_classify_64` (→ 6-mask) — are DIFFERENT
  algorithms with different output types; a kernel-SCHEME unification, not "copy one primitive." The F5
  "100-400 scope reconcile" cost is stale.
- **Leaks (W6).** `crates/ir/src/registry/strategy.rs` (the 9-ident grammar-named table; the Lock-14
  self-gate "asserts ZERO, returns 13"), `crates/core/src/css_types.rs` (CSS types in generic core), PLUS
  131 grammar-name occurrences in IR recognizer passes (`shape_dict_bbnf.rs`, `grammar_facts.rs`) — a leak
  class OUTSIDE the Pattern-H file frame. The Pattern-H census is now 71 files (not the design's 67).
  `LOCKS.md:649` SUPERSEDES `3D` for the named-leak inventory.

## The stale design (must re-anchor before S-P3)
`3D-skinny-fold.md` (generated 2026-06-01) predates the entire SK-V18 close (PROVE/H1, all 2026-06-03) +
the value-api work. Re-anchor list:
1. W3 name `ValueRef<G>` → `ValueRef<K>` / the `Cursor`/`DocumentView` seam (G4 `a9585cc65` DELETED the
   phantom `<G>`). ~6 §13.6 loci.
2. W3 "Sheets/BBNF-self by-construction" → Sheets is PROVEN via the actual generator (PROVE outcome A,
   `generator_grammar_count==3`); only BBNF-self remains a by-construction litmus.
3. "one generator" / "ONE grammar-agnostic accessor generator" → HEAD has THREE shape-specialized emit
   bodies (`json_sink_direct` + `css_scan_direct` + `general_descent`). The genuine C fold (collapse to
   one walk) is W3 WORK, not a met precondition. W3 must own the `json_sink_direct`/`css_scan_direct` →
   `general_descent` collapse-or-keep decision + name the `json_templates` couriers as the retire targets.
4. `materialization_framing == lazy-rich-vs-eager-cssom` → `lazy-rich-HEAD-vs-eager-full-CSSOM` (a97f5f365
   disclosed BOTH retention AND depth asymmetries; `track1_rich` types only the value HEAD).
5. value-api Phase 1 (`b7ab7c558`) is UNCITED — it is the most direct skinny precedent for W3; the W3 cost
   band must absorb skinny Phase 3 (un-courier JSON + retire the false `@generated` header) as the fleet
   value-plane derive.
6. Re-census the blast radii at HEAD `a97f5f365` (the 40-StructBuilder / 960-StructLayout counts are
   anchored to `2a76916ac`).

## The binding sequencing insight (D11)
>SOTA survives the un-fork (the C fold) ONLY via cost-model-gated `(a)-(d)` named primitives, NEVER a
naive general walk (a naive grammar-walk regresses to lightningcss). The C fold (W3) therefore REQUIRES
the decision engine (W5) to gate the hot-leaf primitives. So **W3 (C fold + value-plane) and W5
(decision-engine + SIMD) land TOGETHER** — doing the C fold without the cost model would be throwaway.
This is why the C fold was deferred to SK-V19 "on >SOTA-preservation burden": the burden IS the cost
model, which lives in the fold.

## Disposition → next phase
The fold is READY TO DESIGN. Proceed to Pass Alpha (open SK-V19) → S-P0/1/2/3 (the re-anchored,
greenfield-aware wave SPEC for W0-W6) → reconcile the existing T-P1/2/3 audit (re-census core) → Pass
Omega / G-Omega → implement W0-W6. The design must own: (1) the greenfield introduce-not-wire reality,
(2) the C fold + value-plane + decision-engine joint landing, (3) the re-anchor list above, (4) the
two-kernel SIMD scheme unification, (5) the leak collapses (strategy.rs R16 + css_types.rs + the 131 IR
recognizer occurrences). >SOTA preservation is the gate throughout.
