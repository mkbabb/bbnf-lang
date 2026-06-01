# S-P0 audit-overfit hardening V1 — CH7 Overfit-Prune (the binding new lens)

Lens (`PASS-0-OVERFIT-AUDIT.md:62-87`): the audit-overfit synthesis itself must show that every
SK-V18 surface is grammar-derived or PRUNED; Lock-14 generic-crate compliance preserved; every
admit lands via a real parser/codegen/SIMD source change measured strict-vs-strict same-plane
with a per-iteration equality oracle; every generated output round-trips (delete + regen ⇒
byte-equivalent); no scaffold-only landing counts as an admit; and the 6 V3 addenda fire as
REJECT triggers. CH7 REJECT cannot be carried as "acknowledged but not blocking" (`:86-87`).

Subject: `a0`–`a3` + `SYNTHESIS-AUDIT-OVERFIT.md`. Live HEAD `83b66db42` == current
`git rev-parse HEAD` — every path:line below re-grepped this pass against the live tree.

## Method — independent witness verification

CH7's load-bearing question is whether the 6 addenda are EXECUTABLE and CORRECTLY CATCH the V3
failure modes on a REAL live surface (a decorative addendum that fires on nothing is itself a
paper-close). Each addendum's live witness was re-grepped:

- **L1 verbatim-blob** — `crates/codegen/src/runtime_generator.rs:701 const CSS_GENERATED_RS: &str = r#"`
  confirmed live; 8 sibling const-`&str` couriers at `:195/:550/:572/:594/:598/:612/:665/:701`
  confirmed. The `rg 'const \w*_RS\s*:\s*&str\s*=\s*r#"'` check returns all 8 — the executable
  check FIRES. ✓ (a1 §L1, SYNTHESIS R1/R2.)
- **L2 distinct-grammar-output** — 7× `css_l4_*/generated.rs` share ONE md5
  `b654562ccff46ed62dd48e9ace325830` (`md5 … | sort | uniq -c` = `7 b654562c…`). The 3-co-gate
  conjunction (md5 ∧ branch-count==0 ∧ row-collapse) is the correct hardening — md5-distinctness
  alone would false-green a relocated data-table. ✓ (a1 §L2, SYNTHESIS R4.)
- **L3 single-emitter-path** — `grammar_provider.rs:40-42 enum RuntimeEmitterKind { CompiledLowering, RequestFacts }`
  + `:33 pub emitter:` + `:110` live dispatch confirmed. The neutral variant names defeat the
  L2 arm-census grep — which is exactly why L3 is a DISTINCT lens, not a duplicate. ✓ (a1 §L3,
  SYNTHESIS R3.)
- **L4 phantom-generic** — `tape/mod.rs:175 ValueRef<…, K = AnyKind, G: EventGrammar = AnyGrammar>`
  confirmed; the test-excluded non-test instantiation census returns EMPTY (phantom confirmed).
  The lens correctly targets the `G` axis (NOT the already-real `K` axis) and test-excludes
  `_proof_compiles`. ✓ (a1 §L4, SYNTHESIS R5/R6.)
- **L5 timed-plane + corpus-in-timer** — `nonjson_css_l4.rs:3091 fn measure_mbps` warm
  (grep-count 48) confirmed live; `css_canon_bench.rs` (the honest cold harness KEPT) confirmed
  present. The triple-violation framing (warm ∧ micro-fixture ∧ more-work-competitor) is
  accurate. ✓ (a1 §L5, SYNTHESIS R13/R14.)
- **L6 acceleration-wiring** — `find_css_significant` non-test/non-generated callers = the sole
  hit at `lib.rs:574`, which I confirmed sits inside the `#[cfg(test)] mod tests` opened at
  `lib.rs:51`; the `generated.rs` hot-path grep for the kernel returns EMPTY (NEON unwired at
  admission). The retire branch is correctly gated on an S-P1 samply MEASUREMENT, not an
  assertion. ✓ (a1 §L6, SYNTHESIS R7/R10/R11.)

PRUNE-sequencing witnesses also re-verified: P1 `src/x86_64/`=24 files, `ext/x86/`=4 files,
9 active `bbnf_simd::x86_64::` call sites in `checkasm_parity.rs` (the build-soundness coupling
is real — `cargo test --no-run` blocks without decoupling), `nasm-rs = "0.3"` at
`Cargo.toml:19`. P4 `runtime_generator.rs` lives in `SK_V14_W5C_GEN_OWNER_PATHS:1144`, NOT in
the strict `GENERIC_SCAN_ROOTS:2409` — the green-by-exclusion (R9) is disk-true; the
`diagnostic-x86` exclusion at `:2463` confirmed. R16: `RuntimeTarget` is exactly 12 fields
(`regen.rs:6-19`) with `output_labels: Option<RuntimeOutputLabels>` as field #12;
`RuntimeOutputLabels` is a distinct struct (`grammar_provider.rs:92`) whose
`fact_schema`/`row_id`/`output_plane` are NESTED — confirming the shallow-`Option`-compare
false-green hazard the NEW finding pins. P5 `parse_w11_1_number` ×7 in `json/generated.rs`
confirmed.

## ACCEPT (6)

- **A1 — Every surface is grammar-derived OR pruned; nothing blessed as hand-written-under-`@generated` (ACCEPT).**
  The residual census R1–R16 (`SYNTHESIS §2`) maps every overfit surface to a PRUNE (delete) or
  GENERALIZE (grammar-derive) disposition. The CSS const courier (R1/`runtime_generator.rs:701`)
  is RETIRED (G2), the JSON fixed-literal render (R2) PROJECTED (G1), the fork (R3) un-forked
  (G3), the 7 replicas (R4) collapsed (P3). No hand-written surface is admitted under a
  provenance banner — the audit checks the BODY, not the `@generated` header.

- **A2 — The 6 addenda are EXECUTABLE and CORRECTLY catch the V3 failure modes (ACCEPT — the load-bearing output).**
  Independently verified above: every one of L1–L6 fires on a REAL live witness at
  `83b66db42`, and each catches a DISTINCT V3 failure mode that the others miss (L3's neutral
  enum survives L2's arm-census; L4 targets `G` not `K`; L5's corpus-in-timer is orthogonal to
  L6's hot-path reach). The a1 registry pins for each (1) the V3 finding by path:line, (2) the
  grep/diff/md5/samply check runnable from `skinny/crates/`, (3) distinct REJECT-vs-REVISE
  criteria. The necessary-not-sufficient hardenings (L2 3-co-gate, L4 rich-nav guard, L6
  retire-on-measurement) close the disguised-relocation seams a naive md5/grep/checkasm gate
  would false-green. These ARE the CH7 enforcement — not a rider on it.

- **A3 — Lock-14 generic-crate compliance preserved and made TRUSTWORTHY before the rebuild (ACCEPT).**
  The audit correctly diagnoses R9 as green-BY-EXCLUSION (disk-confirmed:
  `runtime_generator.rs` is in `GEN_OWNER_PATHS`, not the strict `GENERIC_SCAN_ROOTS`), and
  binds P4 to land BEFORE G2/G3 (`SYNTHESIS §5` constraint 2; a2 §1c), with the de-exclusion +
  `CSS_`/`_RS`/`EventGrammar` token extension + `diagnostic-x86` drop + a re-inject falsifier
  (`JsonSink` → gate RED). The gate becomes meaningful when the un-forked emitter is authored —
  the correct sequencing, not a green-over-leaks pass.

- **A4 — Round-trip / strict-same-plane / per-iter-oracle discipline carried (ACCEPT).**
  `regen --check` clean + the byte-for-byte `json_templates/` parity oracle is bound as the
  G1/G2 derivation proof (`SYNTHESIS §2.1.1`), with the `.bbnf`-mutation test so a const-courier
  swap cannot pass. The measurement plane is the cold `css_canon_bench` (N≥200, real corpus, no
  broadcast) + the 9-field EXACT cssparser oracle + JSON 51/51 strict cold — no gate-relabel
  admit (`SYNTHESIS §3` A1/A2). The L5 corpus-in-timer obligation is carried forward to every
  SK-V18 >SOTA re-proof.

- **A5 — No scaffold-only landing counts as an admit (ACCEPT).**
  PROVE-Sheets is bound to emit THROUGH the un-forked generator ONLY; if Sheets cannot be
  generator-emitted, generalization is NOT real and must be surfaced as a genuine §6 finding,
  never stub-proved (`SYNTHESIS §6`, §7 tee-up). The honest-finding escape is machine-gated
  (a)-(c): grammar-INVOKED by name + grammar-DERIVED data + `verbatim_blob_present == false` —
  a primitive failing these is a relabeled blob, REJECT (`SYNTHESIS:238-240` — folded in).

- **A6 — The PRUNE-sequencing is sound, anchored, and build-safe (ACCEPT).**
  PRUNE→GENERALIZE→PROVE→HONESTY with the three load-bearing edges (P3-before-G2 / P4-before-
  emitter-rebuild / exit-gate-blocks-successor) is disk-anchored and each edge carries a
  concrete re-leak or stale-state hazard. The P1↔`checkasm_parity.rs` build-soundness coupling
  is REAL (9 active call sites verified — an intermediate `rm -rf src/x86_64/` without
  same-wave decoupling is a broken-build state, correctly forbidden). PRUNE carries zero
  generalization risk (pure deletion + gate-tightening, no >SOTA-bearing code removed; net
  ≈ −10800 LOC) — the correct standing-order predecessor.

## REVISE (0)

The single REVISE the prior V1 CH7 pass raised (elevate the honest-finding escape gate (a)-(c)
into the §6 CH7 enumeration) is DISCHARGED: `SYNTHESIS-AUDIT-OVERFIT.md:238-240` now carries
"the honest-finding escape qualifies ONLY under (a) grammar-INVOKED by name + (b) grammar-DERIVED
data + (c) `verbatim_blob_present == false` — a primitive failing these is a relabeled
hand-written blob, REJECT." Verified live. The §6 enumeration is now complete; no residual REVISE
remains for CH7 at this read.

## REJECT (0)

Zero. Every addendum fires on a live, un-remediated surface at the present HEAD; none is
decorative. The audit does not soften any REJECT-class V3 finding into a REVISE (the L1–L6
REJECT-vs-REVISE criteria are kept distinct per abrogate-before-patch — a softened REJECT would
itself be a paper-close, and none is present). No CRITICAL S-P0 finding ⇒ the PASS-0 forward-halt
does not trigger; the prune list is the goalset's own already-Alpha-survived PRUNE cluster, so
S-P0 confirms cleanliness + hardens the addenda + pins R16, which is the correct posture.

## Tally
ACCEPT 6 · REVISE 0 · REJECT 0 — **100%**. The 6 addenda are executable, independently
witness-verified live, each catching a distinct V3 failure mode; the residual census is complete;
the PRUNE-sequencing is sound and build-safe; the honest-finding-escape REVISE from the prior
pass is discharged. CH7 is the spine of the audit, not a rider.

TALLY accept=6 revise=0 reject=0
