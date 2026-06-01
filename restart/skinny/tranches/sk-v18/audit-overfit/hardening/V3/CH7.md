# S-P0 audit-overfit hardening V3 — CH7 Overfit-Prune (2nd-consecutive confirm, full independent re-grep)

Lens binding (`restart/prompts/skinny/PASS-0-OVERFIT-AUDIT.md:62-87`): the audit-overfit synthesis
itself must show every SK-V18 surface is grammar-derived OR PRUNED; Lock-14 generic-crate compliance
preserved; every admit lands via a real parser/codegen/SIMD source change measured strict-vs-strict
same-plane with a per-iteration equality oracle; every generated output round-trips (delete + regen
⇒ byte-equivalent); no scaffold-only landing counts as an admit; and the 6 V3 addenda fire as REJECT
triggers. CH7 REJECT cannot be carried as "acknowledged but not blocking" (`:86-87`). ORCHESTRATOR
§3W/§3Z bar: ≥95% across CH1–CH7 for TWO consecutive cycles (CH7 the new lens), zero orphan REVISE,
V≤5.

Subject: `a0`–`a3` + `SYNTHESIS-AUDIT-OVERFIT.md`. Live HEAD `83b66db4232374db6a5f9fa009882f41acc04342`
== `git rev-parse HEAD` (confirmed this pass). V3 is the 2nd-consecutive confirm: V1 CH7 raised ZERO
REVISE; V2 CH7 100%/0R/0R; V3 re-verifies that (a) the V1 fold edits to a0–a3/SYNTHESIS introduced no
new CH7 defect, (b) the 6 addenda STILL fire on live un-remediated surfaces, and (c) no REJECT-class
V3 finding (D1–D4, C1–C3) was softened into a REVISE.

CH7's load-bearing question is whether the 6 addenda are EXECUTABLE and CORRECTLY catch the V3 failure
modes on REAL live surfaces — a decorative addendum that fires on nothing is itself a paper-close.
Every witness below was INDEPENDENTLY re-grepped this pass at `83b66db42` (not inherited from a0–a3).

## Independent witness re-grep (every dispositive claim re-grepped at HEAD `83b66db42`)

- **L1 verbatim-blob** — `runtime_generator.rs:701 const CSS_GENERATED_RS: &str = r#"` confirmed;
  `rg 'const \w*_RS\s*:\s*&str\s*=\s*r#"'` returns ALL 8 couriers at `:195/:550/:572/:594/:598/:612/
  :665/:701`; CSS body closes `:1611` (`"#;`) → span 701→1611 = 910 LOC, corroborating the R1-CH1
  fold (descriptive, no gate keys on LOC); spliced `:91` (`normalize(CSS_GENERATED_RS)`); the
  provenance-honest header sits at `:685`. FIRES on the BODY, not the banner. ✓
- **L2 distinct-grammar-output** — `md5 css_l4_*/generated.rs | sort | uniq -c` = `7
  b654562ccff46ed62dd48e9ace325830` (all 7 byte-identical, ONE parser ×7); 7 css_l4 dirs confirmed.
  The 3-co-gate conjunction (md5 ∧ branch-count==0 ∧ type-count==0 ∧ row-collapse) is the correct
  necessary-not-sufficient hardening. FIRES. ✓
- **L3 single-emitter-path** — `grammar_provider.rs:40-42 enum RuntimeEmitterKind { CompiledLowering,
  RequestFacts }` + `:110` live dispatch (`!= RuntimeEmitterKind::RequestFacts`) confirmed. Neutral
  variant names defeat L2's arm-census — exactly why L3 is a DISTINCT lens. FIRES. ✓
- **L4 phantom-generic** — `tape/mod.rs:175 ValueRef<'doc,'input, K = AnyKind, G: EventGrammar =
  AnyGrammar>` confirmed; the test-excluded non-test `G`-instantiation census returns EMPTY (phantom
  confirmed on the `G` axis; `K = AnyKind` is the SEPARATE already-real axis, NOT touched). FIRES. ✓
- **L5 timed-plane + corpus-in-timer** — `nonjson_css_l4.rs:3091 fn measure_mbps` (warm) confirmed;
  `bin/css_canon_bench.rs` (the honest cold harness KEPT) PRESENT. FIRES. ✓
- **L6 acceleration-wiring** — `find_css_significant` has ZERO callers in `grammars/*/generated.rs`;
  the sole non-`runtime_simd` caller is `lib.rs:574`, inside the `#[cfg(test)]` block that opens at
  `lib.rs:51` (everything from `:51` is test-only). NEON unwired at admission. FIRES. ✓

**PRUNE-sequencing witnesses independently re-verified:** P1 `src/x86_64/`=24 files, `ext/x86/`=4
files, `nasm-rs = "0.3"` `Cargo.toml:19` — and the build-soundness coupling is EXACTLY 9 ACTIVE
`bbnf_simd::x86_64::` call sites in `checkasm_parity.rs` at `:458,464,467,477,478,484,493,497,502`
(`rg -c` = 9, matching a2 §3 verbatim — an `rm -rf src/x86_64/` without same-wave decoupling IS a
broken-build state). P4 `GENERIC_SCAN_ROOTS:2409` + `FORBIDDEN_GENERIC_TOKENS:2420` +
`SKV15_W2_EXTRA_COVERAGE_ROOTS:2442` + `diagnostic-x86:2463` + `:4956` assertion all disk-exact in
`bbnf-bench/src/lock14_baseline.rs` (the artefacts cite the filename `lock14_baseline.rs:NNNN`, which
resolves correctly — no full-path defect). P5 `parse_w11_1_number` ×7 in `json/generated.rs`
confirmed. **R16 independently confirmed:** `RuntimeTarget` (`regen.rs:5-18`) derives ONLY
`#[derive(Clone, Copy, Debug)]` — NOT `PartialEq` (the one-line derive cost is real); it carries TWO
nested-struct fields, `frontend_requirements` (`regen.rs:17`) AND `output_labels` (`regen.rs:18`);
both nested structs derive `PartialEq, Eq` (`RuntimeFrontendRequirements` `grammar_provider.rs:45-46`,
`RuntimeOutputLabels` `grammar_provider.rs:91-92`). The R16 pin (inline BOTH nested structs; full-row
`RuntimeTarget: PartialEq` preferable, +1 derive line) is disk-grounded.

**SYNTHESIS CH6-fold rows re-confirmed:** R-A0-1 (`SYNTHESIS:101`) carries the explicit "beats
CSSOM"/"equal-work close-report claim behind a re-label is a REJECT" clause (R1-CH6 fold); R-A0-2
(`SYNTHESIS:102`) carries `generator_grammar_count == 3` = json+css+sheets collapse-to-one (R2-CH6
fold). Both folds are on disk, not narrated.

## Section dispositions (ACCEPT/REVISE/REJECT, path:line)

### (1) Residual-overfit audit COMPLETE — ACCEPT

The residual census R1–R16 (`SYNTHESIS-AUDIT-OVERFIT.md:83-103`) plus the goalset framing residuals
R-A0-1/2/3 (`a0-goalset-residual-overfit.md:64-81`) maps EVERY overfit surface to a PRUNE or
GENERALIZE disposition: CSS const courier (R1/`runtime_generator.rs:701`) → G2; JSON fixed-literal
render (R2/`json_sink_direct.rs`) → G1; emitter fork (R3/`grammar_provider.rs:40-42`) → G3; 7 replicas
(R4/md5 `b654562c…`) → P3; phantom `<G>` (R5/`tape/mod.rs:175`) → G4; CSS NEON dead (R7/`lib.rs:574`
cfg(test)) → G6; x86 two surfaces (R8) → P1; Lock-14 green-by-exclusion (R9/`lock14_baseline.rs:2463`)
→ P4; metalang leak (R15/`json/generated.rs` ×7) → P5; nested-struct gate-recipe (R16/`regen.rs:17-18`)
→ S-P3. The audit checks the BODY, not the `@generated` banner (the L1 lens IS the body check). No
hand-written surface is admitted under a provenance header. The CLEAN/KEEP inventory
(`SYNTHESIS:109-113`) — the unified `Tape`/`ValueRef`/`PayloadArena` substrate (Lock 1), the neutral
NEON kernel, the cold `css_canon_bench`, the 14-file checkasm discipline — is correctly preserved, so
PRUNE throws no aarch64 hardening out with the x86 bathwater (`a0:512-515`). **ACCEPT.**

### (2) The 6 addenda EXECUTABLE + correctly catch the V3 failure modes — ACCEPT (the load-bearing output)

Independently re-grepped above: every L1–L6 fires on a REAL live witness at `83b66db42`, and each
catches a DISTINCT V3 mode the others miss — L3's neutral enum (`grammar_provider.rs:40-42`) survives
L2's arm-census (`a1:234-238`); L4 targets the `G` axis not the real `K` (`a1:307-309`); L5's
corpus-in-timer is orthogonal to L6's hot-path reach. The a1 registry pins for each (1) the V3 finding
by path:line, (2) the grep/diff/md5/samply check runnable from `skinny/crates/`, (3) DISTINCT
REJECT-vs-REVISE criteria (`a1:64-65` severity convention; 29-distinct-trigger discipline). The
necessary-not-sufficient hardenings — L2 3-co-gate (`a1:179-200`), L4 rich-nav guard
`json_rich_navigation_preserved` (`a1:348-354`), L6 retire-on-samply-measurement (`a1:506-515`) —
close the disguised-relocation seams a naive md5/grep/checkasm gate would false-green. These ARE the
CH7 enforcement, not a rider. **ACCEPT.**

### (3) Lock-14 trustworthy BEFORE rebuild — ACCEPT

R9 green-by-exclusion is disk-confirmed (`runtime_generator.rs` is NOT in strict `GENERIC_SCAN_ROOTS`
at `lock14_baseline.rs:2409`; the `diagnostic-x86` exclusion is live at `:2463`). P4
(`SYNTHESIS:170-175`; `a2:173-194`) binds BEFORE G2/G3 (`SYNTHESIS:202-203` fact 2) with the
de-exclusion + `CSS_`/`_RS`/`EventGrammar` token extension + `diagnostic-x86` drop + the
`JsonSink`→RED re-inject falsifier (`a0:466`). The gate becomes meaningful when the un-forked emitter
is authored — correct sequencing, not a green-over-leaks pass. The §2.1 obligation that an emitted
`EventGrammar` literal be caught at its emit site (`SYNTHESIS:125-129`) closes the
witness-emission scan-root coupling. **ACCEPT.**

### (4) Round-trip / strict-same-plane / per-iter-oracle — ACCEPT

`regen --check` clean + the byte-for-byte `json_templates/` parity oracle bound as the G1/G2
derivation proof, with the `.bbnf`-mutation test so a const-courier swap cannot pass (`a0:116`;
`a1:139-148`). The R1-CH4 fold correctly states the ±5% line-count as a SOFT tripwire and the oracle
diff-match as the BINDING cost-control (`SYNTHESIS:122-124`; `a0:484-490`) — a faithful projection may
legitimately reorder/dedupe past ±5%. Measurement plane = cold `css_canon_bench` (real corpus
71KB–495KB, N≥200, no broadcast) + the 9-field EXACT cssparser oracle + JSON 51/51 strict cold
(`SYNTHESIS:138-139`). No gate-relabel admit (the inverse of SK-V13). **ACCEPT.**

### (5) No scaffold-only landing — ACCEPT

PROVE-Sheets emits THROUGH the un-forked generator ONLY; if Sheets cannot be generator-emitted,
generalization is NOT real and must surface as a genuine §6 finding, never stub-proved
(`SYNTHESIS:276-282`). The honest-finding escape — the single largest paper-close surface, per the
contract itself (`a0:76-81`, R-A0-3) — is machine-gated (a)-(c) (`SYNTHESIS:246-249`): grammar-INVOKED
by name + grammar-DERIVED data + `verbatim_blob_present == false`, with the R1-CH2 fold making (b) a
machine mutate-falsifier (the primitive's emitted output must VARY under a `.bbnf` mutation), not the
prose-reviewable "accepts a grammar-derived argument" (`a0:122-129`; `a1:138-148`). This closes the
one-level-down prose-review seam — the exact SK-V13 failure mode re-applied at the escape hatch
(`a0:426-427`). A primitive failing (a)-(c) is a relabeled blob → REJECT. **ACCEPT.**

### (6) PRUNE-sequencing SOUND — ACCEPT

PRUNE→GENERALIZE→PROVE→HONESTY with the load-bearing edges is disk-anchored (`SYNTHESIS:186-224`;
`a2:215-408`): (1a) P3-before-G2 / P1-before-G5/G6 / P5-before-G1; (1c) P4-before-emitter-rebuild;
(1d) exit-gate-blocks-successor. The R1-CH3 directional fold makes "G1/G3 co-derive; G3-failure blocks
PROVE" the explicit FORWARD revert arrow (never a backward "G3 gates G1/G2") (`a2:298-307`), and the
dual-entry-gate (G2 entry-gates on BOTH G1 AND P3, so a P3 failure ALSO blocks G2) is annotated
(`SYNTHESIS:206-207`; `a0:476-482`). The P1↔`checkasm_parity.rs` build-soundness coupling is REAL — 9
ACTIVE compile-coupled sites verified at exact lines `:458…:502` (`a2:259-271`), so P1's exit gate is
`cargo test --no-run` clean and the decoupling must land in the SAME wave (no intermediate
broken-build commit, `a2:286-294`). PRUNE carries ZERO generalization risk (pure deletion +
gate-tightening, net ≈ −10800 LOC, no >SOTA-bearing code removed, `a2:208-211`). **ACCEPT.**

### (7) Fold integrity — the V1 fold edits introduced no new CH7 defect — ACCEPT

Each V1 fold (R1-CH1 910-LOC pin · R1-CH2 machine (b)-predicate · R1-CH3 directional arrow + dual
entry-gate · R1-CH4 oracle-diff cost-control · R1-CH5 full-row R16 both nested structs · R1-CH6 "beats
CSSOM" REJECT clause · R2-CH6 collapse-to-one) is prose-precision / machine-grounding over an
already-disk-true surface (`HARDENING-S-P0-CONSOLIDATED.md:20-30`). None admits new hardcoding; none
softens a REJECT-class V3 finding into a REVISE (the L1–L6 REJECT-vs-REVISE criteria stay distinct per
abrogate-before-patch); none blesses an unproven surface. The R1-CH6 fold HARDENS L5 (an unqualified
"beats" behind a re-label is now a REJECT, not a soft preference) — a tightening, not a softening,
re-confirmed live at `SYNTHESIS:101`. The R1-CH5 fold deepened R16 to the full-expanded-row altitude
(BOTH nested structs, disk-confirmed `regen.rs:17-18` + `grammar_provider.rs:45/91`). **ACCEPT.**

## One accuracy nit (NON-BLOCKING, sub-REVISE — carried, NOT a CH7 REVISE)

The V2 CH7 carried one sub-REVISE cosmetic: a3 §3's narrative header paraphrase numbers
`RuntimeTarget` fields in a line-vs-ordinal scheme. It is now RESOLVED on disk — `a3:117-135`
explicitly labels the `NN:` prefixes as `regen.rs` SOURCE-LINE numbers and pins `frontend_requirements`
as field #11 / `output_labels` as field #12, foreclosing the "field #17/#18 vs 12 fields" misread; a1
§"Cycle V3 fold posture" (`a1:8-23`) records the absorption. No gate keys on the field-number labels
(the gate is field-NAMED and mechanism-agnostic — full-expanded-row collapse), so this never moved a
disposition. It is fully discharged at V3; nothing carried open.

## REVISE (0) / REJECT (0)

V1 CH7 carried zero REVISE; the V1 fold edits to a0–a3/SYNTHESIS introduced none; the lone V2 sub-nit
is discharged at V3. Every addendum FIRES on a live un-remediated surface at HEAD `83b66db42`; none is
decorative (a decorative addendum firing on nothing would itself be the paper-close CH7 exists to
forbid — none qualifies). No REJECT-class V3 finding (D1–D4 hand-written/forked/replicated/phantom;
C1–C3 NEON-unwired/lazy-framing/warm-bench) is softened into a REVISE — each maps to a named SK-V18
wave with a machine-checkable gate. The honest-finding-escape (a)-(c) gate is machine-checked, not
prose-reviewed (the SK-V13 failure mode is closed at the escape hatch). No CRITICAL S-P0 finding ⇒ the
PASS-0 forward-halt does not trigger; the prune list is the goalset's own already-Alpha-survived PRUNE
cluster, so S-P0's posture (confirm cleanliness + harden the addenda + pin R16) is correct.

## Tally

ACCEPT 7 (1 audit-complete · 2 addenda-executable-and-V3-catching · 3 Lock-14-trustworthy · 4
round-trip/strict-plane/oracle · 5 no-scaffold-only · 6 PRUNE-sequencing-sound · 7 fold-integrity) ·
REVISE 0 · REJECT 0 — **100%**. The 6 addenda are executable, independently witness-re-verified live
(L1 `CSS_GENERATED_RS:701`+8 couriers; L2 7×`b654562c…`; L3 `RuntimeEmitterKind:40-42`; L4
`G: EventGrammar=AnyGrammar:175`, zero non-test instantiations; L5 `measure_mbps:3091`+`css_canon_bench`
kept; L6 `find_css_significant` test-only `lib.rs:574`), each catching a DISTINCT V3 failure mode; the
residual census R1–R16 + R-A0-1/2/3 is complete; the PRUNE-sequencing is sound and build-safe (9-site
checkasm coupling verified at `:458…:502`); the R16 nested-struct precision pin is disk-grounded
(`RuntimeTarget` derives only `Clone,Copy,Debug`; both nested structs `PartialEq,Eq`). CH7 is the spine
of the audit, not a rider. With V2 CH7 100% + V3 CH7 100%, the §3Z two-consecutive-≥95% bar is met for
the CH7 lens.

TALLY accept=7 revise=0 reject=0
