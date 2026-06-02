# STAGED MIGRATION.md Delta — Pass Omega V10 (SK-V18 Generalization)

STAGED ONLY. CRUD-4a applies this to `restart/MIGRATION.md` POST-G-Omega V10.
Anchors are against the live `restart/MIGRATION.md` at this snapshot
(1061-line surface; §0.0 currently = SK-V17 receiver at `:30`).

Sources: T-P3 `3F-migration-handoff.md` deltas 3F-MH-001/003/004/008..013;
`restart/skinny/tranches/sk-v18/SPEC.md:19-21`,`:46-49`,`:58-69`,`:99-102`,
`:130-134`,`:429-449`,`:435`,`:573-600`,`:635-663`; `restart/locks/LOCKS.md:349`,
`:620`; 1F coherence-scan `COH18-001/003/006/008/009`; 1D `D-2`/`D-4`/`D-5`/
`G-6`/`G-8`/`G-13`.

---

## OP-1 — INSERT new §0.0 receiver ABOVE the live §0.0 (renumber-down)

INSERT the following section immediately ABOVE the live
`## 0.0 Current SK-V17 Tape-Fold Migration Receiver` (`restart/MIGRATION.md:30`).
RENUMBER the live §0.0 SK-V17 receiver to §0.1, the live §0.1 (SK-V15 V9) to
§0.2, and so on through the historical Pass Omega V2..V8 receivers — header
renumber only, body unchanged, provenance preserved. Historical sections are NOT
destructively edited; they become deeper provenance, not current authority.

```md
## 0.0 Current SK-V18 Pass Omega V10 Generalization Migration Receiver

Status: proposal-applied at Pass Omega V10 G-Omega (mandatory user gate). This is
the current implementation migration authority; the SK-V17 tape-fold receiver
(§0.1) and all historical Pass Omega V2..V8 receivers below are HISTORICAL
lineage, NOT current dispatch authority.

SK-V18 is the GENERALIZATION cycle on the SKINNY tree (`skinny/crates/`): un-fork
the two hand-written/forked parsers (JSON + CSS) into ONE grammar-driven
generator emitting JSON + CSS + Sheets from `.bbnf`, aarch64-only, preserving
>SOTA honestly (CSS beats lightningcss 1.66–3.38×; JSON beats sonic-rs strict),
≈ −10800 campaign LOC (per-wave SPEC sum ≈ −10685; `sk-v18/SPEC.md:571`).
The totality `crates/core/` adoption is **SK-V19**, NOT SK-V18
(`restart/skinny/tranches/sk-v18/SPEC.md:19-21`,`:58-61`).

Governance (per the 3F-MH-004 record): totality T-P1 SK-V18 near-converged
NON-normal-§3Z (NOT a normal two-clean §3Z lock); T-P2 SK-V18 near-converged
NON-normal-§3Z (converged=false, consec=0); T-P3 SK-V18 CONVERGED into the
3A..3F synthesis + the 3C-locks-v+1-diff (21 candidates: 9 ACCEPT, 11 MODIFY,
0 REJECT, 1 DEFER; git apply --check exit 0 against live LOCKS.md). Do NOT cite
the SK-V15 `HARDENING-T-P3-V5-CONSOLIDATED.md` for the SK-V18 state. Order:
PRUNE-before-GENERALIZE-before-PROVE.

The 12-wave migration receiver (the REDUCTION ledger; ≈ −10800 campaign LOC,
per-wave SPEC sum ≈ −10685, `restart/skinny/tranches/sk-v18/SPEC.md:429-449`,
`:571`):

| Receiver wave | Migration consequence | Net LOC | Exit gate |
|---|---|---|---|
| P1 x86 DELETE | x86 crate-wide gone: `bbnf-simd/src/x86_64/` (24 src/x86_64 + 4 ext/x86 = 28 files, the verify grep's full reach) + `bbnf-simd/build.rs` nasm driver + `nasm-rs` build-dep + `lib.rs` dispatch arms + the 11 `checkasm_parity.rs` x86_64 call sites DECOUPLED in the SAME commit (re-grep before merge as counts may drift). aarch64 is the SOLE admission platform. | ≈ −4500 | `find …/x86_64 …/ext/x86 -type f == 0`; crate-wide aarch64-neutral grep; `cargo build`/`cargo test --no-run` clean; `x86_tree_deleted == true` |
| P2 warm CSS bench DELETE | warm micro-fixture machinery + SHA256 scaffold gone; 9-field cold oracle retained. | ≈ −700 | `grep -c 'measure_mbps\|lightningcss_facts' bbnf-bench/src/nonjson_css_l4.rs == 0` (today 48; the certified SPEC `:627` exit-gate falsifier + owner-path `:614` bind the P2 gate to `bbnf-bench/src/nonjson_css_l4.rs` ALONE — the `src/`-qualified path disambiguates from the 7-hit `bbnf-bench/benches/nonjson_css_l4.rs`, and the 16 crate-wide hits in `bin/gate.rs` are NOT a P2 gate target, no SPEC/1D/3B wave owns their retirement; SPEC `:633` is the R14/H1 INDEPENDENT disclosure note, it binds NOTHING about the gate); css_canon_bench green |
| P3 replica COLLAPSE | 7 byte-identical `css_l4_*/generated.rs` (md5 `b654562c`) → ONE config; 7 `RuntimeTarget` rows → ONE via `PartialEq` full-row over BOTH nested structs (`frontend_requirements` #11 ∧ `output_labels` #12). | ≈ −5500 (`SPEC.md:435`: 6×910 = −5460 replica bodies + ~−40 collapsed rows + 1 `PartialEq`; 6 of 7 deleted) | `md5 …/{json,css_l4}/generated.rs` no-identical-pair ∧ `runtime_target_rows_collapsed == true` (matches master-plan-diff `:171` + handoff-delta `:125` + `SPEC.md:435`; the md5-distinctness half is NECESSARY-not-sufficient, the structural row-collapse co-gate completes it). NOTE: `generator_grammar_count == 3` is a **PROVE-EXIT** gate (`SPEC.md:254`: "MUST be 3 at PROVE (json+css+sheets); 7-css inflation = the P3 overfit, REJECT"), NOT a P3 close-gate — at a PRUNE wave the count is at most 2, and forcing it to 3 at P3 IS the inflation SPEC `:254` rejects. |
| P4 Lock-14 gate FIX | `runtime_generator.rs` into strict `GENERIC_SCAN_ROOTS`; the `diagnostic-x86` exclusion dropped; FORBIDDEN ⊇ {`GENERATED_RS`,`CSS_GENERATED_RS`,`EventGrammar`,`*EventGrammar`}. **MUST LAND BEFORE G2/G3.** | ≈ +15 | re-inject a forbidden token → RED/revert; `lock14_gate_scans_codegen == true` |
| P5 metalang PURGE | `parse_w11_1_number` ×7 → `parse_number_*` at template source `json_sink_direct.rs`; the `lib.rs:565` test-assert updates SAME pass (rename-only; 1:1 regen). | ≈ 0 | `grep -c parse_w11_1_number json/generated.rs == 0` (today 7; the certified SPEC `:755`/`:570` binds the P5 gate to `json/generated.rs` ALONE — the unscoped crate-wide count is 15: + 7 template-source `json_sink_direct.rs` + 1 `lib.rs:565` test-assert, both driven to 0 by the same rename+regen but NOT the SPEC gate target — matching master-plan-diff `:173`, handoff-delta `:181`, ΩF `:230`); `regen --check` clean |
| G1 JSON projection | `json_sink_direct` / `json_templates/` retired; `SinkOnlyExpr` AST-walk emitter; byte-equivalent vs the oracle BEFORE oracle delete. | ≈ 0 generated | JSON byte-equivalence; `verbatim_blob_present == false` (JSON) |
| G2 CSS lowering | `CSS_GENERATED_RS` (`runtime_generator.rs:701`) DELETED; `css_balanced_component_scan` primitive + fact-keyed projection. | ≈ −910 net | `CSS_GENERATED_RS` deleted; `verbatim_blob_present == false`; `emit_shape_source == lowered_program` |
| G3 un-fork emitter | `RuntimeEmitterKind` DELETED; dispatch on `BackendShape`, not grammar tag. | ≤450 hand | `emit_shape_source == lowered_program`; byte-equivalent output |
| G4 value-API + phantom | `Cursor` micro-trait (`tape/cursor.rs`) over the EXISTING tape; `<G>` DELETED, K-axis preserved; JSON rich-nav byte-equal. | ≤450 hand | `phantom_generic_resolved == deleted`; shared trait ≥2 impls |
| G5/G6 NEON retarget | NEON onto the CSS scan shell; `json/scan.rs` neutralized; checkasm-gated scalar twin. | ≤450 hand | named SIMD call-site; checkasm parity green |
| PROVE Sheets | Sheets via the un-forked generator ONLY; precedence-tower core. | ≈ +200 | `sheets_grammar_shape == pratt-operator`; md5-distinct from JSON ∧ CSS |
| H1 honesty close | CSS framing honesty + corpus-in-timer + `regen --check` clean; CSS ratio re-locked. | ≈ 0 | `css_canon_bench` re-locked (≥1 regular corpus crossing >1.0× same-run); `regen --check` clean |

Current SK-V18 wave authority routes through
`restart/skinny/tranches/sk-v18/SPEC.md` (the 12-wave manifest). W-PRUNE (P1-P5)
is the ONLY dispatch-eligible cluster on close (`sk-v18/SPEC.md:46-49`).
```

---

## OP-2 — INSERT the five rename/abrogate/refactor disposition rows

INSERT a new sub-table inside the §0.0 receiver (immediately after the 12-wave
table above), recording the five concrete migration decisions
(3F-MH-008..013):

```md
### §0.0 Migration Decisions (the five rename/abrogate/refactor surfaces)

| Decision | Kind | Receiver | Net LOC | Grounding |
|---|---|---|---|---|
| x86 surface (crate-wide, NOT just `src/x86_64/`) | DELETE | P1 | ≈ −4500 | `sk-v18/SPEC.md:130-134`,`:435`,`:573-600`; 1F `COH18-009`. Deletion list REACH-MATCHED to the verify grep (24 src/x86_64 + 4 ext/x86 = 28 files); 11 `checkasm_parity.rs` x86_64 call sites DECOUPLE in the SAME commit or the build breaks (re-grep before merge as counts may drift). aarch64 is the SOLE admission platform; x86/AVX/AVX-512 are diagnostic-only (2A REFUTATION: x86/AVX-512 closing an M5 Max row is REFUTED). |
| `CSS_GENERATED_RS` const courier (`runtime_generator.rs:701`) + JSON `_RS` fixed-literals (`json_sink_direct.rs`) + `json_templates/` | RETIRE | G2 (CSS) / G1 (JSON) | ≈ −910 CSS + JSON | `sk-v18/SPEC.md:61-69`. The hand-written content becomes byte-for-byte parity ORACLES, deleted POST-equivalence, NOT the product. `verbatim_blob_present == false` campaign-wide; `emit_shape_source == lowered_program` (NOT `runtime_target`) — the relocated-seam falsifier. 2C REFUTATION: `find_css_significant` wire-as-is is REFUTED. |
| 7 byte-identical `css_l4_*/generated.rs` (md5 `b654562c`) + 7 `RuntimeTarget` rows | COLLAPSE | P3 | ≈ −5500 (`SPEC.md:435`: 6×910 = −5460 replica bodies + ~−40 rows + 1 `PartialEq`) | `sk-v18/SPEC.md:80-85`,`:435`,`:635-663`. `xtask/regen.rs` derives `PartialEq` for the R16 full-row collapse over BOTH nested structs. P3 exit gate: `md5 …/{json,css_l4}/generated.rs` no-identical-pair ∧ `runtime_target_rows_collapsed == true` (siblings master-plan-diff `:171` + handoff-delta `:125`). NOTE: `generator_grammar_count == 3` binds the **PROVE-EXIT** wave, NOT P3 (`SPEC.md:254`: "MUST be 3 at PROVE; 7-css inflation = the P3 overfit, REJECT") — it is NOT a P3 close-gate. 2C REFUTATION: md5-distinctness ALONE does NOT prove the un-fork; the structural row-collapse co-gate is required. The totality-tree analog (`ir/registry/strategy.rs` 9-grammar table, COH18-005) the SK-V19 fold inherits. |
| phantom `<G: EventGrammar>` axis (`tape/mod.rs:175`,`:179`,`:197`) | DELETE | G4 | — (decoration removal) | `sk-v18/SPEC.md:99-102`; 1A `1A-SUB-023` (census EMPTY of non-test instantiation); 1F `COH18-008`. The REAL `K=Kind` axis (`JsonNodeKind`/`RootKind`/`ObjectKind`) is PRESERVED untouched. `phantom_generic_resolved == deleted`. **Companion Lock-14 reconcile** (1A-LOCK1-AMEND-001): strike "The `G:EventGrammar` type parameter is the generality vehicle" (`restart/locks/LOCKS.md:620`) and re-anchor the generality claim on (a) the shared `Cursor` micro-trait (G4b, ≥2 non-collapsible impls) + (b) the config-breadth classifier — a 1-line LOCKS reconcile is **Pass Omega CRUD-3 / SK-V19**, NOT an Ω-F edit. No lock-count change; no shape/directive/substrate change. |
| totality `crates/core/src/css_types.rs` (66 LOC) | RELOCATE-or-DELETE | **SK-V19** (NOT SK-V18) | 66 LOC | `restart/locks/LOCKS.md:349` (Lock 14) names it under the heading "Full grammar generalisation; zero overfitting", in the enumeration the line VERBATIM labels "The current overfitting mess — … `shape_dict_bbnf.rs`; `crates/core/src/css_types.rs`; per-grammar runtime/<g>/ hand-written modules"; 1F `COH18-006`/`U-COH18-002`. Lock 14 (c) admits ONLY a separate `crates/<grammar>/` declaration crate; admissible ONLY if relocated to a `crates/css/` declaration crate, else DELETE. The SK-V18 benched tree is skinny; this is a totality-tree carrier — an EXPLICIT SK-V19 migration decision, not a silent drop. |
```

---

## OP-3 — ADD the PRUNE-before-GENERALIZE gate clause (attach to §17 + §19)

ADD the following clause to the live §17 Tranche-Level Migration Sequence
(`restart/MIGRATION.md:886`) AND cross-reference it from §19 Migration Gates
(`restart/MIGRATION.md:925`):

```md
### §17.SK-V18 Deletion/Retirement Order Gate

Under SK-V18 the order is PRUNE-before-GENERALIZE-before-PROVE. No
GENERALIZE/PROVE wave deletes a hand-written ORACLE (JSON `json_templates/`, the
7 css_l4 replica bodies, the `CSS_GENERATED_RS` courier) before its
grammar-DERIVED replacement lands BYTE-EQUIVALENT and the round-trip
diff-control gate is GREEN (G1 JSON byte-equivalence vs oracle BEFORE oracle
delete; G2 `CSS_GENERATED_RS`-deleted; P3 md5-distinct + `runtime_target_rows_collapsed`
post-collapse witness). This prevents the delete-before-replacement failure
pattern (the lightningcss tree-walk regression, 1D `C-3`) re-entering under the
un-fork.

Additionally: **G2/G4/G6 entry is BLOCKED** until the SK-V16/V17 REDRESS
reconcile (the four-item pre-block, complete only for the SK-V15-W11 ledger;
1D U-5) is on the committed ledger as a Pass-Omega-V10 / pre-W-PRUNE blocker —
these waves abut REDRESS items 51/53/246/247 (1D:168-171; item 246 = the W11T
parse-only structural-STREAM driver reject that bounds G4) and run DURING SK-V18,
so the reconcile is NOT deferrable to SK-V19 entry. Absent that committed fence, an
SK-V16/V17-rejected shape (second scanner / structural-stream driver /
parser-local cursor) must not re-enter G2/G4/G6.
```

---

## OP-4 — ADD the governance-honesty paragraph (§0.0 receiver tail)

ADD to the §0.0 receiver:

```md
### §0.0 Totality-Pass Provenance (governance honesty)

The three SK-V18 totality passes carry distinct provenance, stated honestly so
no V1 surface over-claims a normal two-clean §3Z lock:

- **T-P1 SK-V18**: near-converged NON-normal-§3Z. NOT a normal two-clean §3Z
  lock; do NOT cite the SK-V15 `HARDENING-T-P3-V5-CONSOLIDATED.md` (a 42-candidate
  / 23-ACCEPT-19-MODIFY SK-V15 matrix) for the SK-V18 state.
- **T-P2 SK-V18**: near-converged NON-normal-§3Z (converged=false, consec=0;
  single-cell citation-precision qualifiers from V4, no surviving REJECT).
- **T-P3 SK-V18**: CONVERGED into the 3A..3F synthesis + the 3C-locks-v+1-diff
  (21 candidates: 9 ACCEPT, 11 MODIFY, 0 REJECT, 1 DEFER; git apply --check
  exit 0 against live LOCKS.md). The 16-lock count + 5 `BackendShape` variants
  are PRESERVED (amendment by addition; no renumber).
```

---

## Verification (post-CRUD)

- `grep -n "0.0 Current SK-V18 Pass Omega V10" restart/MIGRATION.md` returns 1
  hit ABOVE the (renumbered) §0.1 SK-V17 receiver.
- No occurrence of "Pass Omega V6" labels the CURRENT SK-V18 pass (the historical
  §0.6 W5BR receiver header may keep its V6 token — that is provenance, not
  current).
- The five disposition rows each cite a `sk-v18/SPEC.md` or `LOCKS.md` anchor.
- `css_types.rs` row routes to SK-V19, not SK-V18.
- Lock-count text unchanged at 16; no 6th `BackendShape`.
