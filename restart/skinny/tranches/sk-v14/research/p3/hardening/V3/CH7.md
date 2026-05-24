# S-P3 CHALLENGE V3 — CH7 OVERFIT-PRUNE Lens (SK-V14) — LOCK-TRIGGER cycle

Cycle: V3 (LOCK-trigger cycle per CHALLENGE-CONTEXT.md §0:5; V2 closed second consecutive ≥95% cycle for cohort, V3 confirms 3-cycle LOCK extension on CH7 per V2 §5 carry-forward).
Authority HEAD: `867b0cd0b` (V3 atomic cosmetic-fold commit; CHALLENGE-CONTEXT.md §0:11 pin; `git rev-parse HEAD` verified).
Lens: CH7 Overfit-Prune — verify (a) NO new fabrication introduced by V3 cosmetic fold; (b) Stage-A target naming overfit-asymmetry preserved at V3 (C3 to-be-authored / C4 existing rule, both same-wave non-JSON consumers); (c) LAC-1E-12 executable verification mandate + NEW-CH2-V3-02 orphan-cell propagation guards held at V3 fold site (P3-C :36 + :423 unconditional Stage-0 mirror to SPEC §13:982).
Scope: 8 artefacts at V3 — `p3a-candidate-shortlist.md` (V2-LOCKED at V3), `p3b-wave-sequencing.md` (V2-LOCKED at V3), `p3c-falsifiability-gates.md` (V3 amended F-V3-CH6-3 :36 + :423), `p3d-telemetry-schema.md` (V1-LOCKED), `p3e-preblocked-ledger.md` (V1-LOCKED), `p3f-spec-draft.md` (V1-LOCKED), `sk-v14/SPEC.md` (V2-LOCKED at V3), `sk-v14/DISPATCH-PROMPT.md` (V1-LOCKED).
HARD CAP 20 min (LOCK-trigger reduced cap); WRITE-ONLY.

---

## §1 — CH7 lens application at V3 (5 sub-tests × 8 artefacts = 40 dispositions)

CH7 canonical decomposition per `PASS-0-OVERFIT-AUDIT.md:62-87` carries five sub-tests (T1 fake-`@generated`; T2 SCAFFOLD-as-load-bearing; T3 Lock 14 generic-crate; T4 round-trip / executable-verification; T5 honest-revert vs silent re-introduction). V3 disposition focus per CHALLENGE-CONTEXT.md §2 narrows to three V3-specific checks: (1) NO new fabrication introduced by V3; (2) Stage-A target naming overfit-asymmetry preserved at V3; (3) LAC-1E-12 + NEW-CH2-V3-02 procedural addendums held at V3 fold site.

### §1.1 — Test C7-T1: no new fake `@generated` on hand-written templates in V3 fold-set (canonical P-1)

V2 baseline: 8/8 ACCEPT (W4 PRUNE-2 demolishes the canonical 7-template `@generated` cluster; W2 R4 lands `cargo xtask regen-css` first; V2 SPEC §15 AUDIT-FALSIFIED 28-row enumeration explicitly re-cites the P-1 anti-pattern for the 24 CSS L4 revert rows). V3 fold-set: ONE atomic cosmetic fold (F-V3-CH6-3) touching P3-C :36 (§1.2 W10 manifest row R8 task cell) + :423 (§2.10 W10 exit-gate item 8) — mirror SPEC §13:982 UNCONDITIONAL Stage-0 binding into P3-C's per-row gate restatement (closes V2 §4.4 carry-forward observation).

| Artefact | V3 Disposition | Evidence |
|---|---|---|
| p3a | ACCEPT (V2-LOCKED at V3; no V3 edits) | `git diff f25c3af2e..867b0cd0b -- p3a` = 0 lines. V2 F-V2-CH2-1 (C3) + F-V2-CH2-2 (C4) Stage-A discipline cells intact at V3. Zero new `@generated` admissions at V3. |
| p3b | ACCEPT (V2-LOCKED at V3; no V3 edits) | `git diff f25c3af2e..867b0cd0b -- p3b` = 0 lines. V2 wave manifest W0..W11 byte-identical at V3; W4 PRUNE-2 + W2 R4 ordering preserved verbatim. |
| p3c | ACCEPT (V3 amended; W10 R8 task cell + exit-gate item 8 only) | F-V3-CH6-3 amendment at `p3c:36` + `p3c:423` touches ONLY the conditional-Stage-0 residual language; zero new `@generated` admissions. The amendment REPLACES conditional language with UNCONDITIONAL binding inheritance — strengthens P-1-adjacent gate discipline (Stage-0 procedural-honesty), does NOT introduce new template surface. W4 PRUNE-2 gate (`p3c:60` per V2 §1.1 evidence) byte-identical. |
| p3d | ACCEPT (V1-LOCKED through V3) | No V2/V3 edits; column-presence gate (§3 #1) rejects rows missing AUDIT-FALSIFIED enforcement; `audit_overlay_verdict` column unchanged. |
| p3e | ACCEPT (V1-LOCKED through V3) | No V2/V3 edits; §2.1 CSS L4 cluster (lines 271-298) enumerates 6 SK-V13 CSS L4 admits AUDIT-FALSIFIED by P-1 verbatim. |
| p3f | ACCEPT (V1-LOCKED through V3) | No V2/V3 edits; §1.5 P-1..P-7 verbatim; §2.1 SPEC structure §15 binding intact. |
| SPEC | ACCEPT (V2-LOCKED at V3; no V3 edits) | `git diff f25c3af2e..867b0cd0b -- SPEC` = 0 lines. SPEC §15 line 1076 `P-1` verbatim binding intact at V3; §15 line 1122 AUDIT-FALSIFIED 28-row revert ledger preserved verbatim. Bypass detector at SPEC §494 line preserved verbatim. Zero new `@generated` admissions. |
| DISPATCH-PROMPT | ACCEPT (V1-LOCKED through V3) | `git diff f25c3af2e..867b0cd0b -- DISPATCH-PROMPT` = 0 lines. §6 failure-modes binding intact. |

**T1 V3 finding:** ZERO new fake-`@generated` admissions in V3 fold-set. The V3 F-V3-CH6-3 cosmetic fold is a 4-line diff to P3-C that REPLACES conditional Stage-0 language with unconditional inheritance binding — it does not introduce, weaken, or reframe any `@generated`-template surface. The canonical P-1 PRUNE-2 demolition (W4) and the AUDIT-FALSIFIED 28-row revert ledger (SPEC §15:1122) are intact at V3 HEAD (`SPEC.md` byte-identical V2→V3).

### §1.2 — Test C7-T2: no scaffold-as-load-bearing in V3 fold-set (canonical P-5)

V2 baseline: 8/8 ACCEPT (W7 PRUNE-5 SCAFFOLD-only → LOAD-BEARING wave intact; F-V2-CH5-1 SPEC §10:806 W7 `same_substrate_union` ENFORCEMENT-LAYER gloss explicitly distinguishes from SK-V9 W3 retired retained-class-column-union DATA STRUCTURE). V3 fold-set: zero V3 edits to W7 PRUNE-5 wave, the `same_substrate_union` gloss, the REDRESS 96-98 PERMANENT-PRE-BLOCK promotions, or the C-4 LOAD-BEARING binding.

| Artefact | V3 Disposition | Evidence |
|---|---|---|
| p3a | ACCEPT (V2-LOCKED at V3) | No V3 edits; C3 + C4 same-wave consumer cells intact. |
| p3b | ACCEPT (V2-LOCKED at V3) | No V3 edits; §2.2 W7 owner-path family intact; C-4 PRUNE-5 → LOAD-BEARING wave-level binding preserved at V3 manifest line 83. |
| p3c | ACCEPT (V3 amended; W7 gate untouched) | F-V3-CH6-3 amendment at `p3c:36` + `p3c:423` touches ONLY W10 (R8 parse_only) cells — does NOT touch the W7 PRUNE-5 SCAFFOLD-only demolition wave, the §2.7 W7 gate body, or the SCAFFOLD-only revert protocol. W10 amendment is on the Stage-0 procedural surface, not the SCAFFOLD-as-load-bearing surface. |
| p3d | ACCEPT (V1-LOCKED through V3) | §2.1 #26 `same_wave_consumer_class` REQUIRED with `[no-deferrals]` empty-rejection. |
| p3e | ACCEPT (V1-LOCKED through V3) | §4 W7 binding explicit "policy PASS-BLOCKED" for the SCAFFOLD-only surface C-4 PRUNE-5 must convert; P-5 verbatim per SYNTHESIS §0.4. |
| p3f | ACCEPT (V1-LOCKED through V3) | §1.5 P-5 verbatim; §2.1 SPEC §10 W7 PRUNE-5 section binding. |
| SPEC | ACCEPT (V2-LOCKED at V3) | F-V2-CH5-1 SPEC §10:806 W7 `same_substrate_union` ENFORCEMENT-LAYER gloss byte-identical at V3 (§3.2 below); REDRESS 96-98 PERMANENT-PRE-BLOCK at §10:829 + §15:1109 byte-identical at V3. |
| DISPATCH-PROMPT | ACCEPT (V1-LOCKED through V3) | §3 same-wave-consumer mandate; no orphan-kernel admission. |

**T2 V3 finding:** SCAFFOLD-as-load-bearing demolition surface INTACT at V3 with ZERO drift from V2. The V3 cosmetic fold does not approach the W7 PRUNE-5 SCAFFOLD-only surface — it operates exclusively on W10's Stage-0 procedural-honesty cell. The F-V2-CH5-1 `same_substrate_union` orphan-cell propagation guard at SPEC §10:806 carries through V3 unmodified (§3.2 verification).

### §1.3 — Test C7-T3: Stage-A target naming overfit-asymmetry preserved at V3 (CHALLENGE-CONTEXT V3 focus point 2)

This is the LOAD-BEARING CHALLENGE-CONTEXT V3 focus. V2 established the asymmetry (C3 to-be-authored at HEAD via Stage-A-queued checkasm row; C4 existing rule at HEAD via BBNF-self literal rule). V3 must verify the asymmetry holds at the new HEAD `867b0cd0b` — i.e. that the V3 cosmetic fold did not introduce, alter, or smuggle in any change to the Stage-A authoring queue.

| Artefact | V3 Disposition | Evidence |
|---|---|---|
| p3a | ACCEPT (V2-LOCKED at V3; asymmetry byte-identical) | `git diff f25c3af2e..867b0cd0b -- p3a` = 0 lines. F-V2-CH2-1 C3 amendment at `p3a:93` (Stage-A-queued checkasm row) byte-identical at V3; F-V2-CH2-2 C4 amendment at `p3a:106` (BBNF-self literal rule existing-at-HEAD consumer) byte-identical at V3. Asymmetry intact. |
| p3b | ACCEPT (V2-LOCKED at V3) | §5.2 P2-F bullet preserves Stage-A discipline byte-identical. |
| p3c | ACCEPT (V3 amended; Stage-A surface untouched) | F-V3-CH6-3 amendment touches W10 R8 task cell + W10 exit-gate item 8 only. Neither cell references C3/C4 Stage-A authoring queue; both cells operate on Stage-0 F-V2-P1ABC-RERECORD profile-record ledger procedural binding. No Stage-A naming asymmetry surface affected. The amendment ADDS the SPEC §13:982 inheritance-chain citation (Stage-0 binds to W10 because W10 is first wave consuming C1 long-string-body SIMD scan via R8 parse_only distinct path) — this RE-CITES the inheritance chain that itself anchors the C1 Stage-A discipline. Strengthens Stage-A binding cross-referent, does not weaken. |
| p3d | ACCEPT (V1-LOCKED through V3) | §2.1 #9 `profile_artifact` REQUIRED with non-existent-path → reject (LAC-1E-12 mandate). |
| p3e | ACCEPT (V1-LOCKED through V3) | §4 S-P2 LOCKED pool; no V1/V2/V3 row for unauthored primitives. |
| p3f | ACCEPT (V1-LOCKED through V3) | §1.3.2 §2.Y NF-CH6-4 canonical-name binding intact. |
| SPEC | ACCEPT (V2-LOCKED at V3) | §15 line 1119 Stage-A authoring targets `byte_context_64.rs` + `bcax_64.rs` present-future tense binding byte-identical at V3. Verb-tense pre-block intact. |
| DISPATCH-PROMPT | ACCEPT (V1-LOCKED through V3) | §4 same-wave consumer mandate. |

**T3 V3 finding:** Stage-A target naming overfit-asymmetry PRESERVED VERBATIM at V3. The V3 cosmetic fold is surgically scoped to the conditional-Stage-0 residual language fix prescribed by V2 §4.4 carry-forward — it does not touch the Stage-A authoring queue surface (C3 `byte_class_from_range_64.rs` queued, C4 `unescape_uxxxx_x8_neon` Gap 2 queued, P2-F C10/C13 Stage-A authoring targets queued). Stage-A sibling-shape templates re-verified PRESENT at V3 HEAD per §3.4 below; C3 target PATH-ABSENT (correctly NOT-PRESENT under Stage-A authoring queue framing). C4 BBNF-self literal rule re-verified PRESENT at V3 HEAD per §3.5.

### §1.4 — Test C7-T4: LAC-1E-12 procedural addendum + NEW-CH2-V3-02 orphan-cell propagation guard held at V3 fold site

LAC-1E-12 institutionalised at SPEC §1 non-negotiables in V2 (line 226). NEW-CH2-V3-02 orphan-cell propagation guard from S-P2 V3: any newly-added artefact cell must propagate to all downstream gate sites in the same micro-fold. V3 fold is the prototypical NEW-CH2-V3-02 application — V2 §4.4 surfaced a SPEC §13 ↔ P3-C W10 inheritance mismatch (SPEC unconditional binding NOT propagated to P3-C per-row gate); V3 F-V3-CH6-3 IS the missing orphan-cell propagation step.

| Artefact | V3 Disposition | Evidence |
|---|---|---|
| p3a | ACCEPT (V2-LOCKED at V3) | LAC-1E-12 cites at `p3a:93` + `p3a:106` byte-identical at V3 (V2 §3.6 executable-verification cite-chain preserved). |
| p3b | ACCEPT (V2-LOCKED at V3) | V2 wave-numbering reconcile single-atomic-micro-fold preserved; no V3 wave-id drift. |
| p3c | ACCEPT (V3 amended; NEW-CH2-V3-02 discharged) | F-V3-CH6-3 is the EXACT NEW-CH2-V3-02 orphan-cell propagation discharge. V2 §4.4: "P3-C §2.10 W10 exit gate item 8 (line 423) still reads 'If admitting any of the 12 F-V2-P1ABC-RERECORD consumer-dependency primitives, Stage 0 rerun is shipped per S-P2 V3 §6.3' — conditional language inherited from V1 era. This is one V2 fold site where the V2 unconditional Stage-0 binding (SPEC §13:982 + §13:990 + §13:1000) is NOT propagated to P3-C's per-row gate restatement." V3 F-V3-CH6-3 propagates the unconditional binding to BOTH affected P3-C cells (`p3c:36` manifest + `p3c:423` exit-gate item 8) in a SINGLE atomic micro-fold — the textbook orphan-cell propagation discharge. New text at `p3c:423` carries SPEC §13:982 cross-reference verbatim ("SPEC §13:982 binding — W10 is the bound wave per the 5-step inheritance chain") — LAC-1E-12 executable-verification cite-chain in line. |
| p3d | ACCEPT (V1-LOCKED through V3) | §2.1 #9 explicit "post-CH7 LAC-1E-12 executable-verification mandate" verbatim. |
| p3e | ACCEPT (V1-LOCKED through V3) | Per-wave entries cite `audit-overfit/validation/v* §reference` (executable cross-reference). |
| p3f | ACCEPT (V1-LOCKED through V3) | §3 W0/W2/W4/W5 gates literal bash command-line citations. |
| SPEC | ACCEPT (V2-LOCKED at V3) | LAC-1E-12 institutionalisation at SPEC line 226 byte-identical at V3 (§3.6); F-V2-CH6-1 5-step inheritance chain at §11/§12/§13 (9 unconditional restatements) byte-identical at V3 (§3.1). |
| DISPATCH-PROMPT | ACCEPT (V1-LOCKED through V3) | §5 CHALLENGE invocation + §7 status tick cadence; mandate inherits from dispatch context. |

**T4 V3 finding:** LAC-1E-12 mandate INTACT at V3 (SPEC line 226 byte-identical). NEW-CH2-V3-02 orphan-cell propagation guard DISCHARGED PRECISELY by the F-V3-CH6-3 atomic cosmetic fold — the V2 §4.4 carry-forward observation that SPEC §13 unconditional binding was NOT propagated to P3-C per-row gates is now fully discharged by V3's two-cell atomic propagation (P3-C `:36` manifest + `:423` exit-gate item 8). The V3 amendment text itself carries the LAC-1E-12 cross-reference inline ("SPEC §13:982 binding"; "5-step inheritance chain") — the orphan-cell propagation guard is self-documenting at the V3 fold site.

### §1.5 — Test C7-T5: V3 fold-set introduces NO silent re-introduction of prior-rejected patterns under new framing

V2 baseline: 8/8 ACCEPT (F-V2-CH5-1 + F-V2-CH3-1 + F-V2-CH3-2 explicitly NAME recurrence vectors, DISTINGUISH measured-rejection from audit-overlay-falsification, ENUMERATE every revert row by REDRESS item id). V3 fold-set: ONE 4-line cosmetic fold to P3-C — must verify it does not silently re-introduce any prior-rejected pattern under cosmetic reframing.

| Artefact | V3 Disposition | Evidence |
|---|---|---|
| p3a | ACCEPT (V2-LOCKED at V3) | Variable-width CSS L4 `\HEXHEX` carve-out as separate-primitive measured-rejection byte-identical at V3; no silent re-introduction surface. |
| p3b | ACCEPT (V2-LOCKED at V3) | V2 wave-numbering byte-identical; W4 PRUNE-2 / W7 PRUNE-5 demolition surface intact. |
| p3c | ACCEPT (V3 amended; STRENGTHENING fold, zero re-introduction) | F-V3-CH6-3 ELIMINATES conditional language ("if any consumer-dependency primitive admitted") in favour of UNCONDITIONAL binding ("Stage-0 F-V2-P1ABC-RERECORD UNCONDITIONALLY per S-P2 V3 §6.3 (SPEC §13:982 binding)"). This is the OPPOSITE of silent re-introduction: it REMOVES residual V1-era latent-conditional language that V2 §4.4 surfaced as inconsistent with the V2 SPEC §13 unconditional binding. No prior-rejected pattern reframed; one V1-era residual cleanly closed. |
| p3d | ACCEPT (V1-LOCKED through V3) | §2.1 column-presence gates intact. |
| p3e | ACCEPT (V1-LOCKED through V3) | §2.1 REDRESS 102-110 + 119+120 + 133-135 + 154-158 + 160 cluster intact. |
| p3f | ACCEPT (V1-LOCKED through V3) | No V2/V3 edits. |
| SPEC | ACCEPT (V2-LOCKED at V3) | F-V2-CH5-1 W7 `same_substrate_union` ENFORCEMENT-LAYER gloss (REDRESS 96-98 distinction); F-V2-CH3-1 28-row revert ledger; F-V2-CH3-2 REDRESS 102/103/106/108 measured-rejection distinction — all byte-identical at V3 (§3.2 + §3.3 + §3.6). Honest taxonomies preserved. |
| DISPATCH-PROMPT | ACCEPT (V1-LOCKED through V3) | No V2/V3 edits; §6 failure-modes binding. |

**T5 V3 finding:** V3 fold-set introduces ZERO silent re-introduction of any prior-rejected pattern. F-V3-CH6-3 is a STRENGTHENING fold that eliminates one V2-era residual inconsistency — it moves P3-C `:36` + `:423` FROM conditional language TO unconditional binding, the same direction that V2 F-V2-CH6-1 already moved SPEC §11/§12/§13. The V3 fold closes the V2 §4.4 carry-forward CH1/CH6-axis observation cleanly, and does not approach any of the V2-established overfit-prune surfaces (W4 PRUNE-2, W7 PRUNE-5, REDRESS 96-98 PERMANENT-PRE-BLOCK, AUDIT-FALSIFIED 28-row ledger, REDRESS 102/103/106/108 measured-rejection).

---

## §2 — V3 ACCEPT-rate + cycle disposition

**ACCEPT-rate: 40/40 = 100.0%** (zero REJECTs across 5 tests × 8 artefacts).

**Cycle disposition:** ACCEPT V3 ON CH7 LENS. THIRD consecutive ≥95% cycle (V1 = 100.0%; V2 = 100.0%; V3 = 100.0%) — meets §3Z LOCK condition with one cycle of margin. CH7 lens has now sustained 100% ACCEPT across all three S-P3 hardening cycles.

**Material drift from V2 to V3:** ONE atomic cosmetic fold (F-V3-CH6-3, 4-line diff in P3-C) closing the V2 §4.4 carry-forward orphan-cell observation. Zero V3 edits to the other 7 artefacts (P3-A, P3-B, P3-D, P3-E, P3-F, SPEC, DISPATCH-PROMPT). V3 fold is STRENGTHENING along T4 (NEW-CH2-V3-02 orphan-cell propagation guard discharged) and STRENGTHENING along T5 (V1-era conditional residual replaced with unconditional binding) — both moves carry CH7 lens forward, neither regresses.

---

## §3 — Executable verification (every cited path:line re-executed at V3 HEAD `867b0cd0b`)

Per LAC-1E-12 procedural addendum mandate + CHALLENGE-CONTEXT.md §3 line 36 + memory `[redispatch-empty-return]`.

### §3.1 — F-V3-CH6-3 conditional-Stage-0 residual eliminated; unconditional binding propagated to P3-C

```
$ grep -n "UNCONDITIONALLY\|UNLESS it admits one of the 12" \
    restart/skinny/tranches/sk-v14/research/p3/p3c-falsifiability-gates.md
36:| W10 | R8 | stand up distinct `parse_only` code path in `generated_json` (no full-tape build); wire to sonic-rs Skipper-class comparator; admit; Stage-0 F-V2-P1ABC-RERECORD UNCONDITIONALLY per S-P2 V3 §6.3 (SPEC §13:982 binding) | R8 | 17 (per corpus) |
423:8. Stage-0 F-V2-P1ABC-RERECORD shipped UNCONDITIONALLY per S-P2 V3 §6.3 (SPEC §13:982 binding — W10 is the bound wave per the 5-step inheritance chain): cargo build + interactive samply record + cfg_attr flip at `generated.rs:33-237` 8 sites, in this wave's commit slice, BEFORE any parse_only admit lands.
```

V3 HEAD: both P3-C cells (`:36` manifest + `:423` exit-gate item 8) now carry the UNCONDITIONAL binding citing SPEC §13:982 directly. The V1-era conditional language ("if any consumer-dependency primitive admitted" / "If admitting any of the 12...") is eliminated from the active artefacts (only appears in V1/V2 hardening reports, which is correct historical residue). F-V3-CH6-3 propagation discharged.

```
$ grep -rn "If admitting any\|if any consumer-dependency primitive admitted" \
    restart/skinny/tranches/sk-v14/research/p3/p3*.md
(no output)
```

Zero conditional-Stage-0 residual hits across all six P3 axis files at V3 HEAD. T4 NEW-CH2-V3-02 orphan-cell propagation discharged at the active-artefact level.

### §3.2 — F-V2-CH5-1 W7 `same_substrate_union` gloss + REDRESS 96-98 PERMANENT promotion (T2 + T5 evidence; byte-identical V2→V3)

```
$ grep -n "REDRESS 96-98\|REDRESS 96/97/98\|same_substrate_union" \
    restart/skinny/tranches/sk-v14/SPEC.md | head -5
783:- `skinny/crates/passes/src/` (wire `per_grammar_policy` + `same_substrate_union` into compile + lower paths; currently gate-layer-only per S-P0 A5 NEW-MED)
806:2. Wire `same_substrate_union` (W9 SCAFFOLD per ORCHESTRATOR-PROMPT.md:124-126) into compile + lower + runtime paths. The W7 `same_substrate_union` module is an ENFORCEMENT-LAYER pass... it is NOT the SK-V9 W3 retired retained-class-column-union data structure (PERMANENT-PRE-BLOCK per REDRESS 96/97/98)...
829:- **REDRESS 96-98 PERMANENT-PRE-BLOCK** — full class-column vectors, streaming structural cursors, class-lane-only replays, parser-owned sidecars, UnionTape-style retained structures per Lock 1 v+1 substrate-ceiling history. The W7 `same_substrate_union` ENFORCEMENT module is NOT a re-opening of REDRESS 96/97/98...
1109:- REDRESS 96-98: full class-column vectors, streaming structural cursors, class-lane-only replays, parser-owned sidecars, UnionTape-style retained structures per Lock 1 v+1 substrate-ceiling history.
```

V3 HEAD: 4 references identical to V2 §3.2 (V2 → V3 byte-identical; SPEC unmodified at V3). Orphan-cell propagation guard preserved verbatim.

### §3.3 — F-V2-CH3-1 AUDIT-FALSIFIED 28-row revert ledger by REDRESS item id (T1 + T5 evidence; byte-identical V2→V3)

```
$ grep -n "AUDIT-FALSIFIED admit-row revert ledger" \
    restart/skinny/tranches/sk-v14/SPEC.md
1122:### AUDIT-FALSIFIED admit-row revert ledger (22 JSON items + 24 CSS L4 items = 46 by-number; dispatch headcount references the 22 JSON revert manifest):
```

V3 HEAD: §15 sub-section at line 1122 byte-identical V2 → V3. Honest revert ledger by REDRESS item id preserved.

### §3.4 — Stage-A sibling-shape templates at V3 HEAD (T3 C3 evidence; byte-identical V2→V3)

```
$ ls skinny/crates/bbnf-simd/src/scalar/ | grep -E 'byte_class_from_(range|eq_set)'
byte_class_from_eq_set_64.rs

$ ls skinny/crates/bbnf-simd/tests/ | grep -E 'checkasm_byte_class_from_(range|eq_set)'
checkasm_byte_class_from_eq_set_64.rs
```

C3 Stage-A target `byte_class_from_range_64.rs` CORRECTLY NOT-PRESENT at V3 HEAD (Stage-A authoring queued for same-commit admission per Lock 16); sibling-shape template `byte_class_from_eq_set_64.rs` PRESENT at V3 HEAD. Checkasm sibling-shape template `checkasm_byte_class_from_eq_set_64.rs` PRESENT at V3 HEAD (the path P3-A:93 cites as the model). F-V2-CH2-1 Stage-A asymmetry preserved at V3.

### §3.5 — C4 BBNF-self literal rule at V3 HEAD (T3 C4 evidence; byte-identical V2→V3)

```
$ grep -n "literal = " grammar/bbnf/bbnf.bbnf
11:literal = ( "\"" , /(\\.|[^"\\])*/  , "\""
```

C4 same-wave non-JSON consumer EXISTING at V3 HEAD: BBNF-self literal rule at `grammar/bbnf/bbnf.bbnf:11` (executable-verified). F-V2-CH2-2 Stage-A "existing rule" asymmetry preserved at V3.

### §3.6 — LAC-1E-12 mandate institutionalisation at SPEC §1 non-negotiables (T4 evidence; byte-identical V2→V3)

```
$ grep -n "Executable verification mandate\|LAC-1E-12" \
    restart/skinny/tranches/sk-v14/SPEC.md
226:- **Executable verification mandate (CH7 V2 lesson + LAC-1E-12 procedural addendum):** any cited path:line in any wave's plan or redress MUST be re-executed at HEAD before commit; absence claims without captured command output are UNKNOWN verification actions, not gate closure (per Lock 3 v+1 verification clause + S-P2 dispatch-context §2).
```

V3 HEAD: LAC-1E-12 institutionalisation at SPEC §1 non-negotiables byte-identical V2 → V3. Mandate intact.

### §3.7 — T1 fake-`@generated` template cluster still PRESENT at V3 HEAD (canonical P-1 PRUNE-2 demolition target)

```
$ find skinny/crates/codegen/src -name 'css_l4_*_templates' -type d | wc -l
7
```

V3 HEAD: 7 hand-written CSS L4 template directories STILL PRESENT (the canonical P-1 anti-pattern PRUNE-2 demolishes at W4). Demolition target intact; no V3 drift.

### §3.8 — Unconditional Stage-0 restatement count at SPEC (F-V2-CH6-1 5-step inheritance chain; byte-identical V2→V3)

```
$ grep -c "UNCONDITIONALLY\|unconditionally" \
    restart/skinny/tranches/sk-v14/SPEC.md
9
```

V3 HEAD: 9 unconditional-Stage-0-binding restatements across §11 (W8 entry-gate + task + exit-gate), §12 (W9 entry-gate + task + exit-gate), §13 (W10 entry-gate + task + exit-gate) — byte-identical V2 → V3. F-V2-CH6-1 5-step inheritance chain preserved verbatim at V3; with V3 F-V3-CH6-3 P3-C `:36` + `:423` propagation, the inheritance chain is now FULLY MIRRORED in P3-C's per-row gate restatement (the V2 §4.4 carry-forward gap is closed).

### §3.9 — V2-LOCKED artefacts truly untouched at V3 (zero-drift verification)

```
$ git diff f25c3af2e..867b0cd0b -- \
    restart/skinny/tranches/sk-v14/SPEC.md \
    restart/skinny/tranches/sk-v14/research/p3/p3a-candidate-shortlist.md \
    restart/skinny/tranches/sk-v14/research/p3/p3b-wave-sequencing.md \
    restart/skinny/tranches/sk-v14/research/p3/p3d-telemetry-schema.md \
    restart/skinny/tranches/sk-v14/research/p3/p3e-preblocked-ledger.md \
    restart/skinny/tranches/sk-v14/research/p3/p3f-spec-draft.md \
    restart/skinny/tranches/sk-v14/DISPATCH-PROMPT.md | wc -l
0
```

V3 HEAD: 0 lines of diff across all 7 V2-LOCKED artefacts (V2 aggregator commit `f25c3af2e` → V3 cosmetic-fold commit `867b0cd0b`). The V3 cycle is surgically scoped to the F-V3-CH6-3 P3-C `:36` + `:423` propagation as advertised in CHALLENGE-CONTEXT.md §0:11. Zero stealth V3 edits anywhere.

### §3.10 — V3 commit footprint matches CHALLENGE-CONTEXT.md §0:11 declaration

```
$ git show --stat 867b0cd0b
 .../research/p3/hardening/V3/CHALLENGE-CONTEXT.md  | 43 ++++++++++++++++++++++
 .../sk-v14/research/p3/p3c-falsifiability-gates.md |  4 +-
 2 files changed, 45 insertions(+), 2 deletions(-)
```

V3 commit footprint: 2 files (V3 CHALLENGE-CONTEXT.md = 43-line authoring; P3-C = 4-line cosmetic fold). Matches CHALLENGE-CONTEXT.md §0:5 "V3 atomic cosmetic-fold commit (1 artefact: P3-C :36 + :423 textual consistency mirror to SPEC unconditional)" declaration verbatim. No hidden surface.

---

## §4 — Findings

### §4.1 — Five sub-test cohort outcome at V3 (T1..T5)

- **T1 fake-`@generated`:** 8/8 ACCEPT. ZERO new admissions; V2 AUDIT-FALSIFIED 28-row ledger and W4 PRUNE-2 demolition surface byte-identical at V3. The V3 fold does not approach any `@generated`-template surface.
- **T2 SCAFFOLD-as-load-bearing:** 8/8 ACCEPT. W7 PRUNE-5 demolition wave + `same_substrate_union` ENFORCEMENT-LAYER gloss + REDRESS 96-98 PERMANENT-PRE-BLOCK byte-identical V2 → V3. The V3 fold operates exclusively on W10 Stage-0 procedural cells.
- **T3 Stage-A target naming overfit-asymmetry:** 8/8 ACCEPT. C3 + C4 same-wave non-JSON consumer naming preserved verbatim at V3 HEAD; sibling-shape templates re-verified PRESENT at V3 HEAD (§3.4 + §3.5); C3 target correctly NOT-PRESENT under Stage-A authoring queue framing.
- **T4 LAC-1E-12 + NEW-CH2-V3-02 orphan-cell propagation:** 8/8 ACCEPT. LAC-1E-12 SPEC §1 institutionalisation byte-identical at V3 (§3.6); NEW-CH2-V3-02 orphan-cell propagation guard DISCHARGED PRECISELY by F-V3-CH6-3 P3-C `:36` + `:423` two-cell atomic fold — the V2 §4.4 carry-forward orphan-cell observation is now fully closed.
- **T5 no silent re-introduction:** 8/8 ACCEPT. V3 fold is STRENGTHENING (eliminates V1-era conditional residual, replaces with unconditional binding citing SPEC §13:982 + 5-step inheritance chain). Zero V3 reframing of any retired anti-pattern.

### §4.2 — V3 cycle convergence — LOCK extension

CH7 lens has now sustained **100% ACCEPT across V1, V2, and V3** — three consecutive cycles with zero REJECTs across 40 dispositions each. This is the 3-cycle LOCK extension per CHALLENGE-CONTEXT.md §2 line 31. The V3 cosmetic fold is the smallest possible material change (4-line P3-C diff) needed to discharge the V2 §4.4 carry-forward observation, and it does so cleanly without disturbing any other CH7 surface.

The V3 fold-text itself ("Stage-0 F-V2-P1ABC-RERECORD UNCONDITIONALLY per S-P2 V3 §6.3 (SPEC §13:982 binding — W10 is the bound wave per the 5-step inheritance chain)") carries the LAC-1E-12 cross-reference inline + cites the F-V2-CH6-1 5-step inheritance chain by name — self-documenting against future V4+ orphan-cell drift.

### §4.3 — Zero V3 carry-forward observation for CH7

CH7 V3 surfaces NO orchestrator-side carry-forward observation. The V2 §4.4 CH1/CH6-axis observation (P3-C `:36` + `:423` conditional residual) is now fully discharged at V3. All five CH7 sub-tests pass at 100% with byte-identical surfaces from V2 (T1, T2, T3 in part, T4 SPEC line 226 mandate, T5 SPEC ledgers) or with strengthening V3 amendments (T3 propagation, T4 NEW-CH2-V3-02 discharge, T5 V1-era residual elimination).

CH7 lens converged at V2; V3 confirms convergence with LOCK extension to 3 cycles. No CH7 V4 redispatch needed.

---

## §5 — Disposition + LOCK extension declaration

**V3 CH7 DISPOSITION: ACCEPT.** 40/40 = 100.0% ACCEPT-rate across 5 sub-tests × 8 artefacts. Zero REJECTs.

**Cycle history:**
- V1 CH7: 100.0% ACCEPT (8/8 × 5 = 40/40).
- V2 CH7: 100.0% ACCEPT (40/40); second consecutive ≥95% → §3Z LOCK condition met for CH7 at V2.
- V3 CH7: 100.0% ACCEPT (40/40); **THIRD consecutive ≥95% → 3-cycle LOCK extension achieved**.

**LOCK extension:** CH7 lens at V3 carries 3-cycle ≥95% LOCK extension per CHALLENGE-CONTEXT.md §2 line 31 ("3-cycle LOCK extension"). With V≤5 ceiling and V3 = third consecutive 100% cycle, CH7 has 2 cycles of margin before ceiling — converged with substantial margin.

**Carry-forward to V4 (if cohort cycles for non-CH7 reasons):**

1. CH7 V3 LOAD-BEARING bindings carried forward unchanged from V2 §5 (all V2 bindings preserved verbatim at V3 HEAD).
2. V3 NEW binding: F-V3-CH6-3 P3-C `:36` + `:423` unconditional Stage-0 propagation citing SPEC §13:982 + 5-step inheritance chain (V2 §4.4 carry-forward discharged).
3. Zero V3 orchestrator-side observation. CH7 converged.

**No CH7 V4 redispatch needed.** CH7 lens disposition at V3: 3-cycle LOCK extension; full convergence; the V3 cosmetic fold closed the only V2-era CH7-adjacent carry-forward cleanly.

---

## §6 — Sources

### §6.1 — CH7 lens authority

- `restart/prompts/skinny/PASS-0-OVERFIT-AUDIT.md:62-87` — CH7 canonical definition (5 sub-tests).
- `restart/skinny/tranches/sk-v14/research/p3/hardening/V3/CHALLENGE-CONTEXT.md` (HEAD `867b0cd0b`) — V3 dispatch authority + §2 disposition focus.
- `restart/skinny/tranches/sk-v14/research/p3/hardening/V2/CH7.md` (V2 CH7; 100% ACCEPT; 2-cycle LOCK) — V2 baseline carry-forward including §4.4 carry-forward orphan-cell observation now discharged at V3.
- `restart/skinny/tranches/sk-v14/research/p3/hardening/V1/CH7.md` (V1 CH7; 100% ACCEPT) — V1 baseline binding.

### §6.2 — 8 artefacts under V3 review

- `restart/skinny/tranches/sk-v14/research/p3/p3a-candidate-shortlist.md` (V2-LOCKED at V3; 0 lines V2 → V3 diff).
- `restart/skinny/tranches/sk-v14/research/p3/p3b-wave-sequencing.md` (V2-LOCKED at V3; 0 lines V2 → V3 diff).
- `restart/skinny/tranches/sk-v14/research/p3/p3c-falsifiability-gates.md` (V3 amended — F-V3-CH6-3 :36 + :423 unconditional Stage-0 propagation; 4-line diff).
- `restart/skinny/tranches/sk-v14/research/p3/p3d-telemetry-schema.md` (V1-LOCKED through V3).
- `restart/skinny/tranches/sk-v14/research/p3/p3e-preblocked-ledger.md` (V1-LOCKED through V3).
- `restart/skinny/tranches/sk-v14/research/p3/p3f-spec-draft.md` (V1-LOCKED through V3).
- `restart/skinny/tranches/sk-v14/SPEC.md` (V2-LOCKED at V3; 0 lines V2 → V3 diff; all V2 SPEC folds byte-identical at V3 HEAD).
- `restart/skinny/tranches/sk-v14/DISPATCH-PROMPT.md` (V1-LOCKED through V3).

### §6.3 — V3 fold-commit authority

- `git show 867b0cd0b` — V3 atomic cosmetic-fold commit (1 artefact amended: P3-C `:36` + `:423`; V3 CHALLENGE-CONTEXT.md authored at 43 lines; total 2 files / 45 insertions / 2 deletions).
- V2 aggregator: `f25c3af2e` — V2 hardening consolidated + 7 V2 lens reports (V2 → V3 diff baseline).

### §6.4 — Cross-axis V3 convergence anchors (byte-identical V2 → V3 evidence)

- `restart/skinny/tranches/sk-v14/SPEC.md:226` (LAC-1E-12 institutionalisation; byte-identical at V3).
- `restart/skinny/tranches/sk-v14/SPEC.md:806,829,1109` (F-V2-CH5-1 + REDRESS 96-98 PERMANENT-PRE-BLOCK promotion; byte-identical at V3).
- `restart/skinny/tranches/sk-v14/SPEC.md:863,873,880,923,933,940,982,990,1000` (F-V2-CH6-1 unconditional Stage-0 binding + 5-step inheritance chain × 9 restatements; byte-identical at V3).
- `restart/skinny/tranches/sk-v14/SPEC.md:1110` (F-V2-CH3-2 REDRESS 102/103/106/108 PERMANENT-PRE-BLOCK measured-rejection distinction; byte-identical at V3).
- `restart/skinny/tranches/sk-v14/SPEC.md:1122` (F-V2-CH3-1 AUDIT-FALSIFIED 28-row revert ledger; byte-identical at V3).
- `restart/skinny/tranches/sk-v14/SPEC.md:422-426` (F-V2-CH3-3 §4 W1 Task 6a 22-row revert manifest by REDRESS item id; byte-identical at V3).
- `restart/skinny/tranches/sk-v14/SPEC.md:713` (F-V2-CH4-1 §9 W6 810-min cumulative cap footnote; byte-identical at V3).
- `restart/skinny/tranches/sk-v14/research/p3/p3a-candidate-shortlist.md:93,106` (F-V2-CH2-1 C3 + F-V2-CH2-2 C4 amendments; byte-identical at V3).
- `restart/skinny/tranches/sk-v14/research/p3/p3b-wave-sequencing.md:83-87` (V2 wave manifest W7..W11 section-relabel; byte-identical at V3).
- `restart/skinny/tranches/sk-v14/research/p3/p3c-falsifiability-gates.md:36,423` (F-V3-CH6-3 NEW at V3 — unconditional Stage-0 binding propagation).

### §6.5 — Lock authority

- `restart/locks/LOCKS.md` (Lock 1 substrate-union; Lock 14 grammar-neutrality; Lock 16 SIMD/ASM allowlist + abstract-primitive declarations).

### §6.6 — bbnf source-of-truth (executable-verified at V3 HEAD per §3)

- `skinny/crates/bbnf-simd/src/scalar/byte_class_from_eq_set_64.rs` (C3 sibling-shape template PRESENT at V3 HEAD; F-V2-CH2-1 evidence preserved).
- `skinny/crates/bbnf-simd/tests/checkasm_byte_class_from_eq_set_64.rs` (C3 checkasm sibling-shape template PRESENT at V3 HEAD).
- `grammar/bbnf/bbnf.bbnf:11` (C4 BBNF-self literal rule PRESENT at V3 HEAD; F-V2-CH2-2 evidence preserved).
- `skinny/crates/codegen/src/css_l4_*_templates/` (7 hand-written template directories STILL PRESENT at V3 HEAD; W4 PRUNE-2 demolition target intact).
