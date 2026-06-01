---
cycle: V6
lens: CH6 (V6) — ANTI-PAPER-CLOSE
pass: T-P1-TOTALITY-EXCAVATION
reviewer_role: adversarial CHALLENGE
generated_at: 2026-06-01
targets: [1A-substrate-evidence, 1B-codegen-evidence, 1C-runtime-evidence, 1D-skinny-lessons, 1E-locks-evidence, 1F-coherence-scan, 1F-anti-pattern, 1F-past-corpora]
verification_head: dirty tree at master (3ac131c45 + uncommitted generated.rs)
---

# CH6 (V6) — ANTI-PAPER-CLOSE Verdict

## Lens

No inventory may self-report a divergence "resolved/wired" without a live-evidence
citation; no divergence may be deferred to "a later inventory"; every UNKNOWN must
carry a verify_action. Spot-verify the most load-bearing cited path:line rows
against the V1 spec (ARCHITECTURE / MASTER-PLAN / LOCKS) and live code.

## Method

Read all eight inventories end-to-end. Spot-verified 30+ of the highest-leverage
path:line rows on disk and against the V1 LOCKS surface. Ran the literal
LOCKS:349 self-gate command (both ir+analysis scope AND full-13-crate scope).
Scanned all eight files for paper-close closure words and deferral-to-later-
inventory patterns. Confirmed every UNKNOWN row carries a verify_action.

## Load-Bearing Citations Spot-Verified (all resolve EXACTLY on disk)

| claim | inventory | live result |
|---|---|---|
| `ValueRef<'doc,'input,K=AnyKind,G:EventGrammar=AnyGrammar>` `tape/mod.rs:175`; `_kind`/`_grammar` PhantomData `:178`/`:179`; `Tape` `:94`; `id` `:170` | 1A,1C,1D,1E,1F | verbatim match |
| `BackendShape` 5-variant enum `ir/src/lib.rs:340-346` | all | exact 5 variants |
| `ExprKind` 8 variants (Seq/Alt/Repeat/Optional/Literal/Regex/Ref/Annotation), no Predicate/Lookbehind/Call/Layout/Error | 1A | exact 8, no extras |
| `select_lowering` 5-arm match on `cost.chosen` `lower/mod.rs:18-26` | 1B,1D,1E | exact 5 arms, zero grammar names |
| `RuntimeEmitterKind{CompiledLowering,RequestFacts}` `grammar_provider.rs:40-42`; `emitter` field `:33`; `:110` CSS-exempt | 1B,1C,1D,1E,1F | verbatim |
| `runtime_generator.rs:16-26` match fork; `:91` `normalize(CSS_GENERATED_RS)`; `:701` const open / `:1611` close | 1B,1C,1D,1E,1F | verbatim |
| LOCKS:349 self-gate live = **13 sites** (11 ir + 2 analysis); full 13-crate scope ALSO = 13 | 1C,1E,1F | exact 13/11/2 |
| 9 idents rows `strategy.rs:137,143,149,155,161,167,173,179,185`; consumer `:216` | 1E,1F | verbatim |
| LOCKS:620 "The `G:EventGrammar` type parameter is the generality vehicle" | 1A,1E,1F | verbatim (amend candidate grounded) |
| CSS config.rs: zero `W7_/BackendShape/substrate_target`; JSON config triad `:22-30` present | 1A | 0 vs full triad |
| 7 css_l4 `generated.rs` md5 `b654562c…` | 1C,1D,1F | identical |
| `parse_w11_1_number` ×7 in json `generated.rs` | 1C,1D,1E | exactly 7 |
| DirectParser struct text at `:668`, `cursor: usize` field `:671`, rollback `:361`, instantiation `:56` | 1A | verbatim |
| `css_cold_harness.rs` `track1_full` at `:131`, comment at `:130` | 1D,1F-anti | verbatim (CH6-V4-005 re-anchor correct) |
| sheets_witness 24 + 1 LOC; css_types.rs 66 LOC `:1` host-shim; builder.rs 817 LOC; x86 28 files; simd-scan 217 LOC; Pattern-H 71/67 | 1C,1D,1E,1F | all exact |
| 8-of-9 OnceCell breadth (math has 0 `ensure_structural_index`) | 1E,1F-anti | exact (all 8 non-math = 2-3; math = 0) |
| CSS body `:702 use crate::tape::{...}`; CSS `generated.rs:257` "Holds exactly the existing `Tape` — no second substrate" | 1A,1C | verbatim |
| decision_csp `:151`/`:265`; REWRITE_SET `:9`; derive_backend_shape `:392`; lower_program `:122`; render_rule `:58` | 1B | verbatim |

## ANTI-PAPER-CLOSE Findings (enumerated)

### F1 — Closure-word scan: 3 hits, all correctly handled (ACCEPT)

A full-corpus scan for `resolved|wired|closed|fixed|done|complete` returned exactly
three hits, each verified NOT a paper-close:

- **1F-past-corpora.md:76** — "RESOLVED" appears in quotes with an explicit
  downgrade: "'RESOLVED' downgraded per CH6-V3-F2, the closure word is not carried
  into the SK-V18 inheritance predicate." The inventory is catching a paper-close,
  not committing one. ACCEPT.
- **1D-skinny-lessons.md:66** — "Phantom `<G>` resolved by DELETE" is the SPEC-CLAIM
  column (SK-V18 goal phrasing); the verdict column reads **unimplemented** with the
  live citation showing `<G>` still present at `tape/mod.rs:175` (disk-confirmed).
  The "resolved" is planned, the verdict is NOT-done. ACCEPT.
- **1A-substrate-evidence.md:108 / 1A-SUB-016** — "Prior mislabel resolved" names
  the REMOVAL of the W7 mislabel (disk-confirmed: 0 `W7_` rows in CSS config), and
  the row immediately states the removal OPENED an opposite-direction gap
  (1A-DIV-005, routed to G5 / 1A-UNK-004). This is the CH6-F2 correction working as
  intended — a removal-fact plus a freshly-opened gap, NOT a closure. ACCEPT.

### F2 — Deferral-to-later-inventory scan: ZERO hits (ACCEPT)

No "later inventory / deferred to 1X / covered later" patterns. Every cross-
inventory reference is a CITATION to a CO-EQUAL sibling row (e.g. 1C C12 routes the
generator-provenance closure to 1A-UNK-003 + 1D U-1, both of which EXIST and carry
verify_actions), never a deferral of the finding itself. The 1A-SUB-018 / C12
downgrades from impl_exceeds_spec are accompanied by the live falsifying citation,
not punted. ACCEPT.

### F3 — Every UNKNOWN carries a verify_action (ACCEPT)

- 1A: UNK-001..005 — verify_action column, all 5 populated.
- 1B: U1/U2/U3 — each carries an inline `VERIFY:` clause (`:153`/`:158`/`:163`).
- 1C: U1..U4 + U-1/U-COH18-002 — verify_action prose on each.
- 1D: U-1..U-5 — `**verify_action:**` prose on each.
- 1E: 1E-V5-U1..U3 + U-4 cross-ref — verify_action column populated.
- 1F-coherence: U-COH18-001/002 + U-4 cross-ref — verify_action column populated.
No bare UNKNOWN exists in any inventory. ACCEPT.

### F4 — LOCKS:349 self-gate falsification is REAL and exactly stated (ACCEPT)

1E D-1E-V5-14 / 1F COH18-012 assert Lock 14's OWN verification command "returns
ZERO" but live returns **13 sites**. The literal command returns 13 (11 ir + 2
analysis) at BOTH the ir+analysis scope AND the full 13-crate LOCKS:349 scope (the
other named crates either are absent under `crates/` or add zero hits). This is an
inventory correctly catching a FALSE assertion in the V1 spec (a self-gate that is
RED), which is precisely an admissible divergence finding — NOT an inventory error.
The 4-of-9 / 5-escape ident-row breakdown (`:137,143,149,155` caught vs
`:161,167,173,179,185` escaping the narrow 4-name regex) is disk-exact. ACCEPT.

### F5 — Amendment candidates grounded in live spec text (ACCEPT)

1A-LOCK1-AMEND-001 / 1E LAC-1E-V5-01..07 / 1F COH18-008 rest on LOCKS:620 reading
verbatim "The `G:EventGrammar` type parameter is the generality vehicle" and the
certified SK-V18 DELETE of `<G>` (SPEC:1202-1207 lineage; disk shows `<G>` still
present, test-only). Both poles verified; the contradiction is real, the candidates
are disposition-routed to T-P3 / SK-V19 / Pass Omega (never amended in-pass). ACCEPT.

### F6 — Prior-cycle re-anchor corrections re-verify clean (ACCEPT)

Spot-checked the error-prone re-anchors from V1-V5: track1_full `:131` (not `:130`
comment) — correct; DirectParser cursor field `:671`, struct text on `:668` —
correct; `measure_mbps` resolves to `src/nonjson_css_l4.rs:3091` not the 318-LOC
`benches/` sibling — consistent; REDRESS item-51 span `742-768` / item-53 `784-813`
with item-52 carve-out — internally consistent across 1B/1D/1E; 8-of-9 OnceCell
breadth (math=0) — disk-exact; Tape::id `:170` (not the prior `:172`) — disk-exact.
The 6th-cycle citation discipline is at fixed point. ACCEPT.

## Disposition

This is a 6th-cycle inventory set hardened through 5 prior cycles. Under the
ANTI-PAPER-CLOSE lens, the inventories are SOUND: every divergence claiming a
removal/fact carries a live citation; no divergence is deferred to a later
inventory (only co-equal cross-references); every UNKNOWN carries a verify_action;
every load-bearing path:line resolves exactly on disk and against the V1 LOCKS
surface; the one genuinely FALSE on-disk statement surfaced (the LOCKS:349 self-gate
asserting ZERO) is correctly caught BY the inventories as an admissible spec-defect
divergence, not committed by them.

No residual REVISE rises to the "would mislead a T-P2 reader" bar — the three
closure-word occurrences are all explicitly quoted/downgraded/gap-opened in situ,
which is exactly what a careful T-P2 reader needs. No GENUINE reject: no inventory
states something false on disk (per the corrected convention, catching the spec's
own false self-gate is an ACCEPT of the inventory, not a reject). This lens reaches
a clean fixed point for V6.

TALLY accept=6 revise=0 reject=0
