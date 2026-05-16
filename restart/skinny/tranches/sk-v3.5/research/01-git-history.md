# PSI / DTA / Structural-Indexing Excavation — bbnf-lang

**Date**: 2026-05-12 · **Repo**: `/Users/mkbabb/Programming/bbnf-lang` · **HEAD**: master @ `8f2ee399`

---

## (a) Commit Census + Era Taxonomy

**Total `git log --all --oneline` count**: **3,005 commits**.

**Tranche letters present in `docs/tranches/`** (the era-substrate of the prior arc): `AA AB AC AE AF AG AH AI AJ AK AL AM AN AO AP AQ AR AS AT AU AV AW AX AY-I AY-II-I AY-III AZ-I AZ-II AZ-III AZ-IV B0 B1 B2 B3 B4 B5 B6 B7 BA BB`.

**Canonical 6-era taxonomy** lands in `docs/tranches/meta-audit/06-commit-archaeology.md` (commit `59fff357`, 2026-04-22; described as "2787-event taxonomy across 6 eras"). The per-era deep-dives sit at `docs/tranches/meta-audit/archaeology/era-{II,III,IV,V,VI}-*.md`. Era I is implicit (pre-AU foundations).

| Era | Theme (per archaeology) | Anchor |
|---|---|---|
| II | Foundations | `archaeology/era-II-foundations.md` (106 lines) |
| III | Substrate | `archaeology/era-III-substrate.md` (115 lines) |
| IV | Tape-first | `archaeology/era-IV-tape-first.md` (171 lines) |
| **V** | **DTA / PSI / Activation Rut** | `archaeology/era-V-dta-psi-rut.md` (330 lines) |
| VI | Restart | `archaeology/era-VI-restart.md` (243 lines) |

**Era V is where PSI lived and died.** It spans 2026-04-15 → 2026-04-19 (5 calendar days) across seven tranches AV, AW-I, AW-II, AW-III, AW-IV, AW-V, AX with ~600 tranche-tagged commits per the archaeology ledger: AV 53 + AW-I ~45 + AW-II 40 + AW-III 93 + AW-IV 92 + AW-V 80 + AX 169 = ~572 (`era-V-dta-psi-rut.md:12-15`).

---

## (b) PSI / DTA / Structural-Indexing Commit List

**PSI/psi-tagged commits**: 136. **OpenFrame**: 47. **columnar/Columns SoA**: 11.

### PSI birth — AV.4.1 (2026-04-15)

| Hash | Date | Subject |
|---|---|---|
| `13968fdf` | 2026-04-15 20:10 | `feat(bbnf-tape): PayloadJob + PayloadKind + PayloadStream types (AV.4.1)` — **the PSI substrate landing**. `crates/bbnf-tape/src/psi.rs` 542 lines at birth. |
| `5352b424` | 2026-04-15 20:14 | `feat(emitter): emit PSI stream construction helpers (AV.4.1)` — emitter writes `psi_with_capacity` + `fill_payloads` into every grammar's `generated.rs`. |
| `1949c0e3` | 2026-04-15 20:15 | `chore(codegen): regen after PSI helper emission (AV.4.1)`. |
| `7945d42c` | 2026-04-15 | `feat(bbnf-tape): rayon stage-B payload fill (AV.4.2)` — adds `par_chunks(CHUNK_RECS=4)` parallel path. |
| `23b07987` | 2026-04-15 20:17 | `test(psi): PayloadJob round-trip + rayon threshold tests (AV.4.1 / AV.4.2)` — 12 new tape_basic tests. |
| `58e6389a` | 2026-04-15 | `feat(bbnf-tape): finaliser stage-C prefix scan over frame_depth (AV.4.4)`. |
| `a29948d0` | 2026-04-15 | `docs(AV): V3+V4 CLOSED — DTA + PSI/finaliser/simdjson landed`. |

### PSI growth (AW-I → AW-V), 2026-04-16 → 2026-04-19

- `11f22f1f` `feat(bbnf-tape): dta_run walker with FrameStack + inline frame_depth emission (AW.1.1)` — the DTA walker; PSI's stage-A consumer.
- `08658746` `feat(emitter): emit parse_dta entry point dispatching to dta_run (AW.1.2)`.
- `31de7e3c` `fix(bbnf-tape): resolve forward Refs + release counter slots + refresh iter psi (AW-I.W4δ)`.
- `b7c42c14` `feat(bbnf-tape/dta): add typed payloads to DtaState::Literal/Regex (AW-III.W1.1)`.
- `b3ef8301` `feat(bbnf-tape/driver): activate Literal/Regex payload + KvPair promote (AW-III.W1.3-6)`.
- `df2eeea3` `feat(bbnf-tape/columns): push_compound_fused + push_leaf_fused (AW-III.W5.c.1)`.
- `8d46e4fe` `feat(bbnf-tape/driver): dta_run_parallel + per-worker Columns join (AW-IV.W4.4)` — the parallel DTA over PSI.
- `5af92a8f` `feat(emitter/grammar): parse() routes to dta_run_parallel when list_rules non-empty + input large (AW-IV.W4.4)`.
- `85343341` `docs(bbnf-tape/psi): document inline-decodable-scalar elision boundary (AW-IV.W2.3.c)`.
- `1cf69a69` `feat(bbnf-tape/columns): push_compound_fused_v32 — single 32-byte AVX-256/NEON-Q vector store (AW-V.W1.3)`.

### PSI peak split (B5.W3b, 2026-04-26)

- `c4a53978` `refactor(tape): split psi.rs into directory module per concern (B5.W3b)` — **the 804-LOC peak**. Splits into `psi/{mod,job,stream,column_cells}.rs` (mod 121 + job 160 + stream 314 + column_cells 241 = 836 LOC, replacing 804 LOC).

### PSI death — AX.W0b → AZ-II (2026-04-20 → 2026-04-29)

The DTA interpreter (PSI's consumer) deletes first; PSI itself dies with the tape crate:

- `bc550d2c` (AX.W0b.A) `feat(emitter): retire walker path + gate predicates, regen` — 2026-04-20.
- `a206b962` (AX.W0b.A) `refactor(emitter): delete dta_walker/ + emitter/dta.rs`.
- `b7aa41c0` (AX.W0b.A) `refactor(tape,ir): surgical carves + 7 dead profile slots + Lever 4`.
- `e839378c` `test(AX.W0b.D): delete 8 DTA-coupled test suites`.
- `0adabb23` `test(tape): delete DTA-walker regression tests + carve dead profile fields (AX.W0b.cleanup)`.
- `a143725a` (AZ-II) `fix(az-ii): delete tape crate` — 2026-04-29. **Deletes `crates/tape/src/psi/{column_cells,job,mod,stream}.rs`** entirely.
- `6a6ca1fd` `fix(runtime/tape): delete tape crate` — same date, history-repair commit.

**Lifetime**: PSI lived **14 days** (2026-04-15 → 2026-04-29).

---

## (c) Archive Directory Inventory

### `/Users/mkbabb/Programming/bbnf-lang/restart-archive-2026-05-04/` (the canonical archive)

Last-modified: 2026-05-04. Contents (per `restart-archive-2026-05-04/README.md`):

- `INTERROGATION-2026-05-04.md` — 30,785 bytes
- `README.md` — 5,659 bytes
- `audit/{master-plan,passes,hardening,per-agent}/` — Pass-A/B/C syntheses + 18 per-agent reports + 7 hardening reports + MASTER-PLAN.md + AMENDMENT-01
- `corpora/{PHASE-4-SYNTHESIS,HARDENING-PLAN-SYNTHESIS,lanes/}` — phase-3 8-lane audit, phase-4 synthesis
- `legacy-source/INHERITANCE-INDEX.md` — 7,398 bytes (pointer to `docs/tranches/{BA,BB,BC,BD}/`)
- `locks/PHASE-4-DIRECTIVE.md`
- `prompts/{PASS-A,PASS-B,PASS-C,SYNTHESIZER,HARDENING,HARDENING-STAGE-2-EXTERNAL,README}.md`
- `tranches/{A..J}/` — 10 tranche directories (the original drafts now superseded by the current `restart/`)

### Other archive surfaces

- `/Users/mkbabb/Programming/bbnf-lang/docs/tranches/archive/{pre-restart-BA,pre-restart-BB,pre-restart-BC}/` — pre-restart BA/BB/BC tranche docs (created 2026-05-02 to 2026-05-03)
- `/Users/mkbabb/Programming/bbnf-lang/docs/audit/archives/2026-04-{23,24,25}-*/` — meta-audit + next-tranche-research + deep-audit archives
- `/Users/mkbabb/Programming/bbnf-lang/docs/benchmarks/archive/{AZ-I,AZ-II,AZ-III}/` — bench archive surfaces
- `/Users/mkbabb/Programming/bbnf-lang/docs/tranches/AZ-II/archive/json-prototype/` — JSON prototype archive
- `/Users/mkbabb/Programming/bbnf-lang/docs/tranches/{AU,AV,AX}/` — three Era-V tranche directories preserved verbatim under their canonical letter (with `research/`, `FINAL.md`, `PROGRESS.md`, `AV.md`, etc.)
- `/Users/mkbabb/Programming/bbnf-lang/docs/tranches/{B0..B7,BA,BB}/` — B-prefix tranche directories from Era VI

The user's `restart/` (current) is the **third** workspace surface, distinct from `restart-archive-2026-05-04/` and `docs/tranches/`.

---

## (d) The `pre-restart-2026-05-04` tag/anchor

**Not a git tag.** `git tag --list` shows only `v1.0.5`. The "tag" is a **directory and a date anchor**:

- Directory: `/Users/mkbabb/Programming/bbnf-lang/restart-archive-2026-05-04/` (mtime 2026-05-04 13:04).
- Cited as a logical tag in `restart/locks/LOCKS.md:56` Lock 12: *"The archive ceremony cites `pre-restart-2026-05-04` as the source-of-truth tag; legacy `BA-` / `BB-` / `BC-` / `BD-` slot drift retires under the canonical `A-` / `B-` / `C-` / `D-` tranche letters."*

The ceremony is **prescribed in Lock 12** as A.W0's precondition; the archive directory exists but no git tag matching `pre-restart*` has been cut.

---

## (e) Era-V Failure-Mode Taxonomy

Per `era-V-dta-psi-rut.md` and `restart/locks/LOCKS.md:34` (Lock 1):

### Named failures Lock 1 enumerates (4 in one sentence)

1. **Orthogonal codepaths** — the `Vec<OpenFrame>::clone` parallel substrate that produced the **86.07% samply pathology** (cited in `restart/MIGRATION.md:347`).
2. **Type ambivalence** — tape AND OpenFrame AND direct-to-struct competing for the same role.
3. **Substrate-first / consumer-later** — the "Era V failure mode": ship compile-time emission of tables/constants/shape-dicts; never fully activate the runtime consumer that reads them. Stated in `era-V-dta-psi-rut.md:7-10` and `era-V-dta-psi-rut.md:80-82` as: *"AV's close honest statement: 'The substrate landed; the activation sits one cherry-pick behind, in AW's opening wave.' This is the earliest explicit recognition of the substrate-first-consumer-later anti-pattern — but it is stated as a scope cut, not a warning."*
4. **Columnar SoA** — designed in AV.04 archaeology (`docs/tranches/AV/research/04-columnar-soa.md`, 14.5 B/record kind-partitioned SoA spec) but never activated end-to-end.

### Additional failure modes the archaeology surfaces

- **The "exactly once" demonstration** — `AW-V demonstrated the thesis and lost it within its own tranche. The 'exactly once' at W3 is the peak of the Era V arc` (`era-V-dta-psi-rut.md:187-188`). Shape emitter worked on JSON at W3 close (`c1e86ab3`), regressed by W6.
- **Bench collapse** — at Era V close, every entry below AU-baseline: JSON twitter 486 MB/s (24.7% of AU); CSS / Sheets / BBNF 3-7% of AU baseline (`era-V-dta-psi-rut.md:5-6`).
- **Carry-blindness** — Lock D5 in `14-LOCKS.md:237`: *"Treating every 'deferred to BB' as legitimate without auditing whether BB has the gate. Era V's failure mode replicated."*

### Lock 1's defining quote (verbatim, `restart/locks/LOCKS.md:34`)

> The 2,000-commit prior failure was implementation, not concept: orthogonal codepaths (the Vec<OpenFrame>::clone parallel substrate that produced the 86.07% samply pathology); type ambivalence (tape and OpenFrame and direct-to-struct competing for the same role); substrate-first/consumer-later (Era V failure mode); columnar SoA designed in AV.04 archaeology but never activated.

### AX's six architectural propositions (the "reckoning", `era-V-dta-psi-rut.md:194-204`)

1. The regression must be repaired before the interpreter deletes.
2. The interpreter is architectural debt — ~78,500 LOC reclaim target.
3. The tape's access API shapes the ceiling more than the tape's storage layout does.
4. Novel levers compound only when they share a substrate AND a demonstrable floor.
5. Parallelism is an amortisation multiplier over single-thread exceed, not a single-thread lever.
6. Parity IS the generality claim. No hand-tuned per-grammar prototypes.

---

## (f) Initial Verdict — Does "PSI" map to OpenFrame clone, Columnar SoA, or something else?

**Neither. PSI is its own distinct architectural artefact** — and Lock 1 names it indirectly. Map:

| Lock 1 failure-mode name | What it actually was | Source |
|---|---|---|
| **`Vec<OpenFrame>::clone` parallel substrate / 86.07% samply pathology** | The Era III/IV `OpenFrame` rollback stack used by CSS L4 + alt backtracking. Cloning the `Vec<OpenFrame>` on every speculative branch ran 86.07% inclusive in samply. Distinct from PSI. | `restart/MIGRATION.md:344-352`; `restart/README.md:387` |
| **Columnar SoA** | Era IV/V `crates/bbnf-tape/src/columns.rs`. Six SoA columns + sib_skip column. Landed `f8091cd0` `feat(bbnf-tape): Columns SoA substrate + sibling-skip traversal (AV.2.1-2.3)` 2026-04-15. The kind-partitioned variant in AV.04 archaeology was designed but never activated. | `f8091cd0`; `docs/tranches/AV/research/04-columnar-soa.md` |
| **PSI (Parallel/Pre-computed Structural Index)** | A **two-stage tape emission** with PayloadStream (a `Vec<PayloadJob>` queue) bridging DTA stage-A skeleton mining and stage-B parallel payload fill via `rayon::par_chunks`. Distinct: PSI is the *queue between DTA stage-A and stage-B*, not the storage layout. | `13968fdf`; `docs/tranches/AV/research/06-psi-dta-parallelism.md` |

**The PSI architecture (verbatim, AV.4.1 commit `13968fdf`):**

> Lays the PSI (Payload-Side Information) substrate that bridges the DTA's stage-A skeleton mining and stage-B's parallel payload fill.
>
> - `PayloadKind` — 7-variant enum (F64, U8, Bool, HexU32, I64, String, AggregateLarge).
> - `PayloadJob` — `#[repr(C)]` 16-byte record (`rec_idx`, `input_lo`, `input_hi`, `kind`, `column_idx`, `_pad: [u8; 2]`). 4 jobs per 64 B cache line.
> - `PayloadStream` — `Vec<PayloadJob>` with `with_capacity_for(profile, input_len)` driven by `GrammarProfile::leaves_per_input_byte`. Offers `should_parallelise()` and `fill_columns()` (rayon `par_chunks(CHUNK_RECS=4)`).

**PSI is the DTA/Parallel substrate's payload-fill queue.** It was the embodiment of *all four* Lock-1-named failure modes simultaneously:

1. **Orthogonal codepath** — PSI was a *separate parallel substrate alongside* the tape, not a unification with it.
2. **Type ambivalence** — PSI's `PayloadJob` competed with `TapeRec.payload_idx` and Columnar SoA's `pay_narrow/pay_wide/pay_agg` for the canonical payload identity. Three substrates, one role.
3. **Substrate-first/consumer-later** — PSI shipped at AV.4.1 (2026-04-15); the DTA walker consumer landed at AW.1.1 (`11f22f1f`) the next day; the parallel consumer at AW-IV.W4.4; activation in `parse()` for JSON-only at AW-V.W3; regressed by W6. Substrate landed; full consumer never landed.
4. **Columnar SoA coupling** — PSI's `fill_columns()` wrote into the `Columns` SoA substrate; the two were entwined and died together (`a143725a` deletes both).

**Final verdict**: PSI is **(4) something else** — specifically, it is the *runtime bridge that exemplified all four Lock-1 failure modes simultaneously*. It was the apotheosis of the substrate-first-consumer-later anti-pattern. The user's framing — "nearly 1000 commits, remarkably failed" — corresponds to the ~572 Era V tranche-tagged commits + AW-carry + planning across 14 days (PSI birth 2026-04-15 → tape-crate deletion 2026-04-29). The "remarkable failure" is the AW-V W3 demonstration that beat sonic-rs (0.89-0.94× on every entry per `c2f215a6` *"AW-V: W2 CLOSE — prototype beats sonic-rs on every entry 0.89–0.94×"*) and then lost the win within the same tranche, settling at 24.7% of the prior AU baseline by AX open.

---

## Key path:line citations

- `restart/locks/LOCKS.md:34` — Lock 1 verbatim
- `docs/tranches/meta-audit/archaeology/era-V-dta-psi-rut.md` (330 lines) — canonical retrospective
- `docs/tranches/AV/AV.md` (1,323 lines) — AV plan
- `docs/tranches/AV/research/06-psi-dta-parallelism.md` (129 lines) — PSI architectural sketch
- `docs/tranches/AV/research/04-columnar-soa.md` — kind-partitioned SoA (designed, never activated)
- `restart/README.md:291,318,387,450` — README references to OpenFrame / parallel substrate / Vec<OpenFrame> / 86.07%
- `restart/MIGRATION.md:344-352,752,760` — OpenFrame retirement verification
- `restart/MASTER-PLAN.md:26,115,288,290,297,307,696,773` — OpenFrame absence as plan gate
- `restart-archive-2026-05-04/` — the canonical archive directory (mtime 2026-05-04 13:04)
- `13968fdf` (PSI birth, 2026-04-15) → `c4a53978` (peak split, 2026-04-26, 804 LOC → 836) → `a143725a` / `6a6ca1fd` (tape-crate deletion, 2026-04-29)
