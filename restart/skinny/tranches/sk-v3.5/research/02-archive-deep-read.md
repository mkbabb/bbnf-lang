# PSI / DTA Archaeology Report — 2026-05-12

Filesystem archaeology for the failed "PSI + DTA + columnar substrate"
arc that consumed ~572 tranche-tagged commits (Era V) before its
interpreter was deleted at AX.W0b. Every claim below cites a verbatim
path:line; this report is read-only inventory + verdict.

---

## (a) Archive directory inventory

Top-level filesystem candidates surveyed under
`/Users/mkbabb/Programming/bbnf-lang/`:

| Path | Files | LOC | Last modified |
|------|------:|----:|--------------:|
| `restart-archive-2026-05-04/` | 100+ subdir tree | — | 2026-05-04 |
| `docs/tranches/archive/` | 50 | 6,151 | 2026-05-03 |
| `docs/audit/archives/` | 37 | 8,873 | 2026-05-01 |
| `docs/benchmarks/archive/` | 106 | 59,585 | 2026-05-01 |
| `docs/tranches/AZ-II/archive/` | 9 | 2,522 | 2026-04-29 |

`restart-archive-2026-05-04/` is the canonical pre-restart archive ceremony — its
top-level layout
(`/Users/mkbabb/Programming/bbnf-lang/restart-archive-2026-05-04/`) is
`README.md`, `INTERROGATION-2026-05-04.md` (35 questions, 378 lines),
`audit/{master-plan,passes,hardening,per-agent}/`,
`prompts/{PASS-A.md,PASS-B.md,PASS-C.md,HARDENING.md,SYNTHESIZER.md}`,
`tranches/{A..J}/`, `corpora/`, `legacy-source/`, `locks/`.

No directory named `BA/`, `BB/`, `BC/`, `BD/` at filesystem level; the
referenced "BA/BB/BC/BD" eras live as
`/Users/mkbabb/Programming/bbnf-lang/docs/tranches/BA/`, `BB/`, `BC/`,
`BD/` (each `<letter>.md` + `audit/` + `waves/`). They are *not*
archived — they are post-restart tranches from the prior restart attempt.

No `pre-restart-2026-05-04` git tag was produced (the
`git tag | grep restart` query returned empty). The ceremony was
directory-only: `restart-archive-2026-05-04/` is the artefact.

---

## (b) PSI design — verbatim quotes

PSI = **Precomputed Structural Index** (the AV plan's term) /
**Payload Stream** (the runtime crate's term) /
**Parallel Structural Index** (the era-V archaeology's term). All three
spellings appear in the corpus and refer to the same construct.

### Original architectural proposal

From `docs/tranches/AV/research/06-psi-dta-parallelism.md:32-46`
(verbatim):

> ## Novel proposal: two-stage tape emission with a precomputed structural index (PSI)
>
> ### The idea
>
> Split every generated rule into two generated halves, with a
> grammar-level trigger for parallel deployment at document-level list rules.
>
> **Stage A (structural mine).** A single-threaded linear pass over the
> input using a grammar-derived *dispatch tape automaton* (DTA) — not a
> parser. The DTA is a DFA-plus-counter that tracks Alt branch selection,
> Repeat frame counts, and Seq frame advancement *without* executing rule
> bodies. It reads bytes, recognises opening delimiters (`{`, `[`, `"`,
> `@media`, CSS-selector start, Sheets operator), and emits the tape skeleton:
>
> - A `TapeRec` for every compound and every scalar leaf — correct `kind`,
>   `variant_idx`, `meta_idx` — but with `span_hi = 0`, `child_off = 0`,
>   `payload_idx = 0`.
> - A parallel **PSI** (Vec<PayloadJob>) — one entry per scalar leaf with
>   a non-empty payload kind. Each PayloadJob is (tape_record_index,
>   input_span_lo, input_span_hi, payload_kind).
>
> The DTA state is 4 usize per active Seq/Repeat/Alt frame (call stack
> depth, which for JSON is ~50 and for CSS is ~8). No generated function
> calls; one big `match` on current byte + DTA state.
>
> **Stage B (payload fill).** A `rayon::par_iter_mut` over PSI chunks.
> Each worker owns a chunk range, reads `src[span_lo..span_hi]`, runs the
> terminal scanner (`scan_number_strict_f64`, `decode_json_string_to_arena`,
> `parse_hex_color`), and writes the result into `tape.payloads` at a
> pre-reserved offset. The output payload index is then written back into
> `tape.records[rec_idx].payload_idx`.
>
> **Stage C (span close).** A prefix-scan resolves `span_hi` and
> `child_off` for compounds. Because the DTA emitted records in pre-order
> with Seq/Repeat counters, we know exactly which record is the parent of
> which — it's a tree-from-depth-tag reconstruction identical to
> Prüfer-style.

### Runtime shape (concrete `PayloadJob` struct, the API the runtime shipped)

`docs/tranches/AV/research/06-psi-dta-parallelism.md:57-65` (verbatim):

```rust
#[repr(C)]
pub struct PayloadJob {
    rec_idx: u32,       // which TapeRec to patch
    input_lo: u32,
    input_hi: u32,
    kind: PayloadKind,  // 1 byte — f64 / u8 / bool / hex_u32 / string_decode / span_only
    _pad: [u8; 3],
}
```

The shipped module was `crates/bbnf-tape/src/psi.rs` (805 lines) with
public exports `PayloadStream`, `PayloadJob`, `PayloadKind` and two
runtime entry points: `psi.push()` (line 378) and `psi.fill_columns()`
(line 448) — per the audit at
`docs/tranches/AW/audit/psi-and-dead-substrate.md:9-10`.

### Runtime activation status at AW-V close (2026-04-17)

From `docs/tranches/AW/audit/psi-and-dead-substrate.md:27-34`:

| Grammar | `parse()` path | PSI reached? |
|---------|---------------|--------------|
| JSON | `parse_JsonParser_value(...)` (shape dispatch) | **No** |
| CSS L4 | `psi_with_capacity` → `dta_run_CssL4Parser` → `psi.fill_columns` | **Yes** |
| Sheets | `psi_with_capacity` → `dta_run_GoogleSheetsParser` → `psi.fill_columns` | **Yes** |
| BBNF | `psi_with_capacity` → `dta_run_BbnfBootstrap` → `psi.fill_columns` | **Yes** |

PSI was alive on the non-JSON hot path and dead on the JSON hot path. It
co-deleted with the walker at AX.W0b.

---

## (c) DTA design — verbatim quotes

DTA = **Dispatch Tape Automaton** (the AV plan's coinage at AV.md:621)
/ **Dispatch Table Automaton** (the era-V archaeology's gloss). Same
object.

### Core design

From `docs/tranches/AV/AV.md:621-648` (verbatim):

> ### Phase 3 — Dispatch Tape Automaton (DTA)
>
> The DTA replaces the recursive-descent-per-rule codegen with a
> grammar-derived DFA + counter that mines the full tape skeleton in one
> linear byte pass. Each stage-A record lands with correct `kind_meta` /
> `variant_idx` / `meta_idx` / `span_lo` and empty `span_hi` / `sib_skip`
> / payload column positions.
>
> The DTA has three layers:
>
> 1. **Byte-class dispatch.** The AU.2.7 structural bitmap produces a
>    64-bit-per-stripe mask of the grammar's structural alphabet. The DTA
>    walker consumes `trailing_zeros(mask) → offset` and selects an Alt
>    branch via the grammar's dispatch LUT keyed on `src[offset]` and
>    (optionally) `src[offset + 1]` for digraphs.
> 2. **Frame counter stack.** Seq frames hold a linear advance counter;
>    Alt frames hold the selected branch index; Repeat frames hold a count
>    + body-DFA pointer. The stack is a fixed- size `[Frame; 64]` with the
>    depth tracked in a single `u8`. For grammars with nesting depth > 64
>    (not observed in the target corpus), the stack spills to a
>    heap-allocated overflow region — one allocation, amortised zero on
>    realistic inputs.
> 3. **Counter-DFA extensions.** Pure DFA cannot represent nested
>    optional-with-empty-body (BBNF `__mapped_factor`'s optional `( "->"
>    __value_expr __type_annotation? )?`) without state explosion.
>    Counter-DFA handles it with one extra counter per nested optional,
>    keyed by grammar annotation (optional- counter marked on the IR
>    during lift).

State counts per grammar from `docs/tranches/AV/AV.md:654-656`:
"for CSS L4 ~1200 states, BBNF ~400, JSON ~25, Sheets ~80 (after
precedence collapse)".

### Walker contract (the production interpreter)

From `docs/tranches/AW/research/01-dta-driver-design.md:15-73`,
`dta_run` is a single dispatch loop over `DtaTable.states[...]` — Seq
arm reserves a parent row + pushes a Frame; ByteDispatch arm indexes a
`[DtaStateId; 256]` LUT; Literal arm matches a string + emits a leaf;
Regex arm scans + emits a `PayloadJob` to PSI. The frame stack is
declared at `docs/tranches/AW/research/01-dta-driver-design.md:96-109`
as `[Frame; 64]` inline plus heap overflow plus a
`SmallVec<[u32; 16]>` of counter registers.

### The shipped consumer paths

`crates/bbnf-tape/src/driver.rs` was ~3,323 lines at AX.W0 — per
`docs/tranches/AW/audit/dead-code-manifest.md` Tier 1 row. The
emitter-side counterpart was `crates/core/src/backend/rust/emitter/dta_walker/`
at 4,360 LOC across 5 files (`mod.rs` 479 + `decoders.rs` 819 +
`helpers.rs` 504 + `hot_cold.rs` 211 + `lower_state.rs` 2,347).

---

## (d) Era V failure-mode anatomy

Single canonical document:
`/Users/mkbabb/Programming/bbnf-lang/docs/tranches/meta-audit/archaeology/era-V-dta-psi-rut.md`
(331 lines).

Key claims (verbatim):

`era-V-dta-psi-rut.md:1-15`:

> # Era V — The DTA/PSI/Activation Rut (2026-04-15 → 2026-04-19)
>
> Era V is the hard era. In five calendar days, seven tranche surfaces
> (AV, AW-I, AW-II, AW-III, AW-IV, AW-V, AX) ship ~600 tranche-tagged
> commits. At Era V's close, every bench entry is *below* the AU-baseline
> — JSON twitter 486 MB/s (24.7% of AU), CSS / Sheets / BBNF 3–7% of AU —
> despite a 400-commit substrate build. Era V's signature failure mode
> is **substrate-first-consumer-later**: every tranche ships the
> compile-time emission of constants, tables, and shape dictionaries; no
> tranche fully activates the runtime consumer that reads them.
>
> Commit ledger: AV 53, AW-I ~45, AW-II 40, AW-III 93, AW-IV 92, AW-V 80,
> AX 169. Total: ~572 tranche-tagged commits.

`era-V-dta-psi-rut.md:30-43` — what V planned to ship:

> 1. **DTA (Dispatch Table Automaton)** — a grammar-derived table-driven
>    parser avoiding the recursive `fn __<rule>` descent.
> 2. **PSI (Parallel Structural Index)** — a pre-computed index allowing
>    document-level parallel parse.
> 3. **Columnar tape** (Era IV's columns made first-class).
> 4. **ShapeRef** — compile-time shape dictionary dispatched at the cursor.
> 5. **PHF + SIMD keyword classifiers** — compile-time perfect-hash
>    keyword tables.
> 6. **Bloom + GADT runtime dedup** — shared-substring dedup.
> 7. **Shape emitter** — the unifying substrate. Auto-derives the
>    sonic-rs-class inner loop from any BBNF grammar.
>
> Each of the seven is shipped. None reach break-even parse throughput
> with the AU baseline before Era V ends.

`era-V-dta-psi-rut.md:163-167` — AW-IV's hard gate vs reality:

> Outcome: substrate landed. No entry met the gate. 92 commits of
> compile-time emission with runtime consumers partially wired. The
> hard gate was "every entry exceeds post-AU" and the actual close was
> "0 entries exceed post-AU, 17/17 regressed."

`era-V-dta-psi-rut.md:182-187` — the W3 demonstration peak:

> > AW-V's thesis — "auto-derive the sonic-rs-class inner loop from any
> > BBNF grammar" — was demonstrated exactly once, on JSON, at W3 close
> > (commit `c1e86ab3`), and lost by W6.
>
> **AW-V demonstrated the thesis and lost it within its own tranche.**

`era-V-dta-psi-rut.md:194-209` — AX's reckoning (the six propositions):

> 21 invariants declared. Six architectural propositions:
>
> 1. The regression must be repaired before the interpreter deletes.
> 2. The interpreter is architectural debt — ~78,500 LOC reclaim target.
> 3. The tape's access API shapes the ceiling more than the tape's
>    storage layout does.
> 4. Novel levers compound only when they share a substrate AND a
>    demonstrable floor.
> 5. Parallelism is an amortisation multiplier over single-thread
>    exceed, not a single-thread lever.
> 6. Parity IS the generality claim. No hand-tuned per-grammar
>    prototypes.

`era-V-dta-psi-rut.md:210-227` — the interpreter-deletion commit cluster
at AX.W0b:

> **AX.W0b is the most important code-removal commit set of Era V.**
>
> - `bc550d2c` `feat(emitter): retire walker path + gate predicates, regen (AX.W0b.A)`.
> - `a206b962` `refactor(emitter): delete dta_walker/ + emitter/dta.rs (AX.W0b.A)`.
> - `b7aa41c0` `refactor(tape,ir): surgical carves + 7 dead profile slots + Lever 4 (AX.W0b.A)`.
> - `e4121fdc` `chore(simd-scan): purge dead emit/ directory (AX.W0b.B)`.
> - `b464a99c`, `1327491e`, `6ad76124` — rename `bbnf-tape`, `bbnf-simd-scan`, `bbnf-json-prototype` crates, drop `bbnf-` prefix.
> - `e839378c` delete 8 DTA-coupled test suites.
> - `0d730c8f` retire `tape_parity_*` walker oracles per invariant 20.
> - `6854f18b` delete W0a diagnostic probe harnesses.
> - `0adabb23` delete DTA-walker regression tests + carve dead profile fields — AX.W0b cleanup close.

`era-V-dta-psi-rut.md:310-315` — the canonical lesson:

> The single hardest lesson:
>
> > "Novel levers compound only when they share a substrate AND a
> > demonstrable floor. V's substrate-first-consumer-later anti-pattern
> > must not recur." — AX.md proposition 4.

---

## (e) AV.04 columnar SoA archaeology summary

File: `/Users/mkbabb/Programming/bbnf-lang/docs/tranches/AV/research/04-columnar-soa.md`
(178 lines). Verbatim opener (lines 9-13):

> ## 1. Thesis (one sentence)
>
> Replace the row-oriented `Vec<TapeRec>` tape with a **kind-partitioned
> columnar store** in which the record kind *is itself* the column selector
> and the record index is the *column index* — so a walker that touches
> only structure pays only structure cache lines, SIMD traversals over
> numeric leaves become packed-slice kernels, and the per-record payload
> pointer (`payload_idx`, `child_off-as-offset`) disappears entirely.

Schema (verbatim, lines 17-31):

> Shared backbone (every grammar, every record):
>
> | Column | Type | Width/rec | Role |
> |---|---|---|---|
> | `kinds` | `Vec<u8>` | 1 B | `TapeKind` discriminant; kind also selects payload column |
> | `span_lo` | `Vec<u32>` | 4 B | Source start offset |
> | `span_hi` | `Vec<u32>` | 4 B | Source end offset (= span_lo for leaves with implicit width) |
> | `sib_skip` | `Vec<u32>` | 4 B | Distance to the next sibling (replaces `child_off` — see §3) |
> | `flags` | `Vec<u8>` | 1 B | `variant_idx` (6 bits) + `has_children` (1) + meta_bit (1) |
> | `meta_lo4` | Packed nibble array `Vec<u8>` | 0.5 B | Low 4 bits of `meta_idx`; upper bit in `flags[7]` |

The columnar SoA was **designed but never activated**. Per
`docs/tranches/AW/audit/psi-and-dead-substrate.md:58-62`,
`GrammarProfile.active_columns` emitted `&[]` for all four grammars; the
emitter helper existed but the IR never populated it. Same fate for
`branch_priors` and `reorder_unroll_visitors`. All three slots ship at
`generated.rs:264-270` populated but never read; AX.W0b carved them.

Lock 1 of the current restart names the columnar SoA explicitly as
**buried but not denied**:
`/Users/mkbabb/Programming/bbnf-lang/restart/locks/14-LOCKS.md:34`:

> The 2,000-commit prior failure was implementation, not concept:
> orthogonal codepaths (the Vec<OpenFrame>::clone parallel substrate
> that produced the 86.07% samply pathology); type ambivalence (tape and
> OpenFrame and direct-to-struct competing for the same role);
> substrate-first/consumer-later (Era V failure mode); columnar SoA
> designed in AV.04 archaeology but never activated. [...] Columnar SoA
> stays buried.

---

## (f) LESSONS-LEARNED.md — PSI/DTA/state-machine quotes

File: `/Users/mkbabb/Programming/bbnf-lang/docs/precepts/instructions/LESSONS-LEARNED.md`
(311 lines). The document is the cross-repo precept ledger — entries
are repo-agnostic process incidents — but Era V's failure modes are
distilled into three load-bearing entries:

### LESSONS-LEARNED.md:17-26 — the canonical substrate-without-consumer entry

> ## 2026-04-29 - Substrate Without Consumer Is Not Progress
>
> - **Source**: bbnf-lang AZ-I/AZ-II plans; speedtest and glass-ui
>   activation gates.
> - **Failure**: reusable substrate landed before the consuming path proved it
>   was live.
> - **Rule**: every substrate change must land with a same-wave consumer or an
>   explicitly declared brittleness window and restoration wave.
> - **Check**: wave hard gate cites a runtime call site, test, benchmark, or
>   deletion proof.

### LESSONS-LEARNED.md:73-80 — producer/consumer wire contract

> ## 2026-04-29 - Contracts Need Producer And Consumer Gates
>
> - **Failure**: producer-only checks let message shape drift reach runtime.
> - **Rule**: wire contracts close on producer output and consumer acceptance.
> - **Check**: hard gates cite a build-time or runtime contract test for both
>   sides.

### LESSONS-LEARNED.md:274-292 — generator size-budget for emitter-heavy work

> ## 2026-04-30 - Generated Code Has A Size Budget
>
> - **Failure**: generator changes that mechanically grow output past a
>   per-tranche line-count budget signal an O(N) regression in the
>   generator (per-rule blow-up, redundant emission, exhaustive case
>   enumeration) rather than legitimate scope growth.
> - **Rule**: every tranche that touches a code generator declares an
>   expected output line-count window for each generated artefact.

(LESSONS-LEARNED.md carries no entry literally named "DTA" or "PSI";
the document is process-precept, not implementation-archaeology. The
implementation-archaeology lives in
`docs/tranches/meta-audit/archaeology/era-V-dta-psi-rut.md`. The
LESSONS file distils Era V's recurring symptom — substrate landed
without runtime consumer — into the 2026-04-29 entries above.)

---

## (g) Honest verdict — what was PSI?

**PSI was NOT a columnar SoA.** Columnar SoA (kind-partitioned column
arrays as the entire substrate) was a *separate* Era V proposal
documented at `docs/tranches/AV/research/04-columnar-soa.md`. Columnar
SoA was designed in AV's Phase 7 plan, never reached the runtime, and
its `GrammarProfile.active_columns` slot was carved at AX.W0b. Lock 1
of the current restart names columnar SoA in the *forbidden* list
alongside `Vec<OpenFrame>::clone`.

**PSI was NOT an OpenFrame ladder.** OpenFrame
(`Vec<OpenFrame>::clone` at the 86.07% samply pathology) was a
*different* Era IV/V parallel substrate documented at Lock 1 of
`restart/locks/14-LOCKS.md:34`. OpenFrame and PSI co-existed; both are
named as separate failure mechanisms.

**PSI was a precomputed-payload-job stream, paired with the DTA
interpreter, and structurally analogous to simdjson's two-stage
parse — but with a critical asymmetry that doomed it.** Per
`docs/tranches/AV/research/06-psi-dta-parallelism.md:32`, the PSI is a
`Vec<PayloadJob>` (`{rec_idx, input_lo, input_hi, kind}`) emitted by a
single-threaded DTA walker over the input bytes, then drained by a
`rayon::par_iter_mut` payload-fill stage that ran terminal scanners
(Eisel-Lemire f64, JSON string decode, hex-color parse) on disjoint
input spans and patched back into the tape. It is a **payload-fill
post-pass scheduling abstraction**, not a substrate layout. The
shipped runtime (`crates/bbnf-tape/src/psi.rs`, 805 LOC) carried
`PayloadStream`, `PayloadJob`, `PayloadKind`; the
`#[cfg(feature = "rayon")]` gates at `psi.rs:119, 121, 510` are the
parallel-fill path. See
`docs/tranches/AW/audit/psi-and-dead-substrate.md:9-23`.

**PSI failed for two distinct reasons:**

1. **The DTA interpreter ceiling.** Per
   `docs/tranches/AW/research/aw3-r6-path-b-rip-dta.md:13-15`:
   "`dispatch_one` holds **20-35% self-time across every grammar and
   every input size** [...] No AW-IV lever touches the tagged-union
   match over 20+ `DtaState` variants; it is the canonical
   state-machine-interpreter ceiling." PSI was the
   payload-side-effect of the interpreter — PSI's parallel speedup
   could not overcome the interpreter's serial overhead on the bytes.

2. **The shape emitter (Path B) made PSI moot.** W2.3 inlined
   Eisel-Lemire directly at the F64 site; per
   `psi-and-dead-substrate.md:14` "the F64 path decodes inline (W2.3.a,
   `lower_state.rs:615`, `eisel_lemire_body`)". The W2.1 prototype
   (per `aw5-r5-depart-rip-dta.md:8-11`) showed that a fn-per-rule
   path over `bbnf-simd-scan` + `Columns` could beat sonic-rs without
   PSI at all — empty `nm` for `dispatch_one | try_branch | PayloadStream`
   in the bench binary. PSI's parallel-fill abstraction had no
   irreducible workload once the inline-decoder discipline matured.

**So the unified verdict:**

| Comparison | Verdict | Citation |
|---|---|---|
| PSI = OpenFrame? | **No.** Different artefacts. OpenFrame is the `Vec<OpenFrame>::clone` parallel-substrate pathology; PSI is the per-leaf payload-job stream. They co-existed in Era V; both are listed separately as Lock 1 forbidden surfaces. | `restart/locks/14-LOCKS.md:34` |
| PSI = columnar SoA? | **No.** Columnar SoA is `kind`-partitioned column arrays as the tape substrate (Phase 7); PSI is a payload-fill scheduling stream (Phase 4). Columnar SoA was designed (`AV/research/04-columnar-soa.md`) but never activated; PSI was activated and shipped, then deleted. Both are listed separately as Lock 1 forbidden / buried items. | `AV/research/04-columnar-soa.md`; `psi-and-dead-substrate.md:9` |
| PSI = an FSM-based parser? | **Adjacent but not equal.** PSI itself is a queue/buffer of `PayloadJob` records; the DTA *is* the FSM-based parser that fills the queue. They are paired but distinct: `psi.rs` (805 LOC) is the data structure + rayon drain; `driver.rs` + `dta.rs` (~3,873 LOC) is the interpreter. Together they form a "FSM scans → payload jobs → parallel scanner pool" pipeline structurally analogous to simdjson's stage-1 / stage-2 split but driven by a grammar-derived counter-DFA rather than a JSON-specific stage-1 SIMD kernel. | `06-psi-dta-parallelism.md:32-46`; `01-dta-driver-design.md:15-73` |

The user's framing "full Rust variant of DTA/Structural indexing, PSI
that remarkably failed" is accurate. PSI was real, was shipped, did
run on 3 of 4 grammars at AW-V close, and was deleted alongside the
DTA interpreter at AX.W0b (commit `a206b962` deletes the emitter side;
the runtime crate carve at `bc550d2c` deletes the rest). The failure
mode was substrate-first-consumer-later compounded by an
interpreter-dispatch ceiling that no PSI parallelism could amortise.
The current restart's Lock 1 names PSI's substrate-without-consumer
pattern as the Era V failure mode and forbids resurrection of parallel
substrates by the same architectural shape — but it does NOT forbid
inline payload decoding, which is precisely the shape-emitter
(Path B) escape that the W2.1 prototype proved viable
(`aw5-r5-depart-rip-dta.md`).

**Where the surviving artefacts live today**: the
`crates/bbnf-tape/src/psi.rs`, `crates/bbnf-tape/src/driver.rs`,
`crates/bbnf-tape/src/dta.rs`, and
`crates/core/src/backend/rust/emitter/dta_walker/` paths are all
deleted from the current working tree (confirmed: no `psi*`/`dta*`
files exist under `crates/` or `skinny/crates/` in the present
filesystem survey). The greenfield substrate at `restart/skinny/`
carries no PSI/DTA references whatsoever (verified by grep). The
archaeology survives only in `docs/tranches/AV/`, `AW/`, `AX/`,
`docs/tranches/meta-audit/archaeology/era-V-dta-psi-rut.md`, and the
git history.

---

## Key file paths (absolute)

- `/Users/mkbabb/Programming/bbnf-lang/docs/tranches/meta-audit/archaeology/era-V-dta-psi-rut.md` (331 lines) — primary archaeology
- `/Users/mkbabb/Programming/bbnf-lang/docs/tranches/AV/research/06-psi-dta-parallelism.md` (130 lines) — PSI/DTA design genesis
- `/Users/mkbabb/Programming/bbnf-lang/docs/tranches/AV/research/04-columnar-soa.md` (178 lines) — separate columnar SoA design
- `/Users/mkbabb/Programming/bbnf-lang/docs/tranches/AV/AV.md` (1,323 lines, esp. §621-740 = Phase 3 DTA, §744-880 = Phase 4 PSI)
- `/Users/mkbabb/Programming/bbnf-lang/docs/tranches/AW/research/01-dta-driver-design.md` — DTA runtime walker contract
- `/Users/mkbabb/Programming/bbnf-lang/docs/tranches/AW/research/aw3-r5-path-a-keep-dta.md` — viability defense
- `/Users/mkbabb/Programming/bbnf-lang/docs/tranches/AW/research/aw3-r6-path-b-rip-dta.md` — abandonment thesis
- `/Users/mkbabb/Programming/bbnf-lang/docs/tranches/AW/research/aw5-r5-depart-rip-dta.md` — evidence-backed abandonment
- `/Users/mkbabb/Programming/bbnf-lang/docs/tranches/AW/audit/psi-and-dead-substrate.md` — PSI runtime status + delete manifest
- `/Users/mkbabb/Programming/bbnf-lang/docs/tranches/AW/audit/dead-code-manifest.md` — AX.W0+ delete manifest with exact LOC
- `/Users/mkbabb/Programming/bbnf-lang/docs/tranches/AX/AX.md` (122 lines) — reckoning + 21 invariants + 6 propositions
- `/Users/mkbabb/Programming/bbnf-lang/docs/tranches/AX/FINAL.md` (491 lines) — interpreter deletion close
- `/Users/mkbabb/Programming/bbnf-lang/docs/precepts/instructions/LESSONS-LEARNED.md` (311 lines) — cross-repo precept ledger
- `/Users/mkbabb/Programming/bbnf-lang/restart-archive-2026-05-04/INTERROGATION-2026-05-04.md` (378 lines) — 35-question restart-of-restart synthesis
- `/Users/mkbabb/Programming/bbnf-lang/restart/locks/14-LOCKS.md:34` — Lock 1 (the current canonical forbidden-surface list)
