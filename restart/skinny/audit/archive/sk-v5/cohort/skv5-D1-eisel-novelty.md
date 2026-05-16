# SK-V5 D1 — Eisel-Lemire Scalar Fast-Float Novelty Audit

Scope: interrogate B2 + A3 "Eisel-Lemire scalar fast-float for serde_number_digest
is the single biggest unaddressed lever" finding for novelty. Targets skinny only;
main `crates/core` is out of skinny's perimeter but its prior-art shapes what we
copy.

## §1 Per-Claim Verdict Table

| # | Claim (B2 / A3) | Verdict | Evidence |
|---|---|---|---|
| 1 | "No aarch64 fast-float path exists" in skinny | **NEW** | `skinny/crates/parse-that-regex/Cargo.toml:9-11` has only `bbnf-simd` + `thiserror`; no `fast_float2`, no `parse_that`. `skinny/crates/parse-that-regex/src/lib.rs:149-220` defines only `match_json_number_from_first` (a *span scanner*, no materialization). `skinny/crates/parse-that-regex/src/integration/simd_scan_hook.rs` (19 LOC) is a `SimdScannerHook` trait wrapper — zero number bytes. `grep -rni 'eisel\|lemire\|fast_float\|compute_f64' skinny/crates/parse-that-regex/` returns zero hits. |
| 2 | "AVX-IFMA mantissa kernel is unimplemented" | **EXTANT-STUBBED (NEW-WORK)** | `skinny/crates/bbnf-simd/src/x86_64/avx_ifma/mantissa.rs:37` literally reads `unimplemented!("Wave 6: vpmadd52luq 4-lane Eisel-Lemire mantissa multiply");`. Scalar reference `mul52_low_scalar` (lines 22-27) is real (u128 fold). Stub landed in commit `9eef728c` ("Layer 1 primitive vocabulary skeleton + BYTE_CLASS_FROM_EQ_SET_64 reference primitive end-to-end") — present-tense Wave 6 deferral. |
| 3 | "Eisel-Lemire scalar … is the single biggest unaddressed lever" | **EXTANT-COPYABLE** | A complete, MIT-licensed, **bit-parity-tested** Eisel-Lemire scalar implementation lives in the sibling crate at `/Users/mkbabb/Programming/parse-that/rust/parse_that/src/parsers/eisel_lemire/` (`mod.rs` 177 LOC + `algorithm.rs` + `table.rs` — the power-of-five table). Exposed entry is `compute_f64(exponent, mantissa, negative) -> Option<f64>` (mod.rs:147). Includes Clinger fast-path (lines 99-129), full Eisel-Lemire slow path, and the ambiguous-rounding `None` discriminator. Tested at `parse-that/.../tests/number_fastpath_test.rs:209-345` against `fast_float2` reference, including `compute_f64` direct comparison loop (line 307). Lift is `cp -r` + add `parse_that = { path = "../../../parse-that/rust/parse_that" }` (or vendor the three files). |
| 4 | "Integer materialiser exists misplaced in bbnf-bench/direct_struct.rs:501 and must move" | **EXTANT-COPYABLE** | Confirmed: `skinny/crates/bbnf-bench/src/direct_struct.rs:501-528` defines `fn parse_integer_digest(raw: &str) -> Option<JsonDirectDigest>` — handles `i64::MIN`, `-0.0`, `u64` overflow via `checked_mul`/`checked_add`. The f64 fallback at line 530-545 (`serde_number_digest`) delegates to `serde_json::from_str::<serde_json::Number>` — the exact delegation B2 names. |
| 5 | "Four rows (numbers 33%, canada 41%, mesh 52%, marine_ik 73%) are number-bound" | **NOT INTERROGATED — accept** | Out of scope for this audit; B2's own profile attribution. |
| 6 | "A scalar Eisel-Lemire alone would close ≈half the numbers-row gap" | **PLAUSIBLE on prior art** | `docs/tranches/AV/PROGRESS.md:909`, `docs/tranches/AV/FINAL.md:162` record AV.3.5 landing of identical algorithm in parse-that with "Eisel-Lemire microbench: 2.1× compute_f64 speedup" — that is a substrate datum, not a corpus datum. Corpus impact must be measured. |
| 7 | "fast-float, or any float materializer beyond serde delegation" exists anywhere in skinny | **NEW** | grep `'eisel\|lemire\|fast_float\|compute_f64\|materialize_f64\|f64_from_parts'` across `skinny/`: every hit is in `bbnf-simd/CONCRETIZATION-REPORT.md` (citations), `bbnf-simd/tests/checkasm_parity.rs:489` (test scaffold comment), or `bbnf-simd/src/x86_64/avx_ifma/mantissa.rs` (the Wave 6 stub). Zero hits in `runtime/`, `codegen/`, `bbnf-bench/`, `parse-that-regex/`. |

## §2 Misplaced Extant Code Inventory

In skinny working tree:

| File:line | Identifier | Note |
|---|---|---|
| `skinny/crates/bbnf-bench/src/direct_struct.rs:501-528` | `parse_integer_digest` | Integer fast path. Untracked (M flag) in working tree per task brief. Handles `i64::MIN` edge case and u64 overflow. Move target: `skinny/crates/parse-that-regex/src/number/integer.rs`. |
| `skinny/crates/bbnf-bench/src/direct_struct.rs:530-545` | `serde_number_digest` | Current f64 path — pure serde delegation. Must be replaced, not moved. |
| `skinny/crates/parse-that-regex/src/lib.rs:149-220` | `match_json_number_from_first` | Span scanner. Already in the right crate; emits `JsonNumberMatch { start, end, is_integer }`. Eisel-Lemire materializer becomes its sibling: `materialize_f64(input: &[u8], JsonNumberMatch) -> Option<f64>`. |
| `skinny/crates/bbnf-simd/src/x86_64/avx_ifma/mantissa.rs:22-27` | `mul52_low_scalar` | Scalar reference (u128 mul, low 52 bits). Real, working. The 4-lane IFMA path is the `unimplemented!`. |

In sibling repo `parse-that`:

| File:line | Identifier | Note |
|---|---|---|
| `/Users/mkbabb/Programming/parse-that/rust/parse_that/src/parsers/eisel_lemire/mod.rs:147-177` | `compute_f64(i64, u64, bool) -> Option<f64>` | Drop-in. Entry signature matches what B2 specifies. |
| `…/parsers/eisel_lemire/mod.rs:99-129` | `try_fast_path_f64` (Clinger) | Inner fast path; intercepts twitter's 99.8% / canada's ~85% of literals (per mod.rs docstring line 93-97). |
| `…/parsers/eisel_lemire/algorithm.rs` | `compute_float` | Full 128-bit mul path. Direct copy of `fast_float2::binary` per the file's own header. |
| `…/parsers/eisel_lemire/table.rs` | `POWER_OF_FIVE_128` | The bulk constant table — large but mechanical. |
| `…/tests/number_fastpath_test.rs:286-345` | parity tests | Includes boundary cases (mantissa > 2^53, exponent outside [-22, 37], ambiguous-rounding band, `-0.0`/`+0.0`, subnormals). These tests come with the algorithm. |

## §3 Git History Scan

`git log --all --oneline --grep -iE 'eisel|lemire|fast.?float|mantissa|pow10|exp10|vpmadd52|ifma'` — relevant hits (subject lines only):

| Commit | Subject |
|---|---|
| `9eef728c` | feat(bbnf-simd): Layer 1 primitive vocabulary skeleton + BYTE_CLASS_FROM_EQ_SET_64 reference primitive end-to-end + checkasm differential gate — **first introduction of the `avx_ifma/mantissa.rs` Wave 6 stub** |
| `4ca520d2` | docs(AY.W4.2): canada spot bench + Eisel-Lemire direct-column delta — main-tree AY tranche, not skinny |
| `05617765` | AY.W4.2 regen — number-shape golden uses push_leaf_with_f64_direct — main-tree |
| `b199afea` | AY.W4.2 emitter: number-shape direct-to-column f64 path — **main-tree landing of the same algorithm via `parse_that::parsers::eisel_lemire::compute_f64`** |
| `7e1732d0` | AY.W4.2 tape: pay_f64 column + push_leaf_with_f64_direct — main-tree tape column |
| `0d94657e` | docs(AY): BEAT-sonic reframe … + Eisel-Lemire direct-column |
| `0daf6f01` | refactor(tape): merge pay_f64 into pay_wide (B5.W2) — main-tree retire of the dedicated column |
| `700d6170` | fix(tests/json-parity-fast-float-tolerance): admit 1-ULP divergence between fast-float2 and serde (AZ-IV.W1-zero) — **main-tree parity test acknowledging that fast-float2 and serde diverge by 1 ULP on some inputs; this is the precise hazard REDRESS §353-355 caught a year later in skinny** |
| `e1a795b7` | fix(parity/json): route simd-json oracle through cast_f64 in json_value_parity — main-tree |

Filtered to skinny-perimeter files:
- `git log --all --oneline -- 'skinny/crates/parse-that-regex/*'` → 3 commits: `2d931312` (initial), `c7d2bf93` (bbnf-simd carve-out), `6b3e9bb4` (workspace seed). **None mention numbers, floats, Eisel-Lemire, or fast-float.**
- `git log --all --oneline -- 'skinny/crates/bbnf-simd/src/x86_64/avx_ifma/*'` → `c7d2bf93` only (the stub).

No skinny-side commit has ever attempted, vendored, or stubbed Eisel-Lemire beyond the AVX-IFMA mantissa primitive scaffold in `9eef728c`.

## §4 SK-V4 / MASTER-PLAN Prior-Commitment Audit

| Doc:section | Quote | Commit-level? |
|---|---|---|
| `restart/skinny/audit/IMPLEMENTATION-PACKET-SK-V4-ASMJSON-BEAT.md:144` | "Direct `raw.parse::<f64>()` was tried and rejected on parity." | **Diagnosis only**; not a commitment to Eisel-Lemire. |
| `IMPLEMENTATION-PACKET-SK-V4 §4 (Wave 2)` lines 148-153 | "1. Number primitive: exact raw-span classifier; integer fast path for `i64`/`u64`; **exact f64 materialization** matching serde/sonic on `-0`, subnormals, overflow, exponent boundaries, and `2^53`; no lossy shortcut." | **Gestures** at "exact f64 materialization" without naming the algorithm. The packet does not say "Eisel-Lemire"; that name appears nowhere in the SK-V4 packet (`grep -ni 'eisel\|lemire'` returns zero hits in that file). |
| `skinny/REDRESS.md:353-355` | "A direct `raw.parse::<f64>()` fast path was tested and rejected on parity: `canada` exposed float-shape mismatch against the serde oracle. The remaining direct work is therefore exact float materialization …" | **Diagnosis + scope**; same gestural shape, no algorithm name. |
| `restart/MASTER-PLAN.md:509` (H.W5) | "AVX-IFMA (`vpmadd52luq`/`vpmadd52huq`); … These land first as grammar-neutral primitives consumed by `OffsetTape`, `EventTape`, and `SinkOnly` hot loops" | **SIMD scope only**; commits the *vectorized* Eisel-Lemire mantissa mul. Says nothing about a scalar baseline. |
| `MASTER-PLAN.md:510` (H.W6) | "Early CSS SOTA gates." | **Not number work at all.** The "Wave 6" tag in the `unimplemented!` body refers to the vectorized variant's deferral, not a scalar fallback wave. |
| `restart/skinny/audit/SOTA-BEAT-DESIGN.md:183` | "AVX-IFMA `vpmadd52luq`/`vpmadd52huq` … Eisel-Lemire fast-float mantissa multiplication for `parse_number`; mantissa-mul stays in vector lanes; returns f64 directly without scalar callback" | **SIMD framing**; presumes the scalar exists "without scalar callback" — but it does not (in skinny). |
| `restart/skinny/audit/SOTA-BEAT-DESIGN.md:484` (BENCH Gate 4) | "Float-bit-exact parity must hold on canada/numbers/mesh/marine_ik per BENCH §7.9 Gate 4." | **Exit gate** — predicates the work, does not assign the wave. |
| `restart/skinny/BENCH.md:1298` (Gate 4) | "skinny uses `std::str::parse::<f64>` which is correct for normal IEEE-754 but loses ULP precision on subnormals; spec carries this gate to enforce the Eisel-Lemire wrap landing per `parse-that/float/eisel_lemire.rs` (wraps `fast-float2` crate per Lock 11 + Lock 16 algorithm-class citation)." | **Names the artefact** (`parse-that/float/eisel_lemire.rs`) and the wrap intent. **Closest existing commitment.** Still a *gate*, not a wave assignment. |

No wave in the current H-tranche schedule (`H.W0`…`H.W6`) carries "Eisel-Lemire scalar landing" as its primary deliverable. The closest is SK-V4 Wave 2 ("Exact Direct Materializers") which names the goal in prose but never picks the algorithm.

## §5 Final Novelty Verdict

**The scalar Eisel-Lemire materializer for skinny is `EXTANT-COPYABLE` work that has never been scoped as a discrete wave.**

Decomposition:

- **Algorithm**: not new. Bit-parity-tested implementation exists at
  `/Users/mkbabb/Programming/parse-that/rust/parse_that/src/parsers/eisel_lemire/`.
  Vendoring it (or path-dep'ing parse-that) is a code lift, not a research item.
  Main `crates/core` has been consuming it since AY.W4.2 (commits b199afea +
  7e1732d0 + 4ca520d2).

- **Scoping**: novel in the SK-V5 perimeter. No SK-V3, SK-V4, MASTER-PLAN, or
  SOTA-BEAT-DESIGN wave assigns "land Eisel-Lemire in skinny" as its
  deliverable. The diagnoses in REDRESS:353-355 and SK-V4 §4:144 stop at
  "raw.parse::<f64>() was tried and rejected"; they name the gap but never
  schedule the fill. BENCH.md:1298 names the wrap target but as a gate, not a
  wave.

- **AVX-IFMA mantissa**: explicitly deferred to "Wave 6" in the body of
  `mantissa.rs:37`. That deferral is for the **vectorized 4-lane** variant —
  the scalar reference `mul52_low_scalar` is already implemented (lines 22-27)
  and is not what closes the canada/mesh/marine_ik rows on its own; it is the
  one-mul kernel inside whichever wider implementation the caller picks. The
  Wave 6 tag is not a defence; it covers a different optimisation level.

- **Integer fast path**: `EXTANT-COPYABLE` from `direct_struct.rs:501-528`,
  needs to migrate to `parse-that-regex/src/number/integer.rs`. The
  `i64::MIN`-handling logic is already correct.

Net: SK-V5 B2's finding is accurate and the lever is real. The work is best
characterised as **NEW SCOPE / EXTANT IMPLEMENTATION** — a vendor-and-wire
tranche, not a research-and-author tranche.

## §6 If RE-OPENING: Prior Failure Mode

The only prior skinny-perimeter attempt that exists is the rejected
`raw.parse::<f64>()` shortcut (REDRESS:353-355, SK-V4:144). Failure mode:

- **Symptom**: `canada` parity row failed against serde oracle.
- **Root cause**: `std::str::parse::<f64>` lowers through Rust stdlib's
  `core::num::imp::dec2flt::lemire` path, which fast-float2 disagrees with on
  the 1-ULP rounding band. Confirmed by main-tree commit `700d6170`
  ("admit 1-ULP divergence between fast-float2 and serde, AZ-IV.W1-zero") —
  the **same divergence** was caught and admitted as test tolerance in main
  a year prior to its rediscovery in skinny.
- **Verdict at the time**: scope was deferred to "exact float materialization"
  without naming an algorithm.
- **What changed**: nothing in skinny. The lift target (sibling parse-that
  crate's `compute_f64`) has been available since AV.3.5 (commit `f6cc853` in
  parse-that per docs/tranches/AV/FINAL.md:128).

The "re-opening" frame is therefore inappropriate — there is no prior failed
Eisel-Lemire attempt to re-open. The historic event is a failed shortcut that
was correctly diagnosed and correctly scoped, but never staffed. SK-V5 staffs
it.

## §7 Probe: AVX-IFMA `unimplemented!` Body

Question: when was `mul52_low_ifma` stubbed and where is its scoping
documented?

- **Stubbed**: commit `9eef728c` (2026-05-13-ish based on log position;
  `HEAD~1` of current `master`).
- **Scope citation in the body**: `mantissa.rs:1-12` cites "Lock 16
  (SOTA-BEAT-DESIGN, Wave 1 Agent 3 finding)" and Eisel-Lemire SP&E 2021.
- **Wave routing**: the `unimplemented!` message literal says "Wave 6", but
  H.W6 in MASTER-PLAN.md:510 is **"Early CSS SOTA gates"** (`css/bootstrap ≤
  3.0ms`, `css/animate ≤ 1.6ms`). The wave tag inside the stub does **not**
  match any H-tranche wave's actual scope. This is a scoping bug in the stub
  body — the AVX-IFMA mantissa kernel belongs to H.W5 ("x86_64 AVX-512
  primitive path") per MASTER-PLAN.md:509, not H.W6.
- **No `unimplemented_for_wave_*` discipline** exists in skinny — the
  comment-string wave assignment is not load-bearing. The kernel admits via
  the standard same-wave-consumer rule once a consumer ships.

The probe surfaces a documentation drift: the body's "Wave 6" tag should read
"H.W5" per MASTER-PLAN.md:509, or the body should drop the wave label and rely
on the `unimplemented!()` panic itself as the gate. Not load-bearing for the
SK-V5 D1 question, but worth a paint-fix line in the next pass.

## Bottom-Line

B2's claim is materially accurate: scalar Eisel-Lemire is the largest
unaddressed lever for the four number-bound rows; the algorithm exists in
copyable form in the sibling `parse-that` crate; the integer fast path exists
misplaced; no skinny wave has scoped the landing. **NEW SCOPE; EXTANT
IMPLEMENTATION; NOT RE-OPENING.**
