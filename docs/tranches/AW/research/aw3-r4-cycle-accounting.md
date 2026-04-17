# AW-III R4 — DTA vs RD per-byte cycle accounting

Verdict: the user's thesis is theoretically correct but mis-names the
current DTA. A flattened tape automaton can beat RD — simdjson/sonic-rs
achieve ~1 cyc/byte. bbnf's current DTA sits at ~29 cyc/byte because
it is a *byte-driven tagged-union interpreter over a `&'static` state
table*, not a flattened automaton in the sonic sense. The per-byte
gap is decomposable; W5 closure with a codegen-specialised walker is
the minimum set to reach RD parity, stage-1 SIMD to exceed it.

## 1. Post-AU RD per-byte cycle cost (baseline)

M-series @ 3.5 GHz → cyc/byte = ns/byte × 3.5. Source: AU/FINAL.md
§Appendix.

| Entry        | bytes    | ns/iter    | MB/s | ns/byte | cyc/byte |
|--------------|----------|-----------:|-----:|--------:|---------:|
| twitter      | 631,514  |   320,995  | 1967 | 0.508   | **1.78** |
| bootstrap    | 280,311  |   616,486  |  454 | 2.199   | **7.70** |
| parse_stress | 1,838    |    15,121  |  121 | 8.227   | **28.8** |

RD runs twitter at ~1.78 cyc/byte — roughly one cmp+advance per byte
with SIMD ws-skip amortised. bootstrap pays 4× for denser alternation.
parse_stress is a 6-rung precedence tower — ~6×~5 cyc/byte per level.

## 2. Post-AW DTA per-byte cycle cost

From perf-01/02/03:

| Entry        | bytes     | ns/iter     | MB/s | ns/byte | cyc/byte |
|--------------|-----------|------------:|-----:|--------:|---------:|
| twitter      | 631,514   |  5,251,828  |  120 | 8.316   | **29.1** |
| normalize    |  6,138    |      5,875  | 1044 | 0.957   | **3.35** |
| parse_stress |  1,838    |    509,741  |    3 |277.334  |  **970** |

(Bootstrap's 1429 MB/s is a correctness artefact — only 9 records
parsed; perf-02 §2. Normalize is the only valid CSS DTA signal.)

**Per-byte gap**: twitter **16.4×** slower; parse_stress **33.7×**.

## 3. DTA per-byte cost decomposition (twitter, 29.1 cyc/byte)

Citing `crates/bbnf-tape/src/driver.rs` + perf-01 twitter table:

| Component (fn, % self) | cyc/byte | Evidence |
|---|---:|---|
| `dispatch_one` 11-arm match (22%) | 6.4 | driver.rs:852–867 tagged match |
| scanner.scan + `cached_dfa` + SipHash (28.6%) | 8.3 | scanners.rs:30–55 `HashMap<String,Arc<Dfa>>` under RwLock |
| `advance_or_pop_with` (9%) | 2.6 | driver.rs:1540+ cursor/pop |
| `reserve_compound` (7.8%) | 2.3 | driver.rs:1399–1417 **7 unconditional Vec::push** |
| `emit_leaf` (4.8%) | 1.4 | driver.rs:1366–1394 — same 7 columns |
| `dta_run` outer loop (5.6%) | 1.6 | driver.rs loop |
| `finaliser` (6.1%) | 1.8 | post-pass |
| utf8/libc/misc (11.5%) | 3.4 | memcmp, from_utf8 |
| `close_compound` (3.1%) | 0.9 | driver.rs:1482 |
| **Sum** | **28.7** | (matches measured 29.1) |

## 4. RD per-byte cost decomposition (twitter, 1.78 cyc/byte)

From AU/profiling-2.md:

- inlined `__value` byte-switch over `{`/`[`/`"`/`t-f-n`/digits —
  ~0.8 cyc/byte (83.3% of `__value` self-time)
- `eisel_lemire::compute_f64` on number bytes — ~0.2 cyc/byte
- `TapeBuilder::push_compound` one fused struct push — ~0.1 cyc/byte
- SIMD ws-skip + memchr (AU.2.7 deleted memchr but was here post-AU
  initially) — ~0.4 cyc/byte

Sum ~1.5 + misc matches 1.78. **Key**: RD's hot loop is ONE inlined
fn per rule; LLVM folded the state machine into the instruction
stream.

## 5. Why 16× more cycles? (H1–H5 ranked)

Both DTA and RD examine every byte once; info-theoretic lower bound
is ~1 cyc/byte (sonic achieves it). 16× gap decomposes as:

**H1 — Tagged-union dispatch BPU tax.** driver.rs:867 is `match
table.states[state_idx]` on an 11-variant enum; each byte traverses
≥1 state; branch target is data-dependent on the state table.
Apple M BPUs tolerate 2–4-way well but struggle with 11-way
history-dependent. Evidence: `dispatch_one` is #1 or #2 hotspot on
every profile (22–40% self-time). 6.4 cyc/byte twitter consistent
with 2–3 mispredictions per state. **#1 — largest cost share.**

**H3 — `cached_dfa` HashMap per regex scan.** scanners.rs:30–55:
every regex dispatch calls `cached_dfa(pattern: &str)`, hashes via
SipHasher13, probes `HashMap<String, Arc<Dfa>>` under `RwLock::read`,
`Arc::clone`. RD hoisted the DFA to a function-local static.
Evidence: perf-01 twitter rows 10+11+13 = 6.5%; scanner itself 26.5%
combined body+lookup. 8.3 cyc/byte twitter. **#2 — only
scanner-side lever at this cost tier.**

**H2 — Unfused `reserve_compound` 7-push.** driver.rs:1399–1417
writes 7 parallel Vec columns per compound open; each = bounds check
+ len increment + possible grow. RD's push was 1 `Vec<TapeRec>::push`
of a 16 B record. Evidence: perf-04 §1 names it: "seven Vec::push
calls per compound emission". 7.8% twitter = 2.3 cyc/byte. Scales to
~4 cyc/byte on CSS L4 (234 push_compound sites). **#3.**

**H4 — Table is `&'static` data, not const generic.** `DtaTable::
states: &'static [DtaState]` emitted as `pub const DTA_TABLE` but
read at runtime via `table.states[state_idx]`. LLVM cannot
const-propagate transitions across states because state_idx is
runtime. RD's per-rule fns let LLVM inline transitions
(rule_foo→rule_bar becomes a direct inlined call→body). Evidence:
`dta_run→dispatch_one→arm` stack accounts for 91–95% of every
profile's samples (perf-04 inclusive table) — the walker never
inlines into arms. **#4 — structural; caps H1's recovery
ceiling.**

**H5 — Runtime FrameStack vs compile-time call stack.**
driver.rs:213–244 `[Frame; 64] + Vec<Frame> overflow + counters:
Vec<u32> + iter_savepoints + op_stack`. Every Seq push writes 40 B
Frame + inline_len bump. RD's call stack = native reg ABI; push/pop
is 1 instruction. Evidence: `advance_or_pop_with` 6–11% everywhere
is pure frame bookkeeping. 2.6 cyc/byte. **#5 — smaller but
real.**

## 6. The user's claim addressed

"DTA MUST beat RD" is true for the *correct* definition of DTA.
Taxonomy:

- **(a) Current bbnf DTA**: byte-driven tagged-union interpreter
  over `&'static [DtaState]`. NOT what simdjson/sonic-rs build. 16×
  gap is arithmetic — the interpretation-overhead ceiling.
- **(b) Codegen-specialised stage-2 walker**: emitter lowers the
  flat state machine to inlined Rust — each state's body is a
  labeled block with direct jumps/fall-through. LLVM const-folds
  transitions, BPU sees stable jumps, table evaporates. Matches
  RD's steady state.
- **(c) Stage-1 SIMD bitmap + stage-2 walker**: simdjson's
  architecture. Stage-1 builds structural bitmap of `{}[]":` in one
  NEON pass; stage-2 walks bitmap positions, not input bytes.
  ~1 cyc/byte.

Simdjson + sonic-rs ARE flattened tape automata — they use (c).
bbnf DTA is (a). User's thesis is correct; bbnf's current
architecture is a misnomer.

## 7. Cycle-budget table

| Architecture | Per-byte cycles | Source |
|---|---:|---|
| sonic-rs JSON twitter | ~1.0 | perf-05: 2694 MB/s ÷ 3.5 GHz |
| simdjson x86 AVX-512 | ~0.9 | simdjson.org published |
| post-AU RD (twitter) | **1.78** | AU FINAL Appendix (1967 MB/s) |
| post-AW DTA (twitter) | **29.1** | perf-01 twitter (120 MB/s) |
| DTA + scanner closure + ShapeRef + PHF (W5 partial) | ~12–14 | perf-01 §Rec (33% scanner × 29.1 ≈ −9.6); H2 −2 |
| DTA + W5 + codegen-specialised walker (W5.6) | ~2.5–3.5 | H1+H4+H5 recovered (states inline); H2 partial |
| DTA + W5 + walker + stage-1 SIMD bitmap | ~0.8–1.2 | input bitmap-walked; matches sonic |

## 8. Bottom line

**DTA is viable. It matches RD at W5.6 close; exceeds RD with
stage-1 SIMD.** Minimum set:

1. **Scanner closure** (hoist `Arc<Dfa>` onto `DtaState::Regex` at
   lift; eliminate HashMap+SipHash per scan). Recovers 6–9 cyc/byte.
2. **ShapeRef runtime dispatch** (collapse same-shape compounds into
   one `push_shape_ref`). Recovers 2–4 cyc/byte on structural grammars.
3. **Fused column push** (7 Vec pushes → 1 struct push, 1 bounds
   check via `as_mut_ptr().add(len)` pattern). Recovers 1–2
   cyc/byte uniformly.
4. **Codegen-specialised walker (W5.6)** — load-bearing. Emit
   `const DTA_TABLE` AND a grammar-specific `fn dta_run_specialised`
   that inlines each state's body as a labeled block. Table stays
   as debug-replay substrate; walker never indexes at runtime.
   Removes H1+H4+H5 together.
5. **Stage-1 SIMD structural bitmap** (NEW — not in AW-IV). Pre-pass
   builds u64[] of structural-byte positions in one NEON sweep;
   specialised walker consumes bitmap positions, not bytes. Sonic's
   architecture. Only path to exceed RD meaningfully.

Items 1–4 reach RD parity; item 5 exceeds. If orchestrator ships
only 1–3 (currently-declared AW-IV scope), walker stays ~12–14
cyc/byte — a 6–8× regression vs post-AU RD. **W5.6 codegen-
specialised walker is the load-bearing decision.**

The architectural property bbnf cannot recover without a specialised
walker is **cross-state LLVM inlining**: tagged-union dispatch over
`&'static [DtaState]` structurally prevents it. Every other lever
is decomposable and the cycle budget resolves — this one is binary.
