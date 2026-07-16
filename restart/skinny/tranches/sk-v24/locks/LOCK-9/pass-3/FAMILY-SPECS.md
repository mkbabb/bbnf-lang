# LOCK-9 — THE SCANNER & KERNEL PLANE (parse-that rust) · pass-3 FAMILY-SPECS

*Synthesis, pass 3 (Fable-tier judgment). One cogent specification per family while the routes
remain incompatible, then the incompatibility map. Elegance earns nothing on its own; a family
stalled at a missing primitive equal in difficulty to the original problem is marked
BLOCKED-candidate. Folds research-{A,B,C,D,priorart} pass-3 against the pass-2 registry (52/62/43/30;
PROTO-1 live cross-crate inline; PROTO-2 LEAN/DENSE rebirth) and the six reconciliations the registry
owed. Substrate paths:lines re-anchored against the live trees THIS session. CROSS-LOCK ledger XL-A..H
conveyed as fact.*

The lock is **four dispositions of one aarch64 scanner/kernel plane on three orthogonal axes**:

- **Kernel-home** — *who owns the byte kernels.* 9A (one shared crate `bbnf-simd`) ⊗ 9B (parse-that
  keeps its own). Direct opposites; **the lock's central adjudication**, now a **pure LOCK-6
  maintenance-cost call** (the perf daylight washed at pass 2 and pass-3 research concurs).
- **Scanner-provenance** — *are the character-class scanners authored or derived.* 9C (derived from
  the bootstrapped grammar's DFA; hand membership/prefix scanners cease to exist) vs {9A,9B} (authored,
  thinned to consumers or hand-rebirthed).
- **Input-surface** — *whole-input vs windowed.* 9D (streaming `Window` witness) — orthogonal, but
  contradicts the eager `PaddedView` invariant the other three silently share.

## THE SIX RECONCILIATIONS THE REGISTRY OWED (absorbed; the pass-2 FAMILY-SPECS was stale against its own protos)

1. **The 9A⊗9B axis is a PERFORMANCE WASH → a maintenance-cost call.** Not "which is faster." The
   decider is LOCK-6's "reasonable point of rust↔rust isomorphism" resolved by "null hypothesis
   defeated by measured drift/maintenance cost" (LOCK-6D verbatim).
2. **The OnceLock facade is FREE at production cgu=1** (PROTO-1 CONTROL: byte-identical `blr`-free
   code; the standalone object carried no `OnceLock` symbol — LLVM devirtualised it). Delete every
   "indirect branch survives fat-LTO in-loop" framing. research-A F1b refines: the `cfg`-direct
   `#[inline]` producers cross the rlib seam regardless of cgu/LTO (MIR in rlib metadata); the only
   non-inlinable boundaries are (i) the *intentional* OnceLock runtime-dispatch pointer, present in
   **either** home, and (ii) a producer left un-`#[inline]` under cgu>1 + lto=false — a trivial
   attribute guard.
3. **The "free `vshrn` movemask" is REFUTED.** The two lawful producers are ABSENT from bbnf-simd
   (`vpaddq` in **zero** files — re-verified this session) and must be authored **once into any home
   regardless of ownership.** Adopting 9A is not "inherit good kernels for free" — it is "author the
   canonical producers *and* purge the refuted double-`vaddv` first."
4. **"9B inherits the identical OnceLock constraint" is FALSE.** parse-that's scan files carry zero
   `OnceLock`/`primitive_kernels`; the facade is bbnf-simd-internal (`dispatch.rs`). **The de-face is a
   9A-only hygiene cost** — it counts *against* 9A on the axis, not neutral.
5. **The `bbnf-regex` path is corrected and its blocker DOWNGRADED — NOT dissolved (corrected against the
   live tree this session; the prior synth over-read it).** The `bbnf-regex 0.1.0` name+version collision
   is between **`parse-that/rust/regex`** (package `bbnf-regex 0.1.0` — the real 6742-LOC automata engine,
   the canonical one-regex-home per XL-C) and **`skinny/crates/bbnf-regex`** (package `bbnf-regex 0.1.0` —
   a **322-LOC `RegexFacts` analysis stub**, `analyze()→{hir,byte_classes,first,string}`, NO `automata/`;
   the collision partner, consumed by skinny `ir`/`passes`, archives at GN-0). The resolution MECHANISM is
   **rename-on-vendor** — the engine, vendored into the greenfield, takes package name `parse-that-regex`
   (the established pattern), so no two `bbnf-regex 0.1.0` ever coexist. **But VERIFIED THIS SESSION: the
   engine is NOT yet vendored.** `skinny/crates/parse-that-regex` (package `parse-that-regex 0.1.0`) today
   carries ONLY `number/` (eisel_lemire) + `unicode/` + `integration/simd_scan_hook.rs` — **no `automata/`,
   no `equiv`, no `classify`** (grep: NONE). So there is **no in-tree DFA source-of-truth in skinny at
   all**; scanner-as-DFA has no in-tree substrate until the `automata/`+`equiv`+`classify` tier is vendored
   under the rename (or the generator emits `dfa.find` against the parse-that home). The blocker moves from
   *"collision unresolvable"* to *"vendoring un-executed + emit pass unbuilt"* — a real advance (the engine
   EXISTS and green in parse-that; the mechanism is clear), but the substrate GAP REMAINS (§9C).
6. **Tier-M "parity by construction (Q1 answered)" downgrades to SPEC-ONLY** — the `[u8;256]→SIMD`
   emit pass does not exist (`equiv.rs` emits scalar only); parity is asserted between a hand table and
   a not-yet-emitted classifier. The emit pass is *feasibility*-testable NOW against the parse-that engine
   directly (Brief 2), but production still awaits the un-executed automata-tier vendoring (reconciliation 5).

**Binding laws every gate carries (SPEC §0).** No stable-port perf claim — de-nightly is STABILITY
only, the no-regression gate may FAIL. `vshrn` narrow, never double-`vaddv` (the 2271-vs-509 `addv.8b`
counterexample; live at `number_simd.rs:54-55`). No digit-scan SIMD (§0/02). No block-scheduling
(§0/04). x86 dead. Cold, paired, N≥200, two windows, opposite arm order; PMU cyc/B unavailable in macOS
userspace → divan wall-clock ratio is the sole certified metric. Consumer-less substrate forbidden
(Y.13). parse-that is READ-ONLY (P23) — every mutation is a **named** supersession, never presumed.

## THE SHARED CONVERGENCE (common to 9A/9B/9C — the canonical producer is BOTH a perf idiom AND a correctness contract)

All three kernel families independently reach the **same contract-split movemask**, and pass-3 research
(A §7, B §4, C §3/§6) elevates it from optimisation to **correctness contract**:

- **LEAN — nibble `vshrn_n_u16::<4>`** for consumers that want only the first set position
  (`trailing_zeros() >> 2`, 4 bits/lane, one `shrn`, no reduction). Sites: `span_parser/scan.rs:152`,
  `scanners.rs:208/286`, the LEAN half of `number_simd`.
- **DENSE — `vpaddq_u8` bit-weight tree** (simdjson `neon_movemask_bulk`) for consumers needing an
  *exact* 1-bit-per-byte 64-bit bitmap. Sites: `structural_bitmap.rs:43/57`, `quote_parity.rs:81-82`,
  `quoted_simd.rs:45-46`, `decode.rs:82-83/189-190`, `ws_comment.rs:81/150/247`. LLVM lowers the
  disjoint-bit tree to `uzp/orr` (measured 50 vs 64 insns, zero reduction ops).
- **The DENSE↔LEAN split is a CORRECTNESS contract, not a preference.** A naive nibble mask on a DENSE
  site places each lane's bit at stride-4 instead of stride-1 → `prefix_xor` (clmul over the u64 quote
  mask) computes the in-string region over the WRONG bit lattice → escape-parity diverges → JSON string
  decode corrupts on an odd-backslash run (`"\\\""`). This is the decisive differential-oracle case
  every kernel family must ship (B §4, A R2, C R2).
- **NEVER** the 16-byte full-repack (`bbnf-simd/aarch64/movemask.rs` degrades to a scalar `for pair in
  0..8`), **NEVER** the double-`vaddv` (the four built producers `byte_class_from_eq_set_64.rs:83-84`,
  `structural_classify_64.rs:151-152`, `comment_body_mask_64.rs:72-73`, `bracket_depth_mask_64.rs:78-79`
  — all four re-verified live this session).

**Consequence:** the canonical-producer *form* is SETTLED (measured, pass 2). What is still owed is the
axis-deciding measurement of §9A/§9B — the residual divergence against an **evolved** shared home.

---

## 9A — ONE-KERNEL-LIBRARY · **ADVANCED · born-RED · ~52%**

**Architecture.** Collapse parse-that's eight `portable_simd` scanners onto the extant stable-clean
checkasm-gated `skinny/crates/bbnf-simd` (pure `core::arch::aarch64`, no nightly; dav1d-style checkasm
oracle: stack-canary XOR-fold + callee-saved x19–x28 sentinels + per-primitive scalar-reference
bit-parity, `checkasm_common.rs`). The isomorphism is **physical**: one source, drift impossible by
construction. The shared home is not hypothetical — it already carries a near-superset of parse-that's
scan surface and the scalar-parity oracle parse-that wholly lacks; the work is *finishing* it (author
the absent producers, purge the refuted idiom) and adding a consumer edge.

**Mechanism.** Two consumer tiers, from research-A's owed scanner-file census (§5): **~6 kernel-1:1**
files (`structural_bitmap`→`byte_class_from_table_64`/`structural_classify_64`; `ws_comment`→
`comment_body_mask_64`; `quoted_simd`+`quote_parity`→`escape_mask_64`+`bitmap_prefix_xor_64`;
`scanners`→`byte_class_from_eq_set_64`); **~2–3 vector-tier** files (`span_parser/scan`, `decode`
`store_chunk`) wanting a ~60-LOC shared `u8x16`/`mask8x16` vocabulary (the LEAN movemask + splat/cmp/
store) homed in bbnf-simd beside `prim`; **1 no-go** (`number_simd` — digit-scan SIMD is refutation-
barred, stays per-repo). Kernel-tier is where the win concentrates; vector-tier is the small shared
vocabulary; no-go is explicitly excluded.

**Integration seams.** Physical seam is **one line**: `parse-that/rust/.cargo/config.toml
[patch.crates-io]` gains `bbnf-simd = { path = "…/skinny/crates/bbnf-simd" }` — the memchr model, and
the patch table already cross-links 7 sibling crates (`bbnf`, `bbnf-ir`, `csp-solver`…). The vendor
alternative mirrors the **live in-tree precedent**: `crates/csp-solver` is re-vendored from sibling
`csc411` under a `chore(csp-solver/canonical-diff): re-vendor …@<sha>` beat + a `*_canonical_parity`
byte-identity test (the HEAD commit ledger shows exactly this). A `bbnf_simd_canonical_parity` beat
gates drift identically. De-face: `dispatch.rs:50-61` → the direct-`cfg` form (9A-only hygiene, not a
perf lever). Producer purge: the four double-`vaddv` sites → the contract-split producers.

**Gates it carries (born-RED).** (1) **R-A3 purge as a BLOCKING gate** — a shared home that ships the
double-`vaddv` violates §0 iv; `rg 'vaddv_u8|vaddvq_u8' skinny/crates/bbnf-simd/src/aarch64` must return
ONLY the digit-MAC site, every mask producer must show `shrn`/`vpaddq` in disasm. (2) **Perf half on the
CORRECT surface** — paired cold `parse_that_css → scan_ws_block_comments → classify_stripe` (CSS-only;
the JSON corpus is a null for `classify_stripe`), N≥200, two windows, ≥0.98×. (3) **One-line path-dep
seam under fat-LTO cgu=1** (NOT the vendor fallback PROTO-1 tested): `otool -tv` a wired scanner, zero
`bl _…byte_class…` in-loop, the only indirect the intentional OnceLock pointer. (4) **Evolved-home
gate-5** — author the canonical producers into a scratch bbnf-simd + purge, THEN measure residual
divergence (erasing the manufactured divergence PROTO-2 measured against the un-evolved crate). (5)
checkasm full-64-bit parity over the merged surface incl. the nibble-on-DENSE corruption case. (6) LOC
ledger: ~+60 one-home vs ~+380 two-home (PT-2 W1's private `simd/`), audit surface `rg vaddv` over one
crate vs two.

**Exact open questions.** (Q1) The evolved-home residual divergence — the LOCK-6 datum neither pass
produced (§Incompatibility). (Q2) The perf half on CSS + the path-dep (not vendor) seam — zero-measured
for two passes on a mis-targeted surface. (Q3, cross-lock) P23 supersession named-not-granted — 9A's one
line mutates parse-that's read-only tree. (Q4, SPEC-ONLY) the `vector/{mod,neon,scalar}.rs` module does
not exist; promoting refuted-code demands its own no-regression artifact.

**Why born-RED, not BLOCKED.** No missing primitive of equal difficulty: live cross-crate inline is
measured-GREEN (PROTO-1), the de-face is a known refactor (three sibling kernels already direct-`cfg`),
the vendor gate has a live in-tree precedent. 9A's value is now **honestly maintenance/drift-avoidance +
the refuted-idiom purge, NOT a speedup** — the port is cyc/B-neutral by law; the only perf obligation is
a measured no-regression floor.

---

## 9B — NATIVE-REBIRTH · **ADVANCED · incumbent · ~62%** (the only family off SPEC-ONLY)

**Architecture.** parse-that keeps its own kernels, each rebirthed in idiomatic stable
`core::arch::aarch64` NEON (stable since 1.59 — no nightly), one scalar twin per kernel as ground truth,
a natively-ported checkasm harness proving each NEON body is *reached* (liveness) and *bit-parity*. No
physical code shared with bbnf-simd; the isomorphism is **behavioural, by discipline** — same movemask
idiom, same checkasm protocol, same scalar-oracle law. Central bet: the "reasonable point" of rust↔rust
iso (Constitution iii) is design-level, and duplicated-small-kernel drift is cheaper than a shared
crate's LCD API + sync-gate tax. **That bet is a null hypothesis to defeat by measurement** — if
post-rebirth the kernels measure near-identical against the *evolved* home, 9B loses to 9A on merit.

**Mechanism — movemask-by-downstream-contract IS the family.** The consumer census (B §2, verified):
**3 LEAN · 6 DENSE · 1 no-go · 1 pure-vector**. A thin `parse_that/src/simd/{lean,dense}.rs`
directory-module exposing exactly two lawful producers (`movemask_lean(uint8x16_t)->u64`,
`movemask_dense_64(&[uint8x16_t;4])->u64`) — colocated with `parsers/scan/`, no sibling rlib. **BANKED
(PROTO-2, do not re-litigate):** LEAN `number_simd.rs:54-55` double-`vaddv`→`shrn.8b #4` (0 `vaddv`,
14 vs 19 insns); DENSE `classify_stripe_64` `vpaddq` tree (50 vs 64, 0 reduction ops); paired ≥0.98×
(15/16).

**`number_simd` is triple-refuted → ABROGATE (research-B §3, the cleanest rebirth move).** It embodies
three §0 violations at once: (a) digit-scan SIMD, (b) double-`vaddv` movemask (`:54-55`), (c) a live x86
SSE4.2 path (`:145-235`). Per abrogate-before-patch: **delete** the entire SIMD surface (~200 LOC + two
refuted patterns), retain only the scalar SWAR fold (`parse_eight_digits`, already backs the 9..15-digit
path, its own parity oracle by construction). `quote_parity::prefix_xor`'s aarch64 `vmull_p64` clmul is
the *opposite* case — a legitimate keep (correct simdjson quote-parity kernel); purge only its x86 twin.

**Gates it carries.** (1) Disasm liveness (`shrn`/`vpaddq`/`tbl` present). (2) Insn-count no-regress
(2271-vs-509 must not reproduce). (3) **Native checkasm bit-parity oracle** (the owed artifact) —
alignment sweep 0..63, black_box xorshift + JSON-alphabet vectors, INJECT_BUG flagging ≥90%, plus the
decisive nibble-on-DENSE-quote-parity divergence case; DENSE correctness is currently only "parsed
without panic," NOT bit-parity. (4) Paired divan ≥0.98× every corpus both windows. (5) **Gate 5 the
axis-decider** — the merged-surface LOC/substrate divergence table against the **EVOLVED** home
(`PaddedView`-64 vs `eob_pad_clamp`; single-`vqtbl1q` vs `vqtbl4q`; capacity constants). (6) **`cargo
+stable build`** — the headline (off nightly) is UNVERIFIED; the rebirths were authored *on* the nightly
they claim to shed. research-B §4: the sole residual stable blocker is `regex/generated.rs`
`fmt_helpers_for_derive` (a generator-regen fix, outside the kernel plane); `portable_simd` dissolves the
moment scanners call native producers; `smallbox` nightly→`smallbox!` is codegen-neutral.

**Exact open questions.** (Q1) Gate 5 against the evolved home — SEEDED not closed; PROTO-2's divergence
was measured against the un-evolved crate (overstates 9B's case: the producers must be authored once
regardless of home). (Q2) DENSE bit-parity oracle unbuilt. (Q3) stable cutover never run. (Q4)
self-regression guard (naive-nibble-corrupts) used correctly but never demonstrated as the load-bearing
negative.

**Why not BLOCKED.** Every intrinsic is stable since 1.61; the harness exists to port; the taxonomy is
settled. Its only failure mode is *losing to 9A on gate 5*, not a missing primitive.

---

## 9C — SCANNER-AS-DFA · **BLOCKED-candidate · blocker DOWNGRADED (~46–48%, modest uptick from 43)**

**The pass-3 disposition move.** The registry BLOCKED 9C on **Q2 cross-lock to LOCK-5** — the
`bbnf-regex 0.1.0` name+version collision + an unbuilt grammar→DFA pipeline. Pass-3 **downgrades** the
blocker but does NOT dissolve it (reconciliation 5, corrected against the live tree): the collision now
has a concrete resolution MECHANISM (rename-on-vendor → `parse-that-regex`; delete the 322-LOC stub at
GN-0), and the engine that *produces* the `[u8;256]` byte-class table the shared kernel *consumes* EXISTS
and is green — but in `parse-that/rust/regex`, **not vendored into skinny** (verified: `parse-that-regex`
has no `automata/`). So 9C stays a **BLOCKED-candidate**: substrate-gapped on an un-executed vendoring
(LOCK-5/GN-1-owned) AND an unbuilt emit pass. It is NOT hard-BLOCKED — the remaining gaps are **not a
missing primitive of equal difficulty** (the engine exists; vendoring is mechanical; `lo6_table_admissible`
is a shipped half of the separability predicate) — so the honest move is a modest uptick, not the born-RED
lift a mis-read would grant. Brief 2 proves the emit pass feasible against the real engine NOW, decoupling
feasibility from the vendoring so 9C is selectable the moment the vendoring lands.

**Architecture.** A grammar leaf carrying a regex (`bootstrap/regex.bbnf:10-37`) lowers through the
existing tower — `Hir` → Thompson NFA (`automata/nfa.rs:71`) → DFA via subset construction + Hopcroft
(`automata/dfa.rs:67`) → `byte_classes:[u8;256]` + `num_classes:u16` (`equiv.rs`) — and terminates in
**one byte-class table routed into three existing kernel tiers by separability.** "Scanner" becomes a
codegen tier, not a source file — for the membership/prefix/structural leaves only.

**Mechanism — the tiered lowering (M/S/X, keyed on DFA structure):**
- **Tier M (membership; 1–2 states):** the dominant every-byte pass; the DFA's whole computation is the
  byte→class map. `|set|≤8` → `byte_class_from_eq_set_64` (memchr-tier); else → `byte_class_from_table_64`
  (any `[u8;256]`). Covers ws/digit/hex/ident/url-char.
- **Tier prefix (anchored literal, 1–3 entry bytes):** the PROVE-10 memchr/2/3 prefilter + anchored
  confirm (CONFIRMED 8.8–29.5×, 0.99× controls).
- **Tier structural (multi-kind, per-byte distinct):** `structural_classify_64` `vceqq`-fan (JSON
  `{}[]:,"\`, CSS delimiters), collision-free by exact equality.
- **Tier S (small multistate) — MEASURED NEGATIVE:** `state[i]=tbl(T,class[i],state[i-1])` is a
  loop-carried serial `vqtbl1q` chain (~2–4 cyc/byte unhidable), loses to scalar `find_at` on the short
  runs these DFAs see (`ws_comment` 84–100% of WS runs ≤4 bytes). Sheng earns no leaf.
- **Tier X (explosion) — NO-GO:** `\p{L}`/wide-unicode over UTF-8 explodes to 30–60+ byte-classes +
  hundreds of states (`dfa.rs:262` returns `None`); stays on the vendored scalar Höhrmann DFA
  (`unicode/utf8_hoehrmann.rs`). A correctness floor, excluded — but the census must still report Tier-X
  denominators so coverage is a measured %, not a fit-chosen sample.

**The separability crux — already half-built.** `bbnf_simd::dispatch::lo6_table_admissible`
(`dispatch.rs:101`) ALREADY implements the partition/separability predicate for the single-table tier
(walks the alphabet, rejects on any low-6 collision). An emit pass calls the *same* predicate on the
DFA-derived alphabet. **F3 answered YES, provably (research-C §3):** `structural_classify_64.rs:14`
records `;`≡`{` under low-6 mask (both 0x3B) — a naive nibble/low6 emit silently drops one colliding
member; the gate routes non-separable sets to `byte_class_from_table_64` (full `[u8;256]`, collision-free
by construction) or the `vceqq`-fan. (Registry note: the load-bearing byte is `{`=**0x7B**; the collision
is lo6(0x3B)=lo6(0x7B)=0x3B — the pass-2 spec mis-stated it; corrected here.)

**The honest boundary (research-C §5) — what a DFA CANNOT dissolve.** "Hand scanners cease to exist" is
TRUE for character-class/membership/prefix leaves; **FALSE** for the four **carry-stateful transductions**
— prefix-XOR quote-parity, escape odd-parity, bracket-depth balance, comment-body carry. These thread
state *across* 64-byte blocks and are structurally outside a single-alphabet DFA; they **remain
hand-authored and belong to the 9A/9B kernel plane**, consumed by 9C via the same `byte_class`/mask
vocabulary, never re-derived. Claiming otherwise would be a stub — forbidden. Scanner-file census: **~3
files fully dissolve** (ws-run, membership scanners, number digit-run-scan), **~4 stay carry-stateful
kernels**, 1 is infrastructure (`state.rs` PaddedView).

**Gates it carries (born-RED).** (1) **Emit-parity oracle** — for each shipped leaf derive
`byte_classes`, emit through the separability selector, assert **bit-identical scan vs the hand scanner**
over black_box random + alignment sweep, INJECT_BUG confirming teeth, INCLUDING the `;`/`{` low6-collision
case. (2) **Separability-gate correctness** — fuzz plants a colliding member, asserts NO silent drop off
the `vqtbl4q` tier (gate 4, the load-bearing proof, unrun). (3) **Census reproduction** — compile the
eight leaves through `Dfa::compile_with`, record real `num_classes`+states against the 255/tractable
ceilings incl. `\p{L}` over-ceiling rows as denominators. (4) No-regression: `otool` reduction-insn ≤
hand + paired ≥0.98×.

**Exact open questions & why still RED.** (Q1, downgraded to SPEC-ONLY) dense-classification is at-best
parity with hand simdjson — value is deletion + grammar-specialization + stable-by-construction + free
`vshrn`, real but modest. (Q2, **the residual gap**) the `[u8;256]→SIMD` emit pass + the
bounded-CSP separability solver are the family's **new code and unbuilt** (`equiv.rs` emits scalar only);
`lo6_table_admissible` + the shipped `byte_class_from_table_64` seam mean it is **NOT equal-difficulty to
the original problem** — hence born-RED, not BLOCKED. (Q3, cross-lock, residual) tier-selection is a
LOCK-2 scan-plan island; the codegen home `crates/codegen/src/emit/regex/` is a **named GN-codex
supersession** (constitution ix), not an implicit land-grab. 9C remains **cross-lock-coupled**: LOCK-5
(vendor the automata tier into skinny + the collision resolution) + LOCK-2 (tier island) + GN-codex
codegen. The collision within LOCK-5's leg is resolved-in-mechanism, but the vendoring is un-executed —
the leg is **downgraded, not cleared**.

---

## 9D — LAZY-SURFACE · **RETIRE (memory center refuted; residue folds to gestalt-Ω) — was BLOCKED 30%**

**Disposition.** The registry set the terminus explicitly: "pass-3 runs P3 or converts 9D to RETIRED."
research-D delivered the P3 instrumentation **spec a third time, not the histogram** — so P3 is unrun for
a third consecutive pass, and per directive **9D RETIRES.** research-D itself endorses this as the honest
close: the streaming/memory center is refuted (by analysis + grep, below); the defensible residue is a
LOCK-2 consumer, not a live LOCK-9 family. This synthesis does NOT brief P3 as a prototype — running it
would only upgrade the refutation from by-argument to by-measurement, a lower gestalt-advance than the two
uncertainties chosen (§Briefs); the RETIRE path is the directive-sanctioned outcome.

**Why the memory center is refuted (research-D, measured-by-argument + grep).** (1) **Consumer absent
(Y.13 tripped, grep-confirmed):** `feed|push_bytes|Decoder|BufRead|document_stream|from_reader` → zero
real consumers; every grammar builds a whole owned tree over `&'a str`; `ParserState::new` immediately
materializes a full padded second copy (`state.rs:285-305`). (2) **Span-dangle soundness wall:**
`Span<'a>` borrows one buffer; a chunked `feed` that frees consumed chunks dangles every earlier span;
retaining all chunks = no RSS win; copying spans to owned `String` reintroduces the 5390-alloc mass
(twitter) streaming was meant to avoid — a hard wall, not a knob. (3) **PaddedView is anti-chunking:** the
64-NUL trailing guard breaks at every chunk boundary → re-pad (copy tax) or reinstate the per-stripe tail
guard (surrendering the exact SIMD win). simdjson's `document_stream` is NDJSON (whole docs), never
single-doc chunking. (4) **Dead-by-law:** §0/51 fold-over-lazy-cursor is 2.3–4.9× behind; a lazy-input
cursor resurrects the refuted architecture. (5) **Backtrack:** structurally unbounded (PEG, Ford 2004),
empirically small (`dispatch_byte` is an O(1) 256-LUT peek with zero speculative advance → P50≈0
expected) — but the histogram was never measured; a window sized to a measured max is corpus-conditional,
not semantic.

**The surviving residue (two, both re-homed).** (a) **Cost-model kernel-selection at combinator-compile
time** — 9D's only consumer-bearing surface: at `Parser::new` select `TakeUntilScan::Lut(Box<[bool;256]>)`
(`leaf.rs:182`, today always-LUT, no model) vs a SIMD shufti by a static delimiter-set predicate ported
from `lo6_table_admissible`; the GENERATOR-side precedent is measured (SPEC §1 features 05 CSS shufti
−23.6%, 12 emit-spine residualization). **This is a LOCK-2 decision-driver consumer** (`KernelPlan`, the
same island 9C's tier-selection needs) — not a new LOCK-9 subsystem. (b) **Carry-threading** for the
9C⊗9D gestalt-Ω "compiled resumable scanner" — since Sheng is a measured negative, 9D threads carries
through the **Tier-3/structural kernels** (`prefix_xor`/comment/bracket-depth carries bbnf-simd already
exports), not a saved Sheng state. This is where 9D's residue survives.

---

## THE INCOMPATIBILITY MAP — which families cannot coexist, and precisely why

**9A ⊗ 9B — HARD-INCOMPATIBLE (kernel-home axis). THE central adjudication → a PURE LOCK-6
MAINTENANCE-COST CALL.** A kernel is either shared-source (9A) or own-source (9B); it cannot be both.
Both routes reach the **identical** contract-split producers and checkasm discipline — they differ only
in whether one file or two carry them. The crowding-redirect rule (bank the redundant one) does **not**
apply: the perf axis WASHES (OnceLock free at cgu=1; canonical producers author-once regardless of home),
so the decider is LOCK-6's "null hypothesis defeated by measured drift/maintenance cost" — a verdict
LOCK-9 cannot render. The two pass-2 biases point **opposite** ways (charge 9A the 9A-only de-face →
tilts 9B; re-measure gate-5 against the *evolved* home → tilts 9A), so banking either now picks the axis
without the corrected evidence. **The keystone measurement neither pass produced — the evolved-home
gate-5 — is still owed** (§Brief 1). 9B holds as incumbent-default (PT-2 frozen codex, off SPEC-ONLY);
9A's physical promise is proven live but its value is maintenance-only; neither banked.

**9C ⊗ 9B — INCOMPATIBLE (scanner-provenance axis).** 9C deletes parse-that's hand membership/prefix
scanners and emits them as compiled DFA products; 9B's thesis is that parse-that *authors and rebirths*
its kernels. A scanner cannot be simultaneously hand-authored-in-`core::arch` and
non-existent-because-generated. They co-occur only degenerately (a 9C Tier-miss leaf falling back to a
hand kernel — 9C conceding coverage, not 9B being adopted).

**9C ◐ 9A — PARTIALLY COMPATIBLE / SEQUENCEABLE (a DAG, not a contradiction) — STRENGTHENED this pass.**
Both converge on `bbnf-simd` as the kernel backend: 9A by extraction, 9C by lowering-to-it through the
DFA. research-C makes this **literal**: 9C's emit routes the DFA's `[u8;256]` straight into
`bbnf_simd::byte_class_from_table_64` — 9C IS a consumer of the 9A-shared home. The gestalt holds both as
ordered phases: **(kernel-home verdict: 9A vs 9B) → (scanner-provenance: 9C emits the membership/prefix/
structural leaves into that home; the four carry-stateful kernels stay hand-authored per 9C §5).** The
prefilter double-home the gestalt must collapse: `bbnf_simd::find_ascii_set_member64` ⟷ `Dfa::find` — one
lowering per scan site, never both (GN-2/PT-1 dispatch law).

**9D ∥ {9A,9B,9C} — was ORTHOGONAL, now RETIRED.** 9D's residue (cost-model selection) changes *what
feeds* the kernels, not *who owns* or *how derived* them — a later, conditional LOCK-2 overlay admissible
only if a streaming consumer ever appears (it does not). Its carry-threading folds into the 9C⊗9D
gestalt-Ω object. No live-family incompatibility remains.

**Summary matrix** (⊗ hard-incompatible · ◐ partial/sequenceable · ∥ orthogonal · † retired):

|        | 9A            | 9B            | 9C            | 9D           |
|--------|---------------|---------------|---------------|--------------|
| **9A** | —             | ⊗ (kernel home → LOCK-6) | ◐ (agree home, differ provenance; DAG, 9C consumes 9A) | ∥† |
| **9B** | ⊗             | —             | ⊗ (provenance)| ∥† |
| **9C** | ◐             | ⊗             | —             | ◐† (carries thread structural kernels → gestalt-Ω) |
| **9D** | ∥†            | ∥†            | ◐†            | — (RETIRED) |

---

## CROSS-LOCK DAG ORDERING (blockers LOCK-9 CANNOT retire alone)

- **9A ⊗ 9B → LOCK-6 (maintenance-cost call, perf washes).** LOCK-9 produces the *corrected*
  measurements (9A: perf half on CSS + evolved-home gate-5 + the 9A-only de-face charge; 9B:
  merged-surface divergence vs the evolved home); LOCK-6 renders the verdict. Co-converge in GESTALT-P;
  neither closes the axis alone.
- **Canonical movemask form → RESOLVED (measured); home-collapse → LOCK-9 ∩ LOCK-5.** Four producers
  (bbnf-simd repack + bbnf-simd refuted double-`vaddv` + PROVE-10 lean + parse-that `to_bitmask`) collapse
  to ONE lawful home; memchr's lean form lives in the regex prefilter. The gestalt collapses the home.
- **9C grammar→DFA source-of-truth → LOCK-5 leg DOWNGRADED, not cleared.** The `bbnf-regex 0.1.0`
  collision (real engine `parse-that/rust/regex` × skinny 322-LOC stub) has a concrete resolution
  MECHANISM (rename-on-vendor → `parse-that-regex`; stub deleted at GN-0), but VERIFIED: the `automata/`
  DFA tier is **NOT yet vendored into skinny** (`parse-that-regex` = number/unicode/integration only) — no
  in-tree DFA source exists yet. LOCK-5/GN-1 owns the vendoring. **9C tier-selection → LOCK-2** (scan-plan
  island); **9C codegen home → a named GN-codex supersession.** 9C stays cross-lock-blocked until the
  vendoring executes + LOCK-2 + GN-codex land.
- **Prefilter double-home → GN-2/PT-1 dispatch law.** One lowering per scan site. 9C deepens this.
- **9C/9D resumable-scanner collapse → gestalt-Ω** (re-based onto structural carries; Sheng negative).
- **9D residue (cost-model `KernelPlan`) → LOCK-2** (compile-time decision driver).
- **Un-homed consumer edge → LOCK-4/LOCK-8.** The reborn scanner surface feeds the combinator core
  (LOCK-4) and, via LOCK-6, the TS backbone (LOCK-8); no 9-family names how it presents to LOCK-4's parser
  graph or whether LOCK-8 mirrors 9C's compiled-DFA products. The gestalt-P weave closes it.

---

## DISPOSITION ROLL-UP FOR THE REGISTRY

- **9A ONE-KERNEL-LIBRARY — ADVANCED, born-RED (~52%).** Value is now honestly maintenance/drift-
  avoidance + the refuted-idiom purge, NOT a speedup. Convergence blockers: the evolved-home gate-5 (Q1)
  and the perf half on the CSS surface + path-dep seam (Q2), both zero-measured for two passes. The R-A3
  purge must become a BLOCKING gate.
- **9B NATIVE-REBIRTH — ADVANCED, incumbent (~62%).** The only family off SPEC-ONLY; LEAN+DENSE wins
  banked. Close is the null-hypothesis gate-5 against the *evolved* home (concedes honestly if divergence
  measures small) + the DENSE bit-parity oracle + the stable cutover (headline unverified — never left
  nightly).
- **9C SCANNER-AS-DFA — BLOCKED-candidate · blocker DOWNGRADED (~46–48%, modest uptick from 43).** The
  pass-3 mover: the LOCK-5 name-collision is resolved-in-MECHANISM (rename-on-vendor → `parse-that-regex`;
  322-LOC stub deleted at GN-0) — but VERIFIED, the `automata/` DFA tier is **not yet vendored into skinny**
  (no in-tree DFA source), and the emit pass + separability solver are still unbuilt. NOT equal-difficulty
  (engine exists+green in parse-that; `lo6_table_admissible` is a shipped half), so BLOCKED-candidate, not
  hard-BLOCKED. Still cross-lock-coupled (LOCK-5 vendoring + LOCK-2 tier island + GN-codex codegen). Brief 2
  proves emit-feasibility against the real engine, decoupling it from the un-executed vendoring.
- **9D LAZY-SURFACE — RETIRE.** Memory center refuted (consumer grep-absent, span-dangle wall, PaddedView
  anti-chunking, §0/51); P3 unrun a third time per directive → RETIRED. Residue: the cost-model
  selection plane (a LOCK-2 consumer) + carry-threading for the 9C⊗9D gestalt-Ω. Latency-only, consumer-
  gated.

---

## THE TWO SHARPEST UNCERTAINTIES → pass-4 prototype briefs

The two highest-advance uncertainties for the NEXT pass (GESTALT-P). Brief 1 finally delivers the
axis-deciding LOCK-6 datum neither pass produced; Brief 2 converts 9C's largest FAMILY-INTERNAL gap (the
unbuilt emit pass) from unproven to measured-feasible — run against the real parse-that engine, so it
de-risks 9C independent of the un-executed automata-tier vendoring (reconciliation 5). (P3/9D is
deliberately NOT briefed — 9D retires per directive; running P3 would only upgrade a refutation, a lower
gestalt-advance.)

**BRIEF 1 — the evolved-home axis re-measurement + 9A's perf half on CSS (pt-scratch).** The keystone
neither pass produced. In a scratch copy of parse-that (P23: scratch-only) + a scratch bbnf-simd: (1)
Author the two canonical producers into scratch bbnf-simd — LEAN `vshrn_n_u16::<4>`, DENSE-64 `vpaddq`
tree — and PURGE the four double-`vaddv` sites (`byte_class_from_eq_set_64.rs:83`, `structural_classify_64
.rs:151`, `comment_body_mask_64.rs:72`, `bracket_depth_mask_64.rs:78`). Gate: `rg 'vaddv_u8|vaddvq_u8'
skinny/crates/bbnf-simd/src/aarch64` returns only the digit-MAC site. (2) De-face the five OnceLock
`prim::` kernels to direct-`cfg`. (3) Add the one-line `[patch]` path-dep (NOT vendor) to
`parse-that/rust/.cargo/config.toml`, rewire CSS `scan_ws_block_comments → classify_stripe`. (4)
`[profile.bench]` fat-LTO cgu=1: `otool -tv` the wired `classify_stripe` loop; PROVE-09 paired cold divan
`parse_that_css` N≥200, two windows opposite order over the CSS corpora (tailwind + 2 more). (5) Measure
residual LOC/substrate divergence (9B's kernels vs the evolved home) + charge 9A the de-face cost.
**PASS/FAIL:** zero `bl`/`blr` to the primitive in-loop AND CSS ratio ≥0.98× every corpus both windows
AND the divergence table emitted as data (LOC + `PaddedView`/clamp + `vqtbl1q`/`vqtbl4q` deltas). FAIL if
any in-loop indirect survives, or any CSS corpus <0.98×. Delivers LOCK-6's real input.

**BRIEF 2 — the 9C `[u8;256]→SIMD` emit-pass feasibility, against the REAL engine (pt-scratch).** The
automata tier is not vendored into skinny (reconciliation 5), so run against where the DFA actually lives:
a scratch copy of `parse-that/rust/regex` (package `bbnf-regex`; `automata/dfa.rs:62 Dfa::compile`,
`sets/equiv.rs:17 compute_byte_classes`) with a `[patch]` path-dep on `skinny/crates/bbnf-simd`. (1) For
three leaves — ws `[ \t\n\r]+`, the JSON structural set `{}[]:,"\`, and a lo6-COLLIDING CSS delimiter set
incl. `;`(0x3B)+`{`(0x7B) — compile each via `Dfa::compile_with`, extract `byte_classes:[u8;256]` +
`num_classes`. (2) Build the minimal emit harness: run the separability predicate (port
`bbnf_simd::dispatch::lo6_table_admissible`, `dispatch.rs:101`) on the derived alphabet —
separable→`vqtbl4q` lo6-table (`classify_tbl4`), non-separable→`byte_class_from_table_64` (full `[u8;256]`)
or the `structural_classify_64` `vceqq`-fan. (3) Assert **bit-identical scan output** vs a scalar reference
over black_box xorshift + an alignment sweep 0..63; an INJECT_BUG (flip one class entry) MUST break parity.
(4) Gate 4 (load-bearing): a naive lo6/nibble-only emit MUST **misclassify** the `;`+`{` set
(lo6(0x3B)=lo6(0x7B)=0x3B → drops one member), and the separability gate must route it OFF `vqtbl4q`. (5)
Record real `num_classes`/state counts vs the 255-class/4096-state ceilings incl. `\p{L}`→`None`.
**PASS/FAIL:** all three leaves emit a bit-identical classifier via the correct tier; the colliding set is
provably rejected off the nibble tier with NO silent drop; INJECT_BUG bites ≥90%. FAIL if the emit cannot
reproduce the hand classifier, or the naive form passes the colliding set silently. Proves the emit pass
buildable against the real DFA output — decoupling 9C's largest internal gap from the un-executed vendoring.
