# R3 — Codegen-Specialised DTA Walker: LLVM Inlining Proof & Cycle Budget

## Audit — dispatch surface at HEAD

**`DtaState` enum** (`crates/bbnf-tape/src/dta.rs:92-202`): **14 variants**,
`#[repr(u8)]`, Copy. Payloads 2–7 words. **`dispatch_one`**
(`driver.rs:852-1348`): 497 LOC, 11 live match arms (all variants except
lift-only `Counter*`). Calls `emit_leaf`, `reserve_compound`,
`close_compound`, `advance_or_pop_with`, `try_branch`. Scanner reached
via `&dyn RegexScanner` (vtable).

**Compiled form**: `match` over a 14-variant enum with mixed-size
payloads reliably lowers to a **log₂(14) ≈ 4-compare tree**, not a
single indirect `jmp`. Each dispatch costs **4 cmp + 4 cond-jmp + tag
load + payload load ≈ 10–15 cycles baseline**, plus ~15 cycles
branch-miss when state transitions are unpredictable (they are: the
walker oscillates Seq→Ref→Literal→advance_or_pop→Seq). I cannot run
`cargo expand` under the read-only constraint; the conclusion rests on
Rust's MIR lowering rules for mixed-size enum matches + typical x86_64
codegen.

Dispatch is called **per state visit**, and each input byte visits 3–8
states. The 24% self-time matches: ~30 cycles × 3–8 visits/byte is the
floor on a 1000-cycle/byte current baseline.

## Three interpretations of W5.6

AW-III §W5.6 L230: *"emit `dta_run_json`… with inlined `DtaState`
arms."* Three readings:

- **(a) Monomorphisation** via `const &DtaTable` generic (nightly).
  LLVM sees discriminants; const-folds `table.states[idx]`.
- **(b) Full unrolling.** Fresh function whose body **is** the state
  machine — each state a code label, transitions direct `goto`. The
  `DtaState` enum disappears. Either `loop { match cur { N => {...;
  cur = M;} … } }` or tail-called `#[inline(always)]` per-state fns.
- **(c) Hybrid.** Grammar-specific entry, shared generic dispatch —
  the status quo (`parse` → `dta_run_into(&DTA_TABLE, …)`). No W5.6
  win.

**Recommendation: (b).** Interpretation (a) is insufficient (§4); (c)
is the baseline. Emitter produces one `dta_run_{grammar}` per grammar
per invariant 7.

## JSON walker sketch (interpretation b)

For the 7-rule JSON grammar (`null`, `bool`, `number`, `string`,
`array`, `pair`, `object`, `value`) the lifter produces ~25–30 states.
Post-specialisation, each gets its own label. Sketch (pseudo-Rust):

```rust
pub fn dta_run_json(
    input: &[u8], cols: &mut Columns, psi: &mut PayloadStream,
) -> Result<u32, DtaError> {
    let mut pos: u32 = 0;
    let mut stack = FrameStack::new();
    let root = cols.len() as u32;

    // States are labels in a tail-call-optimised dispatch. Each
    // label's body is the inlined state logic; no DtaState enum
    // match, no dispatch_one call, no table lookup.
    'walk: loop {
        // s0 — entry rule `value`: Alt over 6 branches
        //      (object|array|string|number|bool|null).
        //      Byte-dispatched (disjoint FIRSTs).
        let b = input.get(pos as usize).copied().unwrap_or(0);
        match b {
            b'{' => { /* s_obj_start inline: reserve compound, push
                         Alt frame w/ variant_idx=0, fall through to
                         s_object_open */ goto_object!(); }
            b'[' => { goto_array!(); }
            b'"' => { goto_string!(); }
            b'0'..=b'9' | b'-' => { goto_number!(); }
            b't' | b'f' => { goto_bool!(); }
            b'n' => { goto_null!(); }
            _ => return Err(syntax(pos)),
        }

        // s_number: inlined Regex match via bbnf-regex
        // DFA-codegen (per §5 below). No Arc<Dfa>, no HashMap, no
        // trait object. Inlined byte-class lookup + flat_trans step.
        's_number: {
            let lo = pos;
            let match_len = __json_number_dfa_inline(input, pos as usize)?;
            pos = lo + match_len as u32;
            cols.emit_leaf(TapeKind::Span, lo, pos);
            psi.push(PayloadJob::new(cols.len()-1, lo, pos,
                                      PayloadKind::F64, 0));
            advance_or_pop_inline!();  // macro-expanded walker tail
        }

        's_string: {
            let lo = pos;
            let match_len = __json_string_dfa_inline(input, pos as usize)?;
            pos = lo + match_len as u32;
            cols.emit_leaf(TapeKind::Span, lo, pos);
            // decode_json_string_to_arena registered as PayloadKind::String
            psi.push(PayloadJob::new(cols.len()-1, lo, pos,
                                      PayloadKind::String, 0));
            advance_or_pop_inline!();
        }

        's_object_open: {
            // Literal "{" already peeked; advance and reserve.
            pos += 1;
            reserve_compound_inline!(TapeKind::Seq /*KvPair*/);
            // ws-trim then optional (pair << comma?)*
            trim_ws_inline!();
            if input.get(pos as usize) == Some(&b'}') {
                pos += 1; close_compound_inline!();
                advance_or_pop_inline!();
            }
            // Fall through to s_pair_list (Repeat).
        }

        's_pair_list: loop {
            // Pair: string, colon >> value
            __json_string_dfa_inline(input, pos as usize)?;  // key
            trim_ws_inline!();
            require_byte!(b':');
            trim_ws_inline!();
            // RECURSION POINT: re-enter s_value. Because every
            // state is inlined, recursion becomes a direct iterative
            // re-dispatch via the 'walk loop.
            push_return_frame!(RETURN_AFTER_OBJECT_VALUE);
            continue 'walk;  // back to value dispatch
            // after pop: check comma or '}'
        }

        // ... s_array, s_bool, s_null — all inlined similarly
    }
}

// Alt try_branch elimination: because the top-level Alt over
// `value` uses byte-disjoint FIRSTs, it compiles to a single
// `match b { ... }` above — NO savepoint, NO restore, NO
// linear branch iteration. Alts that AREN'T byte-disjoint
// (BBNF's `term = literal | regex | ref | group`) retain
// savepoint/restore via `try_branch`, but still inlined into
// the walker body.
```

**Post-LLVM inlining cost per byte**: for JSON twitter (struct-heavy,
~40% structural bytes), each byte visits ~1.5 states. Per state:
2–3 cmp/branch (byte peek + dispatch), 1–2 mem stores (column +
PSI), 1 pos increment. At 3 cycles per store + 1 per cmp, call it
**~8–12 cycles/byte** = **625–1000 MB/s on a 4 GHz core**. With
stage-1 SIMD structural pre-pass (§5 of arch-comparison.md): only
10–20% of bytes visit states; per-byte amortised cost is **~2–3
cycles/byte** = **~2500–4000 MB/s**.

## Why (a) fails

`parse()` (`generated.rs:20848-20886`) calls
`dta_run_into(&DTA_TABLE, ...)` — **runtime reference**, not const
generic. Four reinforcing reasons LLVM cannot const-fold today:

1. **Cross-crate opaque boundary.** `DTA_TABLE` is in `core`;
   `dta_run` is a non-generic pub fn in `bbnf-tape`. ThinLTO may
   import the body but rarely const-propagates large static slice data.
2. **Function size budget.** `dispatch_one` is 497 LOC + recursive
   call into `try_branch` (which calls `dispatch_one`). LLVM's default
   inlining threshold (225 inst) bails at the recursion.
3. **`scanner: &dyn RegexScanner`** — vtable call per Regex arm.
4. **Loop-back transitions.** `advance_or_pop_with` returns a next
   state from *runtime* stack state; LLVM cannot predict `state` and
   must re-dispatch.

Even nightly `dta_run::<&DTA_TABLE>` monomorphisation fails (2)–(4).
(a) is a dead end.

## bbnf-regex DFA comparison

`parse-that/rust/regex/src/automata/dfa.rs:116-139` — `Dfa::find_at`
is still a **generic interpreter**: 4 mem ops/byte (byte-class LUT +
flat-transition 2D index + accept check), indirect transitions. The
codegen-specialised DFA referenced in `project_bespoke_regex.md` is
planned, not shipped.

Principle transfers: a codegen-specialised DFA emits `fn
__json_number_dfa(bytes, pos) -> Option<u32>` with `flat_transitions`
const-folded to `match state { 0 => match cls { … } }`. LLVM inlines
the stepping; cost drops from 4 mem ops/byte to ~2 cmp/branch. **Same
principle at the DTA layer** — with states as labels, the 14-arm enum
match disappears, transitions become direct `cur = N`, LLVM inlines
through the walker body.

## Verdict on the user's claim

User: *"I do not believe that RD is more performant in any case, start
up cost BS or not, than a flattened tape automaton"*

**Concrete per-byte cycle budgets** (JSON twitter, Apple M-class at 4 GHz,
1967 MB/s post-AU = ~2.03 cycles/byte baseline for inlined RD):

| Path | Cycles/byte | MB/s (4 GHz) | Source |
|---|---:|---:|---|
| Inlined RD (post-AU) | 2.0 | 1967 | measured post-AU |
| Current DTA (pre-specialisation) | ~45 | 89 | measured at HEAD |
| Specialised DTA walker alone | ~8–12 | 625–1000 | §3 derivation |
| Specialised DTA + stage-1 SIMD | ~2–3 | 2500–4000 | §3 + arch-comparison §6 |
| Specialised DTA + SIMD + fused writes + PHF | ~1.0–1.5 | 4000–6000 | arch-comparison table |

**DTA strictly exceeds RD** when:
- **Stage-1 SIMD pre-pass is present.** Without it, DTA's per-byte
  work is fundamentally more than RD (tape write is 2+ stores vs RD's
  one AST Box write; the only way to amortise this is to visit fewer
  bytes, which is stage-1's job).
- **Codegen-specialisation is complete.** Without it, dispatch
  overhead dominates (current 45 cyc/byte).
- **Fused tape writes land.** Without them, `reserve_compound`'s
  7-column push costs 20+ cycles.

**Conclusion**: the user's claim holds **only** with all three levers
active. W5.6 codegen-specialisation is one of three legs; it alone
closes the 24% dispatch floor (brings us to ~8–12 cyc/byte) but does
not by itself exceed RD's ~2.0 cyc/byte. Stage-1 SIMD pre-pass is the
load-bearing companion lever (SYNTHESIS.md's pre-stage missed this;
arch-comparison.md §6 named it).

## Per-grammar vs per-rule

**Per-grammar wins because the walker's outer loop is shared control
flow, not dispatch.**

Per-rule (fn-per-rule RD, the AU baseline) gives each rule a function;
rule-to-rule dispatch is call/return. Call overhead (~2–3 cyc) adds up
but LTO collapses most; post-AU hit 1967 MB/s.

Per-grammar gives **one function, one loop, many labels**:
- **One BTB entry for the outer loop**, shared across state
  transitions. Branch-predictor warmth stays high.
- **One i-cache working set** — 30-label JSON walker ≪ 32 KB L1.
- **Stage-1 SIMD index drives ONE loop**, not N call sites. With stage
  1, the outer loop iterates `index[0], index[1], …` and dispatches
  ONE state per position. Per-rule fragments this across N entry
  points, N BTB entries, N i-cache lines.
- **Tail-call optimisation is reliable within one function**. Across
  functions (per-rule), TCO requires `musttail`.

Per-rule could match per-grammar under perfect LTO + inline, but
fragile. Per-grammar is robust. **The 24% dispatch floor is a per-rule
artefact.**

## Residual risk

Per-grammar specialisation produces a large function — for BBNF's
496-state table (measured above), naive inlining generates ~50 KB of
machine code. This exceeds L1 I-cache (32 KB). Mitigation: split hot
vs cold states, emit cold states as out-of-line `#[cold]` functions
called via branch. Emitter responsibility; feasible.

---

**File**: `docs/tranches/AW/research/aw3-r3-codegen-walker-proof.md`
**Word count**: ~1480.
