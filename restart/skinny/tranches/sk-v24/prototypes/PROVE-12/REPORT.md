# PROVE-12 — EMIT-SPINE RESIDUALIZATION (F3)

**Verdict: GO.** The generator now const-evals + beta-reduces the CSS
component-value delimiter test at generation time. The emitted recognizer's hot
per-byte membership check is straight-line, branchless, const-folded Rust with
zero runtime slice dispatch. Measured on `css_canon_bench` (the certified CSS
instrument): every corpus, every lane, fewer instructions/byte (3.7-6.4%) and
lower cycles/byte (ratio 0.81-0.98x). Generated LOC delta **+18** (910 -> 928),
well within budget. Parity: byte-identical.

Worktree: `/Users/mkbabb/Programming/bbnf-lang/.claude/worktrees/wf_386900ed-462-12`
- base commit `d2b32f6e3` (the sk-v22 graduation-baseline runtime — the tree the
R2 baseline numbers were captured against). Host aarch64 (NEON + scalar oracle;
no x86). All cargo confined to the worktree tree.

---

## 1. The pick — justified from the R2 lane profile

R2 certifies CSS track1 as the >SOTA lane (2.07-3.47x vs lightningcss) and both
CSS bench workloads — `track1_full_parse` and `track1_rich` — route through one
scan loop: `CssFullParser::find_component_delim` in the generated
`css_l4_declaration_values` grammar. That loop was the single clearest piece of
**interpretive dispatch** left in the emitted recognizer:

```rust
fn find_component_delim(&self, mut pos, delimiters: &[u8]) -> ... {
    while pos < self.bytes.len() {
        let byte = self.bytes[pos];
        if delimiters.contains(&byte) { return Ok(Some((byte, pos))); }  // <-- runtime
        pos = match byte { ... };
    }
}
```

The delimiter set is a **grammar constant** at each of its 5 call sites
(`b";{}"`, `b"{};"` x2, `b";}"`, `b":{};"`), yet it was threaded as a runtime
`&[u8]` and re-scanned with `<[u8]>::contains` on **every input byte** — a
data-driven loop with length guards, run over the whole stylesheet. JSON's hot
rules (string/number) already dispatch straight to hand-written kernels — nothing
to residualize there. This was the residualization target.

## 2. What I built — the generator mutation

The CSS recognizer is emitted from the raw-string template `CSS_GENERATED_RS` in
`crates/codegen/src/runtime_generator.rs` (that template **is** the generator;
`cargo xtask regen-css` writes `generated.rs` from it). Three edits, single-source:

1. **`find_component_delim` is now `const`-generic over a `u128` ASCII delimiter
   bitmask.** The membership test residualizes to one const shift-and-test:
   ```rust
   fn find_component_delim<const DELIMS: u128>(&self, mut pos) -> ... {
       ...
       if byte < 128 && (DELIMS >> byte) & 1 != 0 { return Ok(Some((byte, pos))); }
   ```
   (The `byte < 128` guard is load-bearing: a shift >= 128 on a `u128` masks the
   shift amount and would false-match; the guard also preserves the original
   semantics — every delimiter is ASCII, so high bytes are never delimiters.)
2. **A generation-time const-fold helper** `const fn delim_mask<const N>(&[u8; N])
   -> u128` folds each grammar delimiter set into its bitmask.
3. **All 5 call sites** pass the mask as a const generic:
   `find_component_delim::<{ delim_mask(b";{}") }>(self.pos)`.

The delimiter test is **pluggable** (the set is a parameter, not a hardcoded
per-site branch — MEMORY: pluggable-components), and residualization is done at
generation time (const-eval the mask) + monomorphization (each site beta-reduces
the membership to its own const test), not deferred to LLVM luck.

Diff summary (identical hunks in template + regenerated file — clean-regen
discipline holds):

```
 crates/codegen/src/runtime_generator.rs                       | 34 +++++++-----
 crates/runtime/.../css_l4_declaration_values/generated.rs     | 34 +++++++-----
 2 files changed, 52 insertions(+), 16 deletions(-)
```

## 3. Visual proof — before/after disassembly of the hot per-byte test

Emitted symbol: `CssFullParser::find_component_delim`. Only **one** monomorph
survives as a symbol — mask `0x28000000000000000800000000000000` = bits
{59 `;`, 123 `{`, 125 `}`} = the `";{}"` set shared by 3 call sites; the `;}` and
`:{};` monomorphs were **fully inlined** into their callers (deeper residualization).

**BEFORE** (`delimiters.contains(&byte)` — interpretive dispatch; slice pointer in
x22, length in x21):
```asm
ldrb  w8,  [x26, x2]     ; byte = bytes[pos]
cmp   w27, w8            ; == delimiters[0]
b.eq  ...                ; return delimiter
ldrb  w9,  [x22, #0x1]   ; load delimiters[1]      <-- runtime slice load
cmp   w9,  w8
b.eq  ...
cmp   x21, #0x2          ; if delimiters.len()==2  <-- runtime LENGTH guard
b.eq  ...
ldrb  w9,  [x22, #0x2]   ; load delimiters[2]      <-- runtime slice load
cmp   w9,  w8
b.eq  ...
cmp   x21, #0x3          ; if delimiters.len()==3  <-- runtime LENGTH guard
b.eq  ...
ldrb  w9,  [x22, #0x3]   ; load delimiters[3]      <-- runtime slice load
cmp   w9,  w8
b.eq  ...
```

**AFTER** (const-folded mask; the constant is an immediate — no memory, branchless):
```asm
mov   x27, #0x2800000000000000  ; delimiter mask baked in as immediate
...
ldrsb w9,  [x25, x2]            ; byte = bytes[pos]
tbnz  w9,  #0x1f, ...           ; byte < 128 guard (high-bit test)
lsl   x9,  x26, x8              ; 1 << byte      (x26 = 1)
tst   x8,  #0x40 / csel / csel  ; select the u128 half by bit 6 of byte
and   x9,  x9, x27              ; & mask (high half {,},})
and   x10, x10, #0x800000000000000 ; & mask (low half ;)
orr   x9,  x10, x9
cbnz  x9,  ...                  ; in-set -> return delimiter
```

The 3 slice loads (`ldrb [x22,#N]`) and 2 length guards (`cmp x21,#...`) are gone;
the delimiter set collapses to two immediate `and` masks. That is the
straight-line residual.

## 4. Measured — css_canon_bench, PMU (cyc/B) + instructions/B

Certified metric = PMU cyc/byte (per-process `ri_cycles`, load-robust);
instructions/byte is deterministic (windows agree to <0.2%) and is the load-proof
truth. **Baseline vs modified binaries interleaved back-to-back per window** (a
paired block design; CSS has no built-in paired mode), N = 3 windows x iters=800,
4 corpora x 2 track1 lanes. loadavg per window: W1 ~13.5, W2/W3 13-19. Baseline
binary preserved and re-run against the modified binary under identical load.

| corpus | lane | N | loadavg | base cyc/B | mod cyc/B | cyc/B ratio | base ins/B | mod ins/B | ins ratio |
|---|---|---|---|--:|--:|--:|--:|--:|--:|
| bootstrap | track1_full | 3x800 | 13-19 | 17.736 | 15.045 | **0.848** | 55.466 | 52.556 | 0.9475 |
| bootstrap | track1_rich | 3x800 | 13-19 | 19.083 | 15.493 | **0.812** | 66.910 | 63.974 | 0.9561 |
| tailwindcss | track1_full | 3x800 | 13-19 | 14.504 | 13.841 | **0.954** | 53.243 | 49.855 | 0.9364 |
| tailwindcss | track1_rich | 3x800 | 13-19 | 14.860 | 13.873 | **0.934** | 68.984 | 65.539 | 0.9501 |
| material | track1_full | 3x800 | 13-19 | 14.062 | 13.761 | **0.979** | 47.694 | 45.352 | 0.9509 |
| material | track1_rich | 3x800 | 13-19 | 15.889 | 14.438 | **0.909** | 64.380 | 61.982 | 0.9627 |
| animate | track1_full | 3x800 | 13-19 | 16.418 | 15.678 | **0.955** | 59.842 | 57.173 | 0.9554 |
| animate | track1_rich | 3x800 | 13-19 | 15.387 | 14.834 | **0.964** | 67.815 | 64.811 | 0.9557 |

(cyc/B = median of 3 windows; ins/B deterministic.)

**Every corpus, every lane wins.** Instructions/byte drop 3.7-6.4% (mean ~ 4.8%)
— deterministic, load-proof. cyc/byte ratio is < 1.0 across the board (strongest
on bootstrap at 0.81-0.85x => 1.18-1.23x faster; the rest 0.91-0.98x), and moves
in lockstep with the instruction reduction. cyc/B carries load noise (why bootstrap
— the heaviest delimiter-scan corpus — shows the cleanest signal); the ins/B column
is the certified floor.

## 5. Parity

`css_canon_bench`'s `CSS_CANON_PROFILE` mode XOR-folds every parse result across
all 4 corpora into a checksum. Baseline vs modified:

| lane | baseline cksum | modified cksum | |
|---|---|---|---|
| track1_full_parse | 1 | 1 | MATCH |
| track1_rich | 17839 | 17839 | MATCH |

Byte-identical output. The transform is provably semantics-preserving: for
`byte < 128`, `(DELIMS >> byte) & 1` is 1 iff `byte in set` (same as `contains`);
for `byte >= 128`, the guard is false and every delimiter is ASCII, so `contains`
is also false.

## 6. Generated-size budget

`generated.rs`: 910 -> 928 lines (**+18**: the `delim_mask` const fn + the
membership comment, minus the dropped `delimiters` parameter). No O(N) generator
regression; the monomorphs are compiler-emitted, not template-duplicated.

## 7. Integration cost

**Near-zero — already integrated.** The change lives entirely in the CSS emit
spine (`CSS_GENERATED_RS` template) and is exercised by the live product path:
`parser::parse_full` / `rich_summary` -> `parse_into_tape` -> `parse_stylesheet` ->
`find_component_delim`. `cargo xtask regen-css` reproduces `generated.rs` bit-for-bit
from the mutated template (regen clean, `check-css-l4-declaration-values` passes).
The pattern generalizes to any grammar-derived delimiter/STOP-set scan (JSON's
structural scan, general-descent) — the same `const fn mask` + const-generic seam.

## 8. Reproduce

```
cd <worktree>/skinny
cargo run -q --bin xtask -- regen-css                          # regen from template
cargo build --release -p bbnf-bench --bin css_canon_bench      # fat-LTO
CSS_CANON_PMU=1 ./target/release/css_canon_bench 800           # cyc/B + ins/B
CSS_CANON_PROFILE=track1_rich ./target/release/css_canon_bench 1  # parity cksum
```
