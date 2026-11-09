# PROTO-P2 — Fused RD parse+value as the default materialization lane (cold probe)

## The falsifiable question

Does making the emitted fused visitor lane (measured 0.99x sonic geomean **warm** in the
pre-restart AY corpus — 09:171-179) the default materialization path hold at or under ~1.10x
sonic-rs under **cold per-parse** discipline, with byte-equal value parity against the tape-then-walk
default it replaces?

## Verdict

**SUPPORTS — decisively, directionally.** Under cold per-parse measurement the fused lane runs at
**0.62–0.64x sonic-rs geomean** (it *beats* sonic on all four corpora, 0.46–0.84x), far inside the
1.10x gate. The tape-then-walk lane it replaces runs at **2.0x sonic** — a ~3.2x fused-vs-walk
reconstruction tax, confirming the corpus diagnosis that "the gap was never parse speed, it was the
tape-then-walk reconstruction default" (09:166-188). Byte-equal digest parity fused == tape-walk ==
sonic holds on twitter/citm/canada/gsoc-2018 (the parity `assert_eq!` runs before every timing and
the probe exits 0). The warm 0.99x claim not only survives cold, it strengthens — but the comparator
and load differ (below), so the figure is DIRECTIONAL, not a ranking.

## What "fused lane" and "tape-then-walk lane" are, concretely

The skinny JSON grammar is already a lazy-tape design. Two genuine materialization lanes exist:

- **Fused lane** = `runtime::generated_json::parse_direct(input, &mut sink)` — one recursive-descent
  pass driving a `JsonSink` directly, **no tape built**. The bench sink is
  `direct_struct::JsonDigestSink`; entry `direct_struct::track1_digest`
  (`crates/bbnf-bench/src/direct_struct.rs:421`).
- **Tape-then-walk lane** ("the old default", the `Parsed::to_value()` analog) = default `parse()`
  builds the full lazy offset tape, then a recursive walk over `root.value()` materializes the
  identical digest. This lane did not exist as a digest producer; **P2 added it** (diff below) so the
  two could be benched on identical work with byte-equal output.
- **Comparator** = `sonic_rs::from_slice::<JsonDirectDigest>` (`direct_struct::sonic_digest:436`) —
  sonic's serde direct-to-struct materialization (not its lazy skip path), the fair
  full-materialization opponent.

Byte-equality is structural, not coincidental: every scalar folds via `fold_child(scalar_digest)`,
which equals the sink's `fold_*_scalar` fast path because scalar digests carry `max_depth == 1`
(so `fold_child` computes `max(2)` and `mix(parent, mix(tag, value))` identically); number
i64/u64/f64 discrimination reuses `JsonDirectDigest::number_raw_known`, byte-identical to the sink's
`parse_number_direct` (generated.rs:1062-1076).

## Results (cold per-parse; median of 60, two runs)

Machine: Apple-M, `--profile ax-iter` (release opt-level 3, thin-LTO, cgu=16), mimalloc global
allocator (matches the committed json_parity harness). Cold discipline: input re-cloned to a fresh
heap buffer OUTSIDE each timed region (mirrors criterion `iter_batched(LargeInput)`); prior parse's
tape dropped before the next call. `cyc/B` derived at an **assumed 4.0 GHz** (no PMU access on
Apple-M without entitlements) — disclosed, not measured.

**loadavg during capture: 16.4–16.9** (heavy — many parallel agent builds). Far above the corpus's
"quiet <2" ranking bar (32 U1), so absolute ns and cyc/B are inflated; the fused/walk/sonic ratios,
measured in the same interleaved window and stable across two runs, are the load-robust signal.

| corpus | bytes | fused/sonic | walk/sonic | fused ns/B | sonic ns/B | walk ns/B |
|---|---:|---:|---:|---:|---:|---:|
| twitter | 631515 | **0.54 / 0.50** | 2.17 / 1.98 | 10.6 | 19.4 | 42.2 |
| citm_catalog | 1727204 | **0.47 / 0.46** | 1.42 / 1.33 | 5.8 | 12.2 | 17.3 |
| canada | 2251051 | **0.79 / 0.79** | 1.55 / 1.71 | 18.2 | 23.2 | 36.0 |
| gsoc-2018 `[data_xl]` | 3327831 | **0.84 / 0.84** | 3.46 / 3.80 | 11.8 | 14.1 | 48.9 |
| **GEOMEAN** | | **0.64 / 0.62** | **2.02 / 2.03** | | | |

(two cells per ratio = run-1 / run-2 median; per-corpus min-based ratios tracked medians within
±0.05, e.g. twitter fused/sonic min 0.547.) data_xl is absent from the skinny corpus; **gsoc-2018
(3.3 MB) substitutes** and is labelled as such.

## Reading the result against the success criteria

1. **Fused geomean <= 1.10x sonic cold?** YES — 0.62–0.64x, beats sonic on every corpus. The 0.99x
   warm claim survives cold and is re-narrated as **~0.63x** here — stronger, because the cold
   comparator is sonic's *serde direct-to-struct* (heavier than the lazy-DOM baseline behind the
   original 0.99x), and because the fused lane's win over its own tape-then-walk default (2.0x) is
   the same-substrate contrast the thesis actually predicts.
2. **Byte-equal parity vs the old lane?** YES — `assert_eq!(fused, tape_walk)` and
   `assert_eq!(fused, sonic)` pass on all four corpora before any timing.
3. **The reconstruction tax is real and is the lever.** Fused → tape-then-walk is a **~3.2x**
   slowdown (0.63x → 2.0x sonic). Promoting fused to default removes it. Note the skinny tape-walk
   (2.0x) is *milder* than history's `Parsed::to_value()` 3.5–5.2x (09:171-177): this tape is already
   lazy, so the "old default" penalty here is the lazy-view walk + re-materialization, not the old
   eager generic-tape reconstruction. The lever still points the same way.

## Caveats that bound the claim to DIRECTIONAL

- loadavg 16.4–16.9; a quiet <2 re-capture (TW0) is required before this ranks anything.
- `ax-iter` (thin-LTO) not the fat-LTO `bench`/`release` profile; ratios are LTO-robust but absolute
  cyc/B is not the fat-LTO floor.
- cyc/B computed at assumed 4.0 GHz, not PMU-measured.
- Comparator is sonic serde direct-to-struct; a fused-over-lazy-cursor lane must additionally be
  benched against sonic's *dual-path* direct-to-struct where a fold-over-cursor could lose (23 §1,§8)
  — this probe only proves the fused-sink default beats that path, not that a lazy-cursor fold does.
- The digest workload materializes numbers + strings + folds a structural fingerprint; it is a
  faithful full-materialization proxy, not a typed-struct or serde-Deserialize product surface.

## Decisive diff (resurrectable)

Added to `crates/bbnf-bench/src/direct_struct.rs` (the tape-then-walk lane, byte-equal by
construction):

```rust
fn tape_walk_value(value: &runtime::grammars::json::JsonValue<'_, '_>) -> JsonDirectDigest {
    use runtime::grammars::json::JsonValue;
    match value {
        JsonValue::Object(object) => {
            let mut digest = JsonDirectDigest { objects: 1, max_depth: 1, fingerprint: 0x7b, ..JsonDirectDigest::default() };
            for pair in object.pairs() {
                digest.members += 1;
                digest.fold_key(pair.key().as_str().as_ref());
                digest.fold_child(tape_walk_value(&pair.value()));
            }
            digest.fingerprint = mix(digest.fingerprint, digest.members);
            digest
        }
        JsonValue::Array(array) => {
            let mut digest = JsonDirectDigest { arrays: 1, max_depth: 1, fingerprint: 0x5b, ..JsonDirectDigest::default() };
            for elem in array.values() {
                digest.elements += 1;
                digest.fold_child(tape_walk_value(&elem));
            }
            digest.fingerprint = mix(digest.fingerprint, digest.elements);
            digest
        }
        JsonValue::String(s) => JsonDirectDigest::string(s.as_str().as_ref()),
        JsonValue::Number(number) => {
            let bytes = number.raw().as_bytes();
            let span = match_number_span_from_first(bytes, 0, bytes[0]).expect("validated number");
            JsonDirectDigest::number_raw_known(bytes, &span)
        }
        JsonValue::Bool(v) => JsonDirectDigest::bool(v.value()),
        JsonValue::Null(_) => JsonDirectDigest::null(),
    }
}
pub fn track1_tape_walk_digest(input: &str) -> Result<JsonDirectDigest, DirectStructError> {
    let root = runtime::generated_json::parse(input).map_err(|e| DirectStructError::Parse(e.to_string()))?;
    Ok(tape_walk_value(&root.value()))
}
```

The cold harness is `crates/bbnf-bench/src/bin/p2_fused_cold.rs` (full source in the worktree):
loads twitter/citm/canada/gsoc-2018, asserts fused == walk == sonic byte-equal, then times each lane
cold (fresh clone per sample, median + min), prints per-corpus ratios and geomean. Build+run:
`cargo build -p bbnf-bench --bin p2_fused_cold --profile ax-iter && ./target/ax-iter/p2_fused_cold 60`.

## Recommendation for TW3

Promote the fused single-pass (`parse_direct` + sink) to the DEFAULT materialization lane; keep the
tape as the lazy/random-access substrate, not the eager path. The wave's consumer-invariant gate
should assert (a) byte-equal fused-vs-tape-walk over the corpus (the parity used here) and (b) a
quiet-load cold fused/sonic <= 1.10x re-capture. Next open sub-question this probe did NOT answer:
fused-fold-**over-the-lazy-cursor** (not a bespoke sink) vs sonic's dual-path direct-to-struct.
