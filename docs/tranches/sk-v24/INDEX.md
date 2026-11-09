# SK-V24 TRANCHE CODEX — INDEX (the implementation program of record)

Ten implementation-depth tranches specifying THE implementation — the totality tree of
record, not a skinny pass — for bbnf-lang (GN-0..6) and parse-that (PT-1..3, FULL
IMPLEMENTATION per the owner supersession of 2026-07-16, recorded at SPEC §2b). Binding
skeleton: `restart/skinny/tranches/sk-v24/SPEC.md` (re-homed to
`docs/tranches/sk-v24/evidence/` by GN-0.W2). Every load-bearing performance claim traces to
a certified artifact in `prototypes/PROVE-XX/{REPORT.md, patch.diff, VERIFY.md}`.

Provenance: authored by a 10-agent fleet, audited by 3 independent auditors (interlock,
precepts, gate quality — 31 findings: 2 CRITICAL, 16 MAJOR, 13 MINOR), all CRITICAL/MAJOR
fixed by a surgical fix fleet, MINORs harmonized in a final pass. The codex is
precept-compliant: one challenge wave per tranche, Y.13 consumer-in-wave throughout, zero
grammar-name gates, colocation trees concrete, every gate carries an exact command + numeric
threshold + can-fail semantics.

## The tranches

| Doc | Tranche | Waves | LOC (±) | Gates (RED/verified) |
|-----|---------|-------|---------|----------------------|
| [GN-0](GN-0.md) | The Great Archival + x86 Purge | 5 | +150/−54 (+~185-202G reclaimed, tag-preserved) | 4/1 |
| [GN-1](GN-1.md) | The Loom — greenfield workspace, organ port-in, patch landing | 5 | +4,320/−2,700 atop ~22k relocated | 1/4 |
| [GN-2](GN-2.md) | The Kernel Plane — prefilter, strings, CSS redress, movemask, canada float | 6 | +965/−205 | 6/0 |
| [GN-3](GN-3.md) | The Type Plane — the three walls fall; the hand schema dies | 5 | +1,120/−1,085 | 4/1 |
| [GN-4](GN-4.md) | The Value Fabric — path get, key scanner, direct-to-struct, TS seed | 6 | +3,070/−130 | 3/3 |
| [GN-5](GN-5.md) | Generality + The Fleet — CSV release re-proof, TOML, name-gate CI | 5 | +3,340/−1,040 | 8/2 |
| [GN-6](GN-6.md) | The Shopwindow + Close — playground, four documents, recapture | 5 | +9,850/−0 | close gates born-RED |
| [PT-1](PT-1.md) | The Regex Engine SOTA Uplift (parse-that) | 8 | +1,240/−289 | 5/3 |
| [PT-2](PT-2.md) | The Rust Core Reform (parse-that; zero perf claims by law) | 7 | +982/−406 (+576 net if arena lands) | 4/3 |
| [PT-3](PT-3.md) | The TS Backbone 1.1.0 (parse-that/typescript) | 7 | +406/−120 | 3/4 |

Program: ~25.4k newly authored + ~22k relocated ≈ 47k LoC — coherent with the owner's ~50k
sizing. The dominant single item is the GN-6 shopwindow (~7.3k, staged in three sub-waves).

## Execution order

```
GN-0 ──► GN-1 ──► { GN-2 ∥ GN-3 } ──► GN-4 ──► GN-5 ──► GN-6
                    (serialize writes to crates/passes/src/lib.rs:
                     GN-2.W1 derive_recognizers vs GN-3.W2 infer_expr)

PT-1 ──► PT-2 ──► PT-3          (each opens with the W0 handshake;
                                 PT-1.W0 and PT-2.W0 are joint)
```

Cross-repo interlocks (stated in both documents):
- **GN-2.W1 ⇄ PT-1.W5** — one generator dispatch, one lowering per scan site:
  self-loop → `PrefilterSet::find_member` (bbnf-simd), regex-leaf → `dfa.find` (bbnf-regex);
  never both.
- **GN-1.W3 ⇄ PT-1.W1** — the vendored parse-that-regex pins pre-PT-1 at port time; a named
  GN-1.W3 follow-up re-vendors once PT-1.W1 lands so the greenfield inherits `Dfa::find`.
- **GN-4.W4 ⇄ PT-3** — complementary, not gating: the TS tape decoder seeds the TS backend
  standalone; PT-3 productionizes the combinator backbone the wider backend composes atop.
- **GN-0.W3 ⇄ PT letter** — legacy `crates/` removal notes the egraph patch-table repoint in
  BBNF-PT-G.

## Owner gates

1. SPEC §3 checkboxes (defaults proceed absent veto): letter framing; parse-that
   `release.yml` aarch64-only (sole home **PT-2.W5**); GN-0 timing (immediate).
2. **BBNF-PT-G-2026-07-16** placement — owner or parse-that-side session ONLY — and the PT
   W0 handshake acknowledgments.
3. GN-5.W3 dispatch waits on GN-3 green (direct-to-struct family stability).

## Standing law (by pointer)

The profiling standing law, x86 abrogation, colocation grand edict, extreme parsimony,
regen-only for generated files, one-regex-home, and the refutation laws (no digit-scan SIMD,
no block-scheduling pursuit, no stable-port perf claims, fold-over-cursor dead,
tape-navigation-get reconciliations as stated per-tranche, no warm benches) — all at SPEC §0
and §4. Durability during execution: the CAMPAIGN-STATE resurrection-ledger pattern with an
hourly silent-green heartbeat.
