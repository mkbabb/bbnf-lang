# Alpha-C REDRESS Digest For SK-V9 -> SK-V10

Date: 2026-05-19.

Role: PASS-ALPHA alpha-C REDRESS digest. This artifact classifies SK-V9
admissions, rejects, retirements, and pre-blocks for SK-V10.

## SK-V9 Cycle Entries

| Entry | Route | Disposition | Carry-forward |
|---:|---|---|---|
| 94 | W1 Apache/CITM measured typed-row admission | ADMIT | typed product plane is the live SOTA surface |
| 95 | W2 retained class/event grammar + `ValueRef` proof | ADMIT | proof stands; no row movement |
| 96 | W3 full class-column union substrate | REJECT | measurement evidence against union-substrate thesis |
| 97 | W3 allocation-free streaming cursor | REJECT | second faithful measurement against same thesis |
| 98 | W3 gate retirement | RETIRE | hard pre-block for SK-V10 |

Citation: `skinny/REDRESS.md:2731-2940`.

## Admissions

W1 admitted only the fresh measured typed rows for Apache/CITM. It did not admit
Canada typed rows or direct digest rows by analogy. The live W1 result is:

- `apache_builds/real_typed_struct`: 8174 Mbps Track 1 vs 8110 Mbps sonic typed
  strict (`skinny/RESULTS.md:15`).
- `citm_catalog/real_typed_struct`: 35102 Mbps Track 1 vs 22058 Mbps sonic typed
  strict (`skinny/RESULTS.md:10`).

W2 admitted the proof-only retained grammar/cursor contract. It moved no
`RESULTS.md` rows.

## W3 Measurements

REDRESS 96 implemented the full class-column substrate and move-consumed
`scan_structurals` vector. It was correctness/parity green, but missed all
W3/W10b floors:

| Row | Floor | Measured Mbps |
|---|---:|---:|
| `twitter` | 17685 | 9284 |
| `apache_builds` | 14124 | 7700 |
| `update_center` | 14370 | 6854 |
| `distinct_values` | 15731 | 6229 |
| `canada` | 15866 | 11221 |
| `citm_catalog` | 28630 | 13611 |
| `instruments` | 15865 | 9539 |
| `marine_ik` | 11831 | 8012 |
| `mesh` | 12186 | 10087 |
| `numbers` | 17596 | 13407 |

REDRESS 97 removed the full vector and used a streaming cursor over the aarch64
scanner. It again missed all W3/W10b floors:

| Row | Floor | Measured Mbps |
|---|---:|---:|
| `twitter` | 17685 | 7520 |
| `apache_builds` | 14124 | 6710 |
| `update_center` | 14370 | 5534 |
| `distinct_values` | 15731 | 5338 |
| `canada` | 15866 | 8293 |
| `citm_catalog` | 28630 | 9997 |
| `instruments` | 15865 | 7305 |
| `marine_ik` | 11831 | 5540 |
| `mesh` | 12186 | 6835 |
| `numbers` | 17596 | 9542 |

Rejected patches:

- `/tmp/skv9-waveW3-rejected.patch`
- `/tmp/skv9-waveW3-v2-rejected.patch`

## Hard Pre-Blocks

- No W3 union-substrate retry, split, rename, or class-column variant.
- No W4 cascade-lock that cites W3 as an entry gate or same-wave consumer.
- No parse-only SOTA admission while rows remain `S / NO-GO`.
- No Canada typed admission by analogy to W1 Apache/CITM.
- No direct digest row relabeled as typed product proof.
- No W4 source work without a fresh existing-substrate gate, scalar reference,
  checkasm where applicable, same-wave consumer, and W10b maintain block.

## Routes That May Survive Under Different Framing

- Typed product-row generalization, because W1 measured a real product-plane
  win and does not depend on W3.
- Existing-substrate unicode/string kernels, but only against current
  string-scanner or unicode-unescape call sites. Their old W3 consumer framing
  is retired.
- Comparator/report freshness as gate-only evidence.

## Alpha Disposition

REDRESS 98 is promoted to the SK-V10 pre-block ledger. W3 is falsified, not
blocked. The next contract must route around it.
