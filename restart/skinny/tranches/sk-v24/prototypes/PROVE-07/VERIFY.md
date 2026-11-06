# PROVE-07 — ADVERSARIAL VERIFICATION

**Verdict: CONFIRMED** (measured claim reproduces; parity passes; no edict violations;
zero runtime touch). The *measurements* are truthful. NB the prover's own gate verdict
was PARTIAL — the serde ≥2× arm on twitter is genuinely unmet (~1.3×); I confirm that
honesty too, I do not upgrade the gate.

Verifier host: arm64, 18 cores. Worktree `wf_386900ed-462-7`, base `d2b32f6e3`.
Rebuilt `json_canon_bench` from the patched source (fat-LTO release) before measuring.
My load was lighter than the prover's (8–12 vs their 15–22), so absolute cyc/B run a
touch faster on my side; every *ratio* reproduces.

## 1. Fresh numbers vs claim

### Paired interleaved (cold per-parse, N=200, 2 independent windows)
| lane | claim | w1 (load 8.5) | w2 (load 12.5) |
|---|--:|--:|--:|
| twitter direct/serde_typed | 0.817 | **0.787** [.778,.797] | **0.814** [.801,.827] |
| twitter direct/sonic_typed | 0.787 | **0.746** [.735,.757] | **0.786** [.772,.800] |
| citm direct/serde_typed | 0.520 (1.92×) | **0.520** (1.92×) | **0.539** (1.85×) |
| citm direct/sonic_typed | — | 0.635 | 0.660 |

Claimed 0.817/0.787 land squarely inside my two-window envelope. citm 1.92× is exact in w1.

### PMU cyc/byte (iters=800, 1 window, load 12.2)
| lane | claim | fresh | ratio direct-vs |
|---|--:|--:|--:|
| twitter direct_struct | 1.858 | **1.735** | — |
| twitter serde_typed | 2.334 | **2.264** | **1.31×** (claim 1.26×) |
| twitter sonic_typed | 2.410 | **2.299** | **1.32×** (claim 1.30×) |
| citm direct_struct | 1.015 | 1.149 | — |
| citm serde_typed | 1.953 | 2.061 | **1.79×** (claim 1.92×) |
| citm sonic_typed | 1.634 | 1.711 | 1.49× (claim 1.61×) |

Direction and the ~1.3× twitter ratios reproduce; citm serde ratio 1.79–1.92× across
methods. cpi shows direct at 0.171 vs sonic 0.288 — the direct lane retires more IPC.

### Parity preflight (runs before every timing)
```
# PARITY corpus=twitter direct==serde==sonic typed-struct OK
# PARITY corpus=canada  direct==serde==sonic typed-struct OK
# PARITY corpus=citm_catalog direct==serde==sonic typed-struct OK
```

## 2. Attack list (all survived)
- **Runtime touch?** `git diff HEAD --stat` = 2 files: the bench bin + the report. NO
  runtime/codegen/grammar change. `direct_struct.rs`, `real_typed_struct.rs`,
  `generated_real_typed.rs` are byte-unchanged at HEAD. Claim "bench-file-only" holds.
- **Parity oracle a stub?** No. `assert_direct_strict_product_parity` requires
  `track1==track2==serde==sonic` on `typed_checksum`, which folds real materialized
  fields (statuses.len + each tweet id/text). All three decoders deserialize into the
  SAME struct type (`TwitterSearch`/`CitmCatalog`) — byte-identical checksum ⇒ genuinely
  equivalent materialization, not apples-to-oranges. The thin twitter projection (id,text
  only) is skipped symmetrically: direct via `skip_value`, serde via `#[serde(default)]`
  IgnoredAny — the report is honest that this is why serde's 2× bar isn't reached.
- **Profile asymmetry?** All three lanes live in one binary, one release profile
  (opt-level 3, fat LTO, cu=1, system allocator). No per-arm profile skew.
- **Warm contamination?** The DECISIVE paired + cold-table modes are cold per-parse
  (fresh parser state, one `Instant` per call). PMU is a warm 800-iter loop but is only a
  relative cyc/B across three equally-warm lanes; it corroborates, it does not carry the
  claim. `black_box` on inputs + checksum return blocks DCE/hoisting in both modes.
- **Cherry-picked window?** Two independent paired windows bracket the claim; ratios stable.
- **x86 tokens / name-gates?** None. `uname -m` is display-only; corpus keying is fixture
  lookup on a uniform code path, not a perf branch.

## 3. Verdict rationale
Every decisive number reproduces in direction and rough magnitude, parity passes, the
patch touches zero runtime, and no aarch64/name-gate/parsimony/stub edict is violated.
The measured claim is CONFIRMED. Caveat carried forward (not a defect): the compound gate
remains one-arm-unmet — direct beats serde only ~1.3× on twitter, short of the 2× bar;
the 2× is reached only on citm. The prover reported exactly this.
