# AUDIT-C — AZ-I / AZ-II coherence + sequencing under bootstrap-wall reality

**Auditor**: AUDIT-γ — read-only coherence + sequencing audit, AY-II → AZ-I → AZ-II → BA → BB.
**Date**: 2026-04-25.
**Scope**: AZ-I.W0 forwarded-debt acceptance; whether its prescription
defeats the > 600 s `bbnf-bootstrap` cold expansion wall surfaced in
B1.W0; downstream AZ-II / BA / BB coherence under the recommended
sequencing.

## 1. Headline verdict

**AZ-I.W0 as currently written does not fix the cold expansion wall.**
The wave's two declared mechanisms — derive-cache relocation to
`$XDG_CACHE_HOME/bbnf-derive/` and Watt / WASM-precompiled
proc-macros — both reduce *cumulative* dev-loop overhead. Neither
reduces the work `bbnf_derive` performs *inside a single rustc
expansion of the bootstrap crate*, which is the > 600 s critical path
that B1.W0 measured and routed forward (`B1/FINAL.md:175`). On a
cold-key first run, both mitigations skip a cache miss path that does
not exist (the cache is written *as a result of* a successful
expansion, not consumed *to avoid* one), and Watt eliminates rustc's
re-link of the proc-macro crate but leaves the proc-macro's runtime
inside rustc untouched. The 80-min wall stands after every AZ-I.W0
deliverable lands.

The forwarding from B1 is genuine — B1's debt ledger names AZ-I.W0
explicitly and AZ-I.W0's wave-spec accepts both items (AZ-I.W0
§Scope.2 + §AZ-I.W0.2; `AZ-I/waves/W0.md:24-30, 74-81`). The mismatch
is between *what was forwarded* and *what would actually defeat the
wall*. B1's debt-ledger framing implicitly assumes that the
> 600 s expansion is a cumulative-cache pathology; the W0' diagnostic
(`AY-II/audit/W0p-regen-diagnostic.md:69-99`) says otherwise — the
proc-macro's IR pipeline runs `~10-15 min per invocation` against any
grammar it sees, not just the first.

## 2. AZ-I.W0 mechanism vs measured wall

### 2.1 What AZ-I.W0 changes

AZ-I.W0 §Scope (`AZ-I/waves/W0.md:14-39`) lands four artefacts:

1. **`CLASSIFIER-UNIFICATION.md`** — research disposition. No code.
2. **Derive-cache relocation** — `target/.bbnf-cache/` →
   `$XDG_CACHE_HOME/bbnf-derive/`, composite fingerprint
   `(grammar-sha, derive-version, rustc-sha, codegen-flags)`. Cache
   invalidation tests exercise miss paths; no change to what runs *on
   a miss*.
3. **IR audit pass** — `payload_coverage.rs` enumerates `->` markers.
   Coverage report only; no codegen-time reduction.
4. **AZ-I baseline bench capture** — measurement substrate.

### 2.2 What the wall actually is

W0' diagnostic (`AY-II/audit/W0p-regen-diagnostic.md:69-99`) measured:

- `cargo check` *without* the bootstrap proc-macro completes in
  0.36 s.
- The bootstrap proc-macro alone takes 10-15 min per invocation, with
  `rustc` at 99-100% CPU the entire time.
- gorgeous's `#[derive(Parser)]` on five grammar files matches the
  same per-derive cost — common cause is the `bbnf_derive` IR pipeline
  itself, not anything cumulative.
- Candidate root causes named at `:82-108` are all *intra-expansion*
  pathologies: pathological emit paths in `view/value.rs`, double
  `collect_projection_admissions` calls, N-way match-arm explosion at
  `emit_path_query_impls`, parallel column emission ballooning
  `quote!` token output.

### 2.3 Cache relocation does not help cold

The cache only helps when populated. On a *cold* first run — the
exact case `iter-check-full-cold-pinned` and `bbnf-bootstrap`
fresh-clone exercise — the cache is empty regardless of where it
lives. The relocation only buys: (a) `cargo clean` no longer wipes
it, and (b) cross-clone reuse (multi-worktree, CI-after-bootstrap).
The first cold expansion still pays the full 10-15 min wall *per
grammar*. B1.W0's `iter-check-full-cold-pinned` halted on this
metric; AZ-I.W0's relocation does not move it.

### 2.4 Watt eliminates the wrong overhead

Watt (`watt`) precompiles a proc-macro crate to WASM, then a thin
runtime loader replaces rustc's per-consumer link of the macro. What
this *removes* is the link cost of the macro crate per consumer
invocation — typically tens of seconds amortised across the 5
gorgeous derive sites + bootstrap, plausibly saving a few minutes
total. What Watt *does not remove* is the macro's runtime *inside*
rustc when invoked. The 10-15 min per derive in the diagnostic is the
proc-macro's IR pipeline running on the grammar input — not link, not
hash, not deserialise. WASM-precompiled or native-linked, the
pipeline runs.

### 2.5 Implication

AZ-I.W0's stated mechanisms move *cumulative* dev-loop overhead. The
*cold first-run* > 600 s wall stays. This means:

- AY-II.W0' close ceremony, which depends on bootstrap regen
  succeeding within human iteration limits (`B1/FINAL.md:191-193`),
  does not become tractable just because AZ-I.W0 closes.
- B1's invariant 11 floor (≤ 5 min `iter-check-full` cold) does not
  open at AZ-I.W0 close as B1's debt ledger claims.
- Every subsequent AZ-I wave (W1, W2, W3) that needs a regen pays the
  same wall at first hit.

## 3. AZ-II coherence under bootstrap wall

AZ-II's W2 byte-equal close gate compares Stage A output (pre-AZ-II
compiler builds candidate) to Stage B output (candidate rebuilds
itself) over the full BBNF fixture corpus
(`AZ-II/AZ-II.md:115-123`). Each stage requires a *full bootstrap
build* — Stage A is a `cargo build -p bbnf_derive` with struct
emission enabled, Stage B is `cargo clean && cargo build -p bbnf` on
the W1 candidate. Both pay the wall, on every run, for every
re-attempt under the W2 reversal-and-replan rail
(`AZ-II/AZ-II.md:283-294`).

If the W2 byte-equal check fails (P(declared) = 0.50 per
`RISK-PERF-MATRIX.md:185-188`), AZ-II re-plans against captured drift
evidence and re-attempts. Each re-attempt is one full bootstrap
cycle. Under the live wall, that is ~80 min of ceiling per attempt
where the developer can do nothing but watch. The architectural
cycle-time is fine; the *debugging* cycle-time during W2 reversal is
crippling.

AZ-II *can* still execute, but its planned re-plan rails become
much more expensive. The escape clause refuses partial-closure
floors (`AZ-II.md:283-294`) — under the wall this discipline still
holds, but practical drift-debugging slows by an order of magnitude.

## 4. Sequencing options

| Sequence | Description | Cold-wall fix? | Cost | Fit to existing plan |
|---|---|---|---|---|
| **S1 — Status quo** | AY-II.W0' (pays wall once, cold) → W1-W5 → AZ-I.W0 (cache + Watt) → AZ-I.W1-W3 → AZ-II | No | AZ-I.W0 closes on declared gates but its dev-loop unblock claim falls; AY-II re-plan rails pay full wall; AZ-II.W2 reversal pays full wall per cycle. | High — no plan disturbance, low coherence with measured cause. |
| **S2 — AZ-I.W0 first** | Defer AY-II.W0' close. Run AZ-I.W0 immediately. | No (per §2) | Same wall remains; AY-II re-opens on the same problem one tranche later; AZ-I.W0's bench-baseline capture is invalid because AY-II.W0' has not closed. | Low — violates AZ-I's seven-point handoff dependency on AY-II close. |
| **S3 — AZ-0 mini-tranche: precompute IR pipeline offline** | Insert a single-wave AZ-0 between AY-II and AZ-I. `bbnf_derive` reads pre-serialised IR pipeline output (build-script runs `compile_grammar_request` at build time, serialises to `target/.bbnf-cache/<grammar>.bincode`; proc-macro reads, no IR re-run). | Yes — cuts the 10-15 min per derive to deserialise + emit. | Substantial design surface; build-script ↔ proc-macro coupling; cache invalidation must include grammar AST + IR-pipeline-version. | Medium — a structural change but bounded; isomorphic to derive-cache philosophy at a deeper layer. |
| **S4 — AZ-0 mini-tranche: profile + optimize bbnf_derive** | Insert AZ-0 to actually attribute the 10-15 min cost. Targeted reductions: O(N²)→O(N) on the candidate match-arm explosion, deduplicate `collect_projection_admissions`, fold redundant clones in `collect_variant_classes`. | Probably yes (target: 5x reduction → 2-3 min). | Less ambitious but tractable; bounded by what samply attribution surfaces. | High — uses existing iteration discipline; no architectural transposition; aligns with `feedback_actual-profiling`. |
| **S5 — Architectural transposition: bbnf_derive becomes build.rs** | Retire the proc-macro entirely. `bbnf_derive` becomes a build-script that writes `crates/core/src/grammar/generated.rs` once per build. Main crate `include!`s the generated source. No proc-macro at consume-time. | Yes — eliminates proc-macro from rustc invocation entirely. | Largest scope; touches `crates/derive/`, `crates/core/build.rs`, every consumer of `#[derive(Parser)]`, gorgeous's grammar derives, the entire `derive` test surface. Aligns with `feedback_no-orthogonal-codepaths` (one codegen path, build-time only). | Medium-low — radical but cleanest; AUDIT-δ is independently reaching this conclusion. |

### 4.1 S5 architectural detail (for AUDIT-δ convergence)

S5's elegance is that it collapses the proc-macro / build-script
duality into one decision surface. Concretely:

1. **`crates/bbnf_derive_codegen`** (new crate): library form of the
   current proc-macro logic — `compile_grammar_request` + IR pipeline
   + Rust source emission, callable as a normal function.
2. **`crates/core/build.rs`**: invokes
   `bbnf_derive_codegen::compile(grammar_path)` for each grammar
   input, writes results to
   `$OUT_DIR/<grammar>_generated.rs`.
3. **`crates/core/src/grammar/<name>/mod.rs`**: replaces
   `#[derive(Parser)] struct JsonParser;` with
   `include!(concat!(env!("OUT_DIR"), "/json_generated.rs"));`.
4. **`crates/derive`**: the existing proc-macro shrinks to nothing or
   is deleted (`bbnf_derive` is no longer a proc-macro crate).
5. **gorgeous sibling**: same transposition — `#[derive(Parser)]` on
   five grammar files becomes five `include!` directives over
   build-script outputs.

What this gains:

- The IR pipeline runs *once per cargo build*, not once per consumer
  *crate* (5x in gorgeous, ~6x in workspace).
- rustc never re-runs the proc-macro; it just compiles plain Rust
  source. `cargo expand` on the bootstrap crate becomes
  near-instantaneous.
- Watt becomes irrelevant (no proc-macro).
- Derive-cache relocation becomes a plain build-script artefact
  cache, dispatchable through Cargo's standard rerun-if-changed
  machinery.
- `feedback_no-orthogonal-codepaths` is satisfied: the build-script
  *is* the one codegen path.

What it costs:

- Rewriting consumer call sites — a workspace-wide edit, but
  mechanical.
- Test surface for `crates/derive` — much of it becomes
  build-script test surface.
- IDE integration — `rust-analyzer` handles `include!` over
  `OUT_DIR` outputs but the experience differs from
  proc-macro expansion preview.
- Cross-grammar derive (e.g. gorgeous's grammar files) needs a
  workspace pattern to share the codegen crate.

## 5. Honest recommendation

**Hybrid: S4 immediately, S5 as the planned architectural
endpoint.**

Rationale:

1. S4 is tractable inside one 4-7 day mini-tranche (call it
   **AZ-0**), uses existing samply / cargo expand infrastructure, and
   delivers a *measured* attribution document that S5 would need
   anyway. The 10-15 min cost is currently un-attributed — every
   plan that names it (B1, AZ-I.W0, AZ-II, AY-II.W0') treats it as a
   black box. S4's first deliverable is the attribution; the second
   is targeted reductions on whatever attribution names. Plausibly
   gets the cold wall to 2-3 min per derive without architectural
   risk.
2. S5 is the correct long-term architecture per
   `feedback_no-orthogonal-codepaths` and the project's repeated
   stance against proc-macro complexity in iteration paths. But S5
   undertaken cold (without the S4 attribution) ships against a
   guess; with the attribution in hand, S5 becomes a measured
   transposition.
3. AZ-I.W0 should be re-scoped or partially deferred. The
   classifier-unification research and the IR audit pass are still
   sound work; the derive-cache relocation and Watt items should
   move to AZ-0 (S4) or be retired entirely (S5 makes them moot).

### 5.1 Concrete sequencing

```
B1 (closed) → AZ-0 (S4, 4-7 days, profile + targeted optimisation,
              attribution doc) → AY-II.W0' close + W1-W5 → AZ-I
              (rescoped: classifier unification + IR audit pass + new
              StructRegistry waves; derive-cache + Watt deleted) →
              AZ-II (W2 reversal cycles now tractable) → BA / BB
```

Or, if the user prefers fewer letters:

```
B1 → AY-II (W0' compresses on AZ-0 work; S4 land *during* W0' as a
     scope addition) → AZ-I (rescoped) → AZ-II → BA / BB
```

### 5.2 What NOT to do

Status quo (S1) ships AZ-I.W0 against a debt-ledger claim it cannot
satisfy. AZ-I.W0 closes on its declared gates *as written*, but its
implicit "AY-II.W0' becomes seconds-scale" promise (B1 invariant 11)
is unfounded. Closing AZ-I.W0 then discovering AY-II.W0' is still
crippled would be a 20+ commit retrace.

## 6. Letter-map proposal

Recommended:

| Letter | Scope | Status |
|---|---|---|
| B1 | dev-loop truth, divan, pin (closed) | closed |
| **AZ-0 (new)** | Profile + S4 reduction of `bbnf_derive` IR pipeline (cold-wall fix) | new |
| AY-II | W0' close on compressed wall + W1-W5 unchanged | unchanged scope, faster execution |
| AZ-I | Direct-to-struct on JSON / CSS L4 / Sheets — rescoped W0 (classifier + audit only; derive-cache + Watt deleted) | rescoped |
| AZ-II | BBNF self-hosting + tape deletion — unchanged plan, tractable W2 reversal | unchanged |
| BA | Lazy typed paths — unchanged | unchanged |
| BB | E-graph rule inference — unchanged | unchanged |

S5 (the deeper transposition) sits as a "post-AZ-II refactor or
pre-BA refactor" candidate, gated on AZ-0's attribution evidence.
If S4 lands the wall ≤ 3 min cold, S5 may be deferred indefinitely
or executed concurrently with BA / BB at low risk.

### 6.1 Concerns under this proposal

- **AZ-II**: W2 byte-equal reversal cycles become tractable
  (3 min × N attempts vs 80 min × N). The plan stands. Concern: if
  S4 misses target, AZ-II's re-plan rails remain costly; AZ-0's
  close gate must include an honest "wall is now ≤ X min" floor.
- **BA**: BA depends on AZ-II close, not directly on the wall.
  Concern: if AZ-0 changes the build-script / proc-macro boundary,
  BA's `path!` macro and TS / Python sub-crate plumbing need to
  re-anchor. Low risk under S4 (no architectural change); medium
  risk under S5.
- **BB**: BB depends on AZ-I close + AY-II close. Concern: BB's
  enumerator runs the VM oracle which exercises emitted parser
  binaries; cold-wall reduction makes the per-rule oracle pass
  cheap. Net positive.

The plan does not weaken under this letter-map; the runway becomes
materially more executable.
