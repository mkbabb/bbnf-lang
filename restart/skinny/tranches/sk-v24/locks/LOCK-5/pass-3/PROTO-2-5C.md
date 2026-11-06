# PROTO-2 — 5C AUTOMATA-AS-EXTRACTION: the reference control

**Lock:** LOCK-5 (THE REGEX CONTINUUM) · pass-3 · probe 2 · **family 5C** (reference control)
*(Filed as `PROTO-2-5C.*` — the pass-3 `PROTO-2.md` slot is already held by the family-5B
fire-census probe; this is the distinct 5C deliverable, not a clobber.)*

**Verdict:** **RAN_MEASURED — FAIL (the congruence centerpiece RETIRES).**
The e-graph `DfaSig` congruence rebuild degenerates to **O(n²·k)** on exactly the acyclic
chain-DFAs it was meant to accelerate (`[01]{8}`: n=9 ⇒ **8 passes-to-fixpoint**); and after the
`:358` restart is deleted the strawman is statistically indistinguishable from its fix at
regex-DFA scale, so the strawman never justified the centerpiece. 5C collapses to **2C-as-table +
the `:358` bug-fix**, with Revuz's single reverse-topological pass retained as a modest
(1.4–2×, growing with n) acyclic optimization that needs **no** e-graph.

- **Base-pin:** parse-that (READ-ONLY) `HEAD = ef10d5b78236c4a30a7bb22a6113b60bdc4bdf42` (XL-H:
  `ef10d5b docs(coordination): VALUEJS-PT-E …`). Scratch built per R3 §8 into
  `…/scratchpad/pt-scratch` with `CARGO_TARGET_DIR` inside the copy; the 7 external patch prefixes
  repointed to absolute originals; parse-that itself untouched. No x86.
- **Command:** `cargo bench -p bbnf-regex --bench regex_min_race` (new `harness=false` bench).
- **Cold/paired:** each timed sample operates on a **fresh `clone_dfa`** of the unminimized DFA
  (clone excluded from the timer); median of **401** samples per lane per pattern. Single process.
  **loadavg 4.48** start = end (recorded; a loaded host — treat sub-µs deltas as noise, lean on
  the allocation-independent **passes** measure and the same-constant-factor `incumbent`/`null`
  pair, not on the allocation-heavy Valmari lane).
- **Full run:** `PROTO-2-5C-run.txt`. **Patch:** `PROTO-2-5C.patch`.

---

## QUESTION

The incumbent `parse-that/rust/regex/src/automata/dfa.rs:291 hopcroft_minimize` is **not** Hopcroft —
it is Moore refinement with a **full restart on every split** (`:358 break; // restart refinement
from the beginning`, ~O(n³·k)). Every prior "acyclic-beats-Hopcroft" claim was measured against
this strawman. After the strawman is fixed, does **minimization-as-congruence** (the acyclic Revuz
fast-path + the `DfaSig` e-graph engine) beat a **correct** control — or does **deleting `:358`
alone** erase the e-graph centerpiece's reason to exist? Unrun after three passes.

## METHOD (what actually ran)

One **unminimized** DFA per pattern (`Dfa::compile_with`, `minimize:false`), five lanes over it:

| lane | what | complexity |
|---|---|---|
| **L0 incumbent** | `dfa.rs:291` verbatim — Moore + full restart on every split | ~O(n³·k) |
| **L1 null** | incumbent **minus** the `:358` restart (fall through to next block; loop to a clean fixpoint pass) — the honest "delete `:358`" | O(passes·n·k), Moore |
| **L2 valmari** | correct control: Hopcroft **worklist** (inverse-image split) with an explicit virtual DEAD sink completing the partial DFA ⇒ classical-Hopcroft is unambiguous (Valmari's partial-DFA handling made concrete for this engine's DEAD edges at `:330/:340/:423`) | O(n·k·log n) |
| **L3 revuz** | acyclic single **reverse-topological** congruence pass; acyclic-tag only | O(n·k) |
| **L4 congruence** | `DfaSig{accept, targets:[Id]}` **hashcons rebuild to fixpoint** (the e-graph centerpiece); **passes-to-fixpoint counted** | O(passes·n·k) |

- **(a)** `COMPILE_PATTERNS` grown **26 → 56**; tag **computed at runtime** by an iterative
  **back-edge DFS** (`is_acyclic`), not asserted → **12 acyclic / 44 cyclic** (≈21% acyclic).
- **(c) Correctness oracle FIRST.** Every lane's output is reduced to a **BFS canonical form**
  (state-renumbering-invariant: is_accept + per-class canonical target, DEAD = −1). All four lanes
  are compared byte-for-byte against the incumbent **before any timing**. Divergence = bug.
- **(d)** minimize timed per pattern per tag; the congruence rebuild's **passes-to-fixpoint**
  measured against ceil(log₂ n); the `is_acyclic` dispatch cost folded across **all 56** patterns;
  the `:358`-delete null run as its own lane.

## THE NUMBERS (medians, ns; full table in `PROTO-2-5C-run.txt`)

**Oracle — the load-bearing correctness result:**
```
corpus: 56 patterns  acyclic=12  cyclic=44  divergences = 0
```
**All four minimizers + incumbent emit byte-identical minimal DFAs on every one of the 56
patterns.** The incumbent's *result* is correct; only its *cost model* (the `:358` restart) was
ever in question. (Zero divergence also proves the partial-DFA / DEAD-sentinel equivalence is the
same relation across sentinel-Moore, virtual-sink-Hopcroft, and topological Revuz — no trap-state
merge disagreement arises on this corpus.)

**ACYCLIC slice (Σ median over 12 pats):**
```
incumbent=24124   null=24376   valmari=86669   revuz=17168   cong=37664
revuz vs valmari : valmari/revuz = 5.05×   (PASS gate ≥1.30 — but see CAVEAT: Valmari confounded)
null   vs revuz  : null/revuz    = 1.42×   (delete-:358 does NOT fully recover the acyclic win)
per-pat valmari/revuz median = 3.39×
```

**incumbent ≈ null (the strawman is not slow at this scale):**
```
acyclic: incumbent 24124  ~  null 24376      cyclic: incumbent 20331  ~  null 21247
```
Deleting the `:358` full-restart moves the sum by **<1%** on both tags. At n ≤ 20 the O(n³) restart
costs nothing — the "strawman" and its fix are indistinguishable.

**Congruence rebuild — passes-to-fixpoint (the decisive, allocation-independent measure):**
```
max passes = 8  at n=9        worst passes / log2(n) = 2.00
smallest n=2 → 1 pass         largest  n=20 → 3 passes
```
Passes track DFA **diameter**, not log n. On the **chain-structured** acyclic DFAs that *are* the
fast-path's target (fixed-length dates/hex/IP/bitstrings), passes ≈ depth ≈ n:

| pattern | n | cong passes | revuz passes |
|---|---:|---:|---:|
| `[01]{8}` | 9 | **8** | 1 |
| `\+?\d[\d -]{6,}\d` | 11 | 7 | 1 |
| `\d{4}-\d{2}-\d{2}` | 11 | 6 | 1 |
| `\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}` | 16 | 6 | 1 |
| `\d{2}:\d{2}:\d{2}` | 9 | 6 | 1 |
| `(?:Mon\|Tue\|…\|Sun)` (bushy) | 20 | 2 | 1 |
| `(?:GET\|POST\|…\|HEAD)` (bushy) | 20 | 3 | 1 |

`log n` held only for the **bushy alternations** (shallow); it is **refuted for every bounded
chain**, which is precisely the acyclic class the centerpiece exists to accelerate. Revuz does all
of them in **one** reverse-topological pass by construction.

**Dispatch net-positivity (fold `is_acyclic` across all 56):**
```
acyclic-slice savings (Σ valmari−revuz) = 69501 ns
total dispatch cost   (Σ is_acyclic ×56)=  6786 ns
NET = +62715 ns  → NET-POSITIVE
```
(Net-positive, but the savings figure inherits the Valmari confound — see caveat. Against the fair
`null` control the acyclic saving is Σ(null−revuz)=7208 ns, still > 6786 ns dispatch, so
**marginally** net-positive; this conjunct is the *weakest* of the three even at its best.)

## VERDICT

**FAIL — centerpiece RETIRES.** The brief's FAIL disjunction fires on its second clause:

> *"…OR the rebuild degenerates to O(n²·k) ⇒ centerpiece RETIRES; 5C collapses to 2C-as-table + the
> `:358` bug-fix (named negative result)."*

The `DfaSig` congruence rebuild takes **passes ≈ n** on chain DFAs (`[01]{8}`: n=9 → 8 passes) ⇒
**O(n²·k)**, not the claimed O(n·k·log n). The three-way PASS conjunction therefore breaks at its
middle term. Independently, `incumbent ≈ null` shows the `:358` strawman was **never** the source of
any "acyclic-beats-Hopcroft" gap at regex scale, so the e-graph centerpiece had no cost-model foe to
defeat. The e-graph `DfaSig` engine is, on top of all this, the **slowest correct lane** on acyclic
(cong 37664 > null 24376 > revuz 17168).

**Verdict line:** `RAN · oracle 0/56 divergences · cong passes≈n (O(n²k), [01]{8} n=9→8) · incumbent≈null (<1%) · Revuz single-pass 1.42× over bug-fixed Moore · CENTERPIECE RETIRES → 5C = 2C-as-table + :358 fix; Revuz kept as a no-egraph acyclic fast-path.`

**What survives / what dies.**
- **DIES:** minimization-as-congruence as an **e-graph centerpiece** (L4). Rebuild is O(n²·k) on the
  target class; it is the slowest correct lane; and there is no strawman gap for it to close.
- **SURVIVES (narrowly):** a single **reverse-topological Revuz pass** (L3) is genuinely the fastest
  correct lane on acyclic and the only lane whose passes stay at **1** as n grows — a **structural /
  table** optimization (2C-as-table territory), needing no congruence e-graph.
- **MANDATORY regardless:** delete `:358`. It is a real ~O(n³) latent bug (correctness-neutral: 0
  divergences), free to remove; it simply does not pay off until n ≫ 20.

## CAVEATS (honest boundaries)

1. **Valmari lane is constant-factor-confounded.** L2 allocates `inv` as `Vec<Vec<Vec<u32>>>`
   (k·(n+1)), a `HashMap` per splitter, and a `vec![false; m]` per worklist pop; at n ≤ 20 this
   overhead swamps its asymptotic edge, making it 3.5–5× slower than everything. So the ">= 1.3×
   revuz-beats-Valmari" gate is met *numerically* but is **not** evidence of Revuz's algorithmic
   superiority — it is Valmari's allocation tax. The load-bearing comparisons are therefore (i) the
   **passes** count (allocation-independent) and (ii) `incumbent`/`null`/`revuz`/`cong`, which share
   the same lightweight partition-vector constant factors. The FAIL verdict does **not** rest on the
   Valmari lane.
2. **Small-n regime.** These regex DFAs are n = 2–20; asymptotic separations are near the noise floor
   on a loadavg-4.48 host. The *direction* is stable across 401-sample medians and grows with n
   (n=20 alternations show Revuz ≈ 2× over incumbent/null), and the **passes** signal is unambiguous
   and scale-free — but no claim here should be read as a large-n throughput number.
3. **`delete-:358`-recovers disjunct did NOT fire.** `null/revuz = 1.42×`: the bug-fixed Moore is
   still 1.42× slower than the single Revuz pass, so deleting `:358` does not *fully* recover the
   acyclic win — Revuz keeps a real (if modest) single-pass edge. FAIL is reached via the O(n²·k)
   disjunct, not this one. Both are sufficient; only one fired.
4. **Partial-DFA trap states.** The virtual-sink Hopcroft (L2) would *drop* a non-accepting all-DEAD
   trap that sentinel-Moore keeps; the corpus contains none (0 divergences confirm), so the two
   agree. A corpus with genuine trap states would need the oracle re-checked before reusing L2.

## FILES

- `PROTO-2-5C.md` — this deliverable.
- `PROTO-2-5C-run.txt` — full 56-pattern table + aggregates (the raw run).
- `PROTO-2-5C.patch` — unified diff vs the READ-ONLY parse-that original: `Cargo.toml` (+bench
  stanza), `benches/regex_dfa_compile.rs` (corpus 26→56, tagged), and the new
  `benches/regex_min_race.rs` (5 lanes + BFS-canonical oracle + cold-paired harness).
