# SK-V18 — The Generalization Cycle (Inflection-Point Backtrack)

You inherit the bbnf-lang restart campaign at its **inflection point**. JSON and
CSS are both >SOTA with a working value API (SK-V17 closed at master `f6a38445b`:
CSS rich-summary beats lightningcss 1.9–3.3× cold on the real corpus; JSON beats
sonic-rs strict). Per the binding principle — *"once we perfect parsing + value
API for both CSS and JSON and >SOTA for each, we backtrack and generalize to be
fully grammar-driven, at that exactly inflection point"* — **SK-V18 is that
backtrack: the GENERALIZATION cycle.** Not more proof; the proof is done. The job
is to retire the hand-written, forked parsers into **ONE grammar-driven generator
emitting all grammars from `.bbnf`**, over the already-unified tape/`ValueRef`
substrate, with a unified value API — preserving the >SOTA, proven by a third
grammar.

This prompt is lean: it points at the V3 audit + the pass suite, and does not
restate them. Read the cited files.

## §0 — Binding pin (carry verbatim)

- **aarch64 Apple M5 Max ONLY. x86 is OUT** — and SK-V18 P1 DELETES the x86 tree
  that currently violates this (`bbnf-simd/src/x86_64/`, 742 LOC stubs).
- **No quick solutions, no workarounds, no contrivances** — idiomatic, gestalt,
  grammar-driven. Per `[no-workarounds]`, `[no-workarounds-arch]`, `[one-codegen-path]`.
- **Hand-craft was acceptable to PROVE >SOTA; it is no longer the goal.** SK-V18
  backtracks every hand-written/forked/replicated surface to grammar-derivation.
  The hand-written parsers become byte-for-byte **parity oracles**, not the product.
- **Preserve the >SOTA + the value API** through the generalization — a
  grammar-derived parser that loses the speed or the equality is not done.
- **Indefatigable**, deep parallelization, never let sub-agents race on shared
  files (commit before parallelizing; distinct file sets). Cold benches only.

All other discipline lives in `~/.claude/projects/-Users-mkbabb-Programming-bbnf-lang/memory/MEMORY.md`.

## §1 — Entry state

- **HEAD:** `7dbe44c22` (V3 audit committed). SK-V17 closed at `f6a38445b`.
- **PASS-IMPL V3 is DONE:** `restart/audit/skinny-impl-overfit/V3/CONSOLIDATED-AUDIT.md`
  (6-agent audit) — read it; it is the SK-V18 seed. Its §"SK-V18 actionable backlog"
  is binding input.
- **Substrate is the genuine foundation:** one `Tape`/`ValueRef`/`PayloadArena`
  (`skinny/crates/runtime/src/tape/`), Lock 1 holds; both grammars ride it. The
  SIMD kernels are grammar-neutral (byte-set-as-data) + checkasm-disciplined.
- **What is NOT generalized (the SK-V18 work):** the codegen is forked
  (`RuntimeEmitterKind` JSON-vs-CSS); CSS is a hand-written ~900-LOC scanner
  emitted as a `const &str` (`CSS_GENERATED_RS`, `runtime_generator.rs:701`);
  JSON is hand-written string-literal templates; the 7 CSS `generated.rs` are
  byte-identical replicas; `ValueRef<G>` is a phantom generic; the value API is
  divergent (JSON tree+visitor vs CSS flat stream, no shared trait).

## §2 — The SK loop (per `restart/prompts/SK-V14-V16-INDEFATIGABLE-HANDOFF.md §2`)

SK-V18 runs the full cycle. The phases and their contracts are unchanged; only the
SUBJECT changes (generalization, not new-feature). Per-cycle:

```
(0) PASS-IMPL-OVERFIT-AUDIT   restart/prompts/skinny/PASS-IMPL-OVERFIT-AUDIT.md
    -> DONE for SK-V18 (the V3 audit above). Add the 6 new CHALLENGE addenda (§4).
(1) Pass Alpha (bracket SK-V18) pass-contracts/PASS-ALPHA.md
    -> goalset = the generalization, seeded from V3 §backlog. G-Alpha gate.
(2) Skinny S-P0/1/2/3          skinny/PASS-{0,1,2,3}-*.md  -> each §3Z LOCK
(3) Totality T-P1/2/3          totality/PASS-{1,2,3}-*.md  -> each §3Z LOCK
(4) Pass Omega                 pass-contracts/PASS-OMEGA.md -> G-Omega gate
(5) Wave implementation        pass-contracts/SKINNY-TRIUMVIRATE.md
(6) Close + loop -> SK-V19
```

§3Z convergence (≥95% × 2 consecutive, zero orphan REVISE, V≤5) per
`restart/prompts/ORCHESTRATOR.md §3W/§3Z`. Hard caps per `[dispatch-hard-cap]`.
G-Alpha and G-Omega are the mandatory user gates.

**Infra note (carry):** the §3Z-convergence workflow with schema-bearing CHALLENGE
agents intermittently hit StructuredOutput/socket flakiness (seen in T-P1 + W0).
Mitigations that worked: treat an all-agents-failed cycle as VOID (not a real
cycle, does not consume the V≤5 ceiling); run a clean confirming cycle to recover
a disrupted near-convergence; for mechanical waves, prefer schema-free free-text
agents verified on disk over schema returns.

## §3 — SK-V18 goalset (the generalization; binding backlog from V3 audit)

The SK-V18 Pass Alpha goalset + S-P3 wave plan MUST sequence these. **PRUNE first,
then GENERALIZE** (the campaign's standing order). Each carries the V3 finding id.

**PRUNE waves (delete the overfit / wrong-arch / contrivance):**
- P1: DELETE `bbnf-simd/src/x86_64/` (742 LOC, AVX/GFNI stubs) — aarch64-only. [V3 D3]
- P2: DELETE the OLD contrived CSS bench path (`nonjson_css_l4.rs` `measure_mbps`/
  `lightningcss_facts`: warm, 85–357-byte SHA-fixtures, more-work-lightningcss). [V3 C3]
- P3: COLLAPSE the 7 byte-identical CSS `generated.rs` replicas → one CSS grammar. [V3 D1]
- P4: FIX the Lock-14 gate — extend `lock14_baseline.rs:2409 GENERIC_SCAN_ROOTS`
  to cover `runtime_generator.rs` + the template files; a green gate must be
  meaningful (it currently passes by excluding the leak surface). [V3 D4]
- P5: PURGE the metalang bench-wave-id leak (`parse_w11_1_number`) from the
  shipped JSON runtime `generated.rs`. [V3 misc]

**GENERALIZE waves (backtrack hand-written → grammar-driven; the inflection):**
- G1: `json_sink_direct::render` PROJECTS the JSON parser from the
  `SinkOnlyProgram`/grammar (the current hand-written template = byte-for-byte
  parity oracle; >SOTA preserved). [V3 A1]
- G2: route CSS through grammar LOWERING — retire the `CSS_GENERATED_RS` const
  string; a grammar-DERIVED CSS recognizer. LOW risk: the >SOTA does NOT depend on
  hand-shaping (the hot path is scalar; no fragile kernel to preserve). [V3 A2/A3]
- G3: UN-FORK the generator — one grammar-agnostic emitter; retire the
  `RuntimeEmitterKind` JSON-vs-CSS fork. [V3 A3/A4]
- G4: a shared `Value`/`Document`/`Cursor` trait both JSON+CSS instantiate
  (value-API isomorphism); INSTANTIATE-OR-DELETE the phantom `ValueRef<G>`. [V3 D2]
- G5: migrate JSON's bespoke scanner onto the neutral alphabet-parametric NEON
  kernel (JSON is the legacy holdout). [V3 A6]
- G6: WIRE-OR-RETIRE the CSS NEON honestly into the HOT path (today it is dead at
  admission); wire the 5 scalar-passthrough kernels or mark them honestly; the
  UDOT `digit_mac`/PMULL/TBX/CSSC aarch64 ASM backlog (optimize ASM for this arch). [V3 C1/A4]

**PROVE (the honest generalization litmus):**
- PROVE: bring `sheets_witness/` (today a 25-line stub) up to a real third grammar
  **via the generator ONLY** — if one generator emits a third grammar from `.bbnf`,
  generalization is real (not JSON+CSS-overfit). [V3 A3/A6]

**HONESTY (measurement):**
- H1: re-frame the CSS >SOTA as lazy-rich-summary vs eager-full-CSSOM, OR add a
  symmetric materialization-depth comparator. The canonical harness
  (`css_canon_bench`) is the honest one — keep it; P2 deletes the old one. [V3 C2]

**Success criterion (R10):** all PRUNE + GENERALIZE waves close; one grammar-driven
generator emits JSON + CSS + a third grammar (Sheets) from `.bbnf`; the value API
is a shared trait both instantiate; the phantom `<G>` is instantiated or deleted;
JSON >sonic-rs AND CSS >lightningcss are PRESERVED (cold, real-corpus, honestly
framed) from the grammar-DERIVED parsers; aarch64-only (x86 tree gone); the Lock-14
gate is meaningful (no exclusion holes); regen --check clean. PASS-IMPL V4 (the
SK-V18 close audit) accepts every axis or records intrinsic-block proof.

## §4 — New CHALLENGE-lens addenda (bind into SK-V18 S-P0 + every pass CHALLENGE)

From the V3 agents — these catch the failure modes this audit found:
- **verbatim-blob:** a `@generated` file that is a verbatim `&str` literal in
  codegen is hand-written, not derived — REJECT as "grammar-driven."
- **distinct-grammar-output:** N claimed grammars must have N **non-identical**
  `generated.rs` (diff-census) — replicas don't count.
- **single-emitter-path:** one grammar-agnostic emitter; flag grammar-family forks
  (e.g. `RuntimeEmitterKind` JSON-vs-CSS).
- **phantom-generic:** a generic `<G>` never instantiated with a real type is
  decorative — instantiate-or-delete.
- **timed-plane-symmetry + corpus-in-the-timer:** the >SOTA comparator must do
  equal work on the real corpus, cold (no micro-fixtures, no more-work-competitor).
- **acceleration-wiring:** a "NEON/ASM acceleration" claim must show the kernel is
  reached AT ADMISSION (in the hot path), not only under `#[cfg(test)]`.

## §5 — Inviolable invariants (verify each cycle close)

Per `SK-V14-V16-INDEFATIGABLE-HANDOFF.md §7`, plus SK-V18-specific:
1. 16-lock count (`grep -cE '^[0-9]+\. \*\*' restart/locks/LOCKS.md` = 16).
2. 5-shape BackendShape canon; tape = substrate-manifest CATEGORY (not a 6th shape).
3. aarch64-only: zero x86/AVX/SVE in `bbnf-simd` (P1 enforces).
4. Substrate-union (Lock 1): one tape/`ValueRef`; no parallel/second substrate.
5. Grammar-neutral (Lock 14): zero grammar-named branches in generic crates
   (codegen/xtask/bbnf-simd) — the gate (P4) must actually scan them.
6. preserve-rich-ast; no re-opened REDRESS (AZ-IV eager value-tree, StructRegistry
   per-leaf indirection, fact-stream-as-output, broadcast, FNV-in-runtime).
7. >SOTA preserved from the grammar-DERIVED parsers (the whole point).

## §6 — Termination + relinquish

Relinquish only at G-Alpha and G-Omega (the mandatory user gates). Drive the rest.
SK-V18 closes when the success criterion (§3 R10) is met; then PASS-IMPL V4 audits
the SK-V18 close and Pass Alpha re-enters for SK-V19. If a generalization wave
proves a grammar-derived parser CANNOT preserve the >SOTA without hand-shaping,
that is a genuine finding — surface it honestly (the hand-shaping becomes a
named, validated, grammar-parameterized primitive, not a silent hand-written
blob), do not paper-close.

## §7 — Bootstrap read order

1. `restart/audit/skinny-impl-overfit/V3/CONSOLIDATED-AUDIT.md` — the seed.
2. `restart/audit/skinny-impl-overfit/V3/AGENT-{1..6}-*.md` — the detail + path:line.
3. `restart/skinny/tranches/sk-v17/` (SPEC + research) — the proven substrate + the >SOTA harness.
4. `restart/prompts/SK-V14-V16-INDEFATIGABLE-HANDOFF.md` — the SK-loop discipline.
5. `restart/prompts/pass-contracts/PASS-ALPHA.md` + `skinny/PASS-{0,1,2,3}-*.md` + `totality/PASS-{1,2,3}-*.md` + `pass-contracts/PASS-OMEGA.md` — the pass contracts.
6. `~/.claude/projects/-Users-mkbabb-Programming-bbnf-lang/memory/MEMORY.md`.

Then: dispatch Pass Alpha SK-V17→SK-V18 with the §3 backlog as the goalset seed.

---

End of handoff. SK-V18 is the generalization cycle: prune the overfit, backtrack
the hand-written parsers into one grammar-driven generator, prove it on a third
grammar, preserve the >SOTA — all through the formal passes, no contrivance.
