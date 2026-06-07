# SK-V19 — Pass Alpha (cycle opening): the totality fold

Date: 2026-06-07. Cycle: SK-V19 (the totality fold — skinny→full). Opened after the PASS-IMPL
fold-readiness verdict (`audit/PASS-IMPL-FOLD-READINESS.md`). Predecessor: SK-V18 CLOSED (the skinny
generalization proving cycle — the un-fork concept + the value-api shape proven on 3 grammars).

## Thesis
SK-V18 PROVED, in the skinny tree, that one grammar-driven generator can emit a structurally-novel third
grammar (the negative control) and that the typed value API can be grammar-DERIVED (the value-api
`ValueProject` thread). SK-V19 ADOPTS that proven architecture into the `crates/core` 9-grammar fleet
(json / ebnf / bnf / csv / css_l4 / css_pretty / google_sheets / bbnf / math), eliminating EVERY
grammar-specific item from the generic substrate (the user's binding bar: "a grammar is added by a `.bbnf`
file plus workspace metadata; no Rust crate, match arm, parser registry, or per-grammar code" — Lock 14:
the substrate carries ZERO grammar-specific code), while PRESERVING the JSON/CSS >SOTA throughput.

## What the fold actually is (the PASS-IMPL re-frame)
NOT a light wiring of latent core infrastructure — a GREENFIELD ADOPTION. `crates/core` at HEAD has no SoA
tape (three eager builder families + an unwired AoS tape), no `ValueRef`/single-tape value substrate (eager
per-grammar enums + arena ids), no `BackendShape` (zero — the 5-shape canon is skinny-only), and no
decision-engine-driven selection (`rg backend_shape crates/ir` = 0). The fold INTRODUCES/PORTS the
skinny-proven prototypes at fleet scale. The MP.SK19.W0-W6 receiver map is the structure; the design
(`3D-skinny-fold.md`) is re-anchored to the SK-V18 close (the phantom `<G>` is gone → `ValueRef<K>`/Cursor;
Sheets is PROVEN not by-construction; "one generator" is three emit bodies the C fold must collapse; the
value-api Phase 1 is the W3 skinny precedent; the framing is lazy-rich-HEAD vs eager-full-CSSOM).

## The binding architectural law (D11)
>SOTA survives the un-fork (the C fold — collapse the three shape-specialized emit bodies into one
grammar-agnostic generator) ONLY via cost-model-gated `(a)-(d)` named primitives — a naive grammar-walk
regresses to lightningcss. THEREFORE the C fold (the value-plane, W3) and the decision engine (W5) land
TOGETHER: the cost model IS the >SOTA-preservation burden that deferred the C fold to this cycle. The fold
is one coupled architectural transposition, not a sequence of independent edits.

## The wave structure (re-anchored MP.SK19.W0-W6)
- **W0** — `OnceCell` substrate_target classification (8-carrier report; gates W2; md5-distinctness).
- **W1** — eager builders retire → flat-tape commit-by-construction (delete the StructBuilder families,
  delete-after-tape-wired).
- **W2** — AoS `TapeRec` → SoA `Tape` exactly-one-encoding closure (clear the latent AoS tape; Lock-1).
- **W3** — the C fold: ONE grammar-agnostic accessor/recognizer generator; un-courier JSON; the value-api
  `ValueProject` derive for all 9 grammars; JSON `value_from_ref` byte-equal gate.
- **W4** — StructRegistry/FieldSource compile-time projection fence (no per-leaf runtime layout).
- **W5** — the decision engine: introduce the 5-shape `BackendShape` selector atop egraph+csp-solver; the
  two-kernel SIMD scheme unification (core lo6-LUT vs skinny vceqq); the Lock-16 manifest; the cost model
  that gates the (a)-(d) hot-leaf primitives.
- **W6** — Lock-2 `StructLayout` name reconcile; + the carried leaks: `strategy.rs` R16 9-row collapse,
  `css_types.rs` removal, the 131 IR-recognizer grammar-name occurrences — flipping the Lock-14 self-gate
  RED→GREEN.

## Gates
Two mandatory user gates: G-Alpha (this opening) and G-Omega (the certified SPEC, before implementation).
Per the user's standing directive this cycle (2026-06-07): drive autonomously to totality, maximal
parallelism + workflows, do not relinquish — the gates are documented checkpoints, not stops, unless a
genuine fork requires the user. >SOTA preservation + the Lock-14 zero-grammar-code bar are the throughline
gates. Outcome scale: A (fold lands, >SOTA preserved, Lock-14 GREEN) / S (honest residual) / N (the fold
cannot preserve >SOTA without a contrivance — surfaced honestly, never paper-closed).

Next: S-P0/1/2/3 (the wave design, in flight: `wf_c0dd19f9-f18`) → reconcile T-P1/2/3 (re-census core) →
Pass Omega (certify the SPEC) → implement W0-W6.
