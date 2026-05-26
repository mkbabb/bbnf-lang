# Omega-E Skinny Corpus Alignment - Pass Omega V8 W5B-FRONTENDR

Date: 2026-05-26.
Scope: `restart/skinny/{INDEX,WORKSPACE,HARDENING,COMPILER,BENCH,SUBSTRATE}.md`.
Disposition: ACCEPT-WITH-LIMITED-ALIGNMENT.

## Verdict

REDRESS-212 is a wave-graph and cap-accounting correction inside the existing
V7 W5B-FRONTEND route. It does not change benchmark semantics, substrate shape,
Lock 14 substance, the 16-lock count, the five-shape `BackendShape` canon, or
provider/template deletion ownership.

V7's semantic ordering remains correct:

```text
W5A -> W5B-FRONTEND -> W5C-GEN -> W5D-DELETE -> W6
```

V8 must replace only the one-shot execution shape:

```text
W5A
  -> W5B.0 LOCK14-GATE
  -> W5B.1 IMPORT-CLOSURE
  -> W5B.2 LAYOUT-DISCARD
  -> W5B.3 PRETTY-SPAN-PROJECTION
  -> W5B.4 REQUEST-CONSUMER
  -> W5C-GEN
  -> W5D-DELETE
  -> W6
```

W5B-FRONTEND closes only after W5B.0 through W5B.4 admit. W5C-GEN remains
blocked until that aggregate close. W5D-DELETE remains blocked until W5C-GEN.
W6 root-runtime collapse remains blocked until W5D-DELETE.

## Surface Disposition

| Surface | Disposition |
|---|---|
| `restart/skinny/INDEX.md` | Limited alignment if the surface names W5B-FRONTEND as one next wave. Record REDRESS-212, replace one-shot W5B-FRONTEND wording with aggregate W5B.0..W5B.4 wording, and make W5B.0 the next dispatch. |
| `restart/skinny/WORKSPACE.md` | Limited alignment if the surface says W5B-FRONTEND closes under one PRUNE-3B cap. Replace with the formal sub-wave graph and keep generated-provider replacement at W5C-GEN only after aggregate W5B-FRONTEND close. |
| `restart/skinny/HARDENING.md` | Limited alignment. Add the V8 refusal posture: no W5B.1+ frontend source work before W5B.0 admits; no W5C-GEN before aggregate W5B-FRONTEND close; no W5B.0 close claim. |
| `restart/skinny/COMPILER.md` | Limited alignment if it describes W5B-FRONTEND as a single compiler wave. Split the compiler receiver into W5B.0 pre-compiler Lock 14 authority and W5B.1..W5B.4 frontend/import/IR closure. |
| `restart/skinny/BENCH.md` | Read/no-op. REDRESS-212 does not alter bench planes, thresholds, row schemas, comparator semantics, or SOTA gates. |
| `restart/skinny/SUBSTRATE.md` | Read/no-op. No substrate, Lock 1, FactStream, SIMD/ASM, or BackendShape amendment follows from REDRESS-212. |

## Per-Surface Diff Recommendations

### INDEX

Recommended change: add a Pass Omega V8 authority sentence next to the current
V7 W5B-GENR authority block and update any active-dispatch chain that treats
W5B-FRONTEND as one slot.

Suggested replacement language:

```text
Pass Omega V8 W5B-FRONTENDR closes REDRESS-212 without a LOCKS delta. V7's
semantic order remains binding, but W5B-FRONTEND is no longer a one-shot
PRUNE-3B wave. The active SK-V14 dispatch authority formalizes W5B-FRONTEND as
W5B.0 LOCK14-GATE, W5B.1 IMPORT-CLOSURE, W5B.2 LAYOUT-DISCARD, W5B.3
PRETTY-SPAN-PROJECTION, and W5B.4 REQUEST-CONSUMER. W5B-FRONTEND closes only
after all five sub-waves admit; W5C-GEN remains blocked until that aggregate
close, W5D-DELETE remains blocked until W5C-GEN, and W6.0 remains blocked until
W5D-DELETE.
```

Update the active authority bullet so the next dispatch is W5B.0, not generic
W5B-FRONTEND. Preserve historical V7 wording as evidence where it describes
the REDRESS-211 correction; do not rewrite REDRESS-211 as if it had already
known the V8 cap split.

### WORKSPACE

Recommended change: revise the generated-provider receiver and next-cycle
posture paragraphs only.

Suggested replacement language for receiver ownership:

```text
`runtime_profiles() -> [&'static GrammarProfile; 8]` remains W5C-GEN work, but
W5C-GEN is blocked until aggregate W5B-FRONTEND close. W5B-FRONTEND now closes
through W5B.0 LOCK14-GATE, W5B.1 IMPORT-CLOSURE, W5B.2 LAYOUT-DISCARD, W5B.3
PRETTY-SPAN-PROJECTION, and W5B.4 REQUEST-CONSUMER. Only after W5B.4 admits may
W5C-GEN replace live provider-backed runtime generation; W5D-DELETE then owns
provider/template deletion.
```

Suggested replacement language for dispatch posture:

```text
REDRESS-212 rejected W5B-FRONTEND as a one-cap wave. W5B.0 LOCK14-GATE is the
next dispatch and is authority-only: owner-path roster, parent-diff routing,
modified-provider/template rejection tests, all-template guard, and generic
owner-path leak census. It does not touch grammar/codegen/xtask frontend
implementation paths, does not close W5B-FRONTEND, and does not unblock
W5C-GEN. W5B.1..W5B.4 execute import closure, layout/discard lowering,
pretty/span/projection lowering, and request-consumer proof carry under formal
sub-wave caps.
```

Do not change LOC budgets outside W5B cap-accounting language. Do not change
the crate set, generated-output budget, bench commands, or migration parity
matrix except for references that imply W5B-FRONTEND is one capped wave.

### HARDENING

Recommended change: append a V8 refusal sentence after the V7 W5B-GENR active
refusal posture.

Suggested replacement/addition:

```text
Pass Omega V8 W5B-FRONTENDR adds the active refusal: reject any W5B.1, W5B.2,
W5B.3, or W5B.4 source redress before W5B.0 LOCK14-GATE admits; reject any
W5B.0 plan that edits grammar/codegen/xtask frontend implementation paths;
reject any W5B.0 close claim that treats Lock 14 routing as W5B-FRONTEND
closure; reject any W5C-GEN dispatch before W5B.0 through W5B.4 all admit; and
reject provider/template deletion before W5D-DELETE after W5C-GEN admit.
```

Required hardening guard language:

```text
W5B.0 must pass before frontend source work: Lock 14 owner-path roster,
parent-diff routing, W5C/W5D subject rejection, modified-provider/template
rejection tests, all `_templates` guard, and generic owner-path leak census.
W5B.0 unlocks only W5B.1 IMPORT-CLOSURE. It does not close W5B-FRONTEND and
does not unblock W5C-GEN.
```

W5B.0 exact test names must be preserved in HARDENING / DISPATCH alignment:

```text
w5b_lock14_frontend_owner_paths_admit
w5b_lock14_frontend_rejects_w5c_subject
w5b_lock14_frontend_rejects_w5d_subject
w5b_lock14_frontend_rejects_modified_provider
w5b_lock14_frontend_rejects_modified_template
w5b_lock14_frontend_all_templates_guard_counts_8
w5b_lock14_frontend_allows_grammar_provider_exception
w5b_lock14_frontend_generic_owner_leak_census
```

Every W5B source/consumer sub-wave must preserve per-test/per-log nonzero
assertions. A wildcard `/tmp/skv14-w5b-*.log` grep is not W5B close evidence.
Any touched redress report or reject-only `skinny/REDRESS.md` edit counts in
LOC accounting.

Do not change Lens L/M/N definitions, SK-V14 convergence history, or existing
V5/V6/V7 refusal lessons except to supersede the one-shot W5B-FRONTEND cap
shape.

### COMPILER

Recommended change: replace one-shot compiler receiver language for
W5B-FRONTEND with an aggregate receiver split.

Suggested replacement language:

```text
Pass Omega V8 / SK-V14 W5B-FRONTENDR keeps the W5B-FRONTEND compiler target
but corrects its execution shape. W5B.0 LOCK14-GATE is pre-compiler authority:
owner-path roster, parent-diff routing, modified-provider/template rejection
tests, all-template guard, and generic owner-path leak census; it authorizes no
grammar/codegen/xtask frontend implementation edits. W5B.1 then owns
request-local import DAG closure. W5B.2 owns `@ws`, `?w`, `>>`, and `<<`
compatibility lowering into request-local facts. W5B.3 owns `@pretty`,
`@{...}` span capture, `->` projection metadata, and typed projection lowering
without new public syntax, BIR variants, BackendShape variants, or substrate
state. W5B.4 owns same-wave request-consumer proof through
`emit_runtime_from_request`, JSON/Sheets/BBNF proof carry, `regen-css`, seven
CSS companions, provider/template topology checks, and W5B maintain evidence.
W5C-GEN remains blocked until W5B.4 closes aggregate W5B-FRONTEND.
```

The compiler alignment must retain owner file/type and exact test naming for
the W5B construct table. Construct rows without owner file/type or exact
fail-closed test names remain CH1 REVISE.

Keep the existing no-public-syntax and no-static-centralization constraints.
Do not add compiler recommendations that imply provider-free generation in
W5B. W5C-GEN still owns `RuntimeProvider` / `GrammarProfile` /
`render_runtime_profile` production dispatch retirement; W5D-DELETE still owns
provider/template deletion.

### BENCH

Recommended action: read/no-op.

Rationale: REDRESS-212 changes authority, dispatch caps, and maintain-gate
wording for W5B. It does not change SOTA row classification, same-plane
requirements, the common telemetry envelope, `gate-json` schema/freshness
semantics, comparator anchors, or the threshold matrix. Any W5B exact no-diff
maintain language belongs in SPEC/DISPATCH/HARDENING/WORKSPACE alignment, not
in BENCH threshold definitions.

### SUBSTRATE

Recommended action: read/no-op.

Rationale: REDRESS-212 does not add a retained substrate, cross-call classifier
state, SIMD/ASM primitive, FactStream category, or BackendShape. The existing
Lock 1 substrate-union posture and Lock 10 five-shape canon remain unchanged.

## Exact Next Dispatch

```text
Dispatch: SK-V14 W5B.0 LOCK14-GATE.

Entry: W5A admitted; REDRESS-211 and REDRESS-212 recorded; Pass Omega V8
G-Omega/SPEC/DISPATCH alignment applied.

Scope: Lock 14 W5B-FRONTEND owner-path roster, parent-diff routing,
modified-provider/template rejection tests, all-template guard, and generic
owner-path leak census. No grammar/codegen/xtask frontend implementation edits.

Exit: Lock 14 routing admits only W5B paths; W5C/W5D subjects reject;
provider/template modification tests reject; every `_templates` path is
guarded; the generic owner-path leak census passes.

Cap: HARD CAP 30 min; at 27 min commit safe evidence; at 30 min halt.

Non-close: W5B.0 does not close W5B-FRONTEND, does not unblock W5C-GEN, and
unlocks only W5B.1 IMPORT-CLOSURE after its own admit.
```

## Non-Goals

Do not edit generated outputs, source owner paths, gates, `skinny/RESULTS.md`,
`restart/skinny/ROLLING-SOTA-DELTA.md`, `restart/skinny/BENCH.md`, or
`restart/skinny/SUBSTRATE.md` under Omega-E V8. Do not propose LOCKS,
ARCHITECTURE, BackendShape, BIR, substrate, SIMD/ASM, or provider/template
deletion changes from REDRESS-212.
