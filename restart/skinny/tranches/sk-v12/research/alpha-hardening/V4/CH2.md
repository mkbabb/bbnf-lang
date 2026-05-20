# SK-V12 Pass Alpha Hardening V4 - CH2 Generality / Lock 14

Date: 2026-05-20.
Lens: CH2 generality / Lock 14.
Verdict: PASS.

Scope: pin-aware `SYNTHESIS.md`, `HANDOFF.md`, Alpha-B, Alpha-E, Alpha-F,
`research/g-alpha/G-ALPHA-SK-V12.md`, the V3 consolidated hardening record,
and the current source surfaces named by `skv12-value-api-audit.md`.

## Standard

PASS-ALPHA defines CH2 as the Lock 14/generalization review: the goalset must
respect Lock 14, remain grammar-neutral, and work for non-JSON grammars
including CSS L4, Sheets, and BBNF-self
(`restart/prompts/pass-contracts/PASS-ALPHA.md:37-40`).

The USER PIN narrows the proof shape. CSS L4 is authoritative; Sheets and
BBNF-self are fallbacks only after a CSS L4 redress attempt fails; and CSS L4
admission requires generated Track 1 to beat lightningcss on the same corpus,
same output plane, with strict equality
(`restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md:18-34`). The pin
also keeps Lock 14 grammar-neutrality, requires the seven JSON leaks to be
resolved through `GrammarConfig` before CSS emission, and requires S-P3 to
reconverge W1 under the CSS/lightningcss target
(`restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md:90-103`,
`restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md:143-150`).

V3 left one non-CH2 blocker: the standalone G-Alpha presentation was stale
(`restart/skinny/tranches/sk-v12/research/alpha-hardening/V3/CONSOLIDATED.md:13-26`).
The V4 CH2 question is whether the pin-aware G-Alpha fold preserves the already
accepted Lock 14/generalization constraints.

## Findings

### CH2-1 - PASS: CSS L4 generality is executable and first

The Alpha contract requires G-Alpha, then S-P1/S-P2/S-P3 under the user pin,
before any implementation packet can materialize. ADMIT requires a generated
CSS L4 row with Track 1 throughput strictly greater than
`lightningcss_mbps + 1` on the same corpus, same output plane, same host, and
strict equality, with one canonical CSS fact stream shared by generated Track
1, independent Track 2/oracle, and lightningcss
(`restart/skinny/tranches/sk-v12/SYNTHESIS.md:39-57`). The handoff repeats the
same CSS-first goalset and requires the downstream S-P3 plan to name the exact
row, paths, comparator, equality command, benchmark command, gate command, and
rollback slice
(`restart/skinny/tranches/sk-v12/HANDOFF.md:51-65`,
`restart/skinny/tranches/sk-v12/HANDOFF.md:110-125`).

Alpha-B makes the competitor proof fail closed: missing same-host
CSS/lightningcss fields are `UNMEASURED`, not a pass, and the existing W0
`sk-v12-nonjson-generated-v1` report is only a schema smoke seed, not CSS L4
performance authority
(`restart/skinny/tranches/sk-v12/research/alpha/alpha-B-competitor-deltas.md:84-105`).
Alpha-E splits the executable path into W1a `GrammarConfig` legality and W1b
CSS L4 generated baseline plus lightningcss comparator, with same-wave
Criterion/equality/report consumers and the strict
`generated Track 1 Mbps > lightningcss_mbps + 1` gate
(`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:67-75`,
`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:77-128`).

The G-Alpha fold now preserves this instead of paper-closing it: it is
`PENDING V4 HARDENING`, not a `G-Alpha PASS`, and it presents CSS L4 as
authoritative with the strict lightningcss floor, symmetric fact stream, and
telemetry requirements
(`restart/skinny/tranches/sk-v12/research/g-alpha/G-ALPHA-SK-V12.md:7-12`,
`restart/skinny/tranches/sk-v12/research/g-alpha/G-ALPHA-SK-V12.md:56-65`,
`restart/skinny/tranches/sk-v12/research/g-alpha/G-ALPHA-SK-V12.md:88-109`).

### CH2-2 - PASS: Sheets and BBNF-self are fallback-only

The packet consistently blocks Sheets/BBNF-self before measured CSS redress.
`SYNTHESIS.md` makes Sheets and BBNF-self post-CSS-redress fallbacks, not peers
of CSS L4
(`restart/skinny/tranches/sk-v12/SYNTHESIS.md:77-78`,
`restart/skinny/tranches/sk-v12/SYNTHESIS.md:173-180`,
`restart/skinny/tranches/sk-v12/SYNTHESIS.md:208-213`). `HANDOFF.md` carries
the same ordering and refuses dispatches that skip CSS before a CSS redress
attempt
(`restart/skinny/tranches/sk-v12/HANDOFF.md:64-65`,
`restart/skinny/tranches/sk-v12/HANDOFF.md:157-159`). Alpha-F says a CSS
preflight-only miss is not enough to skip to Sheets
(`restart/skinny/tranches/sk-v12/research/alpha/alpha-F-contract-draft.md:40-47`,
`restart/skinny/tranches/sk-v12/research/alpha/alpha-F-contract-draft.md:154-156`).

Alpha-E leaves Sheets/BBNF-self out of the shortlist and records them only as
non-shortlisted fallbacks after a CSS L4 redress failure
(`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:56-65`,
`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:347-351`).
The G-Alpha presentation matches this: CSS L4 is first for ADMIT, and
FIXPOINT requires a CSS L4 redress attempt before Sheets/BBNF-self are
considered
(`restart/skinny/tranches/sk-v12/research/g-alpha/G-ALPHA-SK-V12.md:56-60`,
`restart/skinny/tranches/sk-v12/research/g-alpha/G-ALPHA-SK-V12.md:76-83`).

### CH2-3 - PASS: Generic JSON policy leaks remain banned before CSS emission

The value API audit identifies the relevant Lock 14 leak classes: JSON
structural alphabet, value dispatch, string/escape policy, number policy,
key/member model, `OffsetFlags` semantics, and `JsonSink` callbacks
(`restart/skinny/tranches/sk-v12/research/skv12-value-api-audit.md:63-107`).
The current JSON template still contains these JSON policies, including the
JSON structural alphabet, JSON byte dispatch, quoted-key/colon parsing,
JSON escape flagging, and JSON number matching
(`skinny/crates/codegen/src/json_templates/generated.rs:10-17`,
`skinny/crates/codegen/src/json_templates/generated.rs:47-58`,
`skinny/crates/codegen/src/json_templates/generated.rs:83-100`,
`skinny/crates/codegen/src/json_templates/generated.rs:205-217`).

The contract correctly treats extraction as a prerequisite, not completed
evidence. `SYNTHESIS.md`, Alpha-F, and G-Alpha all require `GrammarConfig` or an
equivalent grammar-derived metadata surface before CSS L4 emission is legal
(`restart/skinny/tranches/sk-v12/SYNTHESIS.md:54-57`,
`restart/skinny/tranches/sk-v12/research/alpha/alpha-F-contract-draft.md:85-88`,
`restart/skinny/tranches/sk-v12/research/g-alpha/G-ALPHA-SK-V12.md:63-65`).
Alpha-E's W1a gate requires generated metadata or per-grammar modules to supply
the structural alphabet, dispatch primary set, escape policy, number policy,
key/member policy, flag interpretation, and sink trait; it also requires a CSS
generated-config smoke module to consume the surface
(`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:171-184`).

### CH2-4 - PASS: `json_templates/generated.rs` is limited, not promoted

Alpha-E allows `skinny/crates/codegen/src/json_templates/generated.rs` only to
preserve or extract existing JSON parity. It may not become the polymorphic CSS
provider and may not branch on CSS/JSON grammar names
(`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:145-160`).
The same W1a gate rejects new public JSON-named APIs and generic branches on
grammar name, corpus name, object/array role, field name, string role, or layout
role
(`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:171-184`).

This is compatible with the current source state. `json_templates/generated.rs`
is visibly JSON-specific today
(`skinny/crates/codegen/src/json_templates/generated.rs:10-17`,
`skinny/crates/codegen/src/json_templates/generated.rs:47-58`), so the only
legal V4 posture is the one the packet uses: preserve JSON parity while W1a
extracts grammar-derived policy into a new generic/per-grammar surface.

### CH2-5 - PASS: Pre-pin authority is properly qualified

`SYNTHESIS.md` states that it is not behavior implementation authority, demotes
`SPEC.md` to pre-pin context only where non-conflicting, and leaves replacement
implementation authority to downstream S-P3 after G-Alpha
(`restart/skinny/tranches/sk-v12/SYNTHESIS.md:5-19`,
`restart/skinny/tranches/sk-v12/SYNTHESIS.md:235-254`,
`restart/skinny/tranches/sk-v12/SYNTHESIS.md:278-279`). `HANDOFF.md` marks any
pre-pin CSS/Sheets/BBNF-self, old threshold, or category-block clauses as
superseded, and says downstream S-P3 may update `SPEC.md` and
`DISPATCH-PROMPT.md` only after the pin-aware pass sequence converges
(`restart/skinny/tranches/sk-v12/HANDOFF.md:5-26`,
`restart/skinny/tranches/sk-v12/HANDOFF.md:168-173`).

Alpha-E qualifies pre-pin `SPEC.md` and pre-pin S-P1/S-P2/S-P3 artifacts as
context only under the pin and after measured revalidation
(`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:9-29`).
Alpha-F explicitly leaves `SPEC.md` and `DISPATCH-PROMPT.md` unchanged as stale
downstream products where they contradict the user pin
(`restart/skinny/tranches/sk-v12/research/alpha/alpha-F-contract-draft.md:11-16`,
`restart/skinny/tranches/sk-v12/research/alpha/alpha-F-contract-draft.md:235-237`).
The G-Alpha fold now carries the same qualifier and does not authorize behavior
source edits or replacement implementation packets
(`restart/skinny/tranches/sk-v12/research/g-alpha/G-ALPHA-SK-V12.md:31-48`,
`restart/skinny/tranches/sk-v12/research/g-alpha/G-ALPHA-SK-V12.md:126-131`).

### CH2-6 - PASS: No directive/BIR/BackendShape/public substrate expansion

The Alpha contract still bans new directives, BIR variants, BackendShape
variants, public substrate APIs, parser-owned sidecars, and x86 implementation
work
(`restart/skinny/tranches/sk-v12/SYNTHESIS.md:208-217`,
`restart/skinny/tranches/sk-v12/HANDOFF.md:153-166`,
`restart/skinny/tranches/sk-v12/research/alpha/alpha-F-contract-draft.md:194-209`).
E4's union route is limited to generated, CSS-local, output-plane-owned work
consumed immediately by the CSS direct parser; `event_grammar.rs` and
`tape/mod.rs` may only consume existing sealed/internal bounds, and the gate
requires a public API diff proving no directive, BIR variant, BackendShape
variant, `UnionTape`, generic event side vector, retained cursor/list, or
parser-owned fact slot was added
(`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:231-278`).

The source surface matches the fence. `BackendShape` remains the five-shape
enum
(`skinny/crates/ir/src/lib.rs:401-408`), `BackendExpr` has no new union-tape
or sidecar variant
(`skinny/crates/ir/src/lib.rs:416-452`), skinny directives remain limited to
`import` and `token`
(`skinny/crates/grammar/src/lib.rs:80-99`), and the Lock 14 baseline validator
rejects forbidden directive/`UnionTape` surfaces and BackendShape variant-count
drift
(`skinny/crates/bbnf-bench/src/lock14_baseline.rs:349-379`,
`skinny/crates/bbnf-bench/src/lock14_baseline.rs:565-594`).
The existing `EventGrammar`/`ValueRef` substrate is already generic rather than
a public union sidecar API
(`skinny/crates/runtime/src/tape/event_grammar.rs:1-30`,
`skinny/crates/runtime/src/tape/mod.rs:10-23`).

## Required Folds

None for CH2. The V4 G-Alpha fold removes the V3 CH6 stale-presentation
blocker without reopening CH2. Later consolidation must preserve:

- CSS L4 as the executable first target with strict same-plane
  `generated_track1_mbps > lightningcss_mbps + 1`.
- Sheets/BBNF-self as fallback-only after measured CSS L4 redress.
- `GrammarConfig` or equivalent generated metadata before CSS L4 emission.
- `json_templates/generated.rs` limited to JSON parity/extraction, not a
  polymorphic CSS provider.
- No new directive, BIR variant, BackendShape variant, public substrate API,
  parser-owned sidecar, or x86 route.

## Changed Path

- `restart/skinny/tranches/sk-v12/research/alpha-hardening/V4/CH2.md`
