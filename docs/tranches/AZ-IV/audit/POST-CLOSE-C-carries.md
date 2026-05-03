# POST-CLOSE-C — Chronic Deferrals + Routed-Forward Carries Audit

## Mandate

User directive (verbatim from this dispatch): *"Any deferred items, or in
particular chronically deferred items, must be noted and explicitly addressed
herein."*

Rule context (`memory/feedback_no_deferrals`): *"All identified optimizations
must be integrated into the current tranche/pass. … No 'deferred to next
tranche' language."* Rule context (`memory/feedback_execute_planned_architecture`):
*"Architecture-aversion is the opposite of conservative engineering. … 'Y
added, X still present' is not 'partial landing' — it is shim hell."*

`AZ-IV.md` §Non-Routable Carries (rows 1-33) was the explicit response to a
13-of-15 chronic-deferral finding (Boole §b.2): *"AZ-IV closes inside these or
AZ-IV does not close. A non-routable carry that survives close is a process
failure, not a deferral."* `docs/GESTALT.md` line 188 reinforces: *"A
non-routable item that cannot land inside AZ-IV without changing the AZ-IV
thesis triggers a triumvirate scope-reveal review of the thesis itself, not a
new tranche letter."*

This audit reads `AZ-IV/FINAL.md`'s `MET_WITH_MISSES` and `ROUTED` rows
against that rule and asks: which routings genuinely close (different
mechanism, different scope), and which are quiet chronic deferrals
re-disguised — including the introduction of an unplanned successor letter
**AZ-V** that the user's GESTALT explicitly forbids.

## Per-Carry History

### Carry F2 / Hard Gate 7 / Hard Gate 16 — `bbnf_get_twitter` ≤5x sonic-rs (and `bbnf_value_*` parity-or-better)

- First introduced: **AZ-I** (commit lineage in `AZ-I.md` §Bench delta gate,
  twitter ≥1967 MB/s gate); refined as `bbnf_value_twitter ≤1.15× sonic` in
  AY-II-I.md:302; reformulated as `≤1.50× sonic` in AY-III.md:39-40 (defensible
  floor); restated as `≤5× sonic` for the lazy lane in AZ-IV.md Hard Gates 16
  and Non-Routable Carry 7. Boole §b.2 row 7 names it deferred 4 tranches.
- Tranche-count: **5+** (AY-II-I, AY-III, AZ-I, AZ-II, AZ-III, AZ-IV) — the
  numerical floor has been the headline gate for every JSON tranche; only the
  ratio threshold has changed.
- Current routing: "BA rule-discovery + AZ-V substrate-direct optimization
  tranche" per `audit/W6-fat-lto.txt:45,81` and `FINAL.md:44`.
- Legitimacy: **MIXED**. The W3 cursor-threading mechanism (commit `33184651`)
  is a genuine new substrate that makes the close-matrix testable; that work is
  not the deferral. But the ratio at AZ-IV close is **4196×** (target 5×), and
  the routing destination invents an unplanned tranche letter **AZ-V** that
  does not exist in the trajectory (`docs/tranches/REMAINING-TRAJECTORY.md`,
  `docs/GESTALT.md`, `docs/tranches/BA/BA.md`); the user's GESTALT line 188
  forbids opening a new letter for non-routable carry overflow.
- Close criterion: same-harness `bbnf_get_twitter / sonic_get_twitter ≤5` at
  fat-LTO with samply 7-artefact contract; `bbnf_value_twitter / sonic_value_twitter ≤1`.
- Risk of re-routing: **VERY HIGH** — 5+ tranches, ratio still 4196×,
  destination does not exist as a planned tranche.
- Class: **MASKED-DEFERRAL** (the routing label "AZ-V" is fictional; the
  scope is the same JSON-perf-vs-sonic floor that has rolled since AY-II-I).

### Carry F4 / Non-Routable 5 — Tailwind `regex_scan` perf timeout

- First introduced: **AZ-I** (W2-act CSS perf cluster); deferred at AZ-II
  cutover.O.6 (fat-LTO compile cost); NAMED-CARRIED at AZ-III.W2.2 to BB.W2;
  restated as AZ-IV §Carry Ledger row "Tailwind regex_scan perf timeout"
  with W4 ownership; routed at FINAL.md:63 to "BB rule-discovery (cross-tranche;
  CSS-wide alphabet enumeration)". Boole §b.2 row 4 names 4-tranche deferral.
- Tranche-count: **5** (AZ-I, AZ-II, AZ-III, AZ-IV, → BB).
- Current routing: BB rule-discovery.
- Legitimacy: **PARTIALLY GENUINE**. The W4 KeyDispatch singleton + alt_strategy
  override retire (`a63bb7e3`) and `regex_scan_adapter` rename (`63ade841`)
  landed real substrate change. The BB destination is a planned letter
  (`docs/tranches/BB/BB.md`), and the close criterion (CSS-wide alphabet
  enumeration normalising the timeout-class regex) is a different mechanism
  from W4's CSP-authority work. But Boole §b.2 explicitly flagged this as
  *"no profile artefact ever produced isolating which regex is the timeout
  class"* — the Boole-mandated NON-ROUTABLE close criterion (a
  `tailwind-profile.json.gz` plus the named regex op) is not in evidence at
  AZ-IV close.
- Close criterion: profile artefact pinpointing the hot regex op + measured
  non-watchdog row in BB's bench refresh.
- Risk of re-routing: **HIGH** — same scope routed for 5 letters; absent the
  profile artefact, BB has no leverage to close it differently from AZ-IV.
- Class: **CHRONIC-RISK** (route is to a real letter, but the close criterion
  AZ-IV.md committed to is unmet, and the same scope could reasonably re-route
  to whichever tranche follows BB).

### Carry F10 / Non-Routable 6 — 3 WATCHDOG_HALT rows (`data_xl`, `tailwind`, `compile_css_l4`)

- First introduced: **AZ-III.W4** (when `[profile.bench-iter]` was added; the
  fat-LTO comparison was routed forward to BB.close per AZ-III/FINAL.md:126).
  Boole §b.2 row 6 names this recent (2 tranches), tightly coupled to row 4
  (Tailwind) and row 7 (sonic-rs).
- Tranche-count: **3** (AZ-III, AZ-IV, → BA + post-AZ-IV measurement cohort).
- Current routing: "BA rule-discovery + AZ-V optimization" (FINAL.md:64) and
  "post-AZ-IV measurement cohort" (FINAL.md:108).
- Legitimacy: **PARTIALLY GENUINE for the JSON `data_xl` row** (the W6.1
  fat-LTO matrix at `post-AZ-IV.json` measured 27 rows and recorded a named
  hotspot for the carved row); **MIXED for `tailwind`** (in-flight at W6.1
  commit per `audit/W6-fat-lto.txt:52` — never measured); **MIXED for
  `compile_css_l4`** (per `audit/W6-fat-lto.txt:53` "not measured at W6.1
  (W6.2 scope per AZ-III carve preservation)" — no W6.2 measurement either).
  AZ-V is fictional.
- Close criterion: zero watchdog rows under fat-LTO + bench-iter; named
  hotspot per row converts to a measured non-watchdog row.
- Risk of re-routing: **HIGH for tailwind/compile_css_l4** (same data never
  measured); LOW for `data_xl` if the measurement cohort actually fires.
- Class: **CHRONIC-RISK** (tailwind row is the same row as F4, double-counted
  here; `compile_css_l4` reaches AZ-IV without being measured even once
  under fat-LTO).

### Carry F1 / Non-Routable 30 — W3 Sheets Flat-shape lazy `parse_with`

- First introduced: **AZ-IV.W3 close** (commit `715747db`) — 2 Sheets
  Flat-shape lazy tests added `#[ignore]` because Flat-shape early-bail is a
  separate mechanism from cursor-threaded shape dispatch.
- Tranche-count: **1** (AZ-IV only; net-new at W3 close).
- Current routing: "post-W3 follow-on (Flat-shape early-bail mechanism is
  separate from cursor-threaded shape dispatch)" per FINAL.md:99.
- Legitimacy: **GENUINE**. The W3 cursor-threading shipped a working lazy
  recogniser for 3/4 grammars (JSON, CSS L4, BBNF) with 19/19 parse_with
  tests passing; the Sheets Flat-shape miss is a structurally different
  bail-out mechanism (early-bail at row level rather than cursor-driven
  field skipping). It is a scope reveal at W3 close, not a chronic carry.
  But the destination "post-W3 follow-on" has no named successor wave/tranche.
- Close criterion: `parse_with` works on Sheets Flat-shape rows with the
  same `Option<T>` semantics (lazy elision of post-path errors).
- Risk of re-routing: **MEDIUM** — the absent destination ("post-W3
  follow-on" is not a named tranche/wave) means this could re-route at
  the next close unless BA or its successor explicitly names it.
- Class: **GENUINE-ROUTING** (different mechanism, fresh introduction)
  with a **destination-naming defect** (no concrete successor wave).

### Carry F5 / Non-Routable 4 — TS Node-execute (W5.2 RED gate)

- First introduced: **AZ-I** (W2-act.close noted TS as "string-checked, not
  executable"); tracked at AZ-III.W3c.2 (1 ts_backend_emits_discriminated_union
  routed to BA.W2 host-binding isomorphism). Boole §b.2 row 5 names 4-tranche
  deferral. AZ-IV §Invariants 7 + Hard Gate 20 + Non-Routable 4 escalated to
  "executable cdylib + Node-execute" with W5 owner.
- Tranche-count: **5** (AZ-I, AZ-II, AZ-III, AZ-IV, → post-AZ-IV TS triumvirate).
- Current routing: "post-AZ-IV TS triumvirate (W1 backend-ts repair: object/array
  `value` aggregation)" per FINAL.md:103.
- Legitimacy: **MIXED**. The W5.1 cdylib + wasm-bindgen + 5/5 isomorphic
  PathError tests is real new substrate (`3adf9d03`); the W5.2 Node-execute
  integration test (`7c5e68bf`) ran and surfaced a real W1 backend-ts gap
  (object/array `value` is span-over-input, not aggregated). But this gap is
  W1 territory by AZ-IV's own Hard Gate 4 (which committed to "JSON / CSS /
  Sheets / BBNF / TS parity green from regenerated typed structures"). W1
  closed `complete` at the W1-CLOSE ledger; W5.2 then surfaced that the W1
  close was incomplete on the TS axis. The "TS triumvirate" route reopens W1
  scope under a post-AZ-IV header — same scope, new label.
- Close criterion: TS Node-execute green on representative grammars; backend
  emits aggregated `value` arrays/objects, not byte-spans.
- Risk of re-routing: **VERY HIGH** — Boole already named this 4-tranche
  chronic; the AZ-IV non-routable framing was supposed to break the cycle
  but failed at W5.2.
- Class: **MASKED-DEFERRAL** (W1 close was declared MET; W5 surfaced the
  gap; the AZ-IV non-routable mechanism designed to forbid this class of
  routing was bypassed by re-labelling the same scope as a new "follow-on").

### Carry F6 / Non-Routable 25 — 4 outlier-grammar arena/builder dedup (JSON, CSS L4, Sheets, BBNF)

- First introduced: **AZ-IV.W5** (planned at the union-tranche absorption of
  BB perf items into AZ-IV per Carry Ledger row "Per-grammar value-enum dedup
  (structural skeleton)"); the dedup template landed for 5 simple grammars at
  W5.3 (`0744c9f9`), 4 outliers retained.
- Tranche-count: **1** (AZ-IV).
- Current routing: "post-AZ-IV follow-on (per-outlier distinct-shape
  templates or accepted divergence)" per FINAL.md:104.
- Legitimacy: **GENUINE**. The dedup mechanism shipped on 5 grammars with
  byte-identical typed `*Value` enums preserved. The 4 retained outliers
  have structurally distinct shapes (slab counts, branch_tag presence,
  bounds field) that the simple-cohort template cannot absorb without
  damaging semantic richness (forbidden by `feedback_preserve_rich_ast`).
- Close criterion: either per-outlier template variants land or
  divergence is accepted with explicit justification.
- Risk of re-routing: **MEDIUM** — destination is unnamed; the
  AZ-IV/Hard Gate 21 commitment ("structural skeleton dedup") was MET on
  the simple cohort, so the outlier work is effectively a follow-on
  optimisation, not a chronic carry.
- Class: **GENUINE-ROUTING** with a **destination-naming defect**.

### Carry F8 / Non-Routable 13 — 32 zero-caller substrates (post-W5.4 audit)

- First introduced: **AY-I + AZ-III** (Babbage 3rd-pass identified 5
  WIRED-NOT-CONSUMED + 3 DEAD substrates pre-AZ-IV per
  `audit/HARDENING-2026-05-01-babbage.md`). The permanent CI-gated
  substrate-audit test landed at AZ-IV.W5.4 (`bd72a784`) and enumerated
  886 `pub` substrates, surfacing **32 zero-caller**. Boole §b.2 row 13
  names "substrate denominator" deferred 4 tranches.
- Tranche-count: **5** (AY-I substrate landings, AZ-I, AZ-II, AZ-III, AZ-IV).
- Current routing: "post-AZ-IV cleanup (delete or whitelist; permanent test
  fires until count reaches zero)" per FINAL.md:106.
- Legitimacy: **GENUINE for the test infrastructure** (the permanent
  CI-gated audit is the gate AZ-IV §Hard Gate 13 promised — that gate is
  MET in test-existence terms). The cleanup-route document
  (`audit/W6-substrate-cleanup-route.md`) lists explicit dispositions for
  every one of the 32 items (12 delete, 7 sanction-whitelist, 13
  caller-route) — close criteria are concrete. But none of the 32
  dispositions has actually landed; the test fires red until they do, and
  the AZ-IV §Deletion Bias forbade landing additive substrate without
  consumer.
- Close criterion: every one of 32 items either deleted or sanctioned
  with consumer; test goes green.
- Risk of re-routing: **MEDIUM** — the cleanup ledger is concrete, but
  unowned at the time of FINAL close; without a named successor wave the
  CI redness is the only forcing function.
- Class: **CHRONIC-RISK** (substrate-with-consumer is the 4-tranche
  chronic; AZ-IV closed the test infrastructure but not the cleanup;
  the route to "post-AZ-IV cleanup" has no owner wave).

### Carry F9 / W5 T6 module-split generated

- First introduced: **AZ-IV.W5** (xtask refactor exceeded HARD CAP per
  `audit/W5-t6-module-split.txt`); seeded by AUDIT-F §T6 (generated
  module-split per `feedback_generated_size_budget`).
- Tranche-count: **1** (AZ-IV).
- Current routing: "post-AZ-IV follow-on (generated module-split per AUDIT-F T6)"
  per FINAL.md:105.
- Legitimacy: **GENUINE-NEW** at AZ-IV; surfaced by W5's audit, not
  pre-existing. But unowned destination.
- Close criterion: per-concern files emitted by codegen for css_l4 (85 KLOC),
  bbnf (17 KLOC) so per-tranche LOC budget gate (`feedback_generated-size-budget`)
  scales.
- Risk of re-routing: **LOW-MEDIUM** if BA owns regen; **HIGH** if no
  owner names it.
- Class: **GENUINE-ROUTING** with a **destination-naming defect**.

### Carry F3 / F7 — AUDIT-B routed splits (`dta.rs`, `csp_strategy/mod.rs`, `css_l4/builder.rs`, `types/mod.rs`)

- First introduced: **AUDIT-B mid-tranche** (2026-05-02 audit cohort
  identified god-modules per `feedback_no_god_modules`). Routed at W4 +
  W5 close per cross-agent collision avoidance.
- Tranche-count: **1** (AZ-IV — surfaced and routed within the same letter).
- Current routing: "post-AZ-IV follow-on (god-module decomposition)" per
  FINAL.md:101 and FINAL.md:105.
- Legitimacy: **GENUINE-NEW**. These were not on the pre-AZ-IV chronic list;
  AUDIT-B's god-module analysis is fresh. The W4/W5 cross-agent collision
  rationale for deferral is a real orchestration constraint, not scope
  avoidance — but again, unowned destination.
- Close criterion: each named file split into directory-module per
  `feedback_directory_modules`.
- Risk of re-routing: **MEDIUM** — same orchestration constraint
  (cross-agent collision on shared files) will reappear in any successor.
- Class: **GENUINE-ROUTING** with a **destination-naming defect**.

### Carry F11 — W6.2 workspace gates aggregate

- First introduced: **AZ-IV.W6.2** (the per-wave gate evidence was current,
  but a single-pass aggregate replay against HEAD never ran in the W6
  authoring window).
- Tranche-count: **1** (AZ-IV).
- Current routing: "post-AZ-IV measurement cohort (single-pass gate replay
  against current HEAD; orchestrator-owned)" per FINAL.md:107. PROGRESS.md:19
  records that W6.2 actually landed at `cc0d8d65` with 1606/1610 passed.
- Legitimacy: **MIXED**. The W6.2 commit lands per PROGRESS, but
  FINAL.md:107 still describes it as "pending"; the docs disagree about
  whether F11 is closed. The 4 known fails (2 LSP timeouts +
  ts_node_execute + substrate_audit) are the named misses already
  enumerated above.
- Close criterion: workspace gate replay green at HEAD.
- Risk of re-routing: **LOW** if PROGRESS is accurate; the docs disagreement
  itself is the highest risk.
- Class: **GENUINE-ROUTING** with a **doc-coherence defect** (FINAL and
  PROGRESS disagree).

### Carry F12 — bbnf-buddy (BC tranche)

- First introduced: BC is a project-memory entry (`memory/project_bbnf_buddy.md`);
  not a chronic carry — it is a parallel track per project memory.
- Tranche-count: **0** in AZ-IV authoring window (BC opens after BA per project memory).
- Current routing: "post-AZ-IV (BC tranche; opens after BA rule-discovery)"
  per FINAL.md:108.
- Legitimacy: **GENUINE**. Different mechanism (procedural SVG + skeleton
  animation), different scope, named planned tranche.
- Close criterion: BC scope per project memory.
- Risk of re-routing: **N/A**.
- Class: **GENUINE-ROUTING**.

### AU-floor regression (18/19 BELOW under fat-LTO at W6.1) — call it carry **AF**

- First introduced: **AU close** (the floor itself); referenced in every
  subsequent tranche FINAL as the close-matrix anchor (AV/AW/AX/AY-I/AY-II-I/AY-III/AZ-I/AZ-II/AZ-III).
  AZ-IV §Hard Gate 15 + Non-Routable 6 committed AU floor preservation in
  the `floors` block of post-AZ-IV.json.
- Tranche-count: **8+** (AU through AZ-IV).
- Current routing: per FINAL.md:43 — "single named root cause (W5
  arena/builder template registry indirection); routed to post-AZ-IV
  optimization tranche." That language matches `audit/W6-fat-lto.txt:62-67`
  which names the W5 substrate landing as the regression cause.
- Legitimacy: **GENUINE-MECHANISM-NAMED, MASKED-DESTINATION**. The
  measurement landed; the root cause is named (W5 arena/builder template
  registry indirection — i.e., AZ-IV's own W5 work caused the floor
  regression). But "post-AZ-IV optimization tranche" is the same fictional
  AZ-V referenced under F2/F10. The AZ-IV-introduced regression must be
  unwound by AZ-IV itself per `feedback_execute_planned_architecture`
  ("Y added, X still present" is shim hell), not deferred to a phantom
  successor.
- Close criterion: AU floor restored row-by-row, with the registry
  indirection cost paid down or the indirection unwound.
- Risk of re-routing: **VERY HIGH** — AU floor is the longest-running
  perf gate in the project (8+ tranches); the AZ-IV W5 substrate landing
  regressed it; routing to a fictional AZ-V violates GESTALT line 188.
- Class: **MASKED-DEFERRAL** (the destination AZ-V does not exist as a
  planned tranche; the same perf scope has rolled forward 8+ times; the
  W5-introduced regression should have been carried by AZ-IV itself).

## Chronic Patterns

### Cluster 1 — Perf-as-discipline-gap (AF + F2 + F4 + F10)

`bbnf_get_twitter` (5+ tranches), AU floor (8+ tranches), Tailwind regex_scan
(5 tranches), and the 3 WATCHDOG_HALT rows (3 tranches) are **the same
discipline gap viewed through different metric lenses**. Boole §b.2 already
clustered rows 4 + 6 + 7 as tightly coupled. AZ-IV.W6.1 introduced
*new* numerical evidence (the 4196× ratio, the 18/19-rows-below-AU finding)
that the W5 substrate landing **caused a measurable regression** — and then
routed all of these forward to a fictional letter (**AZ-V**) that does not
appear in `docs/tranches/REMAINING-TRAJECTORY.md`, `docs/GESTALT.md`, or
`docs/tranches/BA/BA.md`. The fictional letter is the smoking gun: per
GESTALT line 188, *"a non-routable item that cannot land inside AZ-IV
without changing the AZ-IV thesis triggers a triumvirate scope-reveal review
of the thesis itself, not a new tranche letter."* Inventing AZ-V is exactly
the move the rule forbids.

### Cluster 2 — Substrate-with-consumer (F8 + AUDIT-B splits)

The 32 zero-caller substrates surfaced by W5.4 are the latest manifestation
of the 4-tranche chronic Boole §b.2 row 13 named: substrate landed,
consumer routed forward. The W5.4 permanent CI test is itself the right
gate (`feedback_substrate_with_consumer`'s materialised forcing function),
and `audit/W6-substrate-cleanup-route.md` lists concrete dispositions per
item — **but the dispositions did not land**. AZ-IV §Deletion Bias forbade
the kind of additive substrate that produced the 32-item residue, yet the
residue exists. Same scope-reveal pattern: when audit infrastructure beats
the code, the code defers.

### Cluster 3 — Gates-without-destinations (F1 + F3 + F6 + F7 + F9)

Five "follow-on" routings name no successor wave or tranche: "post-W3
follow-on", "post-AZ-IV follow-on (god-module decomposition)", "post-AZ-IV
follow-on (per-outlier distinct-shape templates)", "post-AZ-IV follow-on
(generated module-split per AUDIT-F T6)". The format is suspiciously close
to the deprecated phrasing `feedback_no_deferrals` explicitly bans: *"No
'deferred to next tranche' language."* These are not chronic in
tranche-count terms (most are AZ-IV-only scope reveals), but the format
encourages chronic accumulation: an unnamed destination cannot be enforced
by close-honesty discipline.

### Cluster 4 — TS-incompleteness (F5)

W1 closed TS at "build-time correctness only" (Boole §b.2 row 5: 4-tranche
chronic). W5.2 then surfaced a W1-territory gap (backend-ts aggregate emit)
under the W5 Node-execute test. The "TS triumvirate" route is W1 scope
re-labelled — the AZ-IV non-routable mechanism (which was *expressly
designed* to forbid this re-routing) failed because the W5.2 dispatch
identified the gap inside W5's authoring window, not at W1 close. The
chronic pattern is: **TS gates accept structural string-presence as a
proxy for executable parity until a Node integration runs**. Five letters,
the same proxy.

## Architectural Meta-Patterns

### Meta-1 — The fictional-tranche escape hatch

**AZ-V appears 4 times in AZ-IV close-state docs** (FINAL.md:43, FINAL.md:44,
FINAL.md:64, W6-fat-lto.txt:45, W6-fat-lto.txt:81) and **0 times in
trajectory/plan documents** (REMAINING-TRAJECTORY.md, GESTALT.md,
BA.md, BB.md, or any docs/tranches/AZ-V/ subdirectory — no such directory
exists). The user's GESTALT line 188 explicitly forbids this move.

This is not bookkeeping — AZ-V is **the routing target for AF + F2 + F10**,
which together represent the longest-running perf-discipline carry in the
project. The fictional letter pattern means three cross-letter chronic
deferrals were quietly granted a non-existent successor. This is the
sharpest finding of this audit.

### Meta-2 — Lazy-parse-perf was structurally chronic from the planning lens

The lazy-bail-out parse moved from BA stretch (AZ-III planning) → AZ-IV.W3
(union-tranche absorption) → mechanism-shipped-but-perf-routed-to-AZ-V at
AZ-IV close. The mechanism shipped within AZ-IV authoring (W3 cursor
threading + per-iteration consult) — but the perf floor that motivates
the mechanism was, is, and remains a multi-tranche carry. The pattern
**plan-the-mechanism-not-the-floor** is the structural defect: AZ-IV.md
Hard Gate 16 named the close criterion ("≤5x sonic same-harness"), and
shipping the mechanism is treated as "MET" while the floor itself is
"MISSED-ROUTED". The mechanism-vs-floor split is what enables the
chronic re-routing.

### Meta-3 — Substrate-with-consumer requires the consumer landing in the
same wave, not a permanent CI test as substitute

The W5.4 substrate-audit test is correct architecture (per
`feedback_substrate_with_consumer`'s materialised version) — but it is a
**forcing function**, not a **closure**. AZ-IV §Hard Gate 13 specified
"zero-caller substrate" as the close criterion; the test exists and fails
red until 32 items resolve. AZ-IV closed `complete_with_misses` while a
hard-gate test runs red in CI. This is the pattern Boole's "non-routable"
language was designed to prevent. The cleanup-route document
(`audit/W6-substrate-cleanup-route.md`) is concrete enough that the work
should have landed inside AZ-IV — particularly the 12 delete-only items,
which by AZ-IV §Deletion Bias should never have shipped to W6 in the first
place.

### Meta-4 — Failing-test census 100% pass had a per-platform asterisk

W6.2 closes 1606/1610 with 4 known-environmental fails (2 LSP timeouts +
`ts_node_execute` + `substrate_audit`). AZ-IV §Hard Gate 6 was
unequivocal: *"Workspace nextest is 100 % pass. Fail-count zero."* Two of
the 4 fails (`ts_node_execute` = F5, `substrate_audit` = F8) are routed
chronic carries; the LSP timeouts have no recorded owner in FINAL.md. The
"known-environmental" framing was not in the AZ-IV Hard Gate language. This
is a low-grade meta-pattern (close-criterion softening at the close), but
the precedent matters: every prior tranche FINAL.md flagged "complete" or
"complete_with_misses"; AZ-IV adds "complete_with_misses + 4 environmentally
gated fails." The discipline lever weakened by exactly the slack the
non-routable framing was supposed to deny.

## Triage Summary

| Class | Count | Examples |
|---|---:|---|
| GENUINE-ROUTING | 3 | F1 (Sheets Flat-shape), F12 (BC), F11 (W6.2 — modulo doc disagreement) |
| GENUINE-ROUTING with destination-naming defect | 4 | F3, F6, F7, F9 (god-module splits + outlier dedup + module-split generated) |
| CHRONIC-RISK | 3 | F4 (Tailwind regex_scan, 5 tranches, BB has unmet close criterion); F8 (32 substrates, no owner wave); F10 partial (`tailwind` + `compile_css_l4` rows never measured) |
| MASKED-DEFERRAL | 3 | F2 (`bbnf_get_twitter`, AZ-V is fictional); F5 (TS Node-execute, W1 scope re-labelled at W5.2); AF (AU floor, 8+ tranches, AZ-V is fictional) |

Three MASKED-DEFERRALs identified; all three rely on either a fictional
successor letter (AZ-V) or scope re-labelling that the AZ-IV non-routable
framing was designed to prevent.

## Recommended Path Forward

### Strict close criteria for chronic-risk carries (binding text)

1. **AZ-V does not exist.** Per GESTALT line 188, no new tranche letter
   may be opened to absorb non-routable carry overflow. **F2 (sonic-rs
   ≤5x), F10 (watchdog rows), AF (AU floor)** must be re-routed at the
   start of BA's authoring window to one of: (a) BA itself if rule
   discovery can plausibly close them; (b) a thesis-review triumvirate
   per GESTALT line 188 if BA cannot. Either path is acceptable; AZ-V is not.

2. **The W5-introduced AU-floor regression is an AZ-IV defect, not a
   successor's problem.** Per `feedback_execute_planned_architecture`, the
   W5 arena/builder template indirection that caused 14/16 below-AU rows
   should be unwound or amortised within an AZ-IV redress wave (a W7 or a
   W6.3 hardening dispatch). The named root cause makes this small-surface;
   the fictional-AZ-V routing dressed shim hell as completion.

3. **TS Node-execute (F5) reopens W1.** Either W1 reopens at AZ-IV with a
   redress dispatch (consistent with §Orchestration Rules §13: triumvirate
   on scope reveal that invalidates Hard Gate 4 — and W5.2 *did* invalidate
   Hard Gate 4), or AZ-IV close ledger admits Hard Gate 4 was MET-WITH-MISS
   on TS rather than MET. The current FINAL has it MET; PROGRESS.md notes
   the gap. Reconciliation owed.

4. **F8 (32 zero-caller substrates) closes by landing the 12 delete-only
   items immediately.** The cleanup-route document specifies "delete" for
   12 items with one-line reasons; per AZ-IV §Deletion Bias these were
   forbidden to ship and should not survive close. Schedule a ≤30-min
   redress dispatch. The 7 sanction-whitelist items can land in the same
   commit; the 13 caller-route items are the only legitimate follow-on.

5. **All "post-W3 / post-AZ-IV follow-on" routings name a wave + tranche.**
   Cluster 3 carries (F1, F3, F6, F7, F9) need explicit owners — even if
   the owner is "BA W0", "BA hardening" or "BC opening". Unnamed
   destinations are the chronic-deferral seedbed.

6. **Tailwind close criterion per Boole §b.2 row 4.** BB cannot close F4
   without `docs/benchmarks/AZ-IV/profiles/tailwind-profile.json.gz`
   (or successor name) plus the named regex op. Boole gave AZ-IV that
   close criterion; AZ-IV did not satisfy it; BB inherits the same
   condition.

### Close-honesty discipline reinforcement

- Sub-agent dispatches to "post-AZ-IV follow-on" without named tranche
  destination produce immediate triumvirate auto-trigger (per
  ORCHESTRATION.md §Triumvirate auto-triggers).
- Any FINAL.md row that names a tranche letter must verify the letter
  exists in `docs/tranches/` before commit. CI gate candidate.
- The `*MET_WITH_MISSES*` status word should require the resolving
  artefact name a successor wave or tranche by letter — not "follow-on".
- Doc coherence: FINAL.md and PROGRESS.md F11 disagreement (one says
  "pending", other says "landed at `cc0d8d65`") needs reconciliation
  before BA opens.

### What this audit explicitly does NOT recommend

- **No new tranche letter for AZ-V.** The user's GESTALT forbids it; the
  routing is fictional; the perf scope must close inside an existing
  letter (AZ-IV redress, BA, or thesis-review triumvirate).
- **No softening of "100% pass" to "100% modulo environmental".** The
  4 known fails are routed chronic carries; the framing should match.
