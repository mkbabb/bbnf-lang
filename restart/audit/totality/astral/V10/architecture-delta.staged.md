# STAGED architecture-delta — Pass Omega V10 SK-V18 (CRUD-1 carrier, R9 fix)

Pass: SK-V18 PASS OMEGA (astral synthesis, cycle V10), SCOPE Ω-A / CRUD-1.
Status: **STAGED ONLY — NOT APPLIED.** The actual CRUD merge into
`restart/ARCHITECTURE.md` executes POST-G-Omega, after user sign-off.
Master HEAD at staging: `25297a7fc`.

CRITICAL BOUNDARY: this file PRODUCES STAGED DIFFS ONLY. The live
`restart/ARCHITECTURE.md` is NOT touched by this pass.

This carrier resolves CH4-V2 R9: the ARCHITECTURE leg previously carried a
per-finding LOC budget table (ΩA `## CRUD-1 ARCHITECTURE Staged-Edit Budget`)
but NO `git apply`-gated artefact, unlike LOCKS (`locks-diff.md`, exit 0),
MASTER (`master-plan-diff.md`), and MIGRATION/HANDOFF (`*-delta.staged.md`). The
two cleanest edits (the §7.4 title re-key and the §9.2 phantom-vehicle strike)
are emitted below as byte-exact `git apply`-gated unified hunks so the CRUD-1
operator can verify they land where stated BEFORE applying; the remaining four
multi-span edits carry their byte-exact old-side anchor strings as re-grep HALT
gates (they are in-body splices across non-contiguous lines that the CRUD-1
operator applies span-by-span, re-matching each quoted anchor first).

## Verification

```sh
# The two gated hunks apply cleanly against live ARCHITECTURE.md at HEAD
awk '/^```diff$/{flag=1;next} /^```$/{flag=0} flag{print}' \
  restart/audit/totality/astral/V10/architecture-delta.staged.md | git apply --check -
```

## Gated Hunk 1 — OA-V10-07 §7.4 title re-key (SK-V15 → SK-V18)

The §7.4 title `### 7.4 SK-V5 Through SK-V15 Implementation Status` (`:1371`)
re-keys to the SK-V18 generalization-cycle frame. The CSS demotion-frame and
courier-prohibition edits at `:1205`/`:1307` are carried as anchored splices
below (Anchor 4) because they are non-contiguous with this title line.

```diff
diff --git a/restart/ARCHITECTURE.md b/restart/ARCHITECTURE.md
--- a/restart/ARCHITECTURE.md
+++ b/restart/ARCHITECTURE.md
@@ -1370,3 +1370,3 @@
 
-### 7.4 SK-V5 Through SK-V15 Implementation Status
+### 7.4 SK-V5 Through SK-V18 Implementation Status
 
```

## Gated Hunk 2 — OA-V10-06 §9.2 phantom-vehicle strike + re-anchor

The §9.2 phantom `G:EventGrammar` "generality vehicle" sentence (`:1998`) is
struck and re-anchored onto the two axes the certified SK-V18 plan keeps — the
shared value-API `Cursor` micro-trait and the config-breadth classifier
(`alphabet: &'static [u8; 64]`) — per the cursor-generality re-anchor clause in
`locks-diff.md`. The `:1990` lazy-`ValueRef` companion re-open is carried as
Anchor 3 (non-contiguous header line).

```diff
diff --git a/restart/ARCHITECTURE.md b/restart/ARCHITECTURE.md
--- a/restart/ARCHITECTURE.md
+++ b/restart/ARCHITECTURE.md
@@ -1997,4 +1997,7 @@
 value enums (`crates/core/src/runtime/css_l4/value.rs:414`). The `G:EventGrammar`
-type parameter is the generality vehicle; `@generated` per-grammar emission keeps
+type parameter is DELETED at SK-V18 (no non-test production animator exists); the
+generality claim re-anchors onto the shared value-API `Cursor` micro-trait (≥2
+non-collapsible impls) and the config-breadth classifier (`alphabet: &'static [u8; 64]`,
+alphabet-as-data, 8-of-9 re-census owed at G4/G5). `@generated` per-grammar emission keeps
 it grammar-neutral by construction (hand-authoring a per-grammar runtime file in
 a generic crate would be the Lock 14 VIOLATION). preserve-rich-ast holds: the
```

## Anchored Splices (re-grep HALT, applied span-by-span POST-G-Omega)

These four edits cross non-contiguous lines; each carries its byte-exact old-side
anchor. The CRUD-1 operator MUST `grep -nF` the quoted anchor, confirm it resolves
at the stated line, and HALT (not force-apply) if the anchor has moved.

| # | Finding | Anchor (byte-exact old-side, re-grep before edit) | Line | Edit |
|---|---|---|---:|---|
| 1 | OA-V10-04 §0 authority | `**SK-V15 current authority (2026-05-28, G-Omega V9 CRUD-1).** The active` | `:19` | block replace `:19`-`:37` → SK-V18 Pass-Omega-V10 authority (net ≈ −6 LOC) |
| 2 | OA-V10-05 §7.3 x86-pin | `   (LAC-2D-06 binds \`admits_collapsed_stage\` to \`target.arch == x86\` +` | `:1151` | demote-to-diagnostic splices at `:1151`/`:1171`/`:1186`/`:1206`; CollapsedStage SHAPE SLOT retained (≈ +8/−10). **HALT-NOTE on `:1206`: this ledger row is the `locks-diff.md` C9 CollapsedStage-clause SECONDARY UNKNOWN-2D-05 discharge cite (C9's PRIMARY anchor is the demote-stable `:1289` U3 directive). Demote the `target.arch == x86 + target.avx512bw` co-require WORDING but PRESERVE the `aarch64 candidate is UNKNOWN-2D-05 (requires 2E source-backed aarch64 strategy before any aarch64 admission)` clause VERBATIM so the C9 cite is not stranded; re-grep `:1206` for that clause before and after the splice.** |
| 3 | OA-V10-06 §9.2 ValueRef re-open | `**Lazy \`ValueRef<G>\` value-plane (ARCH-3A-S17-D02; Lock 14` | `:1990` | annotate the lazy-`ValueRef` value-plane as in-flight-at-SK-V18 (companion to Gated Hunk 2). **HALT-NOTE on `:1990`/`:1997`/`:1998` (symmetric with the OA-V10-05 `:1206` note): these three lines are the `locks-diff.md` C10 cursor-generality clause's `companion §9.2 prose carrier` cite. Gated Hunk 2 STRIKES the `:1997`/`:1998` "The `G:EventGrammar` type parameter is the generality vehicle" sentence at CRUD-1 and this Anchor-3 re-annotates `:1990` — both EARLIER than the SK-V19/CRUD-3 `LOCKS.md:620` reconcile the C10 clause routes to. So this CRUD-1 strike PRE-EMPTS that reconcile: re-grep `:1997` for the `generality vehicle` sentence BEFORE the strike (confirm it resolves at HEAD), and at the SK-V19/CRUD-3 reconcile the carrier is ALREADY struck — the reconcile re-anchors `LOCKS.md:620` onto the post-strike §9.2 post-state WITHOUT re-grepping `:1997` for the live sentence (which will be gone). The C10 cite is the PRE-strike carrier, not a live carrier at the SK-V19 reconcile.** |
| 4 | OA-V10-07 §7.4 CSS frame | `\| \`SinkOnly\` \| direct typed-field sink, no retained document \| SK-V12 CSS L4 declaration-values, audit-demoted by PASS-IMPL V1` | `:1205` (+ `:1307`) | swap the CSS demotion frame for the verbatim-blob-courier prohibition (≈ +10/−8) |
| 5a | OA-V10-10 §7.4 Pattern-H census | `\| (c) Pattern H runtime grammar-named symbols \| \`crates/core/src/runtime/{json,bbnf,css_l4,google_sheets}/` | `:1398` | ADD-after-anchor: record the D11a/D11b SPLIT alongside this 67→0 / 30→0 baseline row — D11a = the SK-V18 skinny P4 green-by-exclusion `+15` lands inline; D11b = the SK-V19 totality 9-ident R16 collapse (≈+217) tees to SK-V19; do NOT bolt the 9-name widen as an SK-V18 patch (≈ +3 ARCH prose) |
| 5b | OA-V10-10 §13.1 fence-canon lint | `\| \`per-grammar-fence-canon\` \| Lock 14 violations: grammar names in generic-crate source,` | `:2402` | ADD-after-anchor: annotate the D11a-vs-D11b ownership on the `BBNF-GRAMMAR-NAME-IN-GENERIC-CRATE` fence lint row — the +15 skinny green-by-exclusion fix is SK-V18; the 9-ident roster-wide collapse is SK-V19-owned (≈ +3 ARCH prose) |
| 6a | OA-V10-11 §7.3 un-fork render (anchored) | `  grep-zero in \`crates/\`) WIRES into core atop the \`EmitStrategy::StructDirect\`` | `:1274` | add `render(program)` un-fork + `emit_shape_source==lowered_program` firewall + PLANNED `runtime_target_rows_collapsed` co-gate; keep skinny-vs-totality firewall scope distinct (≈ +10). NOTE: the surrounding §7.3 bullet at `:1272` carries a stale prior-cycle `CH4-V3-01` self-reference — the CRUD-1 operator MUST re-grep to confirm this span has NOT already absorbed the edit before applying. |
| 6b | OA-V10-11 §10 un-fork render (named ADD) | named insertion at END of §10.1 (the e-graph threshold paragraph closing `the diagnostic identifies which budget pool exhausted and which \`CostFacts\` row the rewrite stalled on.`) | `:2146` (§10 ends; §11 header `:2147`) | ADD a §10 Codegen-And-Lowerers note: the un-forked emitter is ONE `render(program)` reading shape from `backend_shape`, `RuntimeEmitterKind` DELETED post-G3 — companion to the §7.3 firewall (≈ +4). This is a named ADD-after-anchor, NOT an edit of existing §10 text. |

ARCHITECTURE CRUD-1 staged-edit total: ≈ +56 / −39 prose LOC across 14 sites (net
≈ +17) — 12 anchored-splice sites (the 8 splice rows 1/2/3/4/5a/5b/6a/6b spanning
`:19`-`:37`, `:1151`/`:1171`/`:1186`/`:1206`, `:1990`, `:1205`/`:1307`, `:1398`,
`:2402`, `:1274`, `:2146`) + the 2 gated-hunk sites (§7.4 title `:1370`, §9.2
strike `:1997`) — no canon retirement, no 6th shape, 16-lock count preserved
verbatim. The
+15 (skinny D11a) and +217 (SK-V19 D11b) are CODE budgets owned by CRUD-3/SK-V19,
NOT this ARCHITECTURE prose edit, and are not double-counted here.

## Invariant Check

- The two gated hunks (§7.4 title, §9.2 phantom strike) are byte-exact against
  live `restart/ARCHITECTURE.md` at HEAD and `git apply --check` exits 0.
- The eight anchored-splice rows (12 sites) carry byte-exact old-side anchors —
  rows 5a/5b (OA-V10-10 §7.4 Pattern-H census `:1398` / §13.1 fence-canon `:2402`)
  and 6a/6b (OA-V10-11 §7.3 `:1274` / §10 named ADD `:2146`) each carry a grep-able
  old-side string or a named insertion line; a moved anchor HALTS the edit (re-grep
  HALT), it does not force-apply.
- No 6th `BackendShape`; CollapsedStage stays a SHAPE SLOT (demote-to-diagnostic,
  not retired). 16-lock count preserved. No directive, BIR variant, or public
  substrate API enters through this ARCHITECTURE delta.
