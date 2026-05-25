# Pass Omega V3 Proposed MASTER/SPEC/SYNTHESIS Diff - W2R

Status: proposed patch-style document only.
Authority: REDRESS-183 + W2R corrective packet.
Do not apply before G-Omega authorization.

## MASTER-PLAN

```diff
diff --git a/restart/MASTER-PLAN.md b/restart/MASTER-PLAN.md
--- a/restart/MASTER-PLAN.md
+++ b/restart/MASTER-PLAN.md
@@
-| W2 | §5 | regen-css xtask (R4 — first instance of regen-{grammar} family) | Conditional on W1 close | ≤2.0k C-3 part-A source/test; generated output named separately | ≤90 min |
+| W2 | §5 | regen-css xtask (R4 — first instance of regen-{grammar} family; skinny-side runtime only after W2R) | Conditional on W1 close | <=2.0k C-3 part-A source/test; generated output named separately | <=90 min |
@@
-| W6 | §9 | PRUNE-4 — 9 sub-waves: per-grammar runtime collapse (R3 PRUNE-4; C-1 part-B) | Conditional on W5 close | ≤2.0k C-1 part-B aggregate across 9 sub-waves (avg ~220 LOC/grammar; generated output uncounted) | ≤90 min per sub-wave (W6.1..W6.9); aggregate ≤810 min |
+| W6 | §9 | PRUNE-4 — 9 sub-waves: W6.0 CSS L4 core-runtime collapse, then W6.1-W6.8 remaining per-grammar runtime dirs (R3 PRUNE-4; C-1 part-B) | Conditional on W5 close | <=2.0k C-1 part-B aggregate across 9 sub-waves (avg ~220 LOC/grammar; generated output uncounted) | <=90 min per sub-wave (W6.0..W6.8); aggregate <=810 min |
@@
+Pass Omega V3 W2R receiver amendment: W2 is skinny-side only and may emit
+`skinny/crates/runtime/src/grammars/css_l4_*`; it may not emit or require
+`crates/core/src/runtime/css_l4/`. W6 remains nine sub-waves under the same
+caps, but numbering becomes W6.0..W6.8: W6.0 owns CSS L4 root-runtime
+collapse, and W6.1-W6.8 own `math`, `csv`, `bnf`, `ebnf`, `css_pretty`,
+`google_sheets`, `bbnf`, and `json`. W8/W9/W10 remain new-admit waves and
+are globally blocked until PRUNE-1..PRUNE-5 close.
```

## SK-V14 SPEC

```diff
diff --git a/restart/skinny/tranches/sk-v14/SPEC.md b/restart/skinny/tranches/sk-v14/SPEC.md
--- a/restart/skinny/tranches/sk-v14/SPEC.md
+++ b/restart/skinny/tranches/sk-v14/SPEC.md
@@
-| W2 | Section 5 | regen-css xtask (R4 — first instance of regen-{grammar} family) | Conditional on W1 close | ≤2.0k C-3 part-A source/test LOC; generated output named separately | ≤90 min |
+| W2 | Section 5 | regen-css xtask (R4 — first instance of regen-{grammar} family; skinny-side runtime only after W2R) | Conditional on W1 close | <=2.0k C-3 part-A source/test LOC; generated output named separately | <=90 min |
@@
-| W6 | Section 9 | PRUNE-4 — 9 sub-waves: per-grammar runtime collapse (R3 PRUNE-4; C-1 part-B) | Conditional on W5 close | ≤2.0k C-1 part-B aggregate across 9 sub-waves (avg ~220 LOC/grammar; generated output uncounted) | ≤90 min per sub-wave (W6.1..W6.9); aggregate ≤810 min |
+| W6 | Section 9 | PRUNE-4 — 9 sub-waves: W6.0 CSS L4 core-runtime collapse, then W6.1-W6.8 remaining per-grammar runtime dirs (R3 PRUNE-4; C-1 part-B) | Conditional on W5 close | <=2.0k C-1 part-B aggregate across 9 sub-waves (avg ~220 LOC/grammar; generated output uncounted) | <=90 min per sub-wave (W6.0..W6.8); aggregate <=810 min |
@@
-| W6.1..W6.9 | per-grammar runtime collapse test, regen check, per-grammar parser tests, Lock-14 grep | one gate refresh per sub-wave |
+| W6.0..W6.8 | per-grammar runtime collapse test, regen check, per-grammar parser tests, Lock-14 grep | one gate refresh per sub-wave |
@@
-- `crates/core/src/runtime/css_l4/` (dual-tree generated output destination per SYNTHESIS §3 C-3 verbatim)
+- `crates/core/src/runtime/css_l4/` is excluded from W2 and owned by W6.0.
@@
-2. Author `regen_css.rs` consuming the 15 `.bbnf` files at `/grammar/css/l4/` + workspace metadata; emit CSS L4 runtime modules under `skinny/crates/runtime/src/grammars/css_l4_*/` AND `crates/core/src/runtime/css_l4/`.
+2. Author `regen_css.rs` consuming the 15 `.bbnf` files at `/grammar/css/l4/` + workspace metadata; emit the existing skinny CSS L4 runtime profile directories under `skinny/crates/runtime/src/grammars/css_l4_*/`.
@@
-- `cargo xtask regen-css` round-trip clean: `rm -rf skinny/crates/runtime/src/grammars/css_l4_* crates/core/src/runtime/css_l4/ && cargo xtask regen-css && git diff` returns empty on both runtime trees.
+- `cargo xtask regen-css` skinny-side round-trip clean: `rm -rf skinny/crates/runtime/src/grammars/css_l4_* && cargo xtask regen-css && git diff --exit-code -- skinny/crates/runtime/src/grammars` returns empty for the W2-owned runtime mirror.
+- Exact W2 companion checks exist and pass:
+  `cargo xtask check-css-l4-at-rules-and-media`,
+  `cargo xtask check-css-l4-declaration-values`,
+  `cargo xtask check-css-l4-declaration-values-extended`,
+  `cargo xtask check-css-l4-nested-layout`,
+  `cargo xtask check-css-l4-stylesheet-selectors`,
+  `cargo xtask check-css-l4-vendor-and-custom-atrules`,
+  `cargo xtask check-css-l4-visual-functions`.
@@
-Downstream effect: W2 rejection blocks W4 ... W3 + W5 + W6 + W7 + W9 + W10 may proceed independently.
+Downstream effect: W2 rejection blocks W3, W4, W5, W6, W7, and W8 by hard entry-gate chain. It also blocks W9/W10 dispatch under the global PRUNE-before-new-admit rule until PRUNE-1..PRUNE-5 close.
@@
-W6 plan enumerates 9 sub-waves W6.1..W6.9 by grammar name: `bbnf, bnf, css_l4, css_pretty, csv, ebnf, google_sheets, json, math`.
+W6 plan enumerates 9 sub-waves W6.0..W6.8 by grammar name: `css_l4, math, csv, bnf, ebnf, css_pretty, google_sheets, bbnf, json`. W6.0 owns the CSS L4 root-runtime collapse moved out of W2 by W2R.
```

## SK-V14 Tranche-Local Dispatch Surfaces

```diff
diff --git a/restart/skinny/tranches/sk-v14/ORCHESTRATOR-PROMPT.md b/restart/skinny/tranches/sk-v14/ORCHESTRATOR-PROMPT.md
--- a/restart/skinny/tranches/sk-v14/ORCHESTRATOR-PROMPT.md
+++ b/restart/skinny/tranches/sk-v14/ORCHESTRATOR-PROMPT.md
@@
+Pass Omega V3 W2R amendment: R4/W2 is skinny-side `regen-css` only. Root
+`crates/core/src/runtime/css_l4/` collapse is W6.0 after W5. PRUNE-4 remains
+nine sub-waves: W6.0 CSS L4, then W6.1-W6.8 for the remaining Pattern H dirs.
```

```diff
diff --git a/restart/skinny/tranches/sk-v14/HANDOFF.md b/restart/skinny/tranches/sk-v14/HANDOFF.md
--- a/restart/skinny/tranches/sk-v14/HANDOFF.md
+++ b/restart/skinny/tranches/sk-v14/HANDOFF.md
@@
+Pass Omega V3 W2R amendment: REDRESS-183 rejected current W2. W2 must rerun
+under the amended skinny-only `regen-css` gate before W3 or later dispatch.
+W6.0 owns `crates/core/src/runtime/css_l4/` after W5.
```

```diff
diff --git a/restart/skinny/tranches/sk-v14/DISPATCH-PROMPT.md b/restart/skinny/tranches/sk-v14/DISPATCH-PROMPT.md
--- a/restart/skinny/tranches/sk-v14/DISPATCH-PROMPT.md
+++ b/restart/skinny/tranches/sk-v14/DISPATCH-PROMPT.md
@@
+Pre-dispatch guard after W2R: before dispatching W3 or later, verify Pass
+Omega V3 W2R CRUD landed and W2 admitted under the amended skinny-only
+`regen-css` gate. W2 admission must include the exact seven
+`check-css-l4-*` companion commands named in the G-Omega packet.
```

## SK-V14 SYNTHESIS

```diff
diff --git a/restart/skinny/tranches/sk-v14/SYNTHESIS.md b/restart/skinny/tranches/sk-v14/SYNTHESIS.md
--- a/restart/skinny/tranches/sk-v14/SYNTHESIS.md
+++ b/restart/skinny/tranches/sk-v14/SYNTHESIS.md
@@
-Refactor 64 hand-written per-grammar files ... (8 sub-waves).
+Refactor 67 hand-written per-grammar runtime files ... (9 sub-waves).
@@
-round-trip xtask check returns clean on both runtime trees ... skinny ... AND `crates/core/src/runtime/css_l4/`
+W2 round-trip is skinny-side only. Root `crates/core/src/runtime/css_l4/`
+round-trip belongs to W6.0 after W5 stands up the generic generator.
```
