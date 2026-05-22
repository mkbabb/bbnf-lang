# SK-V13 W15.1 CHALLENGE - UpdateCenter Typed Plugin Fast Path

Date: 2026-05-22.
Disposition: ACCEPT.
Gate: `G-W15.1-JSON-TYPED-UPDATE-CENTER-PLUGIN`.

## CH1 Correctness

ACCEPT with fallback as a hard requirement. An order-specialized parser is
correct only if the current generic parser remains the semantic authority. The
fast path must reset `parser.cursor` to its checkpoint on any mismatch and then
run the existing `parse_type_plugin` loop. Retained strings still go through
`parse_option_scalar_string`; ignored values still go through existing
validating skippers.

## CH2 Generality / Lock 14

ACCEPT. The touched code is JSON typed-direct generation and checked-in
generated JSON typed output. No generic grammar crate, directive, BIR variant,
BackendShape variant, or public substrate API is added. The specialization is
not a Lock-14 claim for CSS or Sheets.

## CH3 Regression / REDRESS

ACCEPT with typed guard rows. The row is below the pinned strict sonic+1 bar
despite legacy pre-pin `A / GO` text. W15.1 must measure update_center and
typed guards. Any previously admitted typed row that silently moves backward in
RESULTS/rolling delta falsifies the wave unless recorded as an explicit
measured disposition.

## CH4 Cost

ACCEPT. The patch is bounded to one renderer and one checked-in generated file.
It is larger than W11.4 but localized; redress may proceed under the standard
campaign behavior-wave cap because the helper is row-specific and fallback
retains correctness.

## CH5 Hidden Coupling

ACCEPT with sync condition. Renderer and generated output must remain paired.
If `cargo xtask regen-real-typed` rewrites unrelated generated bodies, redress
must split or reject rather than stage an omnibus generated churn batch.

## CH6 Anti-Paper-Close

ACCEPT. The wave cannot close by proving the fixture has regular field order.
It closes only on a fresh same-host strict typed row exceeding sonic strict + 1
Mbps with independent Track 2 and gate-consumed provenance. A miss saves and
reverts the patch.

## Disposition

Proceed to redress. No CSS sidecar JSON dirty state may be staged.
