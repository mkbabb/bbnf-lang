# SK-V13 W16.1 CHALLENGE - Unicode Escape-Run Validation

Date: 2026-05-22.
Disposition: ACCEPT.
Gate: `G-W16.1-JSON-UNICODE-ESCAPE-RUN-VALIDATION`.

## CH1 Correctness

ACCEPT with a surrogate fallback constraint. The batched helper may advance
only when all four decoded units are valid hex and outside
`U+D800..=U+DFFF`. Any surrogate code unit must return `None` and use the
current scalar branch, preserving pair validation and existing error offsets.
Invalid hex in a batch may return `InvalidUnicodeEscape` at the owning slash;
this is no weaker than the existing parse contract.

## CH2 Generality / Lock 14

ACCEPT. The source touch is `parse-that-regex`, which is a shared primitive
crate, so Lock 14 proof must be factual: the change is JSON-unicode-specific
and must not alter CSS L4 or Sheets grammar behavior. W16.1 may not add a new
grammar directive, BIR variant, BackendShape, public substrate API, or JSON
policy leak. A generic-crate owner-scope check must accompany redress.

## CH3 Regression / REDRESS

ACCEPT with honest miss semantics. The target gap is large, so the most likely
honest outcome is a measured reject. The wave still has a material differential
from REDRESS 82/107/108 because it changes validation-time consumption rather
than materializer-time decoding. If the patch misses the row gate, redress must
save `/tmp/skv13-waveW16.1-rejected.patch`, revert source, and append REDRESS.

## CH4 Cost

ACCEPT. The implementation is bounded to one private helper plus unit tests.
It should not require generated parser churn, new SIMD primitive bodies, or
gate schema expansion unless the row admits. If report/gate work grows beyond
the W16.1 owner list, reject before broadening scope.

## CH5 Hidden Coupling

ACCEPT with two coupling guards:

- `unescape_string` must remain unchanged because it is the REDRESS 107 proof
  surface and already consumes the x4 primitive.
- direct and typed planes must not inherit a status claim from parse-only
  measurements. Any movement outside parse-only requires a separate wave.

## CH6 Anti-Paper-Close

ACCEPT. Micro-proof alone is insufficient. W16.1 closes as ADMIT only on fresh
same-host Criterion row evidence over strict sonic + 1 Mbps and gate-consumed
status movement. A positive helper microbench with a row miss is a measured
REJECT, not a support-only landing.

## Disposition

Proceed to redress. The redress patch must start with the micro-proof and
fallback/parity tests, then measure `unicode_escapes` and `y_string_unicode`
parse-only lanes. Dirty CSS parity sidecar JSON files remain out of scope.
