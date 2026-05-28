# SK-V15 Wave W10 Challenge: FNV Quarantine

Verdict: ACCEPT with implementation hardening.

CH1 Scope:

- The plan keeps W10 on the FNV closed-enum issue and does not alter JSON
  measurement rows, CSS source/generator state, or root runtime files.

CH2 Executable Verification:

- The plan names exact Rust tests plus the `gate-json` report consumer. A
  production scan is required evidence, not a prose claim.

CH3 Regression:

- Equal FNV metadata must not become a strict-product pass condition. The
  matching-hash typed-mismatch fixture is mandatory.

CH4 Dependency:

- `DEP-W10-FNV-QUARANTINE` is consumed by same-wave bench fixtures and xtask
  metadata. Production migration remains `no:block`.

CH5 Hidden Coupling:

- A sidecar that shares the closed enum table is hidden coupling even if every
  checksum matches. The shared-closed-enum sidecar fixture must reject.

CH6 Measurement Honesty:

- W10 does not move throughput rows and does not create admission evidence from
  hash equality. It is a guard/metadata wave only.

CH7 Overfit-Prune:

- A clean FNV scan would be false: production hits exist. The report must record
  the regex codegen-internal hash and CSS diagnostic metadata explicitly, then
  route them without treating FNV as a runtime selector, production arbiter, or
  correctness proof.

Required hardening before admit:

- `--skv15-fnv-quarantine-report` must be a first-class xtask flag.
- The report validator must fail if the closed-enum row set is incomplete, if
  either adversarial fixture is missing, if production hits lack a
  classification/disposition, or if any hit is classified as a production
  arbiter/correctness proof.
- `skinny/REDRESS.md` must record non-absent production scan hits and their
  routed status.
