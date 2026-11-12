# LW3.COMP-4b paired parent-vs-cure cell (S12-P4-6), filed for S12

Filed by LW3 seat S6 KERNEL for S12 CON-SPEC to register in `bench/PREREG.lw3.md` §12 (PRG-1) before the bench window. Nothing here is measured: it is a timing cell, read only inside the bench window (X-7, DIG-QW).

| Field | Value |
|---|---|
| Cell id (proposed) | `F-CURE-K4B` |
| Question | Does kernel ABI 4 (`Failure` carries the expected-label set; `Step<Span>` 72 bytes) cost the success path against ABI 3? |
| Parent arm | parse-that `lw3/s6-kernel` at the COMP-1a head `e1fd8e7` (ABI 3), with `rust/parse_that/benches/kernel/success_path.rs` and its `[[bench]]` entry overlaid from the cure (the file uses only the API both ABIs share) |
| Cure arm | parse-that `lw3/s6-kernel` at the COMP-4b head `a28c250` (ABI 4) |
| Workload | `cargo bench -p parse_that --bench kernel_success_path`: a generated-shaped JSON recognizer over `parse_that::kernel`, every input accepted, documents of shape (depth, width) = (3, 4), (4, 6), (5, 6) |
| Metric | throughput (bytes/s) per shape, paired runs interleaved parent/cure on one plane (this Mac, P-cores, `guarded --cpu-cores 0 --exclusive bench`) |
| Decision rule | PREREG's rule for paired cure cells: a regression beyond the registered minimum effect with a Holm-adjusted CI is a LOSS and reopens COMP-4b's layout (never the contract); otherwise the cure stands. A NOISY record decides nothing. |
| Owner | S12 registers; S14 reads |
