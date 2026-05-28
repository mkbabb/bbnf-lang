# S-P2 V1 CH1 Correctness

Disposition: ACCEPT.

Every candidate in `p2a` through `p2f` traces to an S-P1 hot-leaf family:
string/escape scan, whitespace scan, number scan, tape/view cursor cost,
structural scan diagnostics, or generated product loops. Harness checksum,
local FNV, PMU branch/cache absence, CSS legacy fact streams, and x86 probes
are explicitly excluded as primitive authority.

Comparator claims are grounded in primary or local source:

- simdjson HACKING design notes for two-stage parsing;
- sonic-rs README and local `sonic-rs-0.5.7` source for aarch64 whitespace,
  string block, and number skip paths;
- yyjson README/source links for strict parser posture;
- asmjson docs/local README for x86-only structural/tape pressure;
- VideoLAN/FFmpeg checkasm sources for scalar-reference discipline;
- Arm ACLE for TBL, PMULL, UDOT, CLZ, and related NEON operations.

No candidate is marked implementation-ready; all SIMD/native work remains
candidate-only pending S-P3 scalar, checkasm, same-wave consumer, and cold row
measurement binding.
