# SK-V14 W10X JSON parse_only Residual Rejects

## Scope

W10X tested three post-W10W source candidates against the six remaining
`parse_only` residual rows. No source patch is retained. Each candidate was
measured with cold per-parse `profile_direct` evidence after a same-binary
release build.

## Rejected Candidates

1. Inline parse-only frame stack.
   Replaced the heap-backed iterative frame stack with an inline stack plus
   overflow vector. Correctness held, but no residual row cleared
   Skipper + 1.0 Mbps and several rows regressed versus W10W evidence.
2. 64-byte string special sweep.
   Added a 64-byte aarch64 string-special sweep for trusted parse-only string
   bodies. Correctness held, but all six residual rows still failed and guard
   rows became unstable.
3. Trusted string syntax mask.
   Avoided the non-ASCII mask in the trusted parse-only string scan. Correctness
   held, but the six residual rows still failed.

## Evidence

Raw logs:

- `skv14-W10X-parse-only-reject-inline-stack.raw.log`
  sha256 `4c8507af23e0e5e706746ed4aadaef551e0a3fb54056922aa1dab0b8db2b85b0`
- `skv14-W10X-parse-only-reject-string-sweep64.raw.log`
  sha256 `10fcf3ac28a283e70b10b61e5a89f4dadd184d21b551b5c3ebba03eaefb249be`
- `skv14-W10X-parse-only-reject-string-syntax-mask.raw.log`
  sha256 `b6d21e2fcc366282b87f7f5d92bb6b78b07456bccf6092299723c58eeeee13f8`

Representative final rejected syntax-mask sweep:

| corpus | Track 1 | Skipper | threshold | margin |
|---|---:|---:|---:|---:|
| twitter | 12756.805 | 16217.377 | 16218.377 | -3461.572 |
| github_events | 14100.895 | 17205.322 | 17206.322 | -3105.427 |
| update_center | 10318.193 | 14428.272 | 14429.272 | -4111.079 |
| random | 8562.327 | 10620.057 | 10621.057 | -2058.730 |
| gsoc-2018 | 22683.564 | 36039.646 | 36040.646 | -13357.082 |
| distinct_values | 6695.508 | 11935.433 | 11936.433 | -5240.925 |

## Disposition

REDRESS-224 rejects all three W10X candidates. Current parse_only state remains
11 / 17 admitted and 6 / 17 open. The next parse-only implementation must not
replay these candidate shapes without materially new evidence.
