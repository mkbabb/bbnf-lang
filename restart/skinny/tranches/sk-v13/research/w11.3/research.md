# SK-V13 W11.3 Research - Direct Sink Stack Specialization

Date: 2026-05-21.
Scope: W11.N JSON direct residual reopen after W11.1 admit and W11.2 measured
reject.

## Authority

- `restart/skinny/tranches/sk-v13/SPEC.md` Section 15 authorizes JSON direct
  residual reopen subwaves.
- W11.2 routed object-heavy rows away from scalar wrapper removal: the wrapper
  change improved `github_events` by `+2.4403%` but missed sonic by a wide
  absolute margin.
- The next material differential must change the direct consumer cost center
  itself, not only the parser dispatch envelope.

## Current Residual Cluster

Open direct rows closest or most structurally aligned to a sink-stack route:

| row | Track 1 | sonic+1 | margin | shape notes |
|---|---:|---:|---:|---|
| `json/instruments/direct_to_struct/main` | 12140 | 12443 | -303 | 1012 objects, 196 arrays, many scalar object fields |
| `json/mesh/direct_to_struct/main` | 8703 | 9942 | -1239 | 3610 arrays, 73013 commas, numeric-heavy |
| `json/random/direct_to_struct/main` | 7902 | 8997 | -1095 | object-heavy, many scalar fields |
| `json/canada/direct_to_struct/main` | 10456 | 12097 | -1641 | 56045 arrays, 111129 commas, numeric-heavy |

W11.1 removed the numeric array parser redispatch and admitted `numbers`, but
`mesh` and `canada` still pay per-scalar sink stack access for each numeric
element. `instruments` is close to threshold and P1 classified its top leaf as
generic inline access rather than a parse primitive.

## Candidate

Specialize the `JsonDigestSink` direct consumer methods by replacing
closure-based `with_object_parent` / `with_array_parent` scalar folding on hot
methods with direct `match self.stack.last_mut()` bodies.

This is a consumer specialization, not a parser or substrate change:

- no generated runtime behavior changes;
- no JSON grammar policy changes;
- no SIMD or hash shortcut;
- no comparator relaxation;
- same `JsonSink` trait semantics and same digest output.

## Falsifiability

Primary target rows:

- `json/instruments/direct_to_struct/main`
- `json/mesh/direct_to_struct/main`
- `json/random/direct_to_struct/main`
- `json/canada/direct_to_struct/main`

Admission requires Track 1 > same-run sonic strict + 1, strict equality, and
Track 2 independence. If no primary row admits, reject and revert the sink
patch while recording per-row movement.

## Pre-Blocked Routes

- No digest shortcut, source hook, fixture branch, row-private branch, new
  parser, or SIMD primitive.
- No `JsonSink` trait expansion unless CHALLENGE explicitly accepts it.
- No direct-to-struct comparator weakening; sonic strict remains the anchor.
- REDRESS 119/120 history is cited but not closure authority.
