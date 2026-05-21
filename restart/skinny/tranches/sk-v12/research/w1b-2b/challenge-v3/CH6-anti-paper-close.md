# SK-V12 W1b-2b PLAN-V3 CH6 - Anti-Paper-Close

Date: 2026-05-20.
Wave: W1b-2b - CSS L4 Lightningcss SOTA Report + Admission Gate.
Lens: CH6 anti-paper-close.
Owned artifact: `restart/skinny/tranches/sk-v12/research/w1b-2b/challenge-v3/CH6-anti-paper-close.md`.
Plan under review: `restart/skinny/tranches/sk-v12/research/w1b-2b/PLAN-V3.md`.

## Authorities Read

- `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md` §4.
- `restart/skinny/tranches/sk-v12/SPEC.md` §0.1 and §7.2.
- `restart/skinny/tranches/sk-v12/research/w1b-2b/PLAN-V3.md`.
- Current W1b artifacts:
  `restart/skinny/tranches/sk-v12/research/w1b/artifacts/track1-facts.txt`,
  `oracle-facts.txt`, `lightningcss-facts.txt`, `strict-equality.txt`, and
  `lightningcss-strict-equality.txt`.
- Current W1b-2a Criterion lanes under
  `skinny/target/criterion/nonjson_css_l4/*/new/`.
- W1b-2a REDRESS-124 record in `skinny/REDRESS.md`.

## Findings

1. Live Criterion evidence is consumed by the gate, not trusted from the
   report. PLAN-V3 requires the CSS report command to read the three live
   `nonjson_css_l4` `new/` lanes for Track 1, cssparser oracle, and
   lightningcss, validate `throughput.Bytes == 187`, finite positive
   `mean.point_estimate`, and `sample.json.iters.len() >= 30`, then recompute
   Mbps from `187 * 8000 / mean_ns`. It explicitly rejects `base/`, `change/`,
   report-only Mbps, and hand-entered values.

2. Retained fact and equality artifacts are gate-consumed. PLAN-V3 requires the
   gate to read `track1-facts.txt`, `oracle-facts.txt`,
   `lightningcss-facts.txt`, `strict-equality.txt`, and
   `lightningcss-strict-equality.txt`; verify byte-identical fact streams;
   bind SHA-256, row id, plane, `input_fnv64=27240148e5780a54`,
   `input_bytes=187`, and `stream_fnv64=285dd62f19dea4a8`; and accept the
   retained W1b run id only because W1b-2b consumes already-landed W1b-1/W1b-2a
   artifacts. This prevents a path-only parity claim.

3. The independent oracle remains distinct from the lightningcss comparator.
   PLAN-V3 requires `track2_or_oracle_source_path` to name the cssparser oracle
   and forbids it from naming generated Track 1. It also requires a focused
   redress source audit proving `lightningcss_facts` calls lightningcss
   parse/projection plus fixture sidecar emission, not `oracle_facts`,
   `ParserInput`, `Parser`, or cssparser parser APIs.

4. The lightningcss comparator is same-plane. PLAN-V3 binds
   `lightningcss_command` to `lightningcss-1.0.0-alpha.71` and the
   same-plane source-sidecar projection, requires
   `lightningcss_sequence_status ==
   pass:ast_projection_matches_source_sidecar`, and validates the retained
   `lightningcss-strict-equality.txt` comparator artifact.

5. JSON guard evidence is separated from CSS throughput evidence. PLAN-V3 uses
   two commands: the CSS SOTA report reads the CSS Criterion root, while the
   JSON guard/stale check runs separately with
   `CRITERION_HOME=/tmp/skv12-w1a-json-guard-criterion` and
   `--advisory --check-results`. This fixes the V2 one-root ambiguity and
   prevents a CSS-only Criterion root from becoming JSON guard proof.

6. Outcome routing is honest. PLAN-V3 derives
   `threshold_mbps = lightningcss_mbps + 1` and
   `admission_margin_mbps = track1_mbps - threshold_mbps`; equality at the
   threshold and any lower Track 1 result route to
   `PASS-MEASURED-BASELINE`, not admission. `PASS-ADMIT-CANDIDATE` requires
   strict equality, oracle independence, artifact freshness, live Criterion
   verification, JSON guard command pass, and `track1_mbps >
   lightningcss_mbps + 1`.

7. W1b-2b does not claim final campaign close. PLAN-V3 explicitly records a
   companion-gate row disposition and routes `skinny/RESULTS.md`
   reconciliation to W5 close even on `PASS-ADMIT-CANDIDATE`. SPEC §0.1 ADMIT
   or FIXPOINT still requires later close-document agreement, JSON guard/orphan
   dispositions, and W5 reconciliation.

## Blocking Findings

None.

## Redress Reject Conditions

- Reject if the gate admits from serialized report Mbps without recomputing
  Track 1, cssparser, and lightningcss Mbps from valid live Criterion `new/`
  lanes.
- Reject if missing/stale fact or equality artifacts are accepted, or if the
  gate does not verify byte-identical Track 1, cssparser, and lightningcss fact
  streams.
- Reject if `lightningcss_facts` is found to reuse cssparser oracle parsing or
  generated Track 1 output for the lightningcss measurement lane.
- Reject if the JSON guard command is skipped, run against a CSS-only Criterion
  root, or used to mutate `skinny/RESULTS.md` inside W1b-2b.
- Reject if equality at `lightningcss_mbps + 1` or below is recorded as
  `PASS-ADMIT-CANDIDATE` rather than `PASS-MEASURED-BASELINE`.
- Reject if W1b-2b REDRESS or report language claims final SK-V12 ADMIT,
  FIXPOINT, or campaign close rather than the Section 7.2 gate disposition.

## Disposition

DISPOSITION: ACCEPT

PLAN-V3 is redressable under CH6. It prevents paper-close by making live
Criterion evidence, retained fact/equality artifacts, independent cssparser
oracle proof, same-plane lightningcss comparator proof, and separate JSON guard
evidence mandatory before any W1b-2b admission candidate can be recorded. It
also keeps final campaign close and `skinny/RESULTS.md` reconciliation outside
this wave.
