# SK-V12 W5 CHALLENGE CH2: Generality / Lock 14

VERDICT: ACCEPT

SPEC requires executable Lock 14 proof and says CSS L4 must be exercised by
benchmark/equality, not prose. W5 relies on the W1b-2b
`sk-v12-css-l4-sota-v1` companion gate.

REDRESS-121 records the W1a GrammarConfig/Lock 14 repair, generic-crate scan,
refreshed JSON guards, and no CSS/SOTA row. REDRESS-123 records generated
non-JSON CSS Track 1 plus independent cssparser oracle with
`lock14_status=pass:lock14_baseline::validate`. REDRESS-125 records the CSS
SOTA gate with `grammar_id=css_l4`,
`domain=non_json_generated:css_l4:declaration_values`, generated
source/runtime provenance, strict three-way equality, generated-size guard,
JSON guard state, and `lock14_status=pass:lock14_baseline::validate`.

The W5 plan refuses to use the legacy JSON-shaped `gate --check-results` path
to launder the CSS row; it reruns the dedicated CSS SOTA report gate plus the
JSON floor AWK verifier.

Required changes: none.
