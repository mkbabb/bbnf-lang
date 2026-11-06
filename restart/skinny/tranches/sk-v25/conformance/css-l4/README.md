# CSS L4 conformance inventory

This is a born-RED W0 input, not a CSS implementation and not a portfolio
prototype. Phase one freezes every row whose normalized title contains `Level 4`
on the W3C CSS Current Work page, plus CSS Snapshot 2026 and CSS Syntax 3. Each document
is resolved to its dated publication and bound by byte count and SHA-256. WPT is
bound by repository revision.

The phase-one publication capture contains 21 Level-4 specifications and two
foundations. `data/scope.json` repairs that narrow selection with an independently
pinned CSSWG source and currently enumerates 84 union specifications: Snapshot
§§2.1–2.4, §4 exceptions, every Level-4 current-work row, and the foundations.
Scope membership replays green; dated publication pins for the added members,
normative-row extraction, BBNF coverage, WPT routing, and semantic conformance
remain RED.

The companion `data/source-manifest.json` pins all 84 members to their exact
CSSWG source files at the same revision (10,454,319 source bytes). This removes
the publication host from grammar extraction without pretending editor sources
are dated Recommendations; the independent dated-publication manifest remains a
required closure gate.

`data/syntax.json` is the first measured carrier census over those exact bytes:
1,144 blocks (`583 propdef`, `107 descdef`, `291 prod`, `49 railroad`, `114 idl`)
across 79 specifications. It is born RED. Five legacy/foundation sources expose
no recognized carrier (`css-2026`, `css-namespaces`, `css-style-attr`,
`css3-mediaqueries`, `selectors-3`), 76 production/railroad blocks contain no
literal `=`, and no extracted text is yet accepted as a parsed value grammar.
Those are coverage obligations, not license to invent sidecar syntax.

`data/rows.json` structures all 690 property/descriptor carriers without dropping
repeated `Value:` lines: 679 complete rows, 10 explicit `New values` extension
rows, and one anomalous nonstandard `globalCompositeOperation` block. The anomaly
and every partial remain visible. A structured row is still only extracted source
evidence; its value string has not yet passed the CSS value-definition grammar,
reference closure, BBNF coverage, or semantic oracle.

`data/productions.json` splits the 291 `prod` carriers into 512 named
definitions while retaining each complete carrier text. Of those carriers, 258
split cleanly, 29 are expression-only, three contain unrecognized equality text,
and one yields an empty right-hand side. The latter 33 stay RED. Even a clean
split is not an accepted grammar: right-hand sides still require tokenization,
CSS value-definition parsing, reference resolution, cost analysis, and oracle
agreement.

Current immutable membership receipts:

- `data/inventory.json`: `b7d6d155705e6654a7f80c72711f11fcc21863b8096a79b714b7f4a5de770fae`
- `data/scope.json`: `99e2223fb686443b6e612d659115940a20d3c053014201df51bf9c2ec52be50b`
- `data/source-manifest.json`: `ac236359f163d444a1f2bbb20692f930fb677c10b2f0ec6837bfbb0dd91e6300`
- `data/syntax.json`: `293573fe4071646c9c9eab97e8358a5b21ec16f28a0c25a796a311c2f2518d78`
- `data/rows.json`: `67fbbbcd9e93d0d6f2aafa86aa9031cc00c13e0f4736ffb34bdeb9cefbc1e6f2`
- `data/productions.json`: `e0e4cc80af1f8b038b98d22f569271fda5c7b62bfa2de5f73d06b9289421d17a`
- `src/inventory.py`: `dfe74f5bd3780d5a4035c9024d2439a14228e64242180d2436647202be7efa3d`
- `tests/inventory.py`: `9e18cffd0f734627beecf49f356113ed1167f6166011962c717b1921a83388ef`
- `src/source_manifest.py`: `2644bdfc472564b21e17149d1ad6f6550ce60ae6aa3f1f9fd91410395363e0e1`
- `tests/source_manifest.py`: `d81218fa55abb68d7011e090474679f40dae6ad2545571a2299bf5aa898f3979`
- `src/syntax.py`: `e8244b0d99bf683426363f4b3394d73d5ded30d268f5d057d1b1cafc804dd7e9`
- `tests/syntax.py`: `a487ee8d3de3451158f3cb2ba206a3766438261702c18d99e9b1d052db9d6fa2`
- `src/rows.py`: `b7f63bb5db9e6d98d3de3adb73c0368f6fa861a90171668ce919ea992e0372e8`
- `tests/rows.py`: `eb68d55c8634fd9a7e2df9e25b3491532ea4b68530a09385f74da8f9f4161d4c`
- `src/productions.py`: `b21d78799a118085d3cefd00952c4839e17bdd31d535abe33ddbf1a28b65e17a`
- `tests/productions.py`: `91ce04ad5789e4a154eff03409cbb63c48219db0f460c61e20d64bd77de11bd6`

`scope.json` binds the exact phase-one seed bytes and digest as well as the CSSWG,
Current Work, and WPT epochs; swapping any one input without beginning a new epoch
therefore fails replay.

Local extractor replay:

```sh
python3 -B -m unittest discover \
  -s restart/skinny/tranches/sk-v25/conformance/css-l4/tests \
  -p '*.py'

python3 -B restart/skinny/tranches/sk-v25/conformance/css-l4/src/inventory.py \
  --wpt-revision 37cd7ff74eb974fd41600bf7bc01d37576abf8b5 \
  --scope-from restart/skinny/tranches/sk-v25/conformance/css-l4/data/inventory.json \
  --check restart/skinny/tranches/sk-v25/conformance/css-l4/data/scope.json

python3 -B restart/skinny/tranches/sk-v25/conformance/css-l4/src/source_manifest.py \
  --scope restart/skinny/tranches/sk-v25/conformance/css-l4/data/scope.json \
  --csswg-root /path/to/csswg-drafts-at-c7573530343759ace8e46438a1fa2c44515b5554 \
  --check restart/skinny/tranches/sk-v25/conformance/css-l4/data/source-manifest.json

python3 -B restart/skinny/tranches/sk-v25/conformance/css-l4/src/syntax.py \
  --manifest restart/skinny/tranches/sk-v25/conformance/css-l4/data/source-manifest.json \
  --csswg-root /path/to/csswg-drafts-at-c7573530343759ace8e46438a1fa2c44515b5554 \
  --check restart/skinny/tranches/sk-v25/conformance/css-l4/data/syntax.json

python3 -B restart/skinny/tranches/sk-v25/conformance/css-l4/src/rows.py \
  --census restart/skinny/tranches/sk-v25/conformance/css-l4/data/syntax.json \
  --check restart/skinny/tranches/sk-v25/conformance/css-l4/data/rows.json

python3 -B restart/skinny/tranches/sk-v25/conformance/css-l4/src/productions.py \
  --census restart/skinny/tranches/sk-v25/conformance/css-l4/data/syntax.json \
  --check restart/skinny/tranches/sk-v25/conformance/css-l4/data/productions.json

python3 -B restart/skinny/tranches/sk-v25/conformance/css-l4/src/inventory.py \
  --wpt-revision 37cd7ff74eb974fd41600bf7bc01d37576abf8b5 \
  --check restart/skinny/tranches/sk-v25/conformance/css-l4/data/inventory.json
```

The tests and scope replay are green. The final command is intentionally RED while `inventory.json`
is the phase-one schema-1 capture and the extractor now requires the corrected
schema-2 union; the first expansion attempt was stopped by an upstream HTTP 429 and
did not rewrite the manifest. Once recaptured, an upstream change must fail that
command. Recapture is a new input epoch requiring adjudication; the checked manifest
is never silently refreshed.
