# CH6 — ANTI-PAPER-CLOSE (V5) — SK-V17 T-P1 EXCAVATION

```yaml
lens: CH6-ANTI-PAPER-CLOSE
pass: T-P1-SKV17-excavation
cycle: V5
generated_at: 2026-05-29T00:00:00Z
reviewer: CH6
master_head: 445925167154de73540e3ea3283d0170371de790
subjects_reviewed:
  - restart/audit/totality/sk-v17/p1/1a-substrate-evidence.md
  - restart/audit/totality/sk-v17/p1/1b-codegen-evidence.md
  - restart/audit/totality/sk-v17/p1/1c-runtime-evidence.md
  - restart/audit/totality/sk-v17/p1/1d-skinny-lessons.md
  - restart/audit/totality/sk-v17/p1/1e-locks-evidence.md
  - restart/audit/totality/sk-v17/p1/1f-coherence-scan.md
  - restart/audit/totality/sk-v17/p1/1f-anti-pattern.md
  - restart/audit/totality/sk-v17/p1/1f-past-corpora.md
live_truth_method: "git rev-parse HEAD = 445925167 (matches all 8 frontmatters). Independent re-resolution of every load-bearing citation at master 445925167: grep -c scan_structural over 9 generated grammars (json/ebnf/bnf/csv/css_l4/css_pretty/google_sheets/bbnf = 1 each; math = 0); math.rs OnceCell mentions = 2 doc-comments (:281,:285), ScanState struct (:287-291) holds only nospace_bits/nospace_start (no structural_index field); json.rs genuine structural_index field at :701, scan call :732; css_l4 field :15951, scan call :15982, alphabet construction :15976-15982 (singletons/digraph_mask/digraph_pairs/quote_classes); scan call-site lines per grammar live-confirmed (json:732, css_l4:15982, bbnf:4843, bnf:848, csv:566, ebnf:1381, css_pretty:1905, google_sheets:3559); OnceCell doc-comment lines (ebnf:1335, bnf:802, csv:520, css_pretty:1859, google_sheets:3513, bbnf:4797); value_from_ref/ValueRef grep-zero in crates/core/src/runtime/{json,css_l4}/value.rs; skinny value_from_ref json/value.rs:143 over ValueRef<'doc,'input> :144, ValueRef struct skinny tape/mod.rs:175; skinny Tape SoA mod.rs:94 (offsets:96/flag_cursors:97/flag_values:98/payloads), from_offsets :104-117; core TapeRec #[repr(C,align(4))] :102, const-assert 16B/align4 :120-121; begin_compound(&StructLayout) reads layout.rule_id&0x1F :185-186; TapeStructBuilder grep-zero outside runtime/tape/ (sole non-tape hit = number.rs:17 doc-comment TapeRec::PAYLOAD_F64_DIRECT_BIT); enum BackendShape grep-zero in crates/, present skinny ir/lib.rs:340; derive_backend_shape skinny passes/lib.rs:392,401 only; EmitStrategy single-variant StructDirect strategy.rs:104-119, is_struct_direct :224; FieldSource enum struct.rs:84, StructLayout :202, StructRegistry :313-315 (BTreeMap), layout :331, layout_by_name :337, insert :326; StructLayout grep-rn crates/ = 960; LayoutFacts grep-rn crates/ = 0, present skinny passes/lib.rs:85,91; StructuralAlphabet alphabet.rs:19 (singletons:23,digraph_mask:28,digraph_pairs:32,quote_classes:37); KernelShape::select :118; neon::scan :47; avx2 cfg-gated x86_64; GRAMMAR_STRUCTURAL_ALPHABET emitter profile.rs:43,100; ensure_structural_index get_or_init support.rs:472-485; select_classifier skinny dispatch.rs:42; css_l4 builder pending = 6 Vec (:74-79) + 1 Option (:71), none Vec<Vec>; builder LOC css_l4=817/json=231; file LOC json=3505/css_l4=108406; top-level view.rs=76; tape/mod.rs:16-20 single generic StructBuilder, :54-56 dispatch on StructLayout; substrate.rs:60/73 CssStructBuilder/JsonStructBuilder doc-comments, panic :79; REDRESS-53 SPEC named-lines :577/:657/:825/:839 (:578 = continuation clause); SPEC :791-793 AZ-IV+StructRegistry pre-block, :793-795 StructRegistry block, :806 x86 bar, :808 sixth-shape bar, :807-811 second-substrate block, :854 D6 REJECT, :110-114 monotonic direction; LOCKS :75 (offset-tape admissible + columnar-SoA-buried clauses), :86 lazy-offset admitted, :100-116 FactStream 5th category, :107-108 5-shape canon, :137-149 v+1 ELEVATION, :160 Lock-2 retire, :349 Lock 14, :453 Lock 16, :784 AV.04 register; ARCH :1088 Lock-1 union, :1206 CollapsedStage NOT-ADMITTED x86-only/UNKNOWN-2D-05, :1803 FactStream output-plane (3); alphaC :13/:20-25 grep-zero skinny benched surface; prior-totality COH-014 p1/1F-coherence-scan.md:87. Deferral-pattern grep (will find|later inventory|defer to|resolved later) over all 8 artefacts = 0 outside verify_action/receiver columns."
counts:
  sections_dispositioned: 8
  accept: 8
  revise: 0
  reject: 0
accept_rate: 1.0
```

## Lens charge

CH6 ANTI-PAPER-CLOSE: no "excavated / resolved / wired" claim stands
without a live-evidence citation (cargo asm symbol, bench row, REDRESS
admit, file:line/SHA); no divergence is deferred to "a later inventory"
or "T-P2 will find" in place of present ground truth; every UNKNOWN
carries a verify_action. Ground truth is concrete, resolved to
file:line / SHA.

## V4 → V5 fold posture

V4 CH6 (`hardening/V4/CH6.md`) returned 8/8 ACCEPT / 0 REVISE / 0 REJECT
(100%). CH6 therefore carried no own-lens REVISE into V5. Two OTHER-lens
V4 REVISEs touched the inventories CH6 reviews, and both folded into V5;
CH6 re-resolves the fold evidence live before dispositioning, because a
fold that merely re-words without re-grounding is itself paper-close:

1. **1A Gaps-row (V5 own REVISE).** `1a:126` had said "value APIs are
   hand-written per grammar" — the SAME Lock-14 mischaracterisation
   V3-CH1-V3-003 charged against 1D. Folded: the Gaps row
   (`1a-substrate-evidence.md:140`) now reads "`@generated by xtask
   regen-{json,css}` … but EAGER, NOT hand-written — Lock-14 ALLOWED".
   **Verified live:** `head -1` of both core value modules carries
   `// @generated by xtask regen-json; do not edit by hand.` and
   `regen-css` respectively; `value_from_ref|ValueRef` grep-zero in both.
   The fold is recorded in the frontmatter (`1a:18`) with the disposition
   source.
2. **1E LAC-04 path-(b) + CH5-S8 (V4 REVISEs).** CH4 charged LAC-04
   path-(b) priced `LayoutFacts` as "~0 LOC (LayoutFacts already live)"
   where `grep -rn LayoutFacts crates/` = 0. Folded
   (`1e:181`): re-priced as skinny/prior-totality-only side-table with
   non-zero crates/core materialisation cost. **Verified live:**
   `grep -rn LayoutFacts crates/` = **0**; `LayoutFacts` present only in
   `skinny/crates/passes/src/lib.rs:85,91` (+ :109-110, :395, :404, …).
   CH5-S8 charged the REDRESS-53 anchor sat on the adjacent clause `:578`;
   folded to the verbatim naming lines. **Verified live:**
   `grep -n REDRESS-53 SPEC.md` = `:577`,`:657`,`:825`,`:839`; `:578` is
   the continuation clause "retained cursor / aux density / sidecar event
   vector", not the name. The 1E table (`:141`), the 1f-past-corpora
   ledger (`:59`), and the frontmatters all carry the corrected anchors.

**Both folds landed and are verified clean.** CH6 V5 re-resolves every
load-bearing citation at master `445925167` (see live_truth_method) and
confirms no new ground-truth slip was introduced by the V5 fold. The
cycle returns 8/8 ACCEPT — the second consecutive ≥95% CH6 cycle.

## Verification posture — every load-bearing citation re-resolved at master `445925167`

CH6 does not accept a single self-reported figure on recall. Each row
below was re-run live this cycle.

- **Scan census (the most-cited figure tree-wide).** `grep -c
  scan_structural` over the nine generated grammars: json/ebnf/bnf/csv/
  css_l4/css_pretty/google_sheets/bbnf = **1 each; math = 0**. The
  EIGHT-name wired census is exact, not recall. `math.rs` carries **no**
  `structural_index` field — its `ScanState` (`math.rs:287-291`) holds
  only `nospace_bits: u64` + `nospace_start: isize`; the two
  `OnceCell<StructuralIndex>` mentions (`:281`,`:285`) are doc-comment
  text. Contrast `json.rs:701`, the genuine `pub(crate) structural_index:
  OnceCell<…>` field. The V4-CH1 doc-comment-vs-field correction holds
  tree-wide (1A/1C/1D/1F all carry it).
- **Per-grammar scan call-site lines.** json:732, css_l4:15982,
  bbnf:4843, bnf:848, csv:566, ebnf:1381, css_pretty:1905,
  google_sheets:3559 — **all eight resolve exactly** (1C/1F cite these as
  call-site line anchors, NOT file LOC; 1C explicitly notes json.rs=3505,
  css_l4.rs=108406 are the file LOC). The OnceCell doc-comment lines
  (1f-anti-pattern AP17-002: ebnf:1335, bnf:802, csv:520, css_pretty:1859,
  google_sheets:3513, bbnf:4797) all resolve to `/// OnceCell<…>` lines.
- **Tape shape (core AoS).** `TapeRec` `#[repr(C, align(4))]`
  (`record.rs:102`), const-asserted 16-byte/align-4 (`:120-121`);
  `begin_compound(&StructLayout)` reads `(layout.rule_id & 0x1F)`
  (`mod.rs:185-186`) — **resolve.**
- **Tape shape (skinny SoA).** `Tape<'input>` (`mod.rs:94`) with
  `offsets: Vec<u32>` (`:96`), `flag_cursors: Vec<u32>` (`:97`),
  `flag_values: Vec<u8>` (`:98`), `from_offsets` with
  `debug_assert_eq!(flag_cursors.len(), flag_values.len())`
  (`:104-117`) — the SPARSE position-keyed side-vectors, NOT a dense
  class-column. `ValueRef<'doc,'input,K,G>` (`:175`) — **resolve.**
- **Value-API absence (core) / presence (skinny).** `value_from_ref|
  ValueRef` grep-**zero** in both core value modules; skinny
  `value_from_ref<'doc,'input>` at `json/value.rs:143` taking
  `ValueRef<'doc,'input>` (`:144`) — **resolve.**
- **BackendShape / selector absence in core.** `enum BackendShape` =
  **0** in `crates/`, present skinny `ir/lib.rs:340`; `derive_backend_shape`
  skinny-only at `passes/lib.rs:392,401`; `EmitStrategy` single-variant
  `StructDirect` (`strategy.rs:104-119`), `is_struct_direct` `:224` —
  **resolve.** The V5 1B anchor fix (`StructRegistry::layout` at
  `struct.rs:331`, `layout_by_name` :337, struct decl :313) is correct.
- **StructRegistry / FieldSource fence.** `FieldSource` enum
  (`struct.rs:84`), `StructLayout` (`:202`), `StructRegistry` BTreeMap
  (`:313-315`), `insert` (`:326`), `layout` (`:331`); `grep -rn
  StructLayout crates/` = **exactly 960** — the 960-site re-pricing
  across 1A/1C/1D/1E/1F is exact, not recall.
- **Live eager path / tape UNWIRED.** `TapeStructBuilder` grep-**zero**
  outside `runtime/tape/`; the sole non-`tape/` hit is the doc-comment
  `TapeRec::PAYLOAD_F64_DIRECT_BIT` at `number.rs:17` (a usage-free
  mention, exactly as 1A SUB17-003 states). CSS builder pending shape:
  six `pending_*` Vecs (`:74-79`) + one `pending_value: Option` (`:71`)
  = SEVEN pending_ fields, **none `Vec<Vec>`** — AP17-003 / SK17L-003 /
  CH5-S4 count is exact. CSS builder = 817 LOC, json = 231 LOC,
  top-level `view.rs` = 76 LOC (grammar-clean) — all exact.
- **Alphabet / classifier (BSHAPE17-009 NEW V5).** `StructuralAlphabet`
  {singletons :23, digraph_mask :28, digraph_pairs :32, quote_classes
  :37} (`alphabet.rs:19`); `KernelShape::select(alphabet)` (`:118`);
  `neon::scan` (`neon.rs:47`) aarch64-baseline; `avx2.rs` cfg-gated
  `x86_64`; `GRAMMAR_STRUCTURAL_ALPHABET` emitter (`profile.rs:43,100`);
  `ensure_structural_index` `get_or_init` scan-cache (`support.rs:472-485`,
  per-call alphabet construction inside the cache); skinny
  `select_classifier(alphabet: &'static [u8; 64])` (`dispatch.rs:42`) —
  **resolve.** The NEW V5 BSHAPE17-009 claim "shared classifier already
  grammar-general in core, NOT JSON-only" is grounded at every cited line.
- **LayoutFacts (1E V5 fold).** `grep -rn LayoutFacts crates/` = **0**;
  present skinny `passes/lib.rs:85,91` — **resolve.**
- **Spec / LOCKS surfaces.** ARCH:1088 (Lock-1 union "the structural
  projection IS the tape (no second sidecar)"); ARCH:1206 (CollapsedStage
  NOT-ADMITTED x86-only; "aarch64 candidate is UNKNOWN-2D-05 …
  collapsed_stage.rs:15-17"); ARCH:1803 (FactStream output-plane (3)
  diagnostic/audit-demoted). LOCKS:75 (offset-tape-admissible clause +
  "columnar SoA … never activated … Columnar SoA stays buried"),
  :86 (lazy-offset admitted), :100-116 (FactStream 5th category),
  :107-108 (5-shape canon), :137-149 (v+1 ELEVATION), :160 (Lock-2
  retire), :784 (AV.04 register). SPEC :577/:657/:825/:839 (REDRESS-53
  named-lines), :791-793 (AZ-IV + StructRegistry pre-block), :806/:808
  (x86 + sixth-shape bars), :807-811 (second-substrate block naming
  StructLayout/TapeStructBuilder/TapeCursor + sixth BackendShape +
  cross-call classifier-state retention), :854 (D6 REJECT), :110-114
  (monotonic direction). alphaC:13/:20-25 (grep-zero skinny benched
  surface). Prior-totality COH-014 at `p1/1F-coherence-scan.md:87` —
  **all resolve verbatim.**

A deferral-pattern grep (`will find | later inventory | defer to |
resolved later | T-P2 will | T-P3 will`) over all eight artefacts
returns **zero** hits outside verify_action / receiver columns. Every
Open Questions table carries a populated `verify_action` per declared
UNKNOWN row (1a: U-SUB17-001/002; 1b: U-BSHAPE17-001/002; 1c:
U-RT17-001/002/003; 1d: U-SK17L-001/002/003; 1e: 1E-SKV17-U1/U2;
1f-coherence: U-COH17-001/002/003; 1f-anti-pattern: U-AP17-001 — all
populated). No ground truth is parked for a later inventory.

---

## Dispositions

### §1 — 1d-skinny-lessons SK17L-001..010 + Do-Not-Redrive ledger + UNKNOWNs → **ACCEPT**

No paper-close. Every SK17L row resolves at the skinny + core file:lines
re-run this cycle: SK17L-001 (skinny SoA `Tape` mod.rs:94 vs core AoS
`TapeRec` record.rs:103), SK17L-002 (value-API eager-generated vs lazy
`ValueRef`, with the Lock-14 EAGER-vs-hand-written distinction correct
and `@generated` headers live-verified), SK17L-008 (EIGHT-name scan
census + math.rs field-absence verified by `ScanState` body read at
:287-291). SK17L-009 honestly separates by-construction generality from
by-exercise proof (JSON tape witness only). The JSON-Empirical vs
Grammar-Neutral split is explicit (CH2 firewall). The Do-Not-Redrive
ledger rows L-SK17-01..07 each carry a SPEC/alphaC anchor + fold fence;
L-SK17-02/02b state the StructRegistry no-per-leaf-lookup fence present-
tense at `struct.rs:313,331` + `tape/mod.rs:185`. The three UNKNOWNs
carry verify_actions; the "T-P2/T-P3" language sits only in those
columns. No divergence deferred to a future inventory. ACCEPT.

---

### §2 — 1a-substrate SUB17-001..009 + Substrate-Union Firewall + UNKNOWNs (V5 Gaps-row REVISE fold) → **ACCEPT**

The V5 own-REVISE is fully discharged: the Gaps row no longer claims
"hand-written"; it states `@generated … but EAGER` with the Lock-14
ALLOWED framing, the `@generated` headers live-verified at `value.rs:1`,
mirroring 1f-coherence:114 + 1d:146. The defect was localized to the one
prose Gaps row; the Spec-Claim Table (SUB17-004) and Cross-Tree map
already carried the correct framing. Every SUB17 row resolves at the
cited path:line (record.rs:102,120; skinny tape/mod.rs:94,96-98,175;
struct.rs:84,202,313,331; alphabet.rs:19-37; ARCH:1206; LOCKS:75,160).
SUB17-003 confirms tape UNWIRED (`TapeStructBuilder` grep-zero; sole
non-tape hit = number.rs:17 doc-comment, verified) with NO claimed fold.
The Substrate-Union Firewall states the per-leaf-lookup fence present-
tense. The two UNKNOWNs carry concrete verify_actions; the forward
disposition sits only in those columns. No paper-close, no deferral.
ACCEPT.

---

### §3 — 1b-codegen BSHAPE17-001..009 + do-not-redrive ledger + UNKNOWNs (V5 anchor fix + NEW BSHAPE17-009) → **ACCEPT**

The headline codegen facts resolve hard: `enum BackendShape` = 0 in
`crates/`, present skinny ir/lib.rs:340 (BSHAPE17-001);
`derive_backend_shape` skinny-only passes/lib.rs:392,401 (BSHAPE17-002);
single `EmitStrategy::StructDirect` strategy.rs:104-119 (BSHAPE17-003);
four 17-LOC scaffold lowerers vs 270-LOC sink_only (BSHAPE17-004,
wc-l method stated). The **V5 anchor fix** (BSHAPE17-006: the lookup
symbol is `StructRegistry::layout` at :331 / `layout_by_name` at :337,
NOT `lookup` at the struct-decl line :313) is verified live and recorded
in the frontmatter `symbol_anchor_corrections` — the pre-block fact (no
per-leaf registry indirection; `begin_compound` takes a pre-resolved
`&StructLayout`) is unchanged. The **NEW V5 BSHAPE17-009** is the
strongest anti-paper-close addition: it does NOT claim a fold is done;
it states impl-exceeds-spec with the shared classifier already
grammar-general across 8 grammars, every cited line (lib.rs:80,
alphabet.rs:19/118, profile.rs:43/100, support.rs:472-485, neon.rs:47,
avx2 cfg-gate) re-resolved this cycle. BSHAPE17-005 (CollapsedStage)
is labelled `unknown` → UNKNOWN-2D-05 at ARCH:1206 (a recorded open
unknown, not a fresh gap). Two UNKNOWNs carry verify_actions. No "later
inventory" deferral. ACCEPT.

---

### §4 — 1c-runtime RT17-001..007 + per-grammar census + UNKNOWNs (V5 math.rs field-absence fold) → **ACCEPT**

RT17-003 is the strongest anti-paper-close row: tape UNWIRED (grep-zero)
AND scan wired across all 8, both live-cited, labelled "spec-claims-
implemented (UNWIRED confirmed as stated)" — no claimed fold. The V5
fold (CH1-V4-001: math.rs OnceCell is doc-comment, scan_structural=0, no
`structural_index` field, `ScanState` only nospace_bits/nospace_start at
:288-289) is verified live — the `ScanState` body read confirms field
absence; the per-grammar scan call-site lines (json:732 … google_sheets
:3559) all resolve exactly; the file-LOC disambiguation (json.rs=3505,
css_l4.rs=108406 vs the call-site line anchors) is honest. RT17-002/006
carry the value-API + per-leaf-fence divergences present-tense. RT17-004
confirms Lock 14 honoured (substrate.rs:43,55 data-binding; :79 hard-fail
panic verified). The three UNKNOWNs each carry a verify_action scoped to
ALL 8 (math excluded). ACCEPT.

---

### §5 — 1e-locks Verified Invariants + L01/L02/L10/L14/L16 + COH-014 + LACs + UNKNOWNs (V5 LAC-04 + CH5-S8 folds) → **ACCEPT**

1E re-resolves the 16-lock count, the 5-shape canon, the all-8 scan
census, and the one-tape-construct invariant as a Verified Invariants
block. The two V5 folds are verified clean: LAC-04 path-(b) re-pricing is
grounded at `grep -rn LayoutFacts crates/` = **0** (live), with
`LayoutFacts` present only at skinny passes/lib.rs:85,91 — the "~0 LOC"
now prices only the doc-only lock re-scope, crates/core materialisation
flagged UNKNOWN; CH5-S8 re-anchors REDRESS-53 to :577/:825/:839 (live
`grep -n REDRESS-53` confirms; :578 is the continuation clause). The
per-lock verdicts are honest (L01 "partial / core-tape-UNWIRED, scan-
WIRED"; L02 "drifted" with the 960-site evidence; L10 "canon holds,
CollapsedStage aarch64-refused" → UNKNOWN-2D-05; L16 "aarch64 NEON
proven, x86 diagnostic"). The COH-014 continuity catch is carried. The
six LOCKS-AMENDMENTS-CANDIDATEs each carry supporting path:line +
loc/risk/wave_hint with the explicit "Candidates only; disposition is
T-P3 3C" guard — no premature amendment. Two UNKNOWNs carry
verify_actions. ACCEPT.

---

### §6 — 1f-coherence COH17-001..008 + Gaps row + UNKNOWNs → **ACCEPT**

The all-8 scan census holds (Gaps row + Cross-Tree map). COH17-001..008
resolve at the cited file:lines; the NEW COH17-007 FactStream citation
(ARCH:1803 output-plane (3)) is verified verbatim. COH17-007 is the lone
`unknown` carrying U-COH17-001 with a verify_action. U-COH17-002 is the
load-bearing CATALOGUED DIVERGENCE: it states the Lock-1 exactly-one-
encoding closure as present-tense ("a dual encoding is NOT a permissible
end-state") and rigorously distinguishes the admissible offset-tape-SoA
(skinny `Tape.offsets`, LOCKS:75 "offset tape" clause + :86 lazy-offset
admitted + ARCH:1088) from the dead AV.04 class-column-SoA (LOCKS:75
"Columnar SoA stays buried", AV.04 register :784) — the distinction
resolves at skinny tape/mod.rs:96-98 (sparse side-vectors) and is
correct. No claim of a fold that has not happened; the forward
disposition sits only in the verify_action column. ACCEPT.

---

### §7 — 1f-anti-pattern AP17-001..005 + CH5 verdict + UNKNOWN → **ACCEPT**

AP17-002 enumerates all 8 scan/OnceCell carriers with per-grammar doc-
comment line anchors (all six new ones verified live) and explicitly
scopes the firewall to 8, never 9, with the math.rs field-absence
reasoning (CH1-V4-001 carried). AP17-003 states the pending count as "six
`pending_*` Vecs (:74-79) plus one `pending_value: Option` (:71) = SEVEN
pending_ fields, NONE `Vec<Vec>`" — verified exact, with the fold
recorded. The god-module verdict (817-LOC CSS builder) is named the
SK-V18 fold-DELETION target citing tape/mod.rs:16-20 ("the single generic
StructBuilder impl … serves CSS, JSON, sheets, and bbnf" — verified
verbatim), not a permanent surface. AP17-005 (StructRegistry hot-path
indirection) is the present-tense fence. The CH5 firewall verdict scopes
honestly to "within crates/core in this scan"; AP17-001/004/006 hedge
un-proven absences with verify_actions. U-AP17-001 carries a
verify_action scoped to all 8. No deferral. ACCEPT.

---

### §8 — 1f-past-corpora ledger + COH-014 flag + monotonicity + carrier enumeration (V5 PC17-001 REDRESS-53 anchor fold) → **ACCEPT**

The Do-Not-Redrive ledger rows resolve verbatim live: SPEC :577/:825/:839
(REDRESS-53), :791-793 (AZ-IV + StructRegistry), :806/:826/:854 (x86 +
D6), :807-811 (second-substrate block naming StructLayout/
TapeStructBuilder/TapeCursor + sixth BackendShape + cross-call
classifier-state retention), alphaC:13/:20-25 (grep-zero skinny benched
surface). The V5 fold (PC17-001 REDRESS-53 anchor :578→:577 named-line)
is verified clean against the live `grep -n REDRESS-53`. The Do-Not-Carry-
Undercount flag records the all-8 live census with per-grammar line
anchors; the prior-totality COH-014 reference resolves at
`p1/1F-coherence-scan.md:87`. The Second-Substrate Carrier Enumeration
lists all 8 carrier field/doc lines (json.rs:701 field; css_l4:15951;
ebnf:1335; bnf:802; csv:520; css_pretty:1859; google_sheets:3513;
bbnf:4797 — all verified). The direction-monotonicity note (skinny →
totality, never back) is correct and load-bearing. No divergence
deferred; all present-tense. ACCEPT.

---

## Cross-cutting CH6 observations

1. **The V4 carried zero CH6 REVISE; the two other-lens V5 folds landed
   verified.** 1A's Gaps-row "hand-written" → "@generated EAGER" fold and
   1E's LAC-04 (`LayoutFacts` grep-zero) + CH5-S8 (REDRESS-53 :577/:825/
   :839) folds are each re-grounded live this cycle with no new slip.

2. **No tape claim is paper-closed as "wired."** Every artefact states the
   crates/core tape is UNWIRED dead code (`TapeStructBuilder` grep-zero
   re-confirmed; the sole non-tape hit is the number.rs:17 doc-comment).
   The SK-V18 fold is consistently named FUTURE with the proven skinny
   shape as source; the monotonic skinny→totality direction holds.

3. **The NEW V5 BSHAPE17-009 is anti-paper-close exemplary.** It resolves
   a prompt-named divergence ("NEON JSON-only vs shared-classifier
   generalization") to impl-exceeds-spec with 0-LOC fold, citing the
   already-general classifier across 8 grammars at every line — it does
   NOT claim a fold, it states the spec narrative is what must absorb the
   landed reality. No future-inventory substitution.

4. **The one load-bearing dual-substrate divergence is catalogued
   present-tense, not deferred.** U-COH17-002 (and its U-SUB17-001 /
   U-RT17-002 / U-SK17L-002 / 1E-SKV17-U1 siblings) states the Lock-1
   exactly-one-encoding closure as a present obligation with the
   offset-tape-vs-class-column distinction resolved at file:line — the
   forward disposition ("T-P2 names the convergence target") sits only in
   the verify_action column.

5. **Every UNKNOWN carries a verify_action; zero deferral-pattern hits.**
   The "T-P2/T-P3" language is confined to reconciliation/verify_action
   columns across all eight artefacts; no divergence row substitutes a
   future inventory for present file:line ground truth.

## Disposition summary

| § | Subject | Disposition |
|---|---|---|
| 1 | 1d SK17L-001..010 + ledger | ACCEPT |
| 2 | 1a substrate SUB17-001..009 + firewall + UNKNOWNs (V5 Gaps-row REVISE fold) | **ACCEPT** — fold verified: `@generated` headers live, "hand-written" struck |
| 3 | 1b codegen BSHAPE17-001..009 + ledger + UNKNOWNs (V5 anchor fix + NEW 009) | **ACCEPT** — anchor fix verified (`layout`@:331); BSHAPE17-009 grounded |
| 4 | 1c runtime RT17-001..007 + census + UNKNOWNs (V5 math.rs field-absence fold) | **ACCEPT** — ScanState body :288-289 verified field-absent |
| 5 | 1e locks invariants + L01/02/10/14/16 + COH-014 + LACs (V5 LAC-04 + CH5-S8 folds) | **ACCEPT** — `LayoutFacts` grep-0 + REDRESS-53 :577/:825/:839 verified |
| 6 | 1f-coherence COH17-001..008 + Gaps + U-COH17-002 divergence | ACCEPT |
| 7 | 1f-anti-pattern AP17-001..005 + CH5 verdict | ACCEPT |
| 8 | 1f-past-corpora ledger + COH-014 + monotonicity (V5 PC17-001 anchor fold) | **ACCEPT** — REDRESS-53 :578→:577 fold verified |

**Counts:** 8 dispositioned · 8 ACCEPT · 0 REVISE · 0 REJECT · ACCEPT-rate 100%.

**Verdict:** the SK-V17 T-P1 V5 inventory set is well-grounded on its
substrate / value-API / BackendShape / NEON spine — near-zero recalled
LOC, every load-bearing citation re-resolved live at master `445925167`
(scan census, tape AoS/SoA shapes, value-API absence/presence,
BackendShape grep-zero, 960-site StructLayout, LayoutFacts grep-zero,
REDRESS-53 named-lines, the alphabet/classifier surface, the CSS-builder
seven-pending-field count, all eight scan call-site + OnceCell doc-comment
lines). The three V5 folds (1A Gaps-row "@generated", 1E LAC-04 +
CH5-S8 re-anchor, 1C/1F math.rs field-absence carry, 1f-past-corpora
REDRESS-53 :578→:577) are each verified clean with no new ground-truth
slip introduced. The NEW V5 BSHAPE17-009 resolves a prompt-named
divergence to impl-exceeds-spec without claiming a fold. The core tape
remains honestly UNWIRED dead code across all artefacts; the SK-V18 fold
is named FUTURE; the Lock-1 exactly-one-encoding closure is catalogued
present-tense (not deferred); zero deferral-pattern hits; every UNKNOWN
carries a verify_action. CH6 returns 100% ACCEPT — the second consecutive
≥95% CH6 cycle, with zero orphan REVISE. On the CH6 lens this satisfies
the ORCHESTRATOR §3Z convergence criterion (≥95% × 2 consecutive).
