# G4 Wave Ledger — shared Cursor value-API trait + phantom `<G>` DELETE (R-D A)

Date: 2026-06-02. Wave: SK-V18 G4. Gate: SPEC §7 (achievable AS-SPECIFIED, no amendment — the
research confirmed the phantom has zero production animator + the Cursor trait is structurally
non-LCD-flattening). Outcome: **GREEN** (all §7 conjuncts held; the agent rate-limited before this
ledger — verification completed by the orchestrator).

## 1 — G4a: the phantom `<G>` DELETE
`ValueRef<'doc,'input, K = AnyKind, G: EventGrammar = AnyGrammar>` → `ValueRef<'doc,'input, K = AnyKind>`
(the `_grammar: PhantomData<fn() -> G>` field + the `G` param dropped). The phantom scaffolding deleted:
`tape/event_grammar.rs`, `tape/event_grammar_tests.rs`, `grammars/json/event_grammar_witness.rs`,
`grammars/sheets_witness/event_grammar_witness.rs` + `grammars/sheets_witness/mod.rs`, with the
`lib.rs` module decls. `G` was layout-invariant (every production `ValueRef` already used the 3-arg
form), so the DELETE changes no production type — generated output stays byte-identical (regen clean).

## 2 — G4b: the shared Cursor trait
New `tape/cursor.rs` micro-trait sharing ONLY {tape, cursor/offset, kind() via `type Kind`}, carrying
NO navigation verb. The latent `DocumentView` (`tape/mod.rs:227`) extended to CssDocument. JSON
(`ValueRef<K>`, via grammars/json/view.rs + json_templates/view.rs) and CSS (CssNode/CssDocument, via
css_scan_direct/projection.rs) both impl the seam — emitted via the codegen templates + regenerated 1:1.

## 3 — Telemetry (§7 exit gate, all GREEN)
```text
phantom_generic_resolved        = deleted   (grep G:EventGrammar|EventGrammar|PhantomData<fn()->G> in runtime/src production, test-excluded == 0)
json_rich_navigation_preserved  = true      (JSON get/pairs/typed-JsonValue/recursive-visitor INTACT; json/generated.rs regen byte-equal; JSON 51/51 strict held)
shared_trait_impl_count         = 2+        (JSON ValueRef<K> + CSS CssNode/CssDocument; PROVE adds the 3rd Sheets impl)
shared_trait_non_collapsible    = true      (navigation lives OUTSIDE the trait — substituting JSON's impl for CSS's CssNode does NOT compile; the trait is too narrow to erase JSON's tree)
json_51_held                    = true      (runtime parse/parse_direct/parse_only suite green)
css_9field_parity_held          = true      (rich_cssom_matches_cssparser_on_fixture + _on_real_corpora ok)
regen_clean                     = true      (check-json + check-css-l4-declaration-values exit 0)
build_green                     = true      (runtime + codegen ax-iter exit 0)
```

## 4 — Honest residual (non-blocking, not the phantom)
`codegen/src/lower/tape_plan.rs:25` emits the descriptor STRING `"ParserState+TapeBuilder+EventGrammar"`
— a profile-IR capability label, NOT the `ValueRef<G>` phantom. It lives in codegen/ (outside the
runtime/src phantom grep) and is already documented exempt in `lock14_baseline.rs`. A campaign-wide
`EventGrammar`-token rename is a separate descriptor cleanup, not G4's job.

## 5 — Gate authorization
G4 touched lock14-frozen surfaces (`tape/`, `grammars/json/`, `json_templates/`, the deleted witnesses);
the owner-path set extended to cover the runtime/template generalization surfaces (the established
per-wave pattern). The firewall stays meaningful (re-inject still RED; freeze still catches non-wave changes).

Next: G5/G6 (NEON retarget of the find_component_delim scan / css_balanced_component_scan inner kernel,
under dav1d discipline). G4 BLOCKS PROVE (Sheets instantiates the Cursor trait as the 3rd impl).
