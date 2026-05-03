# Hardening 08 — Substrate Abrogation

Date: 2026-05-03  
Repo: `/Users/mkbabb/Programming/bbnf-lang`  
Baseline observed: `baf7df2d07cd130a5ad2b8f81fc339418406a3b3`

## Sources

- `docs/HARDENING-AUDIT-PROMPT.md` §Substrate-Abrogation.
- `docs/tranches/meta-audit/08-abrogation-catalog.md`.
- `docs/tranches/AZ-IV/audit/DEEP-A-struct-projection-assay.md`, `POST-CLOSE-B-substrate.md`.
- BA/BB/BC top-level and waves.
- Commands:
  - `wc -l crates/core/src/runtime/{arena_template,builder_template}.rs`
  - `find crates/core/src/runtime -path '*/arena.rs' -o -path '*/builder.rs' | sort | xargs wc -l`
  - `rg -c <pattern> crates`

## Verdict Counts

| Verdict | Count | Estimated LOC delta |
|---|---:|---:|
| KEEP | 1 | 0 |
| KEEP-MODERNIZE | 3 | -120 to -250 |
| REPLACE | 5 | -650 to -1,000 |
| ABROGATE | 7 | -4,200 to -5,000 |
| FOLD-INTO-TOOLING | 2 | -50 to -120 |
| Total | 18 catalog rows | approx. -5,020 to -6,370 |

LOC deltas are forecasts. Generated-output deletions are intentionally excluded from the high end unless the wave spec names checked-in generated output deletion.

## Current Occurrence Snapshot

| Target | Observed current surface | Owning wave | Verdict |
|---|---:|---|---|
| `arena_template.rs` | 134 LOC | BA.W2 (`docs/tranches/BA/BA.md:57`, `:142`) | ABROGATE |
| `builder_template.rs` | 286 LOC | BA.W2 (`docs/tranches/BA/BA.md:57`, `:142`) | ABROGATE |
| per-grammar `arena.rs` / `builder.rs` | 19 files, 3,928 LOC including `runtime/builder.rs`; 3,787 LOC excluding shared `runtime/builder.rs` | BA.W2 (`docs/tranches/BA/BA.md:110`, `:143`) | ABROGATE / REPLACE per grammar |
| `__EAGER_EMPTY_PATH` | 20 hits in `crates`, including 9 generated modules and emitter | BA.W4 (`docs/tranches/BA/BA.md:84`, `:141`) | ABROGATE |
| `LegacyPath` / `LegacySegment` | 46 hits in `crates`; production hits in 4 `parse_with.rs` shims plus tests | BA.W5 (`docs/tranches/BA/BA.md:58`, `:144`) | ABROGATE |
| `cursor.match_field` / `match_index` / `decide` | 259 hits in `crates`; generated code and emitter shapes dominate | BA.W5 (`docs/tranches/BA/BA.md:59`, `:145`) | REPLACE |
| `Vec<OpenFrame>::clone` literal | 0 direct literal hits; checkpoint methods still clone stack through per-builder checkpoint structs | BA.W3 (`docs/tranches/BA/BA.md:55`, `:140`) | REPLACE |
| per-grammar `__path_plan` re-exports | 27 hits in `crates`; 9 generated modules, 4 runtime `parse_with` consumers, emitter docs/code | BA.W5 (`docs/tranches/BA/BA.md:60`, `:146`) | ABROGATE |
| 32 zero-caller substrates | predecessor says 32 zero-caller `pub` substrates (`docs/tranches/AZ-IV/audit/POST-CLOSE-B-substrate.md:7-8`) | BA.W0 (`docs/tranches/BA/BA.md:51`, `:70`) | ABROGATE / KEEP-MODERNIZE row-by-row |
| `AscentStrategy` | 18 hits in `crates`; trait, 3 impls, cursor optional hook, bench | Not wave-owned in active BA/BB/BC top-level | KEEP-MODERNIZE |
| `Option<&mut PathCursor>` | 0 direct hits in `crates`; BA deletion-bias forbids reintroduction (`docs/tranches/BA/BA.md:169`) | BA.W4/W5 guard | KEEP as ban |
| `rule_type: TypeDesc::Span` | 3 non-generated hits in fixtures/tests; BA top-level still claims 9 emission sites (`docs/tranches/BA/BA.md:17`) | BA.W1/W2 claim validation | FOLD-INTO-TOOLING |

## Per-Verdict Catalog

### ABROGATE

`arena_template.rs`, `builder_template.rs`, direct per-grammar arena/builder hot-path files, `__EAGER_EMPTY_PATH`, `LegacyPath`/`LegacySegment`, per-grammar `__path_plan`, and the predecessor zero-caller substrate rows are correctly named as deletion targets. BA assigns most of them to W0/W2/W4/W5 with closure proofs. This is the strongest part of the plan.

Risk: BA.W2 says delete per-grammar `{arena,builder}.rs` "where direct-projection subsumes them" (`docs/tranches/BA/waves/W2.md:101-102`). That qualifier is correct engineering, but BA hard gate 9 states the files are deleted across all listed grammars (`docs/tranches/BA/BA.md:143`). The wave needs a per-grammar disposition table so a retained non-hot-path file cannot be mistaken for a hard-gate miss.

Paste-ready amendment:

```md
### BA.W2 per-grammar arena/builder disposition

BA.W2.5 must write `docs/tranches/BA/audit/W2-arena-builder-disposition.md` with one row per grammar:

`grammar | arena.rs action | builder.rs action | consumer if retained | direct-projection replacement | verification command`

BA hard gate 9 closes only when every retained file has a named non-hot-path consumer and every value-API hot-path arena/builder file is deleted.
```

### REPLACE

The cursor triplet and checkpoint discipline should be replaced, not merely deleted. BA.W5's `cursor.consult(&ParsedSegment)` is the correct replacement for `match_field`, `match_index`, and `decide`. BA.W3's value checkpoint is the correct replacement for stack clone. The literal `Vec<OpenFrame>::clone` grep is not sufficient because current code may clone through `self.stack.clone()` in checkpoint methods without spelling the fully-qualified type.

Paste-ready amendment:

```md
### BA.W3 checkpoint grep proof

The W3 close artefact must include both:

- `rg -n "stack\\.clone\\(|\\.stack\\.clone\\(|Vec<OpenFrame>::clone|fn checkpoint" crates/core/src/runtime crates/core/src/grammar/generated`
- samply top-3 proof under the 7-artefact contract

The grep proves source-shape retirement; samply proves cost retirement. Neither alone closes the gate.
```

### KEEP-MODERNIZE

`AscentStrategy` is the unresolved row. It is listed in the prompt's deletion targets, but active BA/BB/BC specs do not own its disposition. Current code exposes the trait through `crates/core/src/path/mod.rs`, stores it optionally in `PathCursor`, and benches it in `crates/core/benches/path_ascent.rs`. This is not a safe silent deletion target during BA unless `Document::get<T>` no longer needs parent ascent after W4/W5.

Fix: add BA.W5 or BC.W0 disposition. If W5's typed-path collapse removes all production consumers, abrogate there; otherwise BC.W0 records it as retained with a consumer.

### FOLD-INTO-TOOLING

The `rule_type: TypeDesc::Span` count has drifted. BA top-level says nine emission sites; current `rg` finds three fixture/test hits in `crates`, while generated parse fns contain many `StructLayout` literals under fully-qualified paths. The close discipline should move from exact phrase grep to a purpose-built AST/text check that looks for generated parse-entry `StructLayout` construction and distinguishes registry initializers from runtime layout literals.

## Cumulative Delta

The named runtime-template and per-grammar arena/builder abrogation alone targets roughly 4,207 LOC (`134 + 286 + 3,787`). Cursor/checkpoint/path-plan replacement should remove more generated and emitter boilerplate, but the net depends on direct-projection emitter size. The audit forecast is a net deletion of roughly 5,020-6,370 LOC if BA.W2/W4/W5 land as specified and BC.W1 performs directory-module splits without preserving duplicate flat siblings.

## Blocking Finding

`AscentStrategy` has no active wave owner despite being in the brief's deletion list. That makes it a scaffold drift finding: either assign it to BA.W5/BC.W0 with a concrete consumer test, or remove it from the current-cycle abrogation target list.
