# SK-V13 W13.5 Research - GSOC Typed Product Surface

Wave: W13.5 typed product surface completion.
Scope: `json/gsoc-2018/real_typed_struct/main`.
Date: 2026-05-22.

## Cohort

Six read-only slices were redeployed after W13.4:

- Slice A, `gsoc-2018`: root object with 1,264 numeric keys, each value a
  `SoftwareSourceCode` proposal record. This is a true map-entry product and
  exercises the existing generated map-entry root path.
- Slice B, unicode rows: `unicode_mixed`, `unicode_escapes`, and
  `y_string_unicode` are feasible typed products, but they carry escape-heavy
  string allocation risk and should stay row-specific after the map-entry row.
- Slice C, `distinct_values`: still requires an honest mixed fixed-field plus
  dynamic-entry collector. A partial struct would hide the dominant payload and
  is not admissible.
- Slice D, direct reopen routes: output-digest SIMD and typed/direct crossover
  remain direct-plane candidates; they do not close this missing typed product
  row.
- Slice E, schema/gate surface: W13.1-W13.4 proved the typed-product companion
  gate can admit one row at a time with strict sonic+1 and serde Track 2.
- Slice F, generated-size risk: `gsoc-2018` should add one map-entry root and
  three product structs, materially smaller than W13.4.

## Fixture Shape

`skinny/test_data/gsoc-2018.json` is a root object:

```text
{
  "0": { "@context", "@type", "name", "description", "sponsor", "author" },
  ...
}
```

Every entry has the same six top-level fields. `sponsor` is an organization
object with `@type`, `name`, `disambiguatingDescription`, `description`, `url`,
and `logo`. `author` is a person object with `@type` and `name`.

The product model is:

```text
Vec<GsocProposalEntry<'i>> {
  key: Cow<'i, str>,
  value: GsocProposal<'i>,
}
```

`GsocProposal<'i>` covers all six proposal fields; `GsocSponsor<'i>` and
`GsocAuthor<'i>` cover the nested objects. Serde sidecars need explicit renames
for `@context`, `@type`, and `disambiguatingDescription`.

## Finding

`gsoc-2018` is the next lowest-risk missing typed surface. It is larger and
string-heavy, but it avoids unicode escape semantics and uses generated
map-entry routing already exercised by CITM and the W5 proof fixture. A
partial digest, root-only key collector, or skipped nested `sponsor`/`author`
object would be support-only and must reject.

## Revert Protocol

If W13.5 fails, revert the GSOC typed root, generated parser, fixture
enum/output/checksum/bench routing, companion report extension, RESULTS and
rolling updates, and REDRESS entry. Record whether the failure is parity,
throughput, generated-size growth, gate consumption, or map-entry coupling.
