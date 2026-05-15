# SK-V6 Wave 3 R1g: real typed-struct direct workload

## Read basis

- `skinny/REDRESS.md` entries 66-69 reject the current direct-string close
  family under the synthetic `JsonDirectDigest` workload:
  - 66: direct source-hook field-layout materializer moved the target rows by
    ~0-2%, not enough.
  - 67: parser-owned decoded scratch regressed `unicode_escapes` by 44%.
  - 68: byte-output `unescape_json_string` regressed `unicode_escapes` by 4%.
  - 69: `DirectBuild` semantic string facts regressed `unicode_escapes` by
    roughly 15%.
- `restart/skinny/audit/GRAND-SYNTHESIS-SK-V6.md` section 12 outcome says the
  generated direct string/Unicode materialization family is closed for the
  current strict digest workload. The next admissible plan must reassess the
  direct output contract: benchmark a real typed-struct workload with
  field-specific access patterns, or explicitly classify the synthetic digest
  workload as a SOTA stressor rather than a representative DirectBuild closure
  gate.
- `skinny/RESULTS.md` direct matrix records `N-direct / NoGo`: correctness is
  green, Track 1 is generated `runtime::generated_json::parse_direct`, Track 2
  is independent hand-coded direct parsing, and 13 of 17 direct rows miss the
  sonic-rs 1.10x time gate.
- `skinny/crates/bbnf-bench/src/direct_struct.rs` currently owns the direct
  workload. It builds `JsonDirectDigest`, not a domain struct. Track 1 uses
  `runtime::generated_json::parse_direct(input, &mut JsonDigestSink)`;
  Track 2 uses `hand::HandParser`; sonic-rs and serde_json deserialize the same
  digest type.
- `skinny/crates/bbnf-bench/src/bin/profile_direct.rs` owns direct profiling
  mode selection (`track1|track2|sonic|serde`) and fixture lookup.
- Corpus shape comes from
  `skinny/crates/test-fixtures/corpus/json/manifest.toml`,
  `skinny/crates/test-fixtures/src/lib.rs`, and `skinny/test_data/*.json`.

## Recommendation

Add a supplemental workload first: `real_typed_struct`, with a gate separate
from the synthetic `direct_to_struct` digest matrix. Do not replace the digest
gate until this typed workload has two consecutive same-HEAD full runs proving
that it is stable and that changes that win typed rows do not hide regressions
in the existing digest rows.

The strict real workload should contain exactly two fixtures:

1. `twitter` from
   `skinny/crates/test-fixtures/corpus/json/twitter.json`.
2. `update_center` from `skinny/test_data/update-center.json`, exposed by the
   manifest as fixture name `update_center`.

This pair is stricter than a cherry-picked projection:

- `twitter` is real social JSON with 100 statuses, nested users/entities,
  nullable fields, booleans, integer IDs, real non-ASCII tweet/user text, and
  recursive retweeted status objects.
- `update_center` is real Jenkins update-center JSON with 654 plugins, a
  dynamic object map, arrays of dependencies/developers/labels, long
  certificate/signature strings, optional fields, and field-specific map access.
- Together they force all tracks to parse full documents, materialize owned
  typed values, preserve dynamic object keys where the real schema requires
  them, and expose the field-specific DirectBuild pattern that the synthetic
  digest cannot represent.

## Exact Output Plane

All four implementations must return the same owned Rust output plane, then a
post-parse checksum may be computed from that output for benchmarking. The
checksum is only the black-box sink; it must not be produced during parse.

Module owner: add under `skinny/crates/bbnf-bench/src/direct_struct.rs` or a
new sibling `skinny/crates/bbnf-bench/src/real_typed_struct.rs`, re-exported
from `bbnf-bench`.

### Twitter Structs

Fixture: `twitter`.

Root:

```rust
pub struct TwitterSearch {
    pub statuses: Vec<TwitterStatus>,
    pub search_metadata: TwitterSearchMetadata,
}
```

Required struct family:

- `TwitterStatus`
  - `metadata: TwitterStatusMetadata`
  - `created_at: String`
  - `id: u64`
  - `id_str: String`
  - `text: String`
  - `source: String`
  - `truncated: bool`
  - `in_reply_to_status_id: Option<u64>`
  - `in_reply_to_status_id_str: Option<String>`
  - `in_reply_to_user_id: Option<u64>`
  - `in_reply_to_user_id_str: Option<String>`
  - `in_reply_to_screen_name: Option<String>`
  - `user: TwitterUser`
  - `geo: Option<serde_json::Value>` initially only if the fixture contains
    only nulls; otherwise promote to a typed geo struct before admission
  - `coordinates: Option<serde_json::Value>` under the same null-only rule
  - `place: Option<serde_json::Value>` under the same null-only rule
  - `contributors: Option<serde_json::Value>` under the same null-only rule
  - `retweet_count: u64`
  - `favorite_count: u64`
  - `entities: TwitterEntities`
  - `favorited: bool`
  - `retweeted: bool`
  - `lang: String`
  - `retweeted_status: Option<Box<TwitterStatus>>`
  - `possibly_sensitive: Option<bool>`
- `TwitterUser`
  - all observed user keys from the fixture:
    `id`, `id_str`, `name`, `screen_name`, `location`, `description`, `url`,
    `entities`, `protected`, `followers_count`, `friends_count`,
    `listed_count`, `created_at`, `favourites_count`, `utc_offset`,
    `time_zone`, `geo_enabled`, `verified`, `statuses_count`, `lang`,
    `contributors_enabled`, `is_translator`, `is_translation_enabled`,
    `profile_background_color`, `profile_background_image_url`,
    `profile_background_image_url_https`, `profile_background_tile`,
    `profile_image_url`, `profile_image_url_https`, `profile_banner_url`,
    `profile_link_color`, `profile_sidebar_border_color`,
    `profile_sidebar_fill_color`, `profile_text_color`,
    `profile_use_background_image`, `default_profile`,
    `default_profile_image`, `following`, `follow_request_sent`,
    `notifications`.
- `TwitterEntities`
  - `hashtags: Vec<TwitterHashtag>`
  - `symbols: Vec<TwitterSymbol>`
  - `urls: Vec<TwitterUrl>`
  - `user_mentions: Vec<TwitterUserMention>`
  - `media: Option<Vec<TwitterMedia>>`
- Leaf structs:
  - `TwitterHashtag { indices: [u64; 2], text: String }`
  - `TwitterUrl { indices: [u64; 2], url: String, expanded_url: String, display_url: String }`
  - `TwitterUserMention { indices: [u64; 2], id: u64, id_str: String, name: String, screen_name: String }`
  - `TwitterSearchMetadata` with all observed search metadata keys:
    `completed_in`, `max_id`, `max_id_str`, `next_results`, `query`,
    `refresh_url`, `count`, `since_id`, `since_id_str`.

Admission rule: no broad `serde_json::Value` escapes except fields proven
null-only in the checked fixture. If a future fixture revision makes a null-only
field non-null, the workload must fail until that field gets a typed struct.

### Update Center Structs

Fixture: `update_center`.

Root:

```rust
pub struct UpdateCenter {
    pub connection_check_url: String,
    pub core: UpdateCore,
    pub id: String,
    pub plugins: BTreeMap<String, UpdatePlugin>,
    pub signature: UpdateSignature,
    pub update_center_version: String,
}
```

Required struct family:

- `UpdateCore`
  - `build_date: String`
  - `name: String`
  - `sha1: String`
  - `url: String`
  - `version: String`
- `UpdatePlugin`
  - `build_date: String`
  - `compatible_since_version: Option<String>`
  - `dependencies: Vec<UpdateDependency>`
  - `developers: Vec<UpdateDeveloper>`
  - `excerpt: String`
  - `gav: String`
  - `labels: Vec<String>` with missing treated as empty only if serde uses the
    same default
  - `name: String`
  - `previous_timestamp: Option<String>`
  - `previous_version: Option<String>`
  - `release_timestamp: String`
  - `required_core: String`
  - `scm: String`
  - `sha1: String`
  - `title: String`
  - `url: String`
  - `version: String`
  - `wiki: Option<String>`
- `UpdateDependency`
  - `name: String`
  - `optional: bool`
  - `version: String`
- `UpdateDeveloper`
  - `developer_id: String`
  - `email: String`
  - `name: String`
- `UpdateSignature`
  - `certificates: Vec<String>`
  - `correct_digest: String`
  - `correct_signature: String`
  - `digest: String`
  - `signature: String`

Admission rule: the dynamic plugin object key must be preserved in the
`BTreeMap`. It is not enough to materialize only the plugin values, because the
real typed workload includes map lookup and key ownership.

## Track Definitions

Owner files:

- `skinny/crates/bbnf-bench/src/direct_struct.rs` or new
  `skinny/crates/bbnf-bench/src/real_typed_struct.rs`: structs, checksum,
  parity, and three/four track entry points.
- `skinny/crates/bbnf-bench/src/bin/profile_direct.rs`: add modes such as
  `typed-track1`, `typed-track2`, `typed-sonic`, `typed-serde`, or add a new
  binary `profile_real_typed`.
- `skinny/crates/bbnf-bench/benches/json_parity.rs` and
  `skinny/crates/bbnf-bench/src/bin/gate.rs`: add reporting rows only after
  smoke proves the workload is stable.
- `skinny/crates/test-fixtures/src/lib.rs` and
  `skinny/crates/test-fixtures/corpus/json/manifest.toml`: fixture ownership
  stays unchanged; no new fixture is needed.
- Generated parser owner remains
  `skinny/crates/runtime/src/grammars/json/generated.rs`, generated from
  `skinny/crates/codegen/src/json_sink_direct.rs`.

Track 1:

- Generated DirectBuild path only:
  `runtime::generated_json::parse_direct(input, &mut TypedBuilderSink)`.
- `TypedBuilderSink` may be hand-written initially as the consumer, but it must
  build the exact owned `TwitterSearch` or `UpdateCenter` output. It may not
  compute the benchmark checksum during parse.
- If the implementation later codegens the typed sink from schema, that is
  admissible, but the parser path must still be generated `parse_direct`.

Track 2:

- Independent parser path:
  a fixture-specific hand parser that never calls
  `runtime::generated_json::parse` or `parse_direct`.
- It may reuse low-level trusted lexical helpers already used by the current
  hand parser (`match_json_string_at_quote_trusted_utf8`,
  `unescape_json_string`, number materializers, whitespace skipper), but object
  field dispatch and typed construction must be independently authored.

Sonic-rs / serde anchors:

- `sonic_rs::from_slice::<TwitterSearch>` and
  `sonic_rs::from_slice::<UpdateCenter>`.
- `serde_json::from_slice::<TwitterSearch>` and
  `serde_json::from_slice::<UpdateCenter>` as a secondary typed anchor.
- The struct derives are shared by sonic-rs and serde_json. Track 1 and Track 2
  must return the same owned structs, not a cheaper mirror type.

## Falsifiability Gate

Correctness gate:

- For each fixture and mode, parse to the owned typed struct.
- Assert `Track1 == Track2 == sonic_rs == serde_json` at the typed output
  level.
- Compute `typed_checksum(&output)` only after parse and assert equal
  checksums. The checksum function must recursively visit every field listed
  above, including map keys and optional/null state.
- Assert fixture-lock shape:
  - `twitter.statuses.len() == 100`
  - `update_center.plugins.len() == 654`
  - `update_center` dependency/developer/label counts match the current
    fixture facts: dependencies 386, developers 852, labels 770.
- Existing correctness commands still apply:
  `cargo test -p runtime --profile ax-iter`,
  `cargo test -p bbnf-bench --profile ax-iter`,
  `cargo run -p xtask --release -- check-json`,
  `cargo run -p xtask --release -- check-conformance`.

Throughput gate:

- Same-HEAD release binaries, five paired samples per mode.
- Primary metric: Track 1 and Track 2 typed median time must each be within
  1.10x sonic-rs typed median time for both `twitter` and `update_center`.
- Secondary anchor: Track 1 must be within 1.10x serde_json typed median time
  unless sonic-rs is slower than serde_json on that row; if sonic-rs is slower,
  sonic remains the S anchor.
- The existing synthetic `direct_to_struct` digest rows remain guard rows for
  at least the first two full typed runs:
  - no existing passing digest row may lose PASS;
  - no existing digest row may regress by more than 5%;
  - `unicode_escapes`, `unicode_mixed`, `distinct_values`, and
    `y_string_unicode` must be reported, not silently omitted.

Reject conditions:

- Reject if Track 1 or Track 2 computes a digest during parse instead of
  materializing the typed output.
- Reject if Track 1 skips unknown fields that serde/sonic materialize into the
  declared typed output, or if serde/sonic ignores fields that Track 1 uses.
- Reject if the workload narrows to only cheap fields, selected scalar facts,
  decoded length/fingerprint facts, or string-only projections.
- Reject if `serde_json::Value` appears outside explicitly null-only Twitter
  placeholders during initial admission.
- Reject if a new directive, new top-level BIR variant, retained side table, or
  parallel source scan is required merely to make the typed workload pass.
- Reject replacing the digest gate if typed rows pass while digest guard rows
  materially regress. In that case the typed workload supplements the digest
  gate and the digest workload remains the SOTA stressor.

## Decision

Use `real_typed_struct` as a supplemental gate first. Its strict form is:

- `twitter` -> `TwitterSearch`
- `update_center` -> `UpdateCenter`
- Track 1 generated `parse_direct` + owned typed builder sink
- Track 2 independent typed hand parser
- sonic-rs and serde_json into the exact same serde-derived structs
- post-parse checksum only

This is a valid replacement candidate only after it proves stable against the
existing digest guard. Until then, it should supplement the synthetic digest
gate, not replace it.
