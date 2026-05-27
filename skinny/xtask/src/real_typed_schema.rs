use codegen::direct_schema::{
    DirectFieldSchema, DirectIgnoredFieldSchema, DirectRootSchema, DirectScalar, DirectSchemaSet,
    DirectSkipKind, DirectStringEnumVariant, DirectTypeKind, DirectTypeRef, DirectTypeSchema,
    DuplicatePolicy, PresencePolicy, UnknownFieldPolicy,
};

pub fn schema() -> DirectSchemaSet {
    DirectSchemaSet {
        module_name: "generated_real_typed".to_string(),
        schema_hash: "sk-v14-w9ab-canada".to_string(),
        roots: vec![
            DirectRootSchema::struct_root(
                "parse_twitter_search",
                "crate::real_typed_struct::TwitterSearch<'i>",
                "TwitterSearch",
            ),
            DirectRootSchema::struct_root(
                "parse_update_center",
                "crate::real_typed_struct::UpdateCenter<'i>",
                "UpdateCenter",
            ),
            DirectRootSchema::struct_root(
                "parse_apache_builds",
                "crate::real_typed_struct::ApacheBuilds<'i>",
                "ApacheBuilds",
            ),
            DirectRootSchema::struct_root(
                "parse_citm_catalog",
                "crate::real_typed_struct::CitmCatalog<'i>",
                "CitmCatalog",
            ),
            DirectRootSchema::typed_root(
                "parse_gsoc_2018",
                "Vec<crate::real_typed_struct::GsocProposalEntry<'i>>",
                map_u32_entries(
                    "crate::real_typed_struct::GsocProposalEntry<'i>",
                    "key",
                    "value",
                    1_264,
                    ty("GsocProposal"),
                ),
            ),
            DirectRootSchema::typed_root(
                "parse_github_events",
                "Vec<crate::real_typed_struct::GithubEvent<'i>>",
                vec_with_capacity(ty("GithubEvent"), 30),
            ),
            DirectRootSchema::struct_root("parse_mesh", "crate::real_typed_struct::Mesh", "Mesh"),
            DirectRootSchema::struct_root(
                "parse_marine_ik",
                "crate::real_typed_struct::MarineIk",
                "MarineIk",
            ),
            DirectRootSchema::struct_root(
                "parse_instruments",
                "crate::real_typed_struct::InstrumentsDocument<'i>",
                "InstrumentsDocument",
            ),
            DirectRootSchema::struct_root(
                "parse_canada",
                "crate::real_typed_struct::CanadaFeatureCollection<'i>",
                "CanadaFeatureCollection",
            ),
            DirectRootSchema::typed_root(
                "parse_numbers",
                "Vec<f64>",
                vec_with_capacity(f64_ty(), 10_001),
            ),
            DirectRootSchema::typed_root(
                "parse_unicode_basic",
                "Vec<crate::real_typed_struct::UnicodeBasicRecord<'i>>",
                vec_with_capacity(ty("UnicodeBasicRecord"), 5_759),
            ),
            DirectRootSchema::struct_root(
                "parse_unicode_mixed",
                "crate::real_typed_struct::UnicodeMixedDocument<'i>",
                "UnicodeMixedDocument",
            ),
            DirectRootSchema::struct_root(
                "parse_unicode_escapes",
                "crate::real_typed_struct::UnicodeEscapesDocument<'i>",
                "UnicodeEscapesDocument",
            ),
            DirectRootSchema::typed_root(
                "parse_distinct_values",
                "Vec<crate::real_typed_struct::DistinctValue<'i>>",
                vec_with_capacity(ty("DistinctValue"), 440),
            ),
            DirectRootSchema::typed_root(
                "parse_y_string_unicode",
                "Vec<crate::real_typed_struct::YStringUnicodeToken>",
                vec_with_capacity(
                    string_enum(
                        "crate::real_typed_struct::YStringUnicodeToken",
                        vec![
                            enum_variant("AWithCombiningTilde", "\u{00e0}\u{0303}"),
                            enum_variant("Quote", "\""),
                            enum_variant("Plane16Noncharacter", "\u{10fffe}"),
                            enum_variant("Plane1Noncharacter", "\u{1fffe}"),
                            enum_variant("InvisiblePlus", "\u{2064}"),
                            enum_variant("BmpNoncharacter", "\u{fffe}"),
                            enum_variant("EuroAndGclef", "\u{20ac}\u{1d11e}"),
                            enum_variant("SamaritanLetter", "\u{0821}"),
                            enum_variant("Rocket", "\u{1f680}"),
                            enum_variant("PartyPopper", "\u{1f389}"),
                            enum_variant(
                                "Family",
                                "\u{1f468}\u{200d}\u{1f469}\u{200d}\u{1f467}\u{200d}\u{1f466}",
                            ),
                        ],
                    ),
                    2_200,
                ),
            ),
            DirectRootSchema::struct_root(
                "parse_random",
                "crate::real_typed_struct::RandomDocument<'i>",
                "RandomDocument",
            ),
            DirectRootSchema::typed_root(
                "parse_w5_array_root_probe",
                "Vec<crate::real_typed_struct::W5ArrayEvent<'i>>",
                vec_with_capacity(ty("W5ArrayEvent"), 2),
            ),
            DirectRootSchema::typed_root(
                "parse_w5_map_entry_root_probe",
                "Vec<crate::real_typed_struct::W5MapMetricEntry<'i>>",
                map_entries(
                    "crate::real_typed_struct::W5MapMetricEntry<'i>",
                    "key",
                    "value",
                    2,
                    ty("W5MapMetric"),
                ),
            ),
        ],
        types: vec![
            struct_ty(
                "TwitterSearch",
                "crate::real_typed_struct::TwitterSearch<'i>",
                vec![default("statuses", "statuses", vec(ty("Tweet")))],
            ),
            struct_ty(
                "Tweet",
                "crate::real_typed_struct::Tweet<'i>",
                vec![
                    default("id", "id", opt(u64_ty())),
                    default("text", "text", opt(string())),
                ],
            ),
            struct_ty(
                "ApacheBuilds",
                "crate::real_typed_struct::ApacheBuilds<'i>",
                vec![
                    default("mode", "mode", opt(string())),
                    default("nodeName", "node_name", opt(string())),
                    default("jobs", "jobs", vec_with_capacity(ty("ApacheJob"), 875)),
                ],
            ),
            struct_ty(
                "ApacheJob",
                "crate::real_typed_struct::ApacheJob<'i>",
                vec![
                    default("name", "name", opt(string())),
                    default("url", "url", opt(string())),
                    default("color", "color", opt(string())),
                ],
            ),
            struct_ty(
                "CitmCatalog",
                "crate::real_typed_struct::CitmCatalog<'i>",
                vec![default(
                    "events",
                    "events",
                    map_entries(
                        "crate::real_typed_struct::CitmEventEntry<'i>",
                        "key",
                        "value",
                        184,
                        ty("CitmEvent"),
                    ),
                )],
            ),
            struct_ty(
                "CitmEvent",
                "crate::real_typed_struct::CitmEvent<'i>",
                vec![
                    default("id", "id", opt(u64_ty())),
                    default("name", "name", opt(string())),
                    default("subTopicIds", "sub_topic_ids", vec(u64_ty())),
                    default("topicIds", "topic_ids", vec(u64_ty())),
                ],
            ),
            struct_ty(
                "GsocProposal",
                "crate::real_typed_struct::GsocProposal<'i>",
                vec![
                    required(
                        "@context",
                        "context",
                        string_enum(
                            "crate::real_typed_struct::GsocContext",
                            vec![enum_variant("SchemaOrg", "http://schema.org")],
                        ),
                    ),
                    required(
                        "@type",
                        "proposal_type",
                        string_enum(
                            "crate::real_typed_struct::GsocProposalType",
                            vec![enum_variant("SoftwareSourceCode", "SoftwareSourceCode")],
                        ),
                    ),
                    required("name", "name", decoded_json_string()),
                    required("description", "description", decoded_json_string()),
                    required("sponsor", "sponsor", ty("GsocSponsor")),
                    required("author", "author", ty("GsocAuthor")),
                ],
            ),
            struct_ty(
                "GsocSponsor",
                "crate::real_typed_struct::GsocSponsor<'i>",
                vec![
                    required(
                        "@type",
                        "sponsor_type",
                        string_enum(
                            "crate::real_typed_struct::GsocSponsorType",
                            vec![enum_variant("Organization", "Organization")],
                        ),
                    ),
                    required("name", "name", decoded_json_string()),
                    required(
                        "disambiguatingDescription",
                        "disambiguating_description",
                        decoded_json_string(),
                    ),
                    required("description", "description", decoded_json_string()),
                    required("url", "url", decoded_json_string()),
                    required("logo", "logo", decoded_json_string()),
                ],
            ),
            struct_ty(
                "GsocAuthor",
                "crate::real_typed_struct::GsocAuthor<'i>",
                vec![
                    required(
                        "@type",
                        "author_type",
                        string_enum(
                            "crate::real_typed_struct::GsocAuthorType",
                            vec![enum_variant("Person", "Person")],
                        ),
                    ),
                    required("name", "name", decoded_json_string()),
                ],
            ),
            struct_ty(
                "UpdateCenter",
                "crate::real_typed_struct::UpdateCenter<'i>",
                vec![
                    default("connectionCheckUrl", "connection_check_url", opt(string())),
                    default("core", "core", opt(ty("UpdateCore"))),
                    default("id", "id", opt(string())),
                    default(
                        "plugins",
                        "plugins",
                        map_entries(
                            "crate::real_typed_struct::PluginEntry<'i>",
                            "key",
                            "value",
                            768,
                            ty("Plugin"),
                        ),
                    ),
                    default(
                        "updateCenterVersion",
                        "update_center_version",
                        opt(string()),
                    ),
                ],
            ),
            struct_ty(
                "UpdateCore",
                "crate::real_typed_struct::UpdateCore<'i>",
                vec![
                    default("name", "name", opt(string())),
                    default("version", "version", opt(string())),
                ],
            ),
            struct_ty(
                "Plugin",
                "crate::real_typed_struct::Plugin<'i>",
                vec![
                    default("name", "name", opt(string())),
                    default("title", "title", opt(string())),
                    default("url", "url", opt(string())),
                    default("version", "version", opt(string())),
                ],
            )
            .with_ignored_fields(vec![
                ignored("buildDate", DirectSkipKind::String),
                ignored("compatibleSinceVersion", DirectSkipKind::String),
                ignored("dependencies", DirectSkipKind::Array),
                ignored("developers", DirectSkipKind::Array),
                ignored("excerpt", DirectSkipKind::String),
                ignored("gav", DirectSkipKind::String),
                ignored("labels", DirectSkipKind::Array),
                ignored("previousTimestamp", DirectSkipKind::String),
                ignored("previousVersion", DirectSkipKind::String),
                ignored("releaseTimestamp", DirectSkipKind::String),
                ignored("requiredCore", DirectSkipKind::String),
                ignored("scm", DirectSkipKind::String),
                ignored("sha1", DirectSkipKind::String),
                ignored("wiki", DirectSkipKind::String),
            ]),
            struct_ty(
                "GithubEvent",
                "crate::real_typed_struct::GithubEvent<'i>",
                vec![
                    default("type", "event_type", opt(string())),
                    default("created_at", "created_at", opt(string())),
                    default("id", "id", opt(string())),
                    default("public", "public", opt(bool_ty())),
                    default("actor", "actor", opt(ty("GithubActor"))),
                    default("repo", "repo", opt(ty("GithubRepo"))),
                    default("org", "org", opt(ty("GithubActor"))),
                    default("payload", "payload", opt(ty("GithubPayload"))),
                ],
            ),
            struct_ty(
                "GithubActor",
                "crate::real_typed_struct::GithubActor<'i>",
                vec![
                    default("id", "id", opt(u64_ty())),
                    default("login", "login", opt(string())),
                    default("url", "url", opt(string())),
                    default("avatar_url", "avatar_url", opt(string())),
                ],
            )
            .with_ignored_fields(vec![ignored("gravatar_id", DirectSkipKind::String)]),
            struct_ty(
                "GithubRepo",
                "crate::real_typed_struct::GithubRepo<'i>",
                vec![
                    default("id", "id", opt(u64_ty())),
                    default("name", "name", opt(string())),
                    default("url", "url", opt(string())),
                ],
            ),
            struct_ty(
                "GithubPayload",
                "crate::real_typed_struct::GithubPayload<'i>",
                vec![
                    default("action", "action", opt(string())),
                    default("ref", "ref_name", opt(string())),
                    default("ref_type", "ref_type", opt(string())),
                    default("push_id", "push_id", opt(u64_ty())),
                    default("size", "size", opt(u64_ty())),
                    default("distinct_size", "distinct_size", opt(u64_ty())),
                    default("head", "head", opt(string())),
                    default("before", "before", opt(string())),
                    default("description", "description", opt(string())),
                    default("master_branch", "master_branch", opt(string())),
                ],
            ),
            struct_ty(
                "CanadaFeatureCollection",
                "crate::real_typed_struct::CanadaFeatureCollection<'i>",
                vec![
                    default("type", "collection_type", opt(string())),
                    default(
                        "features",
                        "features",
                        vec_with_capacity(ty("CanadaFeature"), 1),
                    ),
                ],
            ),
            struct_ty(
                "CanadaFeature",
                "crate::real_typed_struct::CanadaFeature<'i>",
                vec![
                    default("type", "feature_type", opt(string())),
                    default("properties", "properties", opt(ty("CanadaProperties"))),
                    default("geometry", "geometry", opt(ty("CanadaGeometry"))),
                ],
            ),
            struct_ty(
                "CanadaProperties",
                "crate::real_typed_struct::CanadaProperties<'i>",
                vec![default("name", "name", opt(string()))],
            ),
            struct_ty(
                "CanadaGeometry",
                "crate::real_typed_struct::CanadaGeometry<'i>",
                vec![
                    default("type", "geometry_type", opt(string())),
                    default(
                        "coordinates",
                        "coordinates",
                        vec_with_capacity(
                            vec_with_capacity(vec_with_capacity(number_string(), 2), 256),
                            480,
                        ),
                    ),
                ],
            ),
            struct_ty(
                "UnicodeBasicRecord",
                "crate::real_typed_struct::UnicodeBasicRecord<'i>",
                vec![
                    default("id", "id", opt(u64_ty())),
                    default("script", "script", opt(string())),
                    default("text", "text", opt(string())),
                    default("len", "len", opt(u64_ty())),
                    default("tags", "tags", vec_with_capacity(string(), 3)),
                ],
            ),
            struct_ty(
                "UnicodeMixedDocument",
                "crate::real_typed_struct::UnicodeMixedDocument<'i>",
                vec![
                    default("metadata", "metadata", opt(ty("UnicodeMixedMetadata"))),
                    default(
                        "records",
                        "records",
                        vec_with_capacity(ty("UnicodeMixedRecord"), 4_185),
                    ),
                ],
            ),
            struct_ty(
                "UnicodeMixedMetadata",
                "crate::real_typed_struct::UnicodeMixedMetadata<'i>",
                vec![
                    default("purpose", "purpose", opt(string())),
                    default(
                        "classes",
                        "classes",
                        vec_with_capacity(
                            string_enum(
                                "crate::real_typed_struct::UnicodeMixedClass",
                                unicode_mixed_class_variants(),
                            ),
                            5,
                        ),
                    ),
                    default("count", "count", opt(u64_ty())),
                ],
            ),
            struct_ty(
                "UnicodeMixedRecord",
                "crate::real_typed_struct::UnicodeMixedRecord<'i>",
                vec![
                    default("id", "id", opt(u64_ty())),
                    default(
                        "type",
                        "class",
                        opt(string_enum(
                            "crate::real_typed_struct::UnicodeMixedRecordType",
                            unicode_mixed_record_type_variants(),
                        )),
                    ),
                    default("value", "value", opt(decoded_json_string())),
                    default("n", "n", opt(u64_ty())),
                ],
            ),
            struct_ty(
                "UnicodeEscapesDocument",
                "crate::real_typed_struct::UnicodeEscapesDocument<'i>",
                vec![
                    default("meta", "meta", opt(ty("UnicodeEscapesMeta"))),
                    default(
                        "records",
                        "records",
                        vec_with_capacity(ty("UnicodeEscapesRecord"), 1_877),
                    ),
                ],
            ),
            struct_ty(
                "UnicodeEscapesMeta",
                "crate::real_typed_struct::UnicodeEscapesMeta<'i>",
                vec![
                    default("mode", "mode", opt(string())),
                    default("ensure_ascii", "ensure_ascii", opt(bool_ty())),
                ],
            ),
            struct_ty(
                "UnicodeEscapesRecord",
                "crate::real_typed_struct::UnicodeEscapesRecord<'i>",
                vec![
                    default("id", "id", opt(u64_ty())),
                    default("v", "v", opt(raw_json_string())),
                ],
            ),
            struct_ty(
                "DistinctValue",
                "crate::real_typed_struct::DistinctValue<'i>",
                vec![
                    default("timestamp", "timestamp", opt(string())),
                    default("seq", "seq", opt(u64_ty())),
                    default("status", "status", opt(string())),
                ],
            )
            .with_unknown_string_entries(
                "dynamic",
                "crate::real_typed_struct::DistinctField<'i>",
                "key",
                "value",
                11,
            ),
            struct_ty(
                "RandomDocument",
                "crate::real_typed_struct::RandomDocument<'i>",
                vec![
                    default("id", "id", opt(u64_ty())),
                    default("jsonrpc", "jsonrpc", opt(string())),
                    default("total", "total", opt(u64_ty())),
                    default(
                        "result",
                        "result",
                        vec_with_capacity(ty("RandomUser"), 1_000),
                    ),
                ],
            ),
            struct_ty(
                "RandomUser",
                "crate::real_typed_struct::RandomUser<'i>",
                vec![
                    default("id", "id", opt(u64_ty())),
                    default("avatar", "avatar", opt(string())),
                    default("age", "age", opt(u64_ty())),
                    default("admin", "admin", opt(bool_ty())),
                    default("name", "name", opt(string())),
                    default("company", "company", opt(string())),
                    default("phone", "phone", opt(string())),
                    default("email", "email", opt(string())),
                    default("birthDate", "birth_date", opt(string())),
                    default(
                        "friends",
                        "friends",
                        vec_with_capacity(ty("RandomFriend"), 3),
                    ),
                    default("field", "field", opt(string())),
                ],
            ),
            struct_ty(
                "RandomFriend",
                "crate::real_typed_struct::RandomFriend<'i>",
                vec![
                    default("id", "id", opt(u64_ty())),
                    default("name", "name", opt(string())),
                    default("phone", "phone", opt(string())),
                ],
            ),
            struct_ty(
                "Mesh",
                "crate::real_typed_struct::Mesh",
                vec![
                    default("batches", "batches", vec_with_capacity(ty("MeshBatch"), 1)),
                    default(
                        "positions",
                        "positions",
                        vec_with_capacity(f64_ty(), 10_800),
                    ),
                    default("tex0", "tex0", vec_with_capacity(f64_ty(), 7_200)),
                    default("colors", "colors", vec_with_capacity(u32_ty(), 3_600)),
                    default(
                        "influences",
                        "influences",
                        vec_with_capacity(vec_with_capacity(f64_ty(), 2), 3_600),
                    ),
                    default("normals", "normals", vec_with_capacity(f64_ty(), 10_800)),
                    default("indices", "indices", vec_with_capacity(u32_ty(), 33_408)),
                ],
            ),
            struct_ty(
                "MeshBatch",
                "crate::real_typed_struct::MeshBatch",
                vec![
                    default("indexRange", "index_range", vec_with_capacity(u32_ty(), 2)),
                    default(
                        "vertexRange",
                        "vertex_range",
                        vec_with_capacity(u32_ty(), 2),
                    ),
                    default("usedBones", "used_bones", vec_with_capacity(u32_ty(), 4)),
                ],
            ),
            struct_ty(
                "MarineIk",
                "crate::real_typed_struct::MarineIk",
                vec![default(
                    "geometries",
                    "geometries",
                    vec_with_capacity(ty("MarineGeometry"), 1),
                )],
            ),
            struct_ty(
                "MarineGeometry",
                "crate::real_typed_struct::MarineGeometry",
                vec![default("data", "data", opt(ty("MarineGeometryData")))],
            ),
            struct_ty(
                "MarineGeometryData",
                "crate::real_typed_struct::MarineGeometryData",
                vec![
                    default(
                        "uvs",
                        "uvs",
                        vec_with_capacity(vec_with_capacity(f64_ty(), 10_532), 1),
                    ),
                    default("vertices", "vertices", vec_with_capacity(f64_ty(), 17_220)),
                    default(
                        "skinWeights",
                        "skin_weights",
                        vec_with_capacity(f64_ty(), 11_480),
                    ),
                    default(
                        "skinIndices",
                        "skin_indices",
                        vec_with_capacity(u32_ty(), 11_480),
                    ),
                    default("normals", "normals", vec_with_capacity(f64_ty(), 17_208)),
                    default("faces", "faces", vec_with_capacity(u32_ty(), 74_087)),
                ],
            ),
            struct_ty(
                "InstrumentsDocument",
                "crate::real_typed_struct::InstrumentsDocument<'i>",
                vec![
                    default(
                        "instruments",
                        "instruments",
                        vec_with_capacity(ty("Instrument"), 63),
                    ),
                    default("name", "name", opt(string())),
                    default(
                        "patterns",
                        "patterns",
                        vec_with_capacity(ty("InstrumentPattern"), 240),
                    ),
                    default(
                        "samples",
                        "samples",
                        vec_with_capacity(ty("InstrumentSample"), 70),
                    ),
                    default("version", "version", opt(u32_ty())),
                ],
            ),
            struct_ty(
                "Instrument",
                "crate::real_typed_struct::Instrument<'i>",
                vec![
                    default("default_filter_cutoff", "default_filter_cutoff", u32_ty()),
                    default(
                        "default_filter_cutoff_enabled",
                        "default_filter_cutoff_enabled",
                        bool_ty(),
                    ),
                    default("default_filter_mode", "default_filter_mode", u32_ty()),
                    default(
                        "default_filter_resonance",
                        "default_filter_resonance",
                        u32_ty(),
                    ),
                    default(
                        "default_filter_resonance_enabled",
                        "default_filter_resonance_enabled",
                        bool_ty(),
                    ),
                    default("default_pan", "default_pan", u32_ty()),
                    default("duplicate_check_type", "duplicate_check_type", u32_ty()),
                    default("duplicate_note_action", "duplicate_note_action", u32_ty()),
                    default("fadeout", "fadeout", u32_ty()),
                    default("global_volume", "global_volume", u32_ty()),
                    default("graph_insert", "graph_insert", u32_ty()),
                    default("legacy_filename", "legacy_filename", opt(string())),
                    default("midi_bank", "midi_bank", u32_ty()),
                    default("midi_channel", "midi_channel", u32_ty()),
                    default("midi_drum_set", "midi_drum_set", u32_ty()),
                    default("midi_program", "midi_program", u32_ty()),
                    default("name", "name", opt(string())),
                    default("new_note_action", "new_note_action", u32_ty()),
                    default(
                        "panning_envelope",
                        "panning_envelope",
                        opt(ty("InstrumentEnvelope")),
                    ),
                    default(
                        "pitch_envelope",
                        "pitch_envelope",
                        opt(ty("InstrumentEnvelope")),
                    ),
                    default("pitch_pan_center", "pitch_pan_center", u32_ty()),
                    default("pitch_pan_separation", "pitch_pan_separation", u32_ty()),
                    default("pitch_to_tempo_lock", "pitch_to_tempo_lock", u32_ty()),
                    default("random_cutoff_weight", "random_cutoff_weight", u32_ty()),
                    default("random_pan_weight", "random_pan_weight", u32_ty()),
                    default(
                        "random_resonance_weight",
                        "random_resonance_weight",
                        u32_ty(),
                    ),
                    default("random_volume_weight", "random_volume_weight", u32_ty()),
                    default(
                        "volume_envelope",
                        "volume_envelope",
                        opt(ty("InstrumentEnvelope")),
                    ),
                    default("volume_ramp_down", "volume_ramp_down", u32_ty()),
                    default("volume_ramp_up", "volume_ramp_up", u32_ty()),
                ],
            ),
            struct_ty(
                "InstrumentEnvelope",
                "crate::real_typed_struct::InstrumentEnvelope",
                vec![
                    default("loop_end", "loop_end", u32_ty()),
                    default("loop_start", "loop_start", u32_ty()),
                    default(
                        "nodes",
                        "nodes",
                        vec_with_capacity(ty("InstrumentEnvelopeNode"), 8),
                    ),
                    default("release_node", "release_node", u32_ty()),
                    default("sustain_end", "sustain_end", u32_ty()),
                    default("sustain_start", "sustain_start", u32_ty()),
                ],
            ),
            struct_ty(
                "InstrumentEnvelopeNode",
                "crate::real_typed_struct::InstrumentEnvelopeNode",
                vec![
                    default("tick", "tick", u32_ty()),
                    default("value", "value", u32_ty()),
                ],
            ),
            struct_ty(
                "InstrumentPattern",
                "crate::real_typed_struct::InstrumentPattern<'i>",
                vec![
                    default(
                        "data",
                        "data",
                        opt(vec_with_capacity(ty("InstrumentPatternEvent"), 1)),
                    ),
                    default("name", "name", opt(string())),
                    default("rows", "rows", u32_ty()),
                    default("rows_per_beat", "rows_per_beat", u32_ty()),
                    default("rows_per_measure", "rows_per_measure", u32_ty()),
                ],
            ),
            struct_ty(
                "InstrumentPatternEvent",
                "crate::real_typed_struct::InstrumentPatternEvent",
                vec![
                    default("channel", "channel", u32_ty()),
                    default("fxcmd", "fxcmd", u32_ty()),
                    default("fxparam", "fxparam", u32_ty()),
                    default("instr", "instr", u32_ty()),
                    default("note", "note", u32_ty()),
                    default("row", "row", u32_ty()),
                    default("volcmd", "volcmd", u32_ty()),
                    default("volval", "volval", u32_ty()),
                ],
            ),
            struct_ty(
                "InstrumentSample",
                "crate::real_typed_struct::InstrumentSample<'i>",
                vec![
                    default("c5_samplerate", "c5_samplerate", u32_ty()),
                    default("global_volume", "global_volume", u32_ty()),
                    default("legacy_filename", "legacy_filename", opt(string())),
                    default("length", "length", u32_ty()),
                    default("loop_end", "loop_end", u32_ty()),
                    default("loop_start", "loop_start", u32_ty()),
                    default("name", "name", opt(string())),
                    default("pan", "pan", u32_ty()),
                    default("sustain_end", "sustain_end", u32_ty()),
                    default("sustain_start", "sustain_start", u32_ty()),
                    default("vibrato_depth", "vibrato_depth", u32_ty()),
                    default("vibrato_rate", "vibrato_rate", u32_ty()),
                    default("vibrato_sweep", "vibrato_sweep", u32_ty()),
                    default("vibrato_type", "vibrato_type", u32_ty()),
                    default("volume", "volume", u32_ty()),
                ],
            ),
            struct_ty(
                "W5ArrayEvent",
                "crate::real_typed_struct::W5ArrayEvent<'i>",
                vec![
                    default("id", "id", opt(u64_ty())),
                    default("actor", "actor", opt(string())),
                    default("public", "public", opt(bool_ty())),
                ],
            ),
            struct_ty(
                "W5MapMetric",
                "crate::real_typed_struct::W5MapMetric<'i>",
                vec![
                    default("count", "count", opt(u64_ty())),
                    default("label", "label", opt(string())),
                ],
            ),
        ],
    }
}

fn struct_ty(type_id: &str, rust_type: &str, fields: Vec<DirectFieldSchema>) -> DirectTypeSchema {
    DirectTypeSchema {
        type_id: type_id.to_string(),
        rust_type: rust_type.to_string(),
        kind: DirectTypeKind::Struct {
            fields,
            ignored_fields: Vec::new(),
            unknown_fields: UnknownFieldPolicy::Skip,
        },
    }
}

trait DirectTypeSchemaExt {
    fn with_ignored_fields(self, ignored_fields: Vec<DirectIgnoredFieldSchema>) -> Self;
    fn with_unknown_string_entries(
        self,
        rust_field: &str,
        entry_rust_type: &str,
        key_field: &str,
        value_field: &str,
        capacity_hint: usize,
    ) -> Self;
}

impl DirectTypeSchemaExt for DirectTypeSchema {
    fn with_ignored_fields(mut self, ignored_fields: Vec<DirectIgnoredFieldSchema>) -> Self {
        match &mut self.kind {
            DirectTypeKind::Struct {
                ignored_fields: target,
                ..
            } => {
                *target = ignored_fields;
            }
        }
        self
    }

    fn with_unknown_string_entries(
        mut self,
        rust_field: &str,
        entry_rust_type: &str,
        key_field: &str,
        value_field: &str,
        capacity_hint: usize,
    ) -> Self {
        match &mut self.kind {
            DirectTypeKind::Struct { unknown_fields, .. } => {
                *unknown_fields = UnknownFieldPolicy::CaptureStringEntries {
                    rust_field: rust_field.to_string(),
                    entry_rust_type: entry_rust_type.to_string(),
                    key_field: key_field.to_string(),
                    value_field: value_field.to_string(),
                    capacity_hint: Some(capacity_hint),
                };
            }
        }
        self
    }
}

fn default(key_literal: &str, rust_field: &str, ty: DirectTypeRef) -> DirectFieldSchema {
    DirectFieldSchema {
        key_literal: key_literal.to_string(),
        rust_field: rust_field.to_string(),
        ty,
        presence: PresencePolicy::Default,
        duplicate: DuplicatePolicy::LastWins,
    }
}

fn required(key_literal: &str, rust_field: &str, ty: DirectTypeRef) -> DirectFieldSchema {
    DirectFieldSchema {
        key_literal: key_literal.to_string(),
        rust_field: rust_field.to_string(),
        ty,
        presence: PresencePolicy::Required,
        duplicate: DuplicatePolicy::Reject,
    }
}

fn ignored(key_literal: &str, skip: DirectSkipKind) -> DirectIgnoredFieldSchema {
    DirectIgnoredFieldSchema {
        key_literal: key_literal.to_string(),
        skip,
    }
}

fn ty(type_id: &str) -> DirectTypeRef {
    DirectTypeRef::Type(type_id.to_string())
}

fn string() -> DirectTypeRef {
    DirectTypeRef::Scalar(DirectScalar::String)
}

fn decoded_json_string() -> DirectTypeRef {
    DirectTypeRef::Scalar(DirectScalar::DecodedJsonString)
}

fn raw_json_string() -> DirectTypeRef {
    DirectTypeRef::Scalar(DirectScalar::RawJsonString)
}

fn u64_ty() -> DirectTypeRef {
    DirectTypeRef::Scalar(DirectScalar::U64)
}

fn u32_ty() -> DirectTypeRef {
    DirectTypeRef::Scalar(DirectScalar::U32)
}

fn bool_ty() -> DirectTypeRef {
    DirectTypeRef::Scalar(DirectScalar::Bool)
}

fn f64_ty() -> DirectTypeRef {
    DirectTypeRef::Scalar(DirectScalar::F64)
}

fn number_string() -> DirectTypeRef {
    DirectTypeRef::Scalar(DirectScalar::NumberString)
}

fn string_enum(enum_type: &str, variants: Vec<DirectStringEnumVariant>) -> DirectTypeRef {
    DirectTypeRef::StringEnum {
        enum_type: enum_type.to_string(),
        variants,
    }
}

fn enum_variant(variant: &str, decoded: &str) -> DirectStringEnumVariant {
    DirectStringEnumVariant {
        variant: variant.to_string(),
        decoded: decoded.to_string(),
    }
}

fn unicode_mixed_class_variants() -> Vec<DirectStringEnumVariant> {
    vec![
        enum_variant("Ascii", "ascii"),
        enum_variant("Latin1", "latin1"),
        enum_variant("Cjk", "cjk"),
        enum_variant("Emoji", "emoji"),
        enum_variant("MixedEscapes", "mixed_escapes"),
    ]
}

fn unicode_mixed_record_type_variants() -> Vec<DirectStringEnumVariant> {
    vec![
        enum_variant("Ascii", "ascii"),
        enum_variant("Latin1", "latin1"),
        enum_variant("Cjk", "cjk"),
        enum_variant("Emoji", "emoji"),
        enum_variant("Mixed", "mixed"),
    ]
}

fn vec(inner: DirectTypeRef) -> DirectTypeRef {
    DirectTypeRef::Vec {
        inner: Box::new(inner),
        capacity_hint: None,
    }
}

fn vec_with_capacity(inner: DirectTypeRef, capacity_hint: usize) -> DirectTypeRef {
    DirectTypeRef::Vec {
        inner: Box::new(inner),
        capacity_hint: Some(capacity_hint),
    }
}

fn map_entries(
    entry_rust_type: &str,
    key_field: &str,
    value_field: &str,
    capacity_hint: usize,
    value: DirectTypeRef,
) -> DirectTypeRef {
    DirectTypeRef::MapEntriesVec {
        entry_rust_type: entry_rust_type.to_string(),
        key_field: key_field.to_string(),
        value_field: value_field.to_string(),
        capacity_hint: Some(capacity_hint),
        value: Box::new(value),
    }
}

fn map_u32_entries(
    entry_rust_type: &str,
    key_field: &str,
    value_field: &str,
    capacity_hint: usize,
    value: DirectTypeRef,
) -> DirectTypeRef {
    DirectTypeRef::MapU32EntriesVec {
        entry_rust_type: entry_rust_type.to_string(),
        key_field: key_field.to_string(),
        value_field: value_field.to_string(),
        capacity_hint: Some(capacity_hint),
        value: Box::new(value),
    }
}

fn opt(inner: DirectTypeRef) -> DirectTypeRef {
    DirectTypeRef::Option(Box::new(inner))
}
