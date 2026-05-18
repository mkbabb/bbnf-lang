use codegen::direct_schema::{
    DirectFieldSchema, DirectIgnoredFieldSchema, DirectRootSchema, DirectScalar, DirectSchemaSet,
    DirectSkipKind, DirectTypeKind, DirectTypeRef, DirectTypeSchema, DuplicatePolicy,
    PresencePolicy, UnknownFieldPolicy,
};

pub fn schema() -> DirectSchemaSet {
    DirectSchemaSet {
        module_name: "generated_real_typed".to_string(),
        schema_hash: "sk-v7-real-typed-v2".to_string(),
        roots: vec![
            DirectRootSchema {
                function_name: "parse_twitter_search".to_string(),
                rust_type: "crate::real_typed_struct::TwitterSearch<'i>".to_string(),
                type_id: "TwitterSearch".to_string(),
            },
            DirectRootSchema {
                function_name: "parse_update_center".to_string(),
                rust_type: "crate::real_typed_struct::UpdateCenter<'i>".to_string(),
                type_id: "UpdateCenter".to_string(),
            },
            DirectRootSchema {
                function_name: "parse_apache_builds".to_string(),
                rust_type: "crate::real_typed_struct::ApacheBuilds<'i>".to_string(),
                type_id: "ApacheBuilds".to_string(),
            },
            DirectRootSchema {
                function_name: "parse_citm_catalog".to_string(),
                rust_type: "crate::real_typed_struct::CitmCatalog<'i>".to_string(),
                type_id: "CitmCatalog".to_string(),
            },
            DirectRootSchema {
                function_name: "parse_mesh".to_string(),
                rust_type: "crate::real_typed_struct::Mesh".to_string(),
                type_id: "Mesh".to_string(),
            },
            DirectRootSchema {
                function_name: "parse_marine_ik".to_string(),
                rust_type: "crate::real_typed_struct::MarineIk".to_string(),
                type_id: "MarineIk".to_string(),
            },
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

fn u64_ty() -> DirectTypeRef {
    DirectTypeRef::Scalar(DirectScalar::U64)
}

fn u32_ty() -> DirectTypeRef {
    DirectTypeRef::Scalar(DirectScalar::U32)
}

fn f64_ty() -> DirectTypeRef {
    DirectTypeRef::Scalar(DirectScalar::F64)
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

fn opt(inner: DirectTypeRef) -> DirectTypeRef {
    DirectTypeRef::Option(Box::new(inner))
}
