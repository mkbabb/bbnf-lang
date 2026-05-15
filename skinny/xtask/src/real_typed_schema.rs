use codegen::direct_schema::{
    DirectFieldSchema, DirectIgnoredFieldSchema, DirectRootSchema, DirectScalar, DirectSchemaSet,
    DirectSkipKind, DirectTypeKind, DirectTypeRef, DirectTypeSchema, DuplicatePolicy,
    PresencePolicy, UnknownFieldPolicy,
};

pub fn schema() -> DirectSchemaSet {
    DirectSchemaSet {
        module_name: "generated_real_typed".to_string(),
        schema_hash: "sk-v6-real-typed-v1".to_string(),
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

fn default(json_key: &str, rust_field: &str, ty: DirectTypeRef) -> DirectFieldSchema {
    DirectFieldSchema {
        json_key: json_key.to_string(),
        rust_field: rust_field.to_string(),
        ty,
        presence: PresencePolicy::Default,
        duplicate: DuplicatePolicy::LastWins,
    }
}

fn ignored(json_key: &str, skip: DirectSkipKind) -> DirectIgnoredFieldSchema {
    DirectIgnoredFieldSchema {
        json_key: json_key.to_string(),
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

fn vec(inner: DirectTypeRef) -> DirectTypeRef {
    DirectTypeRef::Vec(Box::new(inner))
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
