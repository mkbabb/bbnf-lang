use std::collections::{BTreeMap, BTreeSet};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DirectSchemaSet {
    pub module_name: String,
    pub roots: Vec<DirectRootSchema>,
    pub types: Vec<DirectTypeSchema>,
    pub schema_hash: String,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DirectRootSchema {
    pub function_name: String,
    pub rust_type: String,
    pub ty: DirectTypeRef,
}

impl DirectRootSchema {
    pub fn struct_root(
        function_name: impl Into<String>,
        rust_type: impl Into<String>,
        type_id: impl Into<String>,
    ) -> Self {
        Self::typed_root(
            function_name,
            rust_type,
            DirectTypeRef::Type(type_id.into()),
        )
    }

    pub fn typed_root(
        function_name: impl Into<String>,
        rust_type: impl Into<String>,
        ty: DirectTypeRef,
    ) -> Self {
        Self {
            function_name: function_name.into(),
            rust_type: rust_type.into(),
            ty,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DirectTypeSchema {
    pub type_id: String,
    pub rust_type: String,
    pub kind: DirectTypeKind,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum DirectTypeKind {
    Struct {
        fields: Vec<DirectFieldSchema>,
        ignored_fields: Vec<DirectIgnoredFieldSchema>,
        unknown_fields: UnknownFieldPolicy,
    },
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DirectFieldSchema {
    pub key_literal: String,
    pub rust_field: String,
    pub ty: DirectTypeRef,
    pub presence: PresencePolicy,
    pub duplicate: DuplicatePolicy,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DirectIgnoredFieldSchema {
    pub key_literal: String,
    pub skip: DirectSkipKind,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum DirectSkipKind {
    Any,
    Array,
    Object,
    String,
    Number,
    Bool,
    Null,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum DirectTypeRef {
    Type(String),
    Scalar(DirectScalar),
    Vec {
        inner: Box<DirectTypeRef>,
        capacity_hint: Option<usize>,
    },
    MapString(Box<DirectTypeRef>),
    MapEntriesVec {
        entry_rust_type: String,
        key_field: String,
        value_field: String,
        capacity_hint: Option<usize>,
        value: Box<DirectTypeRef>,
    },
    Option(Box<DirectTypeRef>),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum DirectScalar {
    String,
    Bool,
    I64,
    U64,
    U32,
    F64,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PresencePolicy {
    Required,
    Optional,
    Default,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum DuplicatePolicy {
    LastWins,
    Reject,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum UnknownFieldPolicy {
    Skip,
    Reject,
    CaptureStringEntries {
        rust_field: String,
        entry_rust_type: String,
        key_field: String,
        value_field: String,
        capacity_hint: Option<usize>,
    },
}

impl DirectSchemaSet {
    pub fn validate(&self) -> Result<(), String> {
        if !is_ident(&self.module_name) {
            return Err(format!(
                "invalid generated module name `{}`",
                self.module_name
            ));
        }
        if self.roots.is_empty() {
            return Err("typed direct schema has no roots".to_string());
        }
        let mut type_ids = BTreeSet::new();
        let mut types = BTreeMap::new();
        for ty in &self.types {
            if !is_ident(&ty.type_id) {
                return Err(format!("invalid type id `{}`", ty.type_id));
            }
            if !type_ids.insert(ty.type_id.as_str()) {
                return Err(format!("duplicate type id `{}`", ty.type_id));
            }
            match &ty.kind {
                DirectTypeKind::Struct {
                    fields,
                    ignored_fields,
                    unknown_fields,
                    ..
                } => validate_fields(&ty.type_id, fields, ignored_fields, unknown_fields)?,
            }
            types.insert(ty.type_id.as_str(), ty);
        }
        for root in &self.roots {
            if !is_ident(&root.function_name) {
                return Err(format!("invalid root function `{}`", root.function_name));
            }
            if root.rust_type.trim().is_empty() {
                return Err(format!("root `{}` has empty rust type", root.function_name));
            }
            validate_type_ref(&root.ty, &types)
                .map_err(|error| format!("root `{}` {error}", root.function_name))?;
        }
        for ty in &self.types {
            match &ty.kind {
                DirectTypeKind::Struct { fields, .. } => {
                    for field in fields {
                        validate_type_ref(&field.ty, &types)?;
                    }
                }
            }
        }
        Ok(())
    }
}

fn validate_fields(
    type_id: &str,
    fields: &[DirectFieldSchema],
    ignored_fields: &[DirectIgnoredFieldSchema],
    unknown_fields: &UnknownFieldPolicy,
) -> Result<(), String> {
    let mut rust_fields = BTreeSet::new();
    let mut key_literals = BTreeSet::new();
    for field in fields {
        if !is_ident(&field.rust_field) {
            return Err(format!(
                "type `{type_id}` has invalid rust field `{}`",
                field.rust_field
            ));
        }
        if field.key_literal.is_empty() {
            return Err(format!("type `{type_id}` has empty key literal"));
        }
        if !rust_fields.insert(field.rust_field.as_str()) {
            return Err(format!(
                "type `{type_id}` has duplicate rust field `{}`",
                field.rust_field
            ));
        }
        if !key_literals.insert(field.key_literal.as_str()) {
            return Err(format!(
                "type `{type_id}` has duplicate key literal `{}`",
                field.key_literal
            ));
        }
    }
    for field in ignored_fields {
        if field.key_literal.is_empty() {
            return Err(format!("type `{type_id}` has empty ignored key literal"));
        }
        if !key_literals.insert(field.key_literal.as_str()) {
            return Err(format!(
                "type `{type_id}` has duplicate key literal `{}`",
                field.key_literal
            ));
        }
    }
    if let UnknownFieldPolicy::CaptureStringEntries {
        rust_field,
        entry_rust_type,
        key_field,
        value_field,
        ..
    } = unknown_fields
    {
        if !is_ident(rust_field) {
            return Err(format!(
                "type `{type_id}` has invalid unknown capture field `{rust_field}`"
            ));
        }
        if !rust_fields.insert(rust_field.as_str()) {
            return Err(format!(
                "type `{type_id}` has duplicate rust field `{rust_field}`"
            ));
        }
        if entry_rust_type.trim().is_empty() {
            return Err(format!(
                "type `{type_id}` has empty unknown capture entry type"
            ));
        }
        if !is_ident(key_field) {
            return Err(format!(
                "type `{type_id}` has invalid unknown capture key field `{key_field}`"
            ));
        }
        if !is_ident(value_field) {
            return Err(format!(
                "type `{type_id}` has invalid unknown capture value field `{value_field}`"
            ));
        }
    }
    Ok(())
}

fn validate_type_ref(
    ty: &DirectTypeRef,
    types: &BTreeMap<&str, &DirectTypeSchema>,
) -> Result<(), String> {
    match ty {
        DirectTypeRef::Type(type_id) => {
            if types.contains_key(type_id.as_str()) {
                Ok(())
            } else {
                Err(format!("field references missing type `{type_id}`"))
            }
        }
        DirectTypeRef::Scalar(_) => Ok(()),
        DirectTypeRef::Vec { inner, .. }
        | DirectTypeRef::MapString(inner)
        | DirectTypeRef::MapEntriesVec { value: inner, .. }
        | DirectTypeRef::Option(inner) => validate_type_ref(inner, types),
    }
}

fn is_ident(value: &str) -> bool {
    let mut chars = value.chars();
    let Some(first) = chars.next() else {
        return false;
    };
    if !(first == '_' || first.is_ascii_alphabetic()) {
        return false;
    }
    chars.all(|ch| ch == '_' || ch.is_ascii_alphanumeric())
}
