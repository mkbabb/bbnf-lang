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
    pub type_id: String,
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
    pub json_key: String,
    pub rust_field: String,
    pub ty: DirectTypeRef,
    pub presence: PresencePolicy,
    pub duplicate: DuplicatePolicy,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DirectIgnoredFieldSchema {
    pub json_key: String,
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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum UnknownFieldPolicy {
    Skip,
    Reject,
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
                    ..
                } => validate_fields(&ty.type_id, fields, ignored_fields)?,
            }
            types.insert(ty.type_id.as_str(), ty);
        }
        for root in &self.roots {
            if !is_ident(&root.function_name) {
                return Err(format!("invalid root function `{}`", root.function_name));
            }
            if !types.contains_key(root.type_id.as_str()) {
                return Err(format!(
                    "root `{}` references missing type `{}`",
                    root.function_name, root.type_id
                ));
            }
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
) -> Result<(), String> {
    let mut rust_fields = BTreeSet::new();
    let mut json_keys = BTreeSet::new();
    for field in fields {
        if !is_ident(&field.rust_field) {
            return Err(format!(
                "type `{type_id}` has invalid rust field `{}`",
                field.rust_field
            ));
        }
        if field.json_key.is_empty() {
            return Err(format!("type `{type_id}` has empty JSON key"));
        }
        if !rust_fields.insert(field.rust_field.as_str()) {
            return Err(format!(
                "type `{type_id}` has duplicate rust field `{}`",
                field.rust_field
            ));
        }
        if !json_keys.insert(field.json_key.as_str()) {
            return Err(format!(
                "type `{type_id}` has duplicate JSON key `{}`",
                field.json_key
            ));
        }
    }
    for field in ignored_fields {
        if field.json_key.is_empty() {
            return Err(format!("type `{type_id}` has empty ignored JSON key"));
        }
        if !json_keys.insert(field.json_key.as_str()) {
            return Err(format!(
                "type `{type_id}` has duplicate JSON key `{}`",
                field.json_key
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
