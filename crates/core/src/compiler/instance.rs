use std::collections::BTreeSet;
use std::fmt;

use super::regex_table::RegexTablePlan;
use serde::de::{self, Visitor};
use serde::{Deserialize, Deserializer, Serialize};

use super::budget::CompileOptions;
use super::diagnostic::{Diagnostic, DiagnosticCode, StageResult};
use super::source::Revision;

const GRAMMAR_INSTANCE_SCHEMA: &str = "bbnf.grammar-instance.v1";
const REVISION_DOMAIN: &[u8] = b"bbnf.w3.revision.v1\0";
const INVALID_PRATT_REPRESENTATION: &str = "bbnf.invalid-pratt-representation";
#[cfg(test)]
pub(super) const UNIT_INSTANCE: &[u8] = br#"{
  "schema": "bbnf.grammar-instance.v1",
  "logical_path": "compiler-unit-law.bbnf",
  "entry": "root",
  "trivia_regex": null,
  "pratt_tables": []
}"#;

pub struct GrammarInstanceInput<'a> {
    manifest_bytes: &'a [u8],
    source_bytes: &'a [u8],
}

impl<'a> GrammarInstanceInput<'a> {
    pub const fn new(manifest_bytes: &'a [u8], source_bytes: &'a [u8]) -> Self {
        Self {
            manifest_bytes,
            source_bytes,
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub(super) struct GrammarInstance {
    schema: String,
    logical_path: String,
    entry: String,
    #[serde(deserialize_with = "deserialize_required_optional_string")]
    trivia_regex: Option<String>,
    pratt_tables: Vec<GrammarInstancePrattTable>,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub(super) struct GrammarInstancePrattTable {
    site_rule: String,
    atom_rule: String,
    operators: Vec<GrammarInstancePrattOperator>,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub(super) struct GrammarInstancePrattOperator {
    matcher_rule: String,
    fixity: InstanceFixity,
    #[serde(deserialize_with = "deserialize_optional_binding_power")]
    left_binding_power: Option<u16>,
    #[serde(deserialize_with = "deserialize_optional_binding_power")]
    right_binding_power: Option<u16>,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Serialize)]
#[serde(rename_all = "lowercase")]
pub(super) enum InstanceFixity {
    Prefix,
    Postfix,
    Infix,
}

pub(super) struct ValidatedGrammarInstance<'a> {
    instance: GrammarInstance,
    source_bytes: &'a [u8],
}

impl GrammarInstance {
    pub(super) fn entry(&self) -> &str {
        &self.entry
    }

    pub(super) fn trivia_regex(&self) -> Option<&str> {
        self.trivia_regex.as_deref()
    }

    pub(super) fn pratt_tables(&self) -> &[GrammarInstancePrattTable] {
        &self.pratt_tables
    }

    pub(super) fn revision(&self, source_bytes: &[u8]) -> Revision {
        let mut hasher = blake3::Hasher::new();
        hasher.update(REVISION_DOMAIN);
        hash_string(&mut hasher, &self.schema);
        hash_string(&mut hasher, &self.logical_path);
        hash_string(&mut hasher, &self.entry);
        match &self.trivia_regex {
            None => {
                hasher.update(&[0]);
            }
            Some(pattern) => {
                hasher.update(&[1]);
                hash_string(&mut hasher, pattern);
            }
        }
        hash_count(&mut hasher, self.pratt_tables.len());
        for table in &self.pratt_tables {
            hash_string(&mut hasher, &table.site_rule);
            hash_string(&mut hasher, &table.atom_rule);
            hash_count(&mut hasher, table.operators.len());
            for operator in &table.operators {
                hash_string(&mut hasher, &operator.matcher_rule);
                hasher.update(&[match operator.fixity {
                    InstanceFixity::Prefix => 0,
                    InstanceFixity::Postfix => 1,
                    InstanceFixity::Infix => 2,
                }]);
                hash_optional_power(&mut hasher, operator.left_binding_power);
                hash_optional_power(&mut hasher, operator.right_binding_power);
            }
        }
        hash_bytes(&mut hasher, source_bytes);
        Revision::from_bytes(*hasher.finalize().as_bytes())
    }
}

impl GrammarInstancePrattTable {
    pub(super) fn site_rule(&self) -> &str {
        &self.site_rule
    }

    pub(super) fn atom_rule(&self) -> &str {
        &self.atom_rule
    }

    pub(super) fn operators(&self) -> &[GrammarInstancePrattOperator] {
        &self.operators
    }
}

impl GrammarInstancePrattOperator {
    pub(super) fn matcher_rule(&self) -> &str {
        &self.matcher_rule
    }

    pub(super) const fn fixity(&self) -> InstanceFixity {
        self.fixity
    }

    pub(super) const fn left_binding_power(&self) -> Option<u16> {
        self.left_binding_power
    }

    pub(super) const fn right_binding_power(&self) -> Option<u16> {
        self.right_binding_power
    }
}

impl<'a> ValidatedGrammarInstance<'a> {
    pub(super) fn into_parts(self) -> (GrammarInstance, &'a [u8]) {
        (self.instance, self.source_bytes)
    }
}

pub(super) fn decode_instance<'a>(
    input: GrammarInstanceInput<'a>,
    options: CompileOptions,
) -> StageResult<ValidatedGrammarInstance<'a>> {
    let manifest_bytes = input.manifest_bytes;
    let source_bytes = input.source_bytes;
    if manifest_bytes.len() > options.instance_bytes {
        return StageResult::rejected(Diagnostic::new(
            DiagnosticCode::ResourceLimit,
            0,
            0,
            "grammar-instance bytes exceed the configured limit",
        ));
    }

    let instance = match serde_json::from_slice::<GrammarInstance>(manifest_bytes) {
        Ok(instance) => instance,
        Err(error) => {
            let message = error.to_string();
            let code = if message.contains(INVALID_PRATT_REPRESENTATION) {
                DiagnosticCode::InvalidPrattTable
            } else {
                DiagnosticCode::InvalidGrammarInstance
            };
            return StageResult::rejected(Diagnostic::new(code, 0, 0, message));
        }
    };

    if let Err(diagnostic) = validate_instance(&instance, options) {
        return StageResult::rejected(diagnostic);
    }

    StageResult::accepted(ValidatedGrammarInstance {
        instance,
        source_bytes,
    })
}

fn validate_instance(
    instance: &GrammarInstance,
    options: CompileOptions,
) -> Result<(), Diagnostic> {
    if instance.schema != GRAMMAR_INSTANCE_SCHEMA {
        return Err(invalid_instance(
            "grammar-instance schema is not bbnf.grammar-instance.v1",
        ));
    }
    if !valid_logical_path(&instance.logical_path) {
        return Err(invalid_instance(
            "grammar-instance logical path is not a portable relative path",
        ));
    }
    if !valid_identifier(&instance.entry) {
        return Err(invalid_instance(
            "grammar-instance entry is not a valid rule name",
        ));
    }

    for table in &instance.pratt_tables {
        if !valid_identifier(&table.site_rule)
            || !valid_identifier(&table.atom_rule)
            || table.operators.is_empty()
            || table
                .operators
                .iter()
                .any(|operator| !valid_identifier(&operator.matcher_rule))
        {
            return Err(invalid_instance(
                "grammar-instance Pratt rows do not match the closed representation",
            ));
        }
    }

    if instance.pratt_tables.len() > options.pratt_tables {
        return Err(resource_limit(
            "grammar-instance Pratt-table count exceeds the configured limit",
        ));
    }
    let mut operator_count = 0usize;
    for table in &instance.pratt_tables {
        operator_count = operator_count
            .checked_add(table.operators.len())
            .ok_or_else(|| {
                resource_limit("grammar-instance Pratt-operator count overflows usize")
            })?;
        if operator_count > options.pratt_operators {
            return Err(resource_limit(
                "grammar-instance Pratt-operator count exceeds the configured limit",
            ));
        }
    }

    validate_trivia(instance, options)?;
    validate_pratt_tables(instance)?;
    Ok(())
}

fn validate_trivia(instance: &GrammarInstance, options: CompileOptions) -> Result<(), Diagnostic> {
    let Some(pattern) = &instance.trivia_regex else {
        return Ok(());
    };
    let plan = RegexTablePlan::compile_with(pattern, options.regex.table_options())
        .map_err(|error| invalid_instance(format!("invalid trivia regex: {error}")))?;
    let start = plan.start_state() as usize;
    if !plan.accepting().get(start).copied().unwrap_or(false) {
        return Err(invalid_instance("trivia regex must accept the empty input"));
    }
    Ok(())
}

fn validate_pratt_tables(instance: &GrammarInstance) -> Result<(), Diagnostic> {
    let mut sites = BTreeSet::new();
    for table in &instance.pratt_tables {
        if !sites.insert(table.site_rule.as_str()) {
            return Err(invalid_pratt("duplicate Pratt-table site rule"));
        }
        let mut matchers = BTreeSet::new();
        for operator in &table.operators {
            if !matchers.insert(operator.matcher_rule.as_str()) {
                return Err(invalid_pratt(
                    "duplicate Pratt operator matcher within one table",
                ));
            }
            let powers_are_valid = match (
                operator.fixity,
                operator.left_binding_power,
                operator.right_binding_power,
            ) {
                (InstanceFixity::Prefix, None, Some(right)) => right != 0,
                (InstanceFixity::Postfix, Some(left), None) => left != 0,
                (InstanceFixity::Infix, Some(left), Some(right)) => {
                    left != 0 && right != 0 && (right == left || left.checked_add(1) == Some(right))
                }
                _ => false,
            };
            if !powers_are_valid {
                return Err(invalid_pratt(
                    "Pratt fixity and binding-power shape is invalid",
                ));
            }
        }
    }
    Ok(())
}

fn valid_logical_path(path: &str) -> bool {
    let bytes = path.as_bytes();
    if path.is_empty()
        || path.starts_with('/')
        || path.contains('\\')
        || (bytes.len() >= 3
            && bytes[0].is_ascii_alphabetic()
            && bytes[1] == b':'
            && bytes[2] == b'/')
    {
        return false;
    }
    !bytes
        .iter()
        .any(|byte| byte.is_ascii_control() || *byte == 0x7f)
        && !path.split('/').any(|component| component == "..")
}

fn valid_identifier(name: &str) -> bool {
    let mut bytes = name.bytes();
    let Some(initial) = bytes.next() else {
        return false;
    };
    (initial.is_ascii_alphabetic() || initial == b'_')
        && bytes.all(|byte| byte.is_ascii_alphanumeric() || matches!(byte, b'_' | b'-'))
}

fn invalid_instance(message: impl Into<String>) -> Diagnostic {
    Diagnostic::new(DiagnosticCode::InvalidGrammarInstance, 0, 0, message)
}

fn invalid_pratt(message: impl Into<String>) -> Diagnostic {
    Diagnostic::new(DiagnosticCode::InvalidPrattTable, 0, 0, message)
}

fn resource_limit(message: impl Into<String>) -> Diagnostic {
    Diagnostic::new(DiagnosticCode::ResourceLimit, 0, 0, message)
}

fn hash_count(hasher: &mut blake3::Hasher, count: usize) {
    let count = u32::try_from(count).expect("validated grammar-instance count fits u32");
    hasher.update(&count.to_le_bytes());
}

fn hash_bytes(hasher: &mut blake3::Hasher, bytes: &[u8]) {
    let length = u64::try_from(bytes.len()).expect("byte-slice length fits u64");
    hasher.update(&length.to_le_bytes());
    hasher.update(bytes);
}

fn hash_string(hasher: &mut blake3::Hasher, value: &str) {
    hash_bytes(hasher, value.as_bytes());
}

fn hash_optional_power(hasher: &mut blake3::Hasher, value: Option<u16>) {
    match value {
        None => {
            hasher.update(&[0]);
        }
        Some(value) => {
            hasher.update(&[1]);
            hasher.update(&value.to_le_bytes());
        }
    }
}

fn deserialize_required_optional_string<'de, D>(deserializer: D) -> Result<Option<String>, D::Error>
where
    D: Deserializer<'de>,
{
    Option::<String>::deserialize(deserializer)
}

fn deserialize_optional_binding_power<'de, D>(deserializer: D) -> Result<Option<u16>, D::Error>
where
    D: Deserializer<'de>,
{
    deserializer.deserialize_option(OptionalBindingPowerVisitor)
}

struct OptionalBindingPowerVisitor;

impl<'de> Visitor<'de> for OptionalBindingPowerVisitor {
    type Value = Option<u16>;

    fn expecting(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str("null or an unsigned 16-bit integer binding power")
    }

    fn visit_none<E>(self) -> Result<Self::Value, E>
    where
        E: de::Error,
    {
        Ok(None)
    }

    fn visit_unit<E>(self) -> Result<Self::Value, E>
    where
        E: de::Error,
    {
        Ok(None)
    }

    fn visit_some<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_any(BindingPowerVisitor).map(Some)
    }
}

struct BindingPowerVisitor;

impl Visitor<'_> for BindingPowerVisitor {
    type Value = u16;

    fn expecting(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str("an unsigned 16-bit integer binding power")
    }

    fn visit_u64<E>(self, value: u64) -> Result<Self::Value, E>
    where
        E: de::Error,
    {
        u16::try_from(value).map_err(|_| E::custom(INVALID_PRATT_REPRESENTATION))
    }

    fn visit_i64<E>(self, value: i64) -> Result<Self::Value, E>
    where
        E: de::Error,
    {
        u16::try_from(value).map_err(|_| E::custom(INVALID_PRATT_REPRESENTATION))
    }

    fn visit_f64<E>(self, _value: f64) -> Result<Self::Value, E>
    where
        E: de::Error,
    {
        Err(E::custom(INVALID_PRATT_REPRESENTATION))
    }
}

impl<'de> Deserialize<'de> for InstanceFixity {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_str(PrattFixityVisitor)
    }
}

struct PrattFixityVisitor;

impl Visitor<'_> for PrattFixityVisitor {
    type Value = InstanceFixity;

    fn expecting(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str("one of prefix, postfix, or infix")
    }

    fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
    where
        E: de::Error,
    {
        match value {
            "prefix" => Ok(InstanceFixity::Prefix),
            "postfix" => Ok(InstanceFixity::Postfix),
            "infix" => Ok(InstanceFixity::Infix),
            _ => Err(E::custom(INVALID_PRATT_REPRESENTATION)),
        }
    }
}
