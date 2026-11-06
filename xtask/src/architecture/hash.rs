use serde::Serialize;
use serde::ser::{
    SerializeMap, SerializeSeq, SerializeStruct, SerializeStructVariant, SerializeTuple,
    SerializeTupleStruct, SerializeTupleVariant, Serializer,
};
use sha2::{Digest, Sha256 as AcceleratedSha256};
use std::ffi::OsStr;
use std::fmt;
#[cfg(unix)]
use std::os::unix::ffi::OsStrExt;

struct CountingWriter {
    bytes: usize,
    ceiling: usize,
}

const JSON_VISIT_WORK: usize = 32;
const JSON_MEASURE_SCAN_PASSES: usize = 2;
const JSON_MEASURE_OUTPUT_PASSES: usize = 1;
const JSON_HASH_SCAN_PASSES: usize = 3;
const JSON_HASH_OUTPUT_PASSES: usize = 2;

pub(crate) trait JsonCharge {
    fn charge_json(&mut self, owner: &'static str, amount: usize)
    -> Result<(), BoundedKernelError>;
    fn available_json_bytes(&self) -> Result<usize, BoundedKernelError>;
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[doc(hidden)]
pub enum BoundedKernelError {
    Arithmetic(&'static str),
    Budget {
        owner: &'static str,
        amount: usize,
        limit: usize,
        used: usize,
    },
    Unsupported(&'static str),
    Canonical {
        code: &'static str,
        first: usize,
        second: usize,
    },
    Serialization,
    OutputDrift,
    OutputCeiling {
        ceiling: usize,
    },
}

type JsonStringObserver<'a> = &'a mut dyn FnMut(&str) -> Result<(), BoundedKernelError>;

impl fmt::Display for BoundedKernelError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Arithmetic(code) => {
                write!(formatter, "RESOURCE_BUDGET_EXCEEDED: {code}")
            }
            Self::Budget {
                owner,
                amount,
                limit,
                used,
            } => write!(
                formatter,
                "RESOURCE_BUDGET_EXCEEDED: {owner} declared cost {amount} exceeds limit {limit} after {used} units"
            ),
            Self::Unsupported(code) => write!(formatter, "UNSUPPORTED: {code}"),
            Self::Canonical {
                code,
                first,
                second,
            } => write!(formatter, "{code}: {first}:{second}"),
            Self::Serialization => formatter.write_str("bounded JSON serialization failed"),
            Self::OutputDrift => formatter.write_str("bounded JSON output drifted"),
            Self::OutputCeiling { ceiling } => write!(
                formatter,
                "RESOURCE_BUDGET_EXCEEDED: serialized size exceeds {ceiling} bytes"
            ),
        }
    }
}

impl std::error::Error for BoundedKernelError {}

impl serde::ser::Error for BoundedKernelError {
    fn custom<T: fmt::Display>(_message: T) -> Self {
        Self::Serialization
    }
}

struct PreflightSerializer<'a> {
    budget: &'a mut dyn JsonCharge,
    string_observer: Option<JsonStringObserver<'a>>,
    owner: &'static str,
    raw_string_bytes: usize,
    percent_string_bytes: usize,
    max_percent_string_bytes: usize,
    traversal_work: usize,
    output_upper_bound: usize,
    source_passes: usize,
    output_passes: usize,
    map_key: bool,
    defer_charge: bool,
    inspect_width: bool,
    deferred_work: usize,
}

impl PreflightSerializer<'_> {
    fn charge(&mut self, amount: usize) -> Result<(), BoundedKernelError> {
        if self.defer_charge {
            self.deferred_work =
                self.deferred_work
                    .checked_add(amount)
                    .ok_or(BoundedKernelError::Arithmetic(
                        "deferred JSON admission overflowed",
                    ))?;
            Ok(())
        } else {
            self.budget.charge_json(self.owner, amount)
        }
    }

    fn flush_deferred_charge(&mut self) -> Result<(), BoundedKernelError> {
        if self.defer_charge {
            self.budget.charge_json(self.owner, self.deferred_work)?;
            self.deferred_work = 0;
        }
        Ok(())
    }

    fn record_traversal(&mut self) -> Result<(), BoundedKernelError> {
        self.traversal_work =
            self.traversal_work
                .checked_add(1)
                .ok_or(BoundedKernelError::Arithmetic(
                    "JSON traversal work overflowed",
                ))?;
        Ok(())
    }

    fn visit(&mut self, output_bytes: usize) -> Result<(), BoundedKernelError> {
        let width = output_bytes
            .checked_add(usize::from(self.map_key) * 2)
            .ok_or(BoundedKernelError::Arithmetic(
                "JSON scalar width overflowed",
            ))?;
        self.charge(
            width
                .checked_mul(self.output_passes)
                .and_then(|work| work.checked_add(JSON_VISIT_WORK))
                .ok_or(BoundedKernelError::Arithmetic(
                    "JSON scalar work overflowed",
                ))?,
        )?;
        self.record_traversal()?;
        self.add_output_upper(width)
    }

    fn container(&mut self, declared: Option<usize>) -> Result<(), BoundedKernelError> {
        if self.map_key {
            return Err(BoundedKernelError::Unsupported("composite map key"));
        }
        let declared = declared.unwrap_or(0);
        let work = declared
            .checked_mul(JSON_VISIT_WORK)
            .ok_or(BoundedKernelError::Arithmetic(
                "JSON container admission overflowed",
            ))?;
        let work = work
            .checked_add(JSON_VISIT_WORK)
            .ok_or(BoundedKernelError::Arithmetic(
                "JSON container admission overflowed",
            ))?;
        self.charge(work)?;
        self.record_traversal()?;
        self.add_output_upper(2)
    }

    fn string(&mut self, value: &str, dynamic: bool) -> Result<(), BoundedKernelError> {
        let length = value.len();
        let conservative_output = length
            .checked_mul(6)
            .and_then(|bytes| bytes.checked_add(2))
            .ok_or(BoundedKernelError::Arithmetic(
                "JSON string envelope overflowed",
            ))?;
        let conservative_work = length
            .checked_mul(self.source_passes)
            .and_then(|source| {
                conservative_output
                    .checked_mul(self.output_passes)
                    .and_then(|output| source.checked_add(output))
            })
            .and_then(|work| work.checked_add(JSON_VISIT_WORK))
            .ok_or(BoundedKernelError::Arithmetic(
                "JSON string admission overflowed",
            ))?;
        self.charge(conservative_work)?;
        if dynamic && let Some(observer) = &mut self.string_observer {
            observer(value)?;
        }
        let output = if self.inspect_width {
            let (output, has_percent) = inspect_json_string(value)?;
            if dynamic && has_percent {
                self.percent_string_bytes = self
                    .percent_string_bytes
                    .checked_add(value.len())
                    .ok_or(BoundedKernelError::Arithmetic(
                        "JSON percent string byte counter overflowed",
                    ))?;
                self.max_percent_string_bytes = self.max_percent_string_bytes.max(length);
            }
            output
        } else {
            conservative_output
        };
        self.record_traversal()?;
        self.raw_string_bytes =
            self.raw_string_bytes
                .checked_add(length)
                .ok_or(BoundedKernelError::Arithmetic(
                    "JSON source byte counter overflowed",
                ))?;
        self.add_output_upper(output)
    }

    fn static_string(&mut self, value: &'static str) -> Result<(), BoundedKernelError> {
        self.string(value, false)
    }

    fn bytes(&mut self, value: &[u8]) -> Result<(), BoundedKernelError> {
        if self.map_key {
            return Err(BoundedKernelError::Unsupported("byte map key"));
        }
        let length = value.len();
        let conservative_output = length
            .checked_mul(4)
            .and_then(|bytes| bytes.checked_add(2))
            .and_then(|bytes| bytes.checked_sub(usize::from(length != 0)))
            .ok_or(BoundedKernelError::Arithmetic(
                "JSON byte envelope overflowed",
            ))?;
        let conservative_work = length
            .checked_mul(self.source_passes)
            .and_then(|source| {
                conservative_output
                    .checked_mul(self.output_passes)
                    .and_then(|output| source.checked_add(output))
            })
            .and_then(|work| work.checked_add(JSON_VISIT_WORK))
            .ok_or(BoundedKernelError::Arithmetic(
                "JSON byte admission overflowed",
            ))?;
        self.charge(conservative_work)?;
        let output = if self.inspect_width {
            byte_array_json_len(value)?
        } else {
            conservative_output
        };
        self.record_traversal()?;
        self.raw_string_bytes =
            self.raw_string_bytes
                .checked_add(length)
                .ok_or(BoundedKernelError::Arithmetic(
                    "JSON byte counter overflowed",
                ))?;
        self.add_output_upper(output)
    }

    fn add_output_upper(&mut self, amount: usize) -> Result<(), BoundedKernelError> {
        self.output_upper_bound =
            self.output_upper_bound
                .checked_add(amount)
                .ok_or(BoundedKernelError::Arithmetic(
                    "JSON output bound overflowed",
                ))?;
        Ok(())
    }
}

fn inspect_json_string(value: &str) -> Result<(usize, bool), BoundedKernelError> {
    value
        .chars()
        .try_fold((2usize, false), |(length, has_percent), character| {
            #[cfg(feature = "architecture-test-seams")]
            observe_json(JsonOperation::PreflightBytesTouched(character.len_utf8()));
            let width = match character {
                '"' | '\\' | '\u{0008}' | '\u{000c}' | '\n' | '\r' | '\t' => 2,
                '\u{0000}'..='\u{001f}' => 6,
                character => character.len_utf8(),
            };
            let length = length
                .checked_add(width)
                .ok_or(BoundedKernelError::Arithmetic(
                    "JSON string width overflowed",
                ))?;
            Ok((length, has_percent || character == '%'))
        })
}

fn byte_array_json_len(value: &[u8]) -> Result<usize, BoundedKernelError> {
    value
        .iter()
        .enumerate()
        .try_fold(2usize, |length, (index, byte)| {
            #[cfg(feature = "architecture-test-seams")]
            observe_json(JsonOperation::PreflightBytesTouched(1));
            let digits = if *byte >= 100 {
                3
            } else if *byte >= 10 {
                2
            } else {
                1
            };
            length
                .checked_add(digits + usize::from(index != 0))
                .ok_or(BoundedKernelError::Arithmetic("JSON byte width overflowed"))
        })
}

fn unsigned_width(value: u128) -> usize {
    if value == 0 {
        1
    } else {
        value.ilog10() as usize + 1
    }
}

fn signed_width(value: i128) -> usize {
    unsigned_width(value.unsigned_abs()) + usize::from(value.is_negative())
}

struct FixedJsonScalar {
    bytes: [u8; 32],
    length: usize,
}

impl std::io::Write for FixedJsonScalar {
    fn write(&mut self, bytes: &[u8]) -> std::io::Result<usize> {
        let end = self
            .length
            .checked_add(bytes.len())
            .filter(|end| *end <= self.bytes.len())
            .ok_or_else(|| std::io::Error::other("bounded scalar width overflowed"))?;
        self.bytes[self.length..end].copy_from_slice(bytes);
        self.length = end;
        Ok(bytes.len())
    }

    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}

fn compact_float_width<T: Serialize>(value: &T) -> Result<usize, BoundedKernelError> {
    let mut writer = FixedJsonScalar {
        bytes: [0; 32],
        length: 0,
    };
    serde_json::to_writer(&mut writer, value).map_err(|_| BoundedKernelError::Serialization)?;
    Ok(writer.length)
}

struct PreflightCompound<'a, 'b> {
    serializer: &'a mut PreflightSerializer<'b>,
    has_entries: bool,
}

impl SerializeSeq for PreflightCompound<'_, '_> {
    type Ok = ();
    type Error = BoundedKernelError;

    fn serialize_element<T: Serialize + ?Sized>(&mut self, value: &T) -> Result<(), Self::Error> {
        self.serializer.record_traversal()?;
        self.serializer
            .add_output_upper(usize::from(self.has_entries))?;
        value.serialize(&mut *self.serializer)?;
        self.has_entries = true;
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        self.serializer.record_traversal()?;
        Ok(())
    }
}

impl SerializeTuple for PreflightCompound<'_, '_> {
    type Ok = ();
    type Error = BoundedKernelError;

    fn serialize_element<T: Serialize + ?Sized>(&mut self, value: &T) -> Result<(), Self::Error> {
        self.serializer.record_traversal()?;
        self.serializer
            .add_output_upper(usize::from(self.has_entries))?;
        value.serialize(&mut *self.serializer)?;
        self.has_entries = true;
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        self.serializer.record_traversal()?;
        Ok(())
    }
}

impl SerializeTupleStruct for PreflightCompound<'_, '_> {
    type Ok = ();
    type Error = BoundedKernelError;

    fn serialize_field<T: Serialize + ?Sized>(&mut self, value: &T) -> Result<(), Self::Error> {
        self.serializer.record_traversal()?;
        self.serializer
            .add_output_upper(usize::from(self.has_entries))?;
        value.serialize(&mut *self.serializer)?;
        self.has_entries = true;
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        self.serializer.record_traversal()?;
        Ok(())
    }
}

impl SerializeTupleVariant for PreflightCompound<'_, '_> {
    type Ok = ();
    type Error = BoundedKernelError;

    fn serialize_field<T: Serialize + ?Sized>(&mut self, value: &T) -> Result<(), Self::Error> {
        self.serializer.record_traversal()?;
        self.serializer
            .add_output_upper(usize::from(self.has_entries))?;
        value.serialize(&mut *self.serializer)?;
        self.has_entries = true;
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        self.serializer.record_traversal()?;
        Ok(())
    }
}

impl SerializeMap for PreflightCompound<'_, '_> {
    type Ok = ();
    type Error = BoundedKernelError;

    fn serialize_key<T: Serialize + ?Sized>(&mut self, key: &T) -> Result<(), Self::Error> {
        self.serializer.record_traversal()?;
        self.serializer
            .add_output_upper(usize::from(self.has_entries))?;
        self.serializer.map_key = true;
        let result = key.serialize(&mut *self.serializer);
        self.serializer.map_key = false;
        result
    }

    fn serialize_value<T: Serialize + ?Sized>(&mut self, value: &T) -> Result<(), Self::Error> {
        self.serializer.record_traversal()?;
        self.serializer.add_output_upper(1)?;
        value.serialize(&mut *self.serializer)?;
        self.has_entries = true;
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        self.serializer.record_traversal()?;
        Ok(())
    }
}

impl SerializeStruct for PreflightCompound<'_, '_> {
    type Ok = ();
    type Error = BoundedKernelError;

    fn serialize_field<T: Serialize + ?Sized>(
        &mut self,
        key: &'static str,
        value: &T,
    ) -> Result<(), Self::Error> {
        self.serializer.record_traversal()?;
        self.serializer
            .add_output_upper(1 + usize::from(self.has_entries))?;
        self.serializer.static_string(key)?;
        value.serialize(&mut *self.serializer)?;
        self.has_entries = true;
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        self.serializer.record_traversal()?;
        Ok(())
    }
}

impl SerializeStructVariant for PreflightCompound<'_, '_> {
    type Ok = ();
    type Error = BoundedKernelError;

    fn serialize_field<T: Serialize + ?Sized>(
        &mut self,
        key: &'static str,
        value: &T,
    ) -> Result<(), Self::Error> {
        self.serializer.record_traversal()?;
        self.serializer
            .add_output_upper(1 + usize::from(self.has_entries))?;
        self.serializer.static_string(key)?;
        value.serialize(&mut *self.serializer)?;
        self.has_entries = true;
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        self.serializer.record_traversal()?;
        Ok(())
    }
}

impl<'a, 'b> Serializer for &'a mut PreflightSerializer<'b> {
    type Ok = ();
    type Error = BoundedKernelError;
    type SerializeSeq = PreflightCompound<'a, 'b>;
    type SerializeTuple = PreflightCompound<'a, 'b>;
    type SerializeTupleStruct = PreflightCompound<'a, 'b>;
    type SerializeTupleVariant = PreflightCompound<'a, 'b>;
    type SerializeMap = PreflightCompound<'a, 'b>;
    type SerializeStruct = PreflightCompound<'a, 'b>;
    type SerializeStructVariant = PreflightCompound<'a, 'b>;

    fn serialize_bool(self, value: bool) -> Result<Self::Ok, Self::Error> {
        self.visit(if value { 4 } else { 5 })
    }

    fn serialize_i8(self, value: i8) -> Result<Self::Ok, Self::Error> {
        self.visit(signed_width(value as i128))
    }

    fn serialize_i16(self, value: i16) -> Result<Self::Ok, Self::Error> {
        self.visit(signed_width(value as i128))
    }

    fn serialize_i32(self, value: i32) -> Result<Self::Ok, Self::Error> {
        self.visit(signed_width(value as i128))
    }

    fn serialize_i64(self, value: i64) -> Result<Self::Ok, Self::Error> {
        self.visit(signed_width(value as i128))
    }

    fn serialize_i128(self, value: i128) -> Result<Self::Ok, Self::Error> {
        self.visit(signed_width(value))
    }

    fn serialize_u8(self, value: u8) -> Result<Self::Ok, Self::Error> {
        self.visit(unsigned_width(value as u128))
    }

    fn serialize_u16(self, value: u16) -> Result<Self::Ok, Self::Error> {
        self.visit(unsigned_width(value as u128))
    }

    fn serialize_u32(self, value: u32) -> Result<Self::Ok, Self::Error> {
        self.visit(unsigned_width(value as u128))
    }

    fn serialize_u64(self, value: u64) -> Result<Self::Ok, Self::Error> {
        self.visit(unsigned_width(value as u128))
    }

    fn serialize_u128(self, value: u128) -> Result<Self::Ok, Self::Error> {
        self.visit(unsigned_width(value))
    }

    fn serialize_f32(self, value: f32) -> Result<Self::Ok, Self::Error> {
        if self.map_key && !value.is_finite() {
            return Err(BoundedKernelError::Unsupported("non-finite float map key"));
        }
        self.visit(compact_float_width(&value)?)
    }

    fn serialize_f64(self, value: f64) -> Result<Self::Ok, Self::Error> {
        if self.map_key && !value.is_finite() {
            return Err(BoundedKernelError::Unsupported("non-finite float map key"));
        }
        self.visit(compact_float_width(&value)?)
    }

    fn serialize_char(self, value: char) -> Result<Self::Ok, Self::Error> {
        let mut bytes = [0u8; 4];
        self.string(value.encode_utf8(&mut bytes), true)
    }

    fn serialize_str(self, value: &str) -> Result<Self::Ok, Self::Error> {
        self.string(value, true)
    }

    fn serialize_bytes(self, value: &[u8]) -> Result<Self::Ok, Self::Error> {
        self.bytes(value)
    }

    fn serialize_none(self) -> Result<Self::Ok, Self::Error> {
        if self.map_key {
            return Err(BoundedKernelError::Unsupported("None map key"));
        }
        self.visit(4)
    }

    fn serialize_some<T: Serialize + ?Sized>(self, value: &T) -> Result<Self::Ok, Self::Error> {
        self.charge(JSON_VISIT_WORK)?;
        self.record_traversal()?;
        value.serialize(self)
    }

    fn serialize_unit(self) -> Result<Self::Ok, Self::Error> {
        if self.map_key {
            return Err(BoundedKernelError::Unsupported("unit map key"));
        }
        self.visit(4)
    }

    fn serialize_unit_struct(self, _name: &'static str) -> Result<Self::Ok, Self::Error> {
        if self.map_key {
            return Err(BoundedKernelError::Unsupported("unit-struct map key"));
        }
        self.visit(4)
    }

    fn serialize_unit_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
    ) -> Result<Self::Ok, Self::Error> {
        self.charge(JSON_VISIT_WORK)?;
        self.record_traversal()?;
        self.static_string(variant)
    }

    fn serialize_newtype_struct<T: Serialize + ?Sized>(
        self,
        _name: &'static str,
        value: &T,
    ) -> Result<Self::Ok, Self::Error> {
        self.charge(JSON_VISIT_WORK)?;
        self.record_traversal()?;
        value.serialize(self)
    }

    fn serialize_newtype_variant<T: Serialize + ?Sized>(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        value: &T,
    ) -> Result<Self::Ok, Self::Error> {
        if self.map_key {
            return Err(BoundedKernelError::Unsupported("newtype-variant map key"));
        }
        self.charge(JSON_VISIT_WORK)?;
        self.record_traversal()?;
        self.static_string(variant)?;
        self.add_output_upper(3)?;
        value.serialize(self)
    }

    fn serialize_seq(self, length: Option<usize>) -> Result<Self::SerializeSeq, Self::Error> {
        self.container(length)?;
        Ok(PreflightCompound {
            serializer: self,
            has_entries: false,
        })
    }

    fn serialize_tuple(self, length: usize) -> Result<Self::SerializeTuple, Self::Error> {
        self.container(Some(length))?;
        Ok(PreflightCompound {
            serializer: self,
            has_entries: false,
        })
    }

    fn serialize_tuple_struct(
        self,
        _name: &'static str,
        length: usize,
    ) -> Result<Self::SerializeTupleStruct, Self::Error> {
        self.container(Some(length))?;
        Ok(PreflightCompound {
            serializer: self,
            has_entries: false,
        })
    }

    fn serialize_tuple_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        length: usize,
    ) -> Result<Self::SerializeTupleVariant, Self::Error> {
        self.container(Some(length))?;
        self.static_string(variant)?;
        self.add_output_upper(3)?;
        Ok(PreflightCompound {
            serializer: self,
            has_entries: false,
        })
    }

    fn serialize_map(self, length: Option<usize>) -> Result<Self::SerializeMap, Self::Error> {
        self.container(length)?;
        Ok(PreflightCompound {
            serializer: self,
            has_entries: false,
        })
    }

    fn serialize_struct(
        self,
        _name: &'static str,
        length: usize,
    ) -> Result<Self::SerializeStruct, Self::Error> {
        self.container(Some(length))?;
        Ok(PreflightCompound {
            serializer: self,
            has_entries: false,
        })
    }

    fn serialize_struct_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        length: usize,
    ) -> Result<Self::SerializeStructVariant, Self::Error> {
        self.container(Some(length))?;
        self.static_string(variant)?;
        self.add_output_upper(3)?;
        Ok(PreflightCompound {
            serializer: self,
            has_entries: false,
        })
    }

    fn collect_str<T: fmt::Display + ?Sized>(self, _value: &T) -> Result<Self::Ok, Self::Error> {
        Err(BoundedKernelError::Unsupported(
            "collect_str has no credited bounded representation",
        ))
    }

    fn is_human_readable(&self) -> bool {
        true
    }
}

struct HashWriter {
    hash: Sha256,
    bytes: usize,
    observe_fragments: bool,
}

impl std::io::Write for HashWriter {
    fn write(&mut self, bytes: &[u8]) -> std::io::Result<usize> {
        self.bytes = self
            .bytes
            .checked_add(bytes.len())
            .ok_or_else(|| std::io::Error::other("serialized hash length overflowed"))?;
        self.hash.update(bytes);
        if self.observe_fragments {
            #[cfg(feature = "architecture-test-seams")]
            observe_json(JsonOperation::FragmentWritten(bytes.len()));
        }
        Ok(bytes.len())
    }

    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}

impl std::io::Write for CountingWriter {
    fn write(&mut self, bytes: &[u8]) -> std::io::Result<usize> {
        let next = self
            .bytes
            .checked_add(bytes.len())
            .ok_or_else(|| std::io::Error::other("serialized size overflowed"))?;
        if next > self.ceiling {
            return Err(std::io::Error::other(format!(
                "RESOURCE_BUDGET_EXCEEDED: serialized size exceeds {} bytes",
                self.ceiling
            )));
        }
        self.bytes = next;
        Ok(bytes.len())
    }

    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}

pub struct Sha256 {
    inner: AcceleratedSha256,
}

impl Sha256 {
    pub fn new() -> Self {
        Self {
            inner: AcceleratedSha256::new(),
        }
    }

    pub fn update(&mut self, bytes: &[u8]) {
        self.inner.update(bytes);
    }

    fn finalize_bytes(self) -> [u8; 32] {
        self.inner.finalize().into()
    }

    pub fn finalize(self) -> String {
        encode_sha256(self.finalize_bytes())
    }
}

impl Default for Sha256 {
    fn default() -> Self {
        Self::new()
    }
}

pub fn sha256(bytes: &[u8]) -> String {
    let mut hash = Sha256::new();
    hash.update(bytes);
    hash.finalize()
}

pub(crate) fn exact_os_identity_bytes(domain: &[u8], value: &OsStr) -> Vec<u8> {
    #[cfg(unix)]
    let value = value.as_bytes();
    #[cfg(not(unix))]
    let value = value.as_encoded_bytes();
    let mut framed = Vec::with_capacity(
        b"bbnf.exact-os-identity.v1\0".len()
            + std::mem::size_of::<u64>() * 2
            + domain.len()
            + value.len(),
    );
    framed.extend_from_slice(b"bbnf.exact-os-identity.v1\0");
    framed.extend_from_slice(&(domain.len() as u64).to_le_bytes());
    framed.extend_from_slice(domain);
    framed.extend_from_slice(&(value.len() as u64).to_le_bytes());
    framed.extend_from_slice(value);
    framed
}

#[cfg(feature = "architecture-test-seams")]
#[doc(hidden)]
pub fn exact_os_identity_bytes_for_test(domain: &[u8], value: &OsStr) -> Vec<u8> {
    exact_os_identity_bytes(domain, value)
}

#[cfg(feature = "architecture-test-seams")]
#[doc(hidden)]
pub fn canonical_json<T: Serialize>(value: &T) -> anyhow::Result<Vec<u8>> {
    serde_json::to_vec(value).map_err(Into::into)
}

pub fn hash_serialized<T: Serialize>(value: &T) -> anyhow::Result<String> {
    let mut writer = HashWriter {
        hash: Sha256::new(),
        bytes: 0,
        observe_fragments: false,
    };
    serde_json::to_writer(&mut writer, value)?;
    Ok(encode_sha256(writer.hash.finalize_bytes()))
}

struct JsonPlan<'a, T: Serialize + ?Sized> {
    value: &'a T,
    ceiling: usize,
    #[cfg_attr(not(feature = "architecture-test-seams"), allow(dead_code))]
    raw_string_bytes: usize,
    output_upper_bound: usize,
    hash_pair: bool,
}

struct JsonMetrics {
    pub(crate) length: usize,
    digest: [u8; 32],
}

fn encode_sha256(digest: [u8; 32]) -> String {
    const HEX: &[u8; 16] = b"0123456789abcdef";
    let mut encoded = String::with_capacity(64);
    for byte in digest {
        encoded.push(HEX[usize::from(byte >> 4)] as char);
        encoded.push(HEX[usize::from(byte & 0x0f)] as char);
    }
    encoded
}

fn prepare_json_measure<'a, T: Serialize + ?Sized>(
    value: &'a T,
    budget: &mut impl JsonCharge,
    owner: &'static str,
) -> Result<JsonPlan<'a, T>, BoundedKernelError> {
    let ceiling = budget.available_json_bytes()?;
    prepare_json_with_ceiling(
        value,
        budget,
        owner,
        ceiling,
        JSON_MEASURE_SCAN_PASSES,
        JSON_MEASURE_OUTPUT_PASSES,
        false,
    )
}

fn prepare_json_hash<'a, T: Serialize + ?Sized>(
    value: &'a T,
    budget: &mut impl JsonCharge,
    owner: &'static str,
) -> Result<JsonPlan<'a, T>, BoundedKernelError> {
    prepare_json_with_ceiling(
        value,
        budget,
        owner,
        usize::MAX,
        JSON_HASH_SCAN_PASSES,
        JSON_HASH_OUTPUT_PASSES,
        true,
    )
}

fn prepare_json_with_ceiling<'a, T: Serialize + ?Sized>(
    value: &'a T,
    budget: &mut impl JsonCharge,
    owner: &'static str,
    ceiling: usize,
    source_passes: usize,
    output_passes: usize,
    hash_pair: bool,
) -> Result<JsonPlan<'a, T>, BoundedKernelError> {
    let (raw_string_bytes, output_upper_bound) = {
        let mut serializer = PreflightSerializer {
            budget,
            string_observer: None,
            owner,
            raw_string_bytes: 0,
            percent_string_bytes: 0,
            max_percent_string_bytes: 0,
            traversal_work: 0,
            output_upper_bound: 0,
            source_passes,
            output_passes,
            map_key: false,
            defer_charge: false,
            inspect_width: true,
            deferred_work: 0,
        };
        value.serialize(&mut serializer)?;
        (serializer.raw_string_bytes, serializer.output_upper_bound)
    };
    Ok(JsonPlan {
        value,
        ceiling,
        raw_string_bytes,
        output_upper_bound,
        hash_pair,
    })
}

struct UnboundedJsonCharge;

impl JsonCharge for UnboundedJsonCharge {
    fn charge_json(
        &mut self,
        _owner: &'static str,
        _amount: usize,
    ) -> Result<(), BoundedKernelError> {
        Ok(())
    }

    fn available_json_bytes(&self) -> Result<usize, BoundedKernelError> {
        Ok(usize::MAX)
    }
}

pub(crate) fn walk_json_strings<T: Serialize + ?Sized>(
    value: &T,
    observer: &mut dyn FnMut(&str) -> Result<(), BoundedKernelError>,
) -> Result<(), BoundedKernelError> {
    let mut budget = UnboundedJsonCharge;
    let mut serializer = PreflightSerializer {
        budget: &mut budget,
        string_observer: Some(observer),
        owner: "json-string-walk",
        raw_string_bytes: 0,
        percent_string_bytes: 0,
        max_percent_string_bytes: 0,
        traversal_work: 0,
        output_upper_bound: 0,
        source_passes: 0,
        output_passes: 0,
        map_key: false,
        defer_charge: false,
        inspect_width: false,
        deferred_work: 0,
    };
    value.serialize(&mut serializer)
}

impl<T: Serialize + ?Sized> JsonPlan<'_, T> {
    fn execute(self) -> Result<JsonMetrics, BoundedKernelError> {
        let measured = self.write_once(false)?;
        if self.hash_pair {
            let streamed = self.write_once(true)?;
            if streamed.length != measured.length || streamed.digest != measured.digest {
                return Err(BoundedKernelError::OutputDrift);
            }
        }
        Ok(measured)
    }

    fn write_once(&self, observe_fragments: bool) -> Result<JsonMetrics, BoundedKernelError> {
        #[cfg(feature = "architecture-test-seams")]
        observe_json(JsonOperation::SourceBytesScanned(self.raw_string_bytes));
        let mut writer = HashWriter {
            hash: Sha256::new(),
            bytes: 0,
            observe_fragments,
        };
        serde_json::to_writer(&mut writer, self.value)
            .map_err(|_| BoundedKernelError::Serialization)?;
        if writer.bytes > self.output_upper_bound {
            return Err(BoundedKernelError::OutputDrift);
        }
        if writer.bytes > self.ceiling {
            return Err(BoundedKernelError::OutputCeiling {
                ceiling: self.ceiling,
            });
        }
        Ok(JsonMetrics {
            length: writer.bytes,
            digest: writer.hash.finalize_bytes(),
        })
    }
}

fn execute_stable_hash<T: Serialize + ?Sized>(value: &T) -> Result<[u8; 32], BoundedKernelError> {
    fn write<T: Serialize + ?Sized>(
        value: &T,
        observe_fragments: bool,
    ) -> Result<JsonMetrics, BoundedKernelError> {
        let mut writer = HashWriter {
            hash: Sha256::new(),
            bytes: 0,
            observe_fragments,
        };
        serde_json::to_writer(&mut writer, value).map_err(|_| BoundedKernelError::Serialization)?;
        Ok(JsonMetrics {
            length: writer.bytes,
            digest: writer.hash.finalize_bytes(),
        })
    }

    let measured = write(value, false)?;
    let streamed = write(value, true)?;
    if streamed.length != measured.length || streamed.digest != measured.digest {
        return Err(BoundedKernelError::OutputDrift);
    }
    Ok(measured.digest)
}

fn edge_identity_matches_digest(
    identity: &crate::architecture::schema::EdgeIdentity,
    digest: [u8; 32],
) -> bool {
    const PREFIX: &[u8] = b"edge:sha256:";
    const HEX: &[u8; 16] = b"0123456789abcdef";
    let encoded = identity.as_str().as_bytes();
    if encoded.len() != PREFIX.len() + 64 || !encoded.starts_with(PREFIX) {
        return false;
    }
    digest.iter().enumerate().all(|(index, byte)| {
        encoded[PREFIX.len() + index * 2] == HEX[usize::from(byte >> 4)]
            && encoded[PREFIX.len() + index * 2 + 1] == HEX[usize::from(byte & 0x0f)]
    })
}

pub(crate) fn edge_identity_is_bound(
    edge: &crate::architecture::schema::EdgeFact,
) -> Result<bool, BoundedKernelError> {
    Ok(edge_identity_matches_digest(
        &edge.identity,
        execute_stable_hash(&edge.record)?,
    ))
}

pub(crate) fn bounded_serialized_length<T: Serialize + ?Sized>(
    value: &T,
) -> Result<usize, BoundedKernelError> {
    let mut budget = UnboundedJsonCharge;
    Ok(prepare_json_with_ceiling(
        value,
        &mut budget,
        "bounded-serialized-length",
        usize::MAX,
        0,
        0,
        false,
    )?
    .output_upper_bound)
}

pub(crate) struct EdgeDomainJsonMetrics {
    pub(crate) length: usize,
    pub(crate) invalid_edge: Option<usize>,
}

pub(crate) fn execute_owned_edge_domain_json(
    vertices: &std::collections::BTreeSet<crate::architecture::schema::Identity>,
    edges: &[crate::architecture::schema::EdgeFact],
    budget: &mut impl JsonCharge,
    edge_owner: &'static str,
    domain_owner: &'static str,
) -> anyhow::Result<EdgeDomainJsonMetrics> {
    for edge in edges {
        let _ = prepare_json_hash(&edge.record, budget, edge_owner)?;
    }
    let domain = (vertices, edges);
    let domain_plan = prepare_json_measure(&domain, budget, domain_owner)?;
    let length = domain_plan.execute()?.length;
    let mut invalid_edge = None;
    for (index, edge) in edges.iter().enumerate() {
        let digest = execute_stable_hash(&edge.record)?;
        if !edge_identity_matches_digest(&edge.identity, digest) {
            invalid_edge.get_or_insert(index);
        }
    }
    Ok(EdgeDomainJsonMetrics {
        length,
        invalid_edge,
    })
}

pub(crate) fn execute_borrowed_edge_domain_json(
    vertices: &std::collections::BTreeSet<crate::architecture::schema::Identity>,
    edges: &[&crate::architecture::schema::EdgeFact],
    budget: &mut impl JsonCharge,
    edge_owner: &'static str,
    domain_owner: &'static str,
) -> Result<EdgeDomainJsonMetrics, BoundedKernelError> {
    for edge in edges {
        let _ = prepare_json_hash(&edge.record, budget, edge_owner)?;
    }
    let domain = (vertices, edges);
    let domain_plan = prepare_json_measure(&domain, budget, domain_owner)?;
    let length = domain_plan.execute()?.length;
    let mut invalid_edge = None;
    for (index, edge) in edges.iter().enumerate() {
        let digest = execute_stable_hash(&edge.record)?;
        if !edge_identity_matches_digest(&edge.identity, digest) {
            invalid_edge.get_or_insert(index);
        }
    }
    Ok(EdgeDomainJsonMetrics {
        length,
        invalid_edge,
    })
}

pub(crate) fn execute_borrowed_member_edge_domain_json(
    members: &[&crate::architecture::schema::Identity],
    edges: &[&crate::architecture::schema::EdgeFact],
    budget: &mut impl JsonCharge,
    edge_owner: &'static str,
    domain_owner: &'static str,
) -> Result<EdgeDomainJsonMetrics, BoundedKernelError> {
    for edge in edges {
        let _ = prepare_json_hash(&edge.record, budget, edge_owner)?;
    }
    let domain = (members, edges);
    let domain_plan = prepare_json_measure(&domain, budget, domain_owner)?;
    let length = domain_plan.execute()?.length;
    let mut invalid_edge = None;
    for (index, edge) in edges.iter().enumerate() {
        let digest = execute_stable_hash(&edge.record)?;
        if !edge_identity_matches_digest(&edge.identity, digest) {
            invalid_edge.get_or_insert(index);
        }
    }
    Ok(EdgeDomainJsonMetrics {
        length,
        invalid_edge,
    })
}

pub(crate) fn execute_component_member_hash(
    projection: crate::architecture::schema::ProjectionName,
    members: &[crate::architecture::schema::Identity],
    budget: &mut impl JsonCharge,
    owner: &'static str,
) -> Result<String, BoundedKernelError> {
    Ok(encode_sha256(
        prepare_json_hash(&(projection, members), budget, owner)?
            .execute()?
            .digest,
    ))
}

pub(crate) fn execute_component_member_set_hash(
    projection: crate::architecture::schema::ProjectionName,
    members: &std::collections::BTreeSet<crate::architecture::schema::Identity>,
    budget: &mut impl JsonCharge,
    owner: &'static str,
) -> Result<String, BoundedKernelError> {
    Ok(encode_sha256(
        prepare_json_hash(&(projection, members), budget, owner)?
            .execute()?
            .digest,
    ))
}

pub(crate) fn execute_component_member_refs_hash(
    projection: crate::architecture::schema::ProjectionName,
    members: &[&crate::architecture::schema::Identity],
    budget: &mut impl JsonCharge,
    owner: &'static str,
) -> Result<String, BoundedKernelError> {
    Ok(encode_sha256(
        prepare_json_hash(&(projection, members), budget, owner)?
            .execute()?
            .digest,
    ))
}

pub(crate) fn execute_cycle_member_measure(
    projection: crate::architecture::schema::ProjectionName,
    members: &std::collections::BTreeSet<crate::architecture::schema::Identity>,
    budget: &mut impl JsonCharge,
    owner: &'static str,
) -> Result<usize, BoundedKernelError> {
    Ok(prepare_json_measure(&(projection, members), budget, owner)?
        .execute()?
        .length)
}

pub(crate) fn execute_cycle_member_refs_measure(
    projection: crate::architecture::schema::ProjectionName,
    members: &[&crate::architecture::schema::Identity],
    budget: &mut impl JsonCharge,
    owner: &'static str,
) -> Result<usize, BoundedKernelError> {
    Ok(prepare_json_measure(&(projection, members), budget, owner)?
        .execute()?
        .length)
}

pub(crate) struct GraphJsonPreflight {
    pub(crate) structural_work: usize,
    pub(crate) raw_string_bytes: usize,
    pub(crate) percent_string_bytes: usize,
    pub(crate) max_percent_string_bytes: usize,
    pub(crate) serialized_length: usize,
}

pub(crate) fn execute_graph_measure_with_admission<B: JsonCharge>(
    graph: &crate::architecture::schema::GraphArtifact,
    budget: &mut B,
    owner: &'static str,
    admission: impl FnOnce(&GraphJsonPreflight, &mut B) -> Result<(), BoundedKernelError>,
) -> Result<usize, BoundedKernelError> {
    // This sealed graph survey uses only cardinalities and string lengths. Its
    // single charge admits both the structural survey and the following exact
    // dry preflight before either pass inspects payload bytes. P consists of
    // two structural traversals and one source-byte scan; the later actual
    // writer/source pass belongs to C.
    let (structural_work, raw_string_bytes, admitted_dry_preflight_work) = {
        let mut serializer = PreflightSerializer {
            budget,
            string_observer: None,
            owner,
            raw_string_bytes: 0,
            percent_string_bytes: 0,
            max_percent_string_bytes: 0,
            traversal_work: 0,
            output_upper_bound: 0,
            source_passes: 0,
            output_passes: 0,
            map_key: false,
            defer_charge: true,
            inspect_width: false,
            deferred_work: 0,
        };
        graph.serialize(&mut serializer)?;
        let weighted_structural_admission = serializer.deferred_work;
        let admitted_dry_preflight_work = weighted_structural_admission
            .checked_add(serializer.raw_string_bytes)
            .ok_or(BoundedKernelError::Arithmetic(
                "graph exact dry-preflight overflowed",
            ))?;
        serializer.deferred_work = weighted_structural_admission
            .checked_add(admitted_dry_preflight_work)
            .ok_or(BoundedKernelError::Arithmetic(
                "graph paired dry-preflight admission overflowed",
            ))?;
        serializer.flush_deferred_charge()?;
        (
            serializer.traversal_work,
            serializer.raw_string_bytes,
            admitted_dry_preflight_work,
        )
    };
    // The exact dry preflight uses the same private serializer and caller
    // budget. Its charge is deferred and intentionally not applied because P
    // already admitted its conservative envelope. It writes no JSON fragment.
    let (
        plan,
        exact_structural_work,
        exact_raw_string_bytes,
        percent_string_bytes,
        max_percent_string_bytes,
        dry_preflight_work,
    ) = {
        let mut serializer = PreflightSerializer {
            budget,
            string_observer: None,
            owner,
            raw_string_bytes: 0,
            percent_string_bytes: 0,
            max_percent_string_bytes: 0,
            traversal_work: 0,
            output_upper_bound: 0,
            source_passes: 1,
            output_passes: 0,
            map_key: false,
            defer_charge: true,
            inspect_width: true,
            deferred_work: 0,
        };
        graph.serialize(&mut serializer)?;
        (
            JsonPlan {
                value: graph,
                ceiling: usize::MAX,
                raw_string_bytes: serializer.raw_string_bytes,
                output_upper_bound: serializer.output_upper_bound,
                hash_pair: false,
            },
            serializer.traversal_work,
            serializer.raw_string_bytes,
            serializer.percent_string_bytes,
            serializer.max_percent_string_bytes,
            serializer.deferred_work,
        )
    };
    if exact_structural_work != structural_work
        || exact_raw_string_bytes != raw_string_bytes
        || dry_preflight_work != admitted_dry_preflight_work
    {
        return Err(BoundedKernelError::OutputDrift);
    }
    let preflight = GraphJsonPreflight {
        structural_work,
        raw_string_bytes,
        percent_string_bytes,
        max_percent_string_bytes,
        serialized_length: plan.output_upper_bound,
    };
    admission(&preflight, budget)?;
    Ok(plan.execute()?.length)
}

pub fn serialized_len_capped<T: Serialize>(value: &T, ceiling: usize) -> anyhow::Result<usize> {
    let mut writer = CountingWriter { bytes: 0, ceiling };
    serde_json::to_writer(&mut writer, value)?;
    Ok(writer.bytes)
}

#[cfg(feature = "architecture-test-seams")]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[doc(hidden)]
pub enum JsonOperation {
    PreflightBytesTouched(usize),
    SourceBytesScanned(usize),
    FragmentWritten(usize),
}

#[cfg(feature = "architecture-test-seams")]
thread_local! {
    static JSON_OBSERVER: std::cell::Cell<Option<fn(JsonOperation)>> =
        const { std::cell::Cell::new(None) };
}

#[cfg(feature = "architecture-test-seams")]
fn observe_json(operation: JsonOperation) {
    JSON_OBSERVER.with(|slot| {
        if let Some(observer) = slot.get() {
            observer(operation);
        }
    });
}

#[cfg(feature = "architecture-test-seams")]
#[doc(hidden)]
pub fn with_json_observer_for_test<T>(
    observer: fn(JsonOperation),
    operation: impl FnOnce() -> T,
) -> T {
    struct Restore(Option<fn(JsonOperation)>);

    impl Drop for Restore {
        fn drop(&mut self) {
            JSON_OBSERVER.with(|slot| slot.set(self.0));
        }
    }

    let previous = JSON_OBSERVER.with(|slot| slot.replace(Some(observer)));
    let restore = Restore(previous);
    let result = operation();
    drop(restore);
    result
}

#[cfg(feature = "architecture-test-seams")]
fn budgeted_json_for_test<T: Serialize + ?Sized>(
    value: &T,
    work_limit: usize,
) -> Result<(usize, String, usize, usize), BoundedKernelError> {
    struct TestBudget {
        limit: usize,
        used: usize,
    }

    impl JsonCharge for TestBudget {
        fn charge_json(
            &mut self,
            owner: &'static str,
            amount: usize,
        ) -> Result<(), BoundedKernelError> {
            let next = self
                .used
                .checked_add(amount)
                .ok_or(BoundedKernelError::Arithmetic("test JSON work overflowed"))?;
            if next > self.limit {
                return Err(BoundedKernelError::Budget {
                    owner,
                    amount,
                    limit: self.limit,
                    used: self.used,
                });
            }
            self.used = next;
            Ok(())
        }

        fn available_json_bytes(&self) -> Result<usize, BoundedKernelError> {
            Ok(usize::MAX)
        }
    }

    let mut budget = TestBudget {
        limit: work_limit,
        used: 0,
    };
    let plan = prepare_json_hash(value, &mut budget, "test-json")?;
    let output_upper_bound = plan.output_upper_bound;
    let measured = plan.execute()?;
    Ok((
        measured.length,
        encode_sha256(measured.digest),
        budget.used,
        output_upper_bound,
    ))
}

#[cfg(feature = "architecture-test-seams")]
#[doc(hidden)]
pub enum JsonTestView<'a> {
    String(&'a str),
    Bytes(&'a [u8]),
    I128(i128),
    I128Slice(&'a [i128]),
    UnknownI128Slice(&'a [i128]),
    U128BoolMap(&'a std::collections::BTreeMap<u128, bool>),
    BoolI128Map(&'a std::collections::BTreeMap<bool, i128>),
    MapKey(JsonMapKeyTestCase),
    CollectStr(&'a dyn fmt::Display),
    MapCollectStrKey(&'a dyn fmt::Display),
    OverflowSequence,
    OverflowMap,
}

#[cfg(feature = "architecture-test-seams")]
#[derive(Clone, Copy, Debug)]
#[doc(hidden)]
pub enum JsonMapKeyTestCase {
    F32(f32),
    F64(f64),
    Char(char),
    UnitVariant,
    Unit,
    UnitStruct,
    Bytes,
    None,
    NewtypeStruct,
    Some,
    NewtypeVariant,
    Seq,
    Tuple,
    TupleStruct,
    TupleVariant,
    Map,
    Struct,
    StructVariant,
}

#[cfg(feature = "architecture-test-seams")]
struct ByteValue<'a>(&'a [u8]);

#[cfg(feature = "architecture-test-seams")]
impl Serialize for ByteValue<'_> {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        serializer.serialize_bytes(self.0)
    }
}

#[cfg(feature = "architecture-test-seams")]
struct SingleKeyMap<T>(T);

#[cfg(feature = "architecture-test-seams")]
impl<T: Serialize> Serialize for SingleKeyMap<T> {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let mut map = serializer.serialize_map(Some(1))?;
        map.serialize_entry(&self.0, &true)?;
        map.end()
    }
}

#[cfg(feature = "architecture-test-seams")]
impl Serialize for JsonMapKeyTestCase {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        match *self {
            Self::F32(value) => serializer.serialize_f32(value),
            Self::F64(value) => serializer.serialize_f64(value),
            Self::Char(value) => serializer.serialize_char(value),
            Self::UnitVariant => serializer.serialize_unit_variant("Key", 0, "ready"),
            Self::Unit => serializer.serialize_unit(),
            Self::UnitStruct => serializer.serialize_unit_struct("Key"),
            Self::Bytes => serializer.serialize_bytes(b"key"),
            Self::None => serializer.serialize_none(),
            Self::NewtypeStruct => serializer.serialize_newtype_struct("Key", &7i32),
            Self::Some => serializer.serialize_some(&7i32),
            Self::NewtypeVariant => serializer.serialize_newtype_variant("Key", 0, "n", &7i32),
            Self::Seq => {
                let mut value = serializer.serialize_seq(Some(1))?;
                value.serialize_element(&7i32)?;
                value.end()
            }
            Self::Tuple => {
                let mut value = serializer.serialize_tuple(1)?;
                value.serialize_element(&7i32)?;
                value.end()
            }
            Self::TupleStruct => {
                let mut value = serializer.serialize_tuple_struct("Key", 1)?;
                value.serialize_field(&7i32)?;
                value.end()
            }
            Self::TupleVariant => {
                let mut value = serializer.serialize_tuple_variant("Key", 0, "t", 1)?;
                value.serialize_field(&7i32)?;
                value.end()
            }
            Self::Map => {
                let mut value = serializer.serialize_map(Some(1))?;
                value.serialize_entry("nested", &7i32)?;
                value.end()
            }
            Self::Struct => {
                let mut value = serializer.serialize_struct("Key", 1)?;
                value.serialize_field("field", &7i32)?;
                value.end()
            }
            Self::StructVariant => {
                let mut value = serializer.serialize_struct_variant("Key", 0, "s", 1)?;
                value.serialize_field("field", &7i32)?;
                value.end()
            }
        }
    }
}

#[cfg(feature = "architecture-test-seams")]
struct CollectStrValue<'a>(&'a dyn fmt::Display);

#[cfg(feature = "architecture-test-seams")]
impl Serialize for CollectStrValue<'_> {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        serializer.collect_str(self.0)
    }
}

#[cfg(feature = "architecture-test-seams")]
struct UnknownI128Slice<'a>(&'a [i128]);

#[cfg(feature = "architecture-test-seams")]
impl Serialize for UnknownI128Slice<'_> {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let mut sequence = serializer.serialize_seq(None)?;
        for value in self.0 {
            sequence.serialize_element(value)?;
        }
        sequence.end()
    }
}

#[cfg(feature = "architecture-test-seams")]
struct OverflowSequence;

#[cfg(feature = "architecture-test-seams")]
impl Serialize for OverflowSequence {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        serializer.serialize_seq(Some(usize::MAX))?.end()
    }
}

#[cfg(feature = "architecture-test-seams")]
struct OverflowMap;

#[cfg(feature = "architecture-test-seams")]
impl Serialize for OverflowMap {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        serializer.serialize_map(Some(usize::MAX))?.end()
    }
}

#[cfg(feature = "architecture-test-seams")]
#[doc(hidden)]
pub fn budgeted_json_view_for_test(
    view: JsonTestView<'_>,
    work_limit: usize,
) -> Result<(usize, String, usize, usize), BoundedKernelError> {
    match view {
        JsonTestView::String(value) => budgeted_json_for_test(value, work_limit),
        JsonTestView::Bytes(value) => budgeted_json_for_test(&ByteValue(value), work_limit),
        JsonTestView::I128(value) => budgeted_json_for_test(&value, work_limit),
        JsonTestView::I128Slice(value) => budgeted_json_for_test(value, work_limit),
        JsonTestView::UnknownI128Slice(value) => {
            budgeted_json_for_test(&UnknownI128Slice(value), work_limit)
        }
        JsonTestView::U128BoolMap(value) => budgeted_json_for_test(value, work_limit),
        JsonTestView::BoolI128Map(value) => budgeted_json_for_test(value, work_limit),
        JsonTestView::MapKey(key) => budgeted_json_for_test(&SingleKeyMap(key), work_limit),
        JsonTestView::CollectStr(value) => {
            budgeted_json_for_test(&CollectStrValue(value), work_limit)
        }
        JsonTestView::MapCollectStrKey(key) => {
            budgeted_json_for_test(&SingleKeyMap(CollectStrValue(key)), work_limit)
        }
        JsonTestView::OverflowSequence => budgeted_json_for_test(&OverflowSequence, work_limit),
        JsonTestView::OverflowMap => budgeted_json_for_test(&OverflowMap, work_limit),
    }
}
