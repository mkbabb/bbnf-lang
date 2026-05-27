use serde::de::{MapAccess, Visitor};
use serde::{Deserialize, Deserializer};
use serde_json::value::RawValue;
use std::borrow::Cow;
use std::fmt;
use std::marker::PhantomData;
use std::path::PathBuf;

use crate::direct_struct::DirectStructError;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RealTypedFixture {
    Twitter,
    ApacheBuilds,
    CitmCatalog,
    Gsoc2018,
    GithubEvents,
    UpdateCenter,
    Mesh,
    MarineIk,
    Instruments,
    Canada,
    Numbers,
    UnicodeBasic,
    UnicodeMixed,
    DistinctValues,
    YStringUnicode,
    Random,
}

#[derive(Debug, Deserialize)]
pub struct TwitterSearch<'a> {
    #[serde(default, borrow)]
    pub statuses: Vec<Tweet<'a>>,
}

#[derive(Debug, Deserialize)]
pub struct Tweet<'a> {
    #[serde(default)]
    pub id: Option<u64>,
    #[serde(default, borrow)]
    pub text: Option<Cow<'a, str>>,
}

#[derive(Debug, Deserialize)]
pub struct ApacheBuilds<'a> {
    #[serde(default, borrow)]
    pub mode: Option<Cow<'a, str>>,
    #[serde(default, borrow, rename = "nodeName")]
    pub node_name: Option<Cow<'a, str>>,
    #[serde(default, borrow)]
    pub jobs: Vec<ApacheJob<'a>>,
}

#[derive(Debug, Deserialize)]
pub struct ApacheJob<'a> {
    #[serde(default, borrow)]
    pub name: Option<Cow<'a, str>>,
    #[serde(default, borrow)]
    pub url: Option<Cow<'a, str>>,
    #[serde(default, borrow)]
    pub color: Option<Cow<'a, str>>,
}

#[derive(Debug, Deserialize)]
pub struct CitmCatalog<'a> {
    #[serde(default, borrow, deserialize_with = "deserialize_citm_event_entries")]
    pub events: Vec<CitmEventEntry<'a>>,
}

#[derive(Debug, Deserialize)]
pub struct CitmEvent<'a> {
    #[serde(default)]
    pub id: Option<u64>,
    #[serde(default, borrow)]
    pub name: Option<Cow<'a, str>>,
    #[serde(default, rename = "subTopicIds")]
    pub sub_topic_ids: Vec<u64>,
    #[serde(default, rename = "topicIds")]
    pub topic_ids: Vec<u64>,
}

#[derive(Debug)]
pub struct GsocProposalEntry<'a> {
    pub key: u32,
    pub value: GsocProposal<'a>,
}

#[derive(Debug, Deserialize)]
pub struct GsocProposal<'a> {
    #[serde(rename = "@context")]
    pub context: GsocContext,
    #[serde(rename = "@type")]
    pub proposal_type: GsocProposalType,
    #[serde(borrow)]
    pub name: DecodedJsonString<'a>,
    #[serde(borrow)]
    pub description: DecodedJsonString<'a>,
    #[serde(borrow)]
    pub sponsor: GsocSponsor<'a>,
    #[serde(borrow)]
    pub author: GsocAuthor<'a>,
}

#[derive(Debug, Deserialize)]
pub struct GsocSponsor<'a> {
    #[serde(rename = "@type")]
    pub sponsor_type: GsocSponsorType,
    #[serde(borrow)]
    pub name: DecodedJsonString<'a>,
    #[serde(borrow, rename = "disambiguatingDescription")]
    pub disambiguating_description: DecodedJsonString<'a>,
    #[serde(borrow)]
    pub description: DecodedJsonString<'a>,
    #[serde(borrow)]
    pub url: DecodedJsonString<'a>,
    #[serde(borrow)]
    pub logo: DecodedJsonString<'a>,
}

#[derive(Debug, Deserialize)]
pub struct GsocAuthor<'a> {
    #[serde(rename = "@type")]
    pub author_type: GsocAuthorType,
    #[serde(borrow)]
    pub name: DecodedJsonString<'a>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum GsocContext {
    SchemaOrg,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum GsocProposalType {
    SoftwareSourceCode,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum GsocSponsorType {
    Organization,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum GsocAuthorType {
    Person,
}

impl GsocContext {
    fn id(self) -> u64 {
        match self {
            Self::SchemaOrg => 1,
        }
    }
}

impl GsocProposalType {
    fn id(self) -> u64 {
        match self {
            Self::SoftwareSourceCode => 2,
        }
    }
}

impl GsocSponsorType {
    fn id(self) -> u64 {
        match self {
            Self::Organization => 3,
        }
    }
}

impl GsocAuthorType {
    fn id(self) -> u64 {
        match self {
            Self::Person => 4,
        }
    }
}

impl<'de> Deserialize<'de> for GsocContext {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct GsocContextVisitor;

        impl Visitor<'_> for GsocContextVisitor {
            type Value = GsocContext;

            fn expecting(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
                formatter.write_str("the Schema.org context token")
            }

            fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                match value {
                    "http://schema.org" => Ok(GsocContext::SchemaOrg),
                    _ => Err(E::custom("unexpected GSoC context token")),
                }
            }
        }

        deserializer.deserialize_str(GsocContextVisitor)
    }
}

impl<'de> Deserialize<'de> for GsocProposalType {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct GsocProposalTypeVisitor;

        impl Visitor<'_> for GsocProposalTypeVisitor {
            type Value = GsocProposalType;

            fn expecting(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
                formatter.write_str("the GSoC proposal type token")
            }

            fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                match value {
                    "SoftwareSourceCode" => Ok(GsocProposalType::SoftwareSourceCode),
                    _ => Err(E::custom("unexpected GSoC proposal type token")),
                }
            }
        }

        deserializer.deserialize_str(GsocProposalTypeVisitor)
    }
}

impl<'de> Deserialize<'de> for GsocSponsorType {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct GsocSponsorTypeVisitor;

        impl Visitor<'_> for GsocSponsorTypeVisitor {
            type Value = GsocSponsorType;

            fn expecting(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
                formatter.write_str("the GSoC sponsor type token")
            }

            fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                match value {
                    "Organization" => Ok(GsocSponsorType::Organization),
                    _ => Err(E::custom("unexpected GSoC sponsor type token")),
                }
            }
        }

        deserializer.deserialize_str(GsocSponsorTypeVisitor)
    }
}

impl<'de> Deserialize<'de> for GsocAuthorType {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct GsocAuthorTypeVisitor;

        impl Visitor<'_> for GsocAuthorTypeVisitor {
            type Value = GsocAuthorType;

            fn expecting(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
                formatter.write_str("the GSoC author type token")
            }

            fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                match value {
                    "Person" => Ok(GsocAuthorType::Person),
                    _ => Err(E::custom("unexpected GSoC author type token")),
                }
            }
        }

        deserializer.deserialize_str(GsocAuthorTypeVisitor)
    }
}

#[derive(Debug)]
struct GsocProposalEntries<'a>(Vec<GsocProposalEntry<'a>>);

impl<'de> Deserialize<'de> for GsocProposalEntries<'de> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct GsocProposalEntriesVisitor;

        impl<'de> Visitor<'de> for GsocProposalEntriesVisitor {
            type Value = GsocProposalEntries<'de>;

            fn expecting(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
                formatter.write_str("a GSoC proposal map")
            }

            fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
            where
                A: MapAccess<'de>,
            {
                let mut entries = Vec::with_capacity(map.size_hint().unwrap_or(0));
                while let Some((key, value)) =
                    map.next_entry::<Cow<'de, str>, GsocProposal<'de>>()?
                {
                    let key = key
                        .parse::<u32>()
                        .map_err(|_| serde::de::Error::custom("invalid GSoC numeric key"))?;
                    entries.push(GsocProposalEntry { key, value });
                }
                Ok(GsocProposalEntries(entries))
            }
        }

        deserializer.deserialize_map(GsocProposalEntriesVisitor)
    }
}

#[derive(Debug)]
pub struct CitmEventEntry<'a> {
    pub key: Cow<'a, str>,
    pub value: CitmEvent<'a>,
}

#[derive(Debug, Deserialize)]
pub struct UpdateCenter<'a> {
    #[serde(default, borrow, rename = "connectionCheckUrl")]
    pub connection_check_url: Option<Cow<'a, str>>,
    #[serde(default, borrow)]
    pub core: Option<UpdateCore<'a>>,
    #[serde(default, borrow)]
    pub id: Option<Cow<'a, str>>,
    #[serde(default, borrow, deserialize_with = "deserialize_plugin_entries")]
    pub plugins: Vec<PluginEntry<'a>>,
    #[serde(default, borrow, rename = "updateCenterVersion")]
    pub update_center_version: Option<Cow<'a, str>>,
}

#[derive(Debug, Deserialize)]
pub struct UpdateCore<'a> {
    #[serde(default, borrow)]
    pub name: Option<Cow<'a, str>>,
    #[serde(default, borrow)]
    pub version: Option<Cow<'a, str>>,
}

#[derive(Debug, Deserialize)]
pub struct Plugin<'a> {
    #[serde(default, borrow)]
    pub name: Option<Cow<'a, str>>,
    #[serde(default, borrow)]
    pub title: Option<Cow<'a, str>>,
    #[serde(default, borrow)]
    pub url: Option<Cow<'a, str>>,
    #[serde(default, borrow)]
    pub version: Option<Cow<'a, str>>,
}

#[derive(Debug)]
pub struct PluginEntry<'a> {
    pub key: Cow<'a, str>,
    pub value: Plugin<'a>,
}

#[derive(Debug, Deserialize)]
pub struct GithubEvent<'a> {
    #[serde(default, borrow, rename = "type")]
    pub event_type: Option<Cow<'a, str>>,
    #[serde(default, borrow)]
    pub created_at: Option<Cow<'a, str>>,
    #[serde(default, borrow)]
    pub id: Option<Cow<'a, str>>,
    #[serde(default)]
    pub public: Option<bool>,
    #[serde(default, borrow)]
    pub actor: Option<GithubActor<'a>>,
    #[serde(default, borrow)]
    pub repo: Option<GithubRepo<'a>>,
    #[serde(default, borrow)]
    pub org: Option<GithubActor<'a>>,
    #[serde(default, borrow)]
    pub payload: Option<GithubPayload<'a>>,
}

#[derive(Debug, Deserialize)]
pub struct GithubActor<'a> {
    #[serde(default)]
    pub id: Option<u64>,
    #[serde(default, borrow)]
    pub login: Option<Cow<'a, str>>,
    #[serde(default, borrow)]
    pub url: Option<Cow<'a, str>>,
    #[serde(default, borrow)]
    pub avatar_url: Option<Cow<'a, str>>,
}

#[derive(Debug, Deserialize)]
pub struct GithubRepo<'a> {
    #[serde(default)]
    pub id: Option<u64>,
    #[serde(default, borrow)]
    pub name: Option<Cow<'a, str>>,
    #[serde(default, borrow)]
    pub url: Option<Cow<'a, str>>,
}

#[derive(Debug, Deserialize)]
pub struct GithubPayload<'a> {
    #[serde(default, borrow)]
    pub action: Option<Cow<'a, str>>,
    #[serde(default, borrow, rename = "ref")]
    pub ref_name: Option<Cow<'a, str>>,
    #[serde(default, borrow)]
    pub ref_type: Option<Cow<'a, str>>,
    #[serde(default)]
    pub push_id: Option<u64>,
    #[serde(default)]
    pub size: Option<u64>,
    #[serde(default)]
    pub distinct_size: Option<u64>,
    #[serde(default, borrow)]
    pub head: Option<Cow<'a, str>>,
    #[serde(default, borrow)]
    pub before: Option<Cow<'a, str>>,
    #[serde(default, borrow)]
    pub description: Option<Cow<'a, str>>,
    #[serde(default, borrow)]
    pub master_branch: Option<Cow<'a, str>>,
}

#[derive(Debug)]
pub struct CanadaFeatureCollection<'a> {
    pub collection_type: Option<Cow<'a, str>>,
    pub features: Vec<CanadaFeature<'a>>,
}

#[derive(Debug)]
pub struct CanadaFeature<'a> {
    pub feature_type: Option<Cow<'a, str>>,
    pub properties: Option<CanadaProperties<'a>>,
    pub geometry: Option<CanadaGeometry<'a>>,
}

#[derive(Debug)]
pub struct CanadaProperties<'a> {
    pub name: Option<Cow<'a, str>>,
}

#[derive(Debug)]
pub struct CanadaGeometry<'a> {
    pub geometry_type: Option<Cow<'a, str>>,
    pub coordinates: Vec<Vec<Vec<Cow<'a, str>>>>,
}

#[derive(Debug, Deserialize)]
struct CanadaSerdeFeatureCollection<'a> {
    #[serde(default, borrow, rename = "type")]
    collection_type: Option<Cow<'a, str>>,
    #[serde(default, borrow)]
    features: Vec<CanadaSerdeFeature<'a>>,
}

#[derive(Debug, Deserialize)]
struct CanadaSerdeFeature<'a> {
    #[serde(default, borrow, rename = "type")]
    feature_type: Option<Cow<'a, str>>,
    #[serde(default, borrow)]
    properties: Option<CanadaSerdeProperties<'a>>,
    #[serde(default, borrow)]
    geometry: Option<CanadaSerdeGeometry<'a>>,
}

#[derive(Debug, Deserialize)]
struct CanadaSerdeProperties<'a> {
    #[serde(default, borrow)]
    name: Option<Cow<'a, str>>,
}

#[derive(Debug, Deserialize)]
struct CanadaSerdeGeometry<'a> {
    #[serde(default, borrow, rename = "type")]
    geometry_type: Option<Cow<'a, str>>,
    #[serde(default, borrow)]
    coordinates: Vec<Vec<Vec<&'a RawValue>>>,
}

#[derive(Debug, Deserialize)]
struct CanadaSonicFeatureCollection<'a> {
    #[serde(default, borrow, rename = "type")]
    collection_type: Option<Cow<'a, str>>,
    #[serde(default, borrow)]
    features: Vec<CanadaSonicFeature<'a>>,
}

#[derive(Debug, Deserialize)]
struct CanadaSonicFeature<'a> {
    #[serde(default, borrow, rename = "type")]
    feature_type: Option<Cow<'a, str>>,
    #[serde(default, borrow)]
    properties: Option<CanadaSonicProperties<'a>>,
    #[serde(default, borrow)]
    geometry: Option<CanadaSonicGeometry<'a>>,
}

#[derive(Debug, Deserialize)]
struct CanadaSonicProperties<'a> {
    #[serde(default, borrow)]
    name: Option<Cow<'a, str>>,
}

#[derive(Debug, Deserialize)]
struct CanadaSonicGeometry<'a> {
    #[serde(default, borrow, rename = "type")]
    geometry_type: Option<Cow<'a, str>>,
    #[serde(default)]
    coordinates: Vec<Vec<Vec<sonic_rs::RawNumber>>>,
}

#[derive(Debug, Deserialize)]
pub struct UnicodeBasicRecord<'a> {
    #[serde(default)]
    pub id: Option<u64>,
    #[serde(default, borrow)]
    pub script: Option<Cow<'a, str>>,
    #[serde(default, borrow)]
    pub text: Option<Cow<'a, str>>,
    #[serde(default)]
    pub len: Option<u64>,
    #[serde(default, borrow)]
    pub tags: Vec<Cow<'a, str>>,
}

#[derive(Debug, Deserialize)]
pub struct UnicodeMixedDocument<'a> {
    #[serde(default, borrow)]
    pub metadata: Option<UnicodeMixedMetadata<'a>>,
    #[serde(default, borrow)]
    pub records: Vec<UnicodeMixedRecord<'a>>,
}

#[derive(Debug, Deserialize)]
pub struct UnicodeMixedMetadata<'a> {
    #[serde(default, borrow)]
    pub purpose: Option<Cow<'a, str>>,
    #[serde(default)]
    pub classes: Vec<UnicodeMixedClass>,
    #[serde(default)]
    pub count: Option<u64>,
}

#[derive(Debug, Deserialize)]
pub struct UnicodeMixedRecord<'a> {
    #[serde(default)]
    pub id: Option<u64>,
    #[serde(default, rename = "type")]
    pub class: Option<UnicodeMixedRecordType>,
    #[serde(default, borrow)]
    pub value: Option<DecodedJsonString<'a>>,
    #[serde(default)]
    pub n: Option<u64>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum UnicodeMixedClass {
    Ascii,
    Latin1,
    Cjk,
    Emoji,
    MixedEscapes,
}

impl UnicodeMixedClass {
    fn id(self) -> u64 {
        match self {
            Self::Ascii => 1,
            Self::Latin1 => 2,
            Self::Cjk => 3,
            Self::Emoji => 4,
            Self::MixedEscapes => 5,
        }
    }

    fn from_decoded(value: &str) -> Option<Self> {
        match value {
            "ascii" => Some(Self::Ascii),
            "latin1" => Some(Self::Latin1),
            "cjk" => Some(Self::Cjk),
            "emoji" => Some(Self::Emoji),
            "mixed_escapes" => Some(Self::MixedEscapes),
            _ => None,
        }
    }
}

impl<'de> Deserialize<'de> for UnicodeMixedClass {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct UnicodeMixedClassVisitor;

        impl Visitor<'_> for UnicodeMixedClassVisitor {
            type Value = UnicodeMixedClass;

            fn expecting(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
                formatter.write_str("a unicode_mixed class string")
            }

            fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                UnicodeMixedClass::from_decoded(value).ok_or_else(|| {
                    E::invalid_value(serde::de::Unexpected::Str(value), &"known class")
                })
            }
        }

        deserializer.deserialize_str(UnicodeMixedClassVisitor)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum UnicodeMixedRecordType {
    Ascii,
    Latin1,
    Cjk,
    Emoji,
    Mixed,
}

impl UnicodeMixedRecordType {
    fn id(self) -> u64 {
        match self {
            Self::Ascii => 1,
            Self::Latin1 => 2,
            Self::Cjk => 3,
            Self::Emoji => 4,
            Self::Mixed => 5,
        }
    }

    fn from_decoded(value: &str) -> Option<Self> {
        match value {
            "ascii" => Some(Self::Ascii),
            "latin1" => Some(Self::Latin1),
            "cjk" => Some(Self::Cjk),
            "emoji" => Some(Self::Emoji),
            "mixed" => Some(Self::Mixed),
            _ => None,
        }
    }
}

impl<'de> Deserialize<'de> for UnicodeMixedRecordType {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct UnicodeMixedRecordTypeVisitor;

        impl Visitor<'_> for UnicodeMixedRecordTypeVisitor {
            type Value = UnicodeMixedRecordType;

            fn expecting(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
                formatter.write_str("a unicode_mixed record type string")
            }

            fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                UnicodeMixedRecordType::from_decoded(value).ok_or_else(|| {
                    E::invalid_value(serde::de::Unexpected::Str(value), &"known record type")
                })
            }
        }

        deserializer.deserialize_str(UnicodeMixedRecordTypeVisitor)
    }
}

#[derive(Debug, Clone)]
pub struct DecodedJsonString<'a> {
    repr: DecodedJsonStringRepr<'a>,
    fingerprint: u64,
    len: u64,
}

#[derive(Debug, Clone)]
enum DecodedJsonStringRepr<'a> {
    Decoded(Cow<'a, str>),
    RawEscaped(&'a str),
}

impl<'a> DecodedJsonString<'a> {
    pub fn from_decoded_borrowed(value: &'a str, fingerprint: u64, len: u64) -> Self {
        Self {
            repr: DecodedJsonStringRepr::Decoded(Cow::Borrowed(value)),
            fingerprint,
            len,
        }
    }

    fn from_decoded_owned(value: String) -> Self {
        let (fingerprint, len) = decoded_json_string_fingerprint(&value);
        Self {
            repr: DecodedJsonStringRepr::Decoded(Cow::Owned(value)),
            fingerprint,
            len,
        }
    }

    pub fn from_raw_escaped(raw: &'a str, fingerprint: u64, len: u64) -> Self {
        Self {
            repr: DecodedJsonStringRepr::RawEscaped(raw),
            fingerprint,
            len,
        }
    }

    fn fingerprint(&self) -> u64 {
        self.fingerprint
    }

    fn decoded_len(&self) -> u64 {
        self.len
    }

    pub fn representation_len(&self) -> u64 {
        match &self.repr {
            DecodedJsonStringRepr::Decoded(value) => value.len() as u64,
            DecodedJsonStringRepr::RawEscaped(raw) => raw.len() as u64,
        }
    }

    pub fn is_raw_escaped(&self) -> bool {
        matches!(self.repr, DecodedJsonStringRepr::RawEscaped(_))
    }
}

impl<'de: 'a, 'a> Deserialize<'de> for DecodedJsonString<'a> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct DecodedJsonStringVisitor<'a>(PhantomData<&'a ()>);

        impl<'de: 'a, 'a> Visitor<'de> for DecodedJsonStringVisitor<'a> {
            type Value = DecodedJsonString<'a>;

            fn expecting(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
                formatter.write_str("a JSON string")
            }

            fn visit_borrowed_str<E>(self, value: &'de str) -> Result<Self::Value, E> {
                let (fingerprint, len) = decoded_json_string_fingerprint(value);
                Ok(DecodedJsonString::from_decoded_borrowed(
                    value,
                    fingerprint,
                    len,
                ))
            }

            fn visit_str<E>(self, value: &str) -> Result<Self::Value, E> {
                Ok(DecodedJsonString::from_decoded_owned(value.to_string()))
            }

            fn visit_string<E>(self, value: String) -> Result<Self::Value, E> {
                Ok(DecodedJsonString::from_decoded_owned(value))
            }
        }

        deserializer.deserialize_str(DecodedJsonStringVisitor(PhantomData))
    }
}

#[derive(Debug)]
pub struct DistinctValue<'a> {
    pub timestamp: Option<Cow<'a, str>>,
    pub seq: Option<u64>,
    pub status: Option<Cow<'a, str>>,
    pub dynamic: Vec<DistinctField<'a>>,
}

#[derive(Debug)]
pub struct DistinctField<'a> {
    pub key: Cow<'a, str>,
    pub value: Cow<'a, str>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum YStringUnicodeToken {
    AWithCombiningTilde,
    Quote,
    Plane16Noncharacter,
    Plane1Noncharacter,
    InvisiblePlus,
    BmpNoncharacter,
    EuroAndGclef,
    SamaritanLetter,
    Rocket,
    PartyPopper,
    Family,
}

impl YStringUnicodeToken {
    fn id(self) -> u64 {
        match self {
            Self::AWithCombiningTilde => 1,
            Self::Quote => 2,
            Self::Plane16Noncharacter => 3,
            Self::Plane1Noncharacter => 4,
            Self::InvisiblePlus => 5,
            Self::BmpNoncharacter => 6,
            Self::EuroAndGclef => 7,
            Self::SamaritanLetter => 8,
            Self::Rocket => 9,
            Self::PartyPopper => 10,
            Self::Family => 11,
        }
    }

    fn from_decoded(value: &str) -> Option<Self> {
        match value {
            "\u{00e0}\u{0303}" => Some(Self::AWithCombiningTilde),
            "\"" => Some(Self::Quote),
            "\u{10fffe}" => Some(Self::Plane16Noncharacter),
            "\u{1fffe}" => Some(Self::Plane1Noncharacter),
            "\u{2064}" => Some(Self::InvisiblePlus),
            "\u{fffe}" => Some(Self::BmpNoncharacter),
            "\u{20ac}\u{1d11e}" => Some(Self::EuroAndGclef),
            "\u{0821}" => Some(Self::SamaritanLetter),
            "\u{1f680}" => Some(Self::Rocket),
            "\u{1f389}" => Some(Self::PartyPopper),
            "\u{1f468}\u{200d}\u{1f469}\u{200d}\u{1f467}\u{200d}\u{1f466}" => Some(Self::Family),
            _ => None,
        }
    }
}

impl<'de> Deserialize<'de> for YStringUnicodeToken {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct YStringUnicodeTokenVisitor;

        impl<'de> Visitor<'de> for YStringUnicodeTokenVisitor {
            type Value = YStringUnicodeToken;

            fn expecting(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
                formatter.write_str("a y_string_unicode token")
            }

            fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                YStringUnicodeToken::from_decoded(value)
                    .ok_or_else(|| E::custom("unexpected y_string_unicode token"))
            }

            fn visit_borrowed_str<E>(self, value: &'de str) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                self.visit_str(value)
            }
        }

        deserializer.deserialize_str(YStringUnicodeTokenVisitor)
    }
}

#[derive(Debug, Deserialize)]
pub struct RandomDocument<'a> {
    #[serde(default)]
    pub id: Option<u64>,
    #[serde(default, borrow)]
    pub jsonrpc: Option<Cow<'a, str>>,
    #[serde(default)]
    pub total: Option<u64>,
    #[serde(default, borrow)]
    pub result: Vec<RandomUser<'a>>,
}

#[derive(Debug, Deserialize)]
pub struct RandomUser<'a> {
    #[serde(default)]
    pub id: Option<u64>,
    #[serde(default, borrow)]
    pub avatar: Option<Cow<'a, str>>,
    #[serde(default)]
    pub age: Option<u64>,
    #[serde(default)]
    pub admin: Option<bool>,
    #[serde(default, borrow)]
    pub name: Option<Cow<'a, str>>,
    #[serde(default, borrow)]
    pub company: Option<Cow<'a, str>>,
    #[serde(default, borrow)]
    pub phone: Option<Cow<'a, str>>,
    #[serde(default, borrow)]
    pub email: Option<Cow<'a, str>>,
    #[serde(default, borrow, rename = "birthDate")]
    pub birth_date: Option<Cow<'a, str>>,
    #[serde(default, borrow)]
    pub friends: Vec<RandomFriend<'a>>,
    #[serde(default, borrow)]
    pub field: Option<Cow<'a, str>>,
}

#[derive(Debug, Deserialize)]
pub struct RandomFriend<'a> {
    #[serde(default)]
    pub id: Option<u64>,
    #[serde(default, borrow)]
    pub name: Option<Cow<'a, str>>,
    #[serde(default, borrow)]
    pub phone: Option<Cow<'a, str>>,
}

#[derive(Debug, Deserialize)]
pub struct InstrumentsDocument<'a> {
    #[serde(default, borrow)]
    pub instruments: Vec<Instrument<'a>>,
    #[serde(default, borrow)]
    pub name: Option<Cow<'a, str>>,
    #[serde(default, borrow)]
    pub patterns: Vec<InstrumentPattern<'a>>,
    #[serde(default, borrow)]
    pub samples: Vec<InstrumentSample<'a>>,
    #[serde(default)]
    pub version: Option<u32>,
}

#[derive(Debug, Deserialize)]
pub struct Instrument<'a> {
    #[serde(default)]
    pub default_filter_cutoff: u32,
    #[serde(default)]
    pub default_filter_cutoff_enabled: bool,
    #[serde(default)]
    pub default_filter_mode: u32,
    #[serde(default)]
    pub default_filter_resonance: u32,
    #[serde(default)]
    pub default_filter_resonance_enabled: bool,
    #[serde(default)]
    pub default_pan: u32,
    #[serde(default)]
    pub duplicate_check_type: u32,
    #[serde(default)]
    pub duplicate_note_action: u32,
    #[serde(default)]
    pub fadeout: u32,
    #[serde(default)]
    pub global_volume: u32,
    #[serde(default)]
    pub graph_insert: u32,
    #[serde(default, borrow)]
    pub legacy_filename: Option<Cow<'a, str>>,
    #[serde(default)]
    pub midi_bank: u32,
    #[serde(default)]
    pub midi_channel: u32,
    #[serde(default)]
    pub midi_drum_set: u32,
    #[serde(default)]
    pub midi_program: u32,
    #[serde(default, borrow)]
    pub name: Option<Cow<'a, str>>,
    #[serde(default)]
    pub new_note_action: u32,
    #[serde(default)]
    pub panning_envelope: Option<InstrumentEnvelope>,
    #[serde(default)]
    pub pitch_envelope: Option<InstrumentEnvelope>,
    #[serde(default)]
    pub pitch_pan_center: u32,
    #[serde(default)]
    pub pitch_pan_separation: u32,
    #[serde(default)]
    pub pitch_to_tempo_lock: u32,
    #[serde(default)]
    pub random_cutoff_weight: u32,
    #[serde(default)]
    pub random_pan_weight: u32,
    #[serde(default)]
    pub random_resonance_weight: u32,
    #[serde(default)]
    pub random_volume_weight: u32,
    #[serde(default)]
    pub volume_envelope: Option<InstrumentEnvelope>,
    #[serde(default)]
    pub volume_ramp_down: u32,
    #[serde(default)]
    pub volume_ramp_up: u32,
}

#[derive(Debug, Deserialize)]
pub struct InstrumentEnvelope {
    #[serde(default)]
    pub loop_end: u32,
    #[serde(default)]
    pub loop_start: u32,
    #[serde(default)]
    pub nodes: Vec<InstrumentEnvelopeNode>,
    #[serde(default)]
    pub release_node: u32,
    #[serde(default)]
    pub sustain_end: u32,
    #[serde(default)]
    pub sustain_start: u32,
}

#[derive(Debug, Deserialize)]
pub struct InstrumentEnvelopeNode {
    #[serde(default)]
    pub tick: u32,
    #[serde(default)]
    pub value: u32,
}

#[derive(Debug, Deserialize)]
pub struct InstrumentPattern<'a> {
    #[serde(default)]
    pub data: Option<Vec<InstrumentPatternEvent>>,
    #[serde(default, borrow)]
    pub name: Option<Cow<'a, str>>,
    #[serde(default)]
    pub rows: u32,
    #[serde(default)]
    pub rows_per_beat: u32,
    #[serde(default)]
    pub rows_per_measure: u32,
}

#[derive(Debug, Deserialize)]
pub struct InstrumentPatternEvent {
    #[serde(default)]
    pub channel: u32,
    #[serde(default)]
    pub fxcmd: u32,
    #[serde(default)]
    pub fxparam: u32,
    #[serde(default)]
    pub instr: u32,
    #[serde(default)]
    pub note: u32,
    #[serde(default)]
    pub row: u32,
    #[serde(default)]
    pub volcmd: u32,
    #[serde(default)]
    pub volval: u32,
}

#[derive(Debug, Deserialize)]
pub struct InstrumentSample<'a> {
    #[serde(default)]
    pub c5_samplerate: u32,
    #[serde(default)]
    pub global_volume: u32,
    #[serde(default, borrow)]
    pub legacy_filename: Option<Cow<'a, str>>,
    #[serde(default)]
    pub length: u32,
    #[serde(default)]
    pub loop_end: u32,
    #[serde(default)]
    pub loop_start: u32,
    #[serde(default, borrow)]
    pub name: Option<Cow<'a, str>>,
    #[serde(default)]
    pub pan: u32,
    #[serde(default)]
    pub sustain_end: u32,
    #[serde(default)]
    pub sustain_start: u32,
    #[serde(default)]
    pub vibrato_depth: u32,
    #[serde(default)]
    pub vibrato_rate: u32,
    #[serde(default)]
    pub vibrato_sweep: u32,
    #[serde(default)]
    pub vibrato_type: u32,
    #[serde(default)]
    pub volume: u32,
}

#[derive(Debug, Deserialize)]
pub struct Mesh {
    #[serde(default)]
    pub batches: Vec<MeshBatch>,
    #[serde(default)]
    pub positions: Vec<f64>,
    #[serde(default)]
    pub tex0: Vec<f64>,
    #[serde(default)]
    pub colors: Vec<u32>,
    #[serde(default)]
    pub influences: Vec<Vec<f64>>,
    #[serde(default)]
    pub normals: Vec<f64>,
    #[serde(default)]
    pub indices: Vec<u32>,
}

#[derive(Debug, Deserialize)]
pub struct MeshBatch {
    #[serde(default, rename = "indexRange")]
    pub index_range: Vec<u32>,
    #[serde(default, rename = "vertexRange")]
    pub vertex_range: Vec<u32>,
    #[serde(default, rename = "usedBones")]
    pub used_bones: Vec<u32>,
}

#[derive(Debug, Deserialize)]
pub struct MarineIk {
    #[serde(default)]
    pub geometries: Vec<MarineGeometry>,
}

#[derive(Debug, Deserialize)]
pub struct MarineGeometry {
    #[serde(default)]
    pub data: Option<MarineGeometryData>,
}

#[derive(Debug, Deserialize)]
pub struct MarineGeometryData {
    #[serde(default)]
    pub uvs: Vec<Vec<f64>>,
    #[serde(default)]
    pub vertices: Vec<f64>,
    #[serde(default, rename = "skinWeights")]
    pub skin_weights: Vec<f64>,
    #[serde(default, rename = "skinIndices")]
    pub skin_indices: Vec<u32>,
    #[serde(default)]
    pub normals: Vec<f64>,
    #[serde(default)]
    pub faces: Vec<u32>,
}

#[derive(Debug, Deserialize)]
pub struct W5ArrayEvent<'a> {
    #[serde(default)]
    pub id: Option<u64>,
    #[serde(default, borrow)]
    pub actor: Option<Cow<'a, str>>,
    #[serde(default)]
    pub public: Option<bool>,
}

#[derive(Debug, Deserialize)]
pub struct W5MapMetric<'a> {
    #[serde(default)]
    pub count: Option<u64>,
    #[serde(default, borrow)]
    pub label: Option<Cow<'a, str>>,
}

#[derive(Debug)]
pub struct W5MapMetricEntry<'a> {
    pub key: Cow<'a, str>,
    pub value: W5MapMetric<'a>,
}

#[cfg(test)]
#[derive(Debug)]
struct W5MapMetricEntries<'a>(Vec<W5MapMetricEntry<'a>>);

#[cfg(test)]
impl<'de> Deserialize<'de> for W5MapMetricEntries<'de> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct W5MapMetricEntriesVisitor;

        impl<'de> Visitor<'de> for W5MapMetricEntriesVisitor {
            type Value = W5MapMetricEntries<'de>;

            fn expecting(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
                formatter.write_str("a W5 map-entry root object")
            }

            fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
            where
                A: MapAccess<'de>,
            {
                let mut entries = Vec::with_capacity(map.size_hint().unwrap_or(0));
                while let Some((key, value)) =
                    map.next_entry::<Cow<'de, str>, W5MapMetric<'de>>()?
                {
                    entries.push(W5MapMetricEntry { key, value });
                }
                Ok(W5MapMetricEntries(entries))
            }
        }

        deserializer.deserialize_map(W5MapMetricEntriesVisitor)
    }
}

pub enum RealTypedOutput<'a> {
    Twitter(TwitterSearch<'a>),
    ApacheBuilds(ApacheBuilds<'a>),
    CitmCatalog(CitmCatalog<'a>),
    Gsoc2018(Vec<GsocProposalEntry<'a>>),
    GithubEvents(Vec<GithubEvent<'a>>),
    UpdateCenter(UpdateCenter<'a>),
    Mesh(Mesh),
    MarineIk(MarineIk),
    Instruments(InstrumentsDocument<'a>),
    Canada(CanadaFeatureCollection<'a>),
    Numbers(Vec<f64>),
    UnicodeBasic(Vec<UnicodeBasicRecord<'a>>),
    UnicodeMixed(UnicodeMixedDocument<'a>),
    DistinctValues(Vec<DistinctValue<'a>>),
    YStringUnicode(Vec<YStringUnicodeToken>),
    Random(RandomDocument<'a>),
}

pub fn fixture_for_name(name: &str) -> Option<RealTypedFixture> {
    match name {
        "twitter" => Some(RealTypedFixture::Twitter),
        "apache_builds" | "apache-builds" => Some(RealTypedFixture::ApacheBuilds),
        "citm_catalog" | "citm-catalog" => Some(RealTypedFixture::CitmCatalog),
        "gsoc-2018" | "gsoc_2018" => Some(RealTypedFixture::Gsoc2018),
        "github_events" | "github-events" => Some(RealTypedFixture::GithubEvents),
        "update_center" | "update-center" => Some(RealTypedFixture::UpdateCenter),
        "mesh" => Some(RealTypedFixture::Mesh),
        "marine_ik" | "marine-ik" => Some(RealTypedFixture::MarineIk),
        "instruments" => Some(RealTypedFixture::Instruments),
        "canada" => Some(RealTypedFixture::Canada),
        "numbers" => Some(RealTypedFixture::Numbers),
        "unicode_basic" | "unicode-basic" => Some(RealTypedFixture::UnicodeBasic),
        "unicode_mixed" | "unicode-mixed" => Some(RealTypedFixture::UnicodeMixed),
        "distinct_values" | "distinct-values" => Some(RealTypedFixture::DistinctValues),
        "y_string_unicode" | "y-string-unicode" => Some(RealTypedFixture::YStringUnicode),
        "random" => Some(RealTypedFixture::Random),
        _ => None,
    }
}

pub fn locate_fixture(name: &str) -> PathBuf {
    let manifest = std::env::var("CARGO_MANIFEST_DIR")
        .map(PathBuf::from)
        .unwrap_or_else(|_| std::env::current_dir().unwrap());
    for dir in manifest.ancestors() {
        for file_name in candidate_names(name) {
            let candidate = dir
                .join("crates/test-fixtures/corpus/json")
                .join(format!("{file_name}.json"));
            if candidate.exists() {
                return candidate;
            }
            let candidate = dir.join("test_data").join(format!("{file_name}.json"));
            if candidate.exists() {
                return candidate;
            }
        }
    }
    panic!("could not locate real typed fixture {name}.json");
}

fn candidate_names(name: &str) -> [&str; 2] {
    match name {
        "update_center" => ["update-center", "update_center"],
        "update-center" => ["update-center", "update_center"],
        "marine_ik" => ["marine_ik", "marine-ik"],
        "marine-ik" => ["marine_ik", "marine-ik"],
        "gsoc_2018" => ["gsoc-2018", "gsoc_2018"],
        "gsoc-2018" => ["gsoc-2018", "gsoc_2018"],
        "unicode_mixed" => ["unicode_mixed", "unicode-mixed"],
        "unicode-mixed" => ["unicode_mixed", "unicode-mixed"],
        "y_string_unicode" => ["y_string_unicode", "y-string-unicode"],
        "y-string-unicode" => ["y_string_unicode", "y-string-unicode"],
        _ => [name, name],
    }
}

fn canada_from_serde<'a>(value: CanadaSerdeFeatureCollection<'a>) -> CanadaFeatureCollection<'a> {
    CanadaFeatureCollection {
        collection_type: value.collection_type,
        features: value
            .features
            .into_iter()
            .map(|feature| CanadaFeature {
                feature_type: feature.feature_type,
                properties: feature.properties.map(|properties| CanadaProperties {
                    name: properties.name,
                }),
                geometry: feature.geometry.map(|geometry| CanadaGeometry {
                    geometry_type: geometry.geometry_type,
                    coordinates: geometry
                        .coordinates
                        .into_iter()
                        .map(|ring| {
                            ring.into_iter()
                                .map(|point| {
                                    point
                                        .into_iter()
                                        .map(|number| Cow::Borrowed(number.get()))
                                        .collect()
                                })
                                .collect()
                        })
                        .collect(),
                }),
            })
            .collect(),
    }
}

fn canada_from_sonic<'a>(value: CanadaSonicFeatureCollection<'a>) -> CanadaFeatureCollection<'a> {
    CanadaFeatureCollection {
        collection_type: value.collection_type,
        features: value
            .features
            .into_iter()
            .map(|feature| CanadaFeature {
                feature_type: feature.feature_type,
                properties: feature.properties.map(|properties| CanadaProperties {
                    name: properties.name,
                }),
                geometry: feature.geometry.map(|geometry| CanadaGeometry {
                    geometry_type: geometry.geometry_type,
                    coordinates: geometry
                        .coordinates
                        .into_iter()
                        .map(|ring| {
                            ring.into_iter()
                                .map(|point| {
                                    point
                                        .into_iter()
                                        .map(|number| Cow::Owned(number.as_str().to_string()))
                                        .collect()
                                })
                                .collect()
                        })
                        .collect(),
                }),
            })
            .collect(),
    }
}

pub fn track1_typed<'a>(
    fixture: RealTypedFixture,
    input: &'a str,
) -> Result<RealTypedOutput<'a>, DirectStructError> {
    match fixture {
        RealTypedFixture::Twitter => crate::generated_real_typed::parse_twitter_search(input)
            .map(RealTypedOutput::Twitter)
            .map_err(|error| DirectStructError::Parse(error.to_string())),
        RealTypedFixture::ApacheBuilds => crate::generated_real_typed::parse_apache_builds(input)
            .map(RealTypedOutput::ApacheBuilds)
            .map_err(|error| DirectStructError::Parse(error.to_string())),
        RealTypedFixture::CitmCatalog => crate::generated_real_typed::parse_citm_catalog(input)
            .map(RealTypedOutput::CitmCatalog)
            .map_err(|error| DirectStructError::Parse(error.to_string())),
        RealTypedFixture::Gsoc2018 => crate::generated_real_typed::parse_gsoc_2018(input)
            .map(RealTypedOutput::Gsoc2018)
            .map_err(|error| DirectStructError::Parse(error.to_string())),
        RealTypedFixture::GithubEvents => crate::generated_real_typed::parse_github_events(input)
            .map(RealTypedOutput::GithubEvents)
            .map_err(|error| DirectStructError::Parse(error.to_string())),
        RealTypedFixture::UpdateCenter => crate::generated_real_typed::parse_update_center(input)
            .map(RealTypedOutput::UpdateCenter)
            .map_err(|error| DirectStructError::Parse(error.to_string())),
        RealTypedFixture::Mesh => crate::generated_real_typed::parse_mesh(input)
            .map(RealTypedOutput::Mesh)
            .map_err(|error| DirectStructError::Parse(error.to_string())),
        RealTypedFixture::MarineIk => crate::generated_real_typed::parse_marine_ik(input)
            .map(RealTypedOutput::MarineIk)
            .map_err(|error| DirectStructError::Parse(error.to_string())),
        RealTypedFixture::Instruments => crate::generated_real_typed::parse_instruments(input)
            .map(RealTypedOutput::Instruments)
            .map_err(|error| DirectStructError::Parse(error.to_string())),
        RealTypedFixture::Canada => crate::generated_real_typed::parse_canada(input)
            .map(RealTypedOutput::Canada)
            .map_err(|error| DirectStructError::Parse(error.to_string())),
        RealTypedFixture::Numbers => crate::generated_real_typed::parse_numbers(input)
            .map(RealTypedOutput::Numbers)
            .map_err(|error| DirectStructError::Parse(error.to_string())),
        RealTypedFixture::UnicodeBasic => crate::generated_real_typed::parse_unicode_basic(input)
            .map(RealTypedOutput::UnicodeBasic)
            .map_err(|error| DirectStructError::Parse(error.to_string())),
        RealTypedFixture::UnicodeMixed => crate::generated_real_typed::parse_unicode_mixed(input)
            .map(RealTypedOutput::UnicodeMixed)
            .map_err(|error| DirectStructError::Parse(error.to_string())),
        RealTypedFixture::DistinctValues => {
            crate::generated_real_typed::parse_distinct_values(input)
                .map(RealTypedOutput::DistinctValues)
                .map_err(|error| DirectStructError::Parse(error.to_string()))
        }
        RealTypedFixture::YStringUnicode => {
            crate::generated_real_typed::parse_y_string_unicode(input)
                .map(RealTypedOutput::YStringUnicode)
                .map_err(|error| DirectStructError::Parse(error.to_string()))
        }
        RealTypedFixture::Random => crate::generated_real_typed::parse_random(input)
            .map(RealTypedOutput::Random)
            .map_err(|error| DirectStructError::Parse(error.to_string())),
    }
}

pub fn track2_typed<'a>(
    fixture: RealTypedFixture,
    input: &'a str,
) -> Result<RealTypedOutput<'a>, DirectStructError> {
    serde_typed(fixture, input.as_bytes())
}

pub fn serde_typed<'a>(
    fixture: RealTypedFixture,
    bytes: &'a [u8],
) -> Result<RealTypedOutput<'a>, DirectStructError> {
    match fixture {
        RealTypedFixture::Twitter => serde_json::from_slice::<TwitterSearch<'a>>(bytes)
            .map(RealTypedOutput::Twitter)
            .map_err(|error| DirectStructError::Serde(error.to_string())),
        RealTypedFixture::ApacheBuilds => serde_json::from_slice::<ApacheBuilds<'a>>(bytes)
            .map(RealTypedOutput::ApacheBuilds)
            .map_err(|error| DirectStructError::Serde(error.to_string())),
        RealTypedFixture::CitmCatalog => serde_json::from_slice::<CitmCatalog<'a>>(bytes)
            .map(RealTypedOutput::CitmCatalog)
            .map_err(|error| DirectStructError::Serde(error.to_string())),
        RealTypedFixture::Gsoc2018 => serde_json::from_slice::<GsocProposalEntries<'a>>(bytes)
            .map(|entries| entries.0)
            .map(RealTypedOutput::Gsoc2018)
            .map_err(|error| DirectStructError::Serde(error.to_string())),
        RealTypedFixture::GithubEvents => serde_json::from_slice::<Vec<GithubEvent<'a>>>(bytes)
            .map(RealTypedOutput::GithubEvents)
            .map_err(|error| DirectStructError::Serde(error.to_string())),
        RealTypedFixture::UpdateCenter => serde_json::from_slice::<UpdateCenter<'a>>(bytes)
            .map(RealTypedOutput::UpdateCenter)
            .map_err(|error| DirectStructError::Serde(error.to_string())),
        RealTypedFixture::Mesh => serde_json::from_slice::<Mesh>(bytes)
            .map(RealTypedOutput::Mesh)
            .map_err(|error| DirectStructError::Serde(error.to_string())),
        RealTypedFixture::MarineIk => serde_json::from_slice::<MarineIk>(bytes)
            .map(RealTypedOutput::MarineIk)
            .map_err(|error| DirectStructError::Serde(error.to_string())),
        RealTypedFixture::Instruments => serde_json::from_slice::<InstrumentsDocument<'a>>(bytes)
            .map(RealTypedOutput::Instruments)
            .map_err(|error| DirectStructError::Serde(error.to_string())),
        RealTypedFixture::Canada => {
            serde_json::from_slice::<CanadaSerdeFeatureCollection<'a>>(bytes)
                .map(canada_from_serde)
                .map(RealTypedOutput::Canada)
                .map_err(|error| DirectStructError::Serde(error.to_string()))
        }
        RealTypedFixture::Numbers => serde_json::from_slice::<Vec<f64>>(bytes)
            .map(RealTypedOutput::Numbers)
            .map_err(|error| DirectStructError::Serde(error.to_string())),
        RealTypedFixture::UnicodeBasic => {
            serde_json::from_slice::<Vec<UnicodeBasicRecord<'a>>>(bytes)
                .map(RealTypedOutput::UnicodeBasic)
                .map_err(|error| DirectStructError::Serde(error.to_string()))
        }
        RealTypedFixture::UnicodeMixed => serde_json::from_slice::<UnicodeMixedDocument<'a>>(bytes)
            .map(RealTypedOutput::UnicodeMixed)
            .map_err(|error| DirectStructError::Serde(error.to_string())),
        RealTypedFixture::DistinctValues => serde_json::from_slice::<Vec<DistinctValue<'a>>>(bytes)
            .map(RealTypedOutput::DistinctValues)
            .map_err(|error| DirectStructError::Serde(error.to_string())),
        RealTypedFixture::YStringUnicode => {
            serde_json::from_slice::<Vec<YStringUnicodeToken>>(bytes)
                .map(RealTypedOutput::YStringUnicode)
                .map_err(|error| DirectStructError::Serde(error.to_string()))
        }
        RealTypedFixture::Random => serde_json::from_slice::<RandomDocument<'a>>(bytes)
            .map(RealTypedOutput::Random)
            .map_err(|error| DirectStructError::Serde(error.to_string())),
    }
}

pub fn sonic_typed<'a>(
    fixture: RealTypedFixture,
    bytes: &'a [u8],
) -> Result<RealTypedOutput<'a>, DirectStructError> {
    match fixture {
        RealTypedFixture::Twitter => sonic_rs::from_slice::<TwitterSearch<'a>>(bytes)
            .map(RealTypedOutput::Twitter)
            .map_err(|error| DirectStructError::Sonic(error.to_string())),
        RealTypedFixture::ApacheBuilds => sonic_rs::from_slice::<ApacheBuilds<'a>>(bytes)
            .map(RealTypedOutput::ApacheBuilds)
            .map_err(|error| DirectStructError::Sonic(error.to_string())),
        RealTypedFixture::CitmCatalog => sonic_rs::from_slice::<CitmCatalog<'a>>(bytes)
            .map(RealTypedOutput::CitmCatalog)
            .map_err(|error| DirectStructError::Sonic(error.to_string())),
        RealTypedFixture::Gsoc2018 => sonic_rs::from_slice::<GsocProposalEntries<'a>>(bytes)
            .map(|entries| entries.0)
            .map(RealTypedOutput::Gsoc2018)
            .map_err(|error| DirectStructError::Sonic(error.to_string())),
        RealTypedFixture::GithubEvents => sonic_rs::from_slice::<Vec<GithubEvent<'a>>>(bytes)
            .map(RealTypedOutput::GithubEvents)
            .map_err(|error| DirectStructError::Sonic(error.to_string())),
        RealTypedFixture::UpdateCenter => sonic_rs::from_slice::<UpdateCenter<'a>>(bytes)
            .map(RealTypedOutput::UpdateCenter)
            .map_err(|error| DirectStructError::Sonic(error.to_string())),
        RealTypedFixture::Mesh => sonic_rs::from_slice::<Mesh>(bytes)
            .map(RealTypedOutput::Mesh)
            .map_err(|error| DirectStructError::Sonic(error.to_string())),
        RealTypedFixture::MarineIk => sonic_rs::from_slice::<MarineIk>(bytes)
            .map(RealTypedOutput::MarineIk)
            .map_err(|error| DirectStructError::Sonic(error.to_string())),
        RealTypedFixture::Instruments => sonic_rs::from_slice::<InstrumentsDocument<'a>>(bytes)
            .map(RealTypedOutput::Instruments)
            .map_err(|error| DirectStructError::Sonic(error.to_string())),
        RealTypedFixture::Canada => sonic_rs::from_slice::<CanadaSonicFeatureCollection<'a>>(bytes)
            .map(canada_from_sonic)
            .map(RealTypedOutput::Canada)
            .map_err(|error| DirectStructError::Sonic(error.to_string())),
        RealTypedFixture::Numbers => sonic_rs::from_slice::<Vec<f64>>(bytes)
            .map(RealTypedOutput::Numbers)
            .map_err(|error| DirectStructError::Sonic(error.to_string())),
        RealTypedFixture::UnicodeBasic => {
            sonic_rs::from_slice::<Vec<UnicodeBasicRecord<'a>>>(bytes)
                .map(RealTypedOutput::UnicodeBasic)
                .map_err(|error| DirectStructError::Sonic(error.to_string()))
        }
        RealTypedFixture::UnicodeMixed => sonic_rs::from_slice::<UnicodeMixedDocument<'a>>(bytes)
            .map(RealTypedOutput::UnicodeMixed)
            .map_err(|error| DirectStructError::Sonic(error.to_string())),
        RealTypedFixture::DistinctValues => sonic_rs::from_slice::<Vec<DistinctValue<'a>>>(bytes)
            .map(RealTypedOutput::DistinctValues)
            .map_err(|error| DirectStructError::Sonic(error.to_string())),
        RealTypedFixture::YStringUnicode => sonic_rs::from_slice::<Vec<YStringUnicodeToken>>(bytes)
            .map(RealTypedOutput::YStringUnicode)
            .map_err(|error| DirectStructError::Sonic(error.to_string())),
        RealTypedFixture::Random => sonic_rs::from_slice::<RandomDocument<'a>>(bytes)
            .map(RealTypedOutput::Random)
            .map_err(|error| DirectStructError::Sonic(error.to_string())),
    }
}

pub fn assert_real_typed_parity(input: &str, bytes: &[u8], fixture: RealTypedFixture) {
    let track1 = track1_typed(fixture, input).expect("track1 real typed");
    let track2 = track2_typed(fixture, input).expect("track2 real typed");
    let serde = serde_typed(fixture, bytes).expect("serde real typed");
    let sonic = sonic_typed(fixture, bytes).expect("sonic real typed");
    let checksums = [
        typed_checksum(&track1),
        typed_checksum(&track2),
        typed_checksum(&serde),
        typed_checksum(&sonic),
    ];
    assert_eq!(checksums[0], checksums[1], "track1/track2 mismatch");
    assert_eq!(checksums[0], checksums[2], "track1/serde mismatch");
    assert_eq!(checksums[0], checksums[3], "track1/sonic mismatch");
}

pub fn typed_checksum(output: &RealTypedOutput<'_>) -> u64 {
    match output {
        RealTypedOutput::Twitter(value) => checksum_twitter(value),
        RealTypedOutput::ApacheBuilds(value) => checksum_apache_builds(value),
        RealTypedOutput::CitmCatalog(value) => checksum_citm_catalog(value),
        RealTypedOutput::Gsoc2018(value) => checksum_gsoc_2018(value),
        RealTypedOutput::GithubEvents(value) => checksum_github_events(value),
        RealTypedOutput::UpdateCenter(value) => checksum_update_center(value),
        RealTypedOutput::Mesh(value) => checksum_mesh(value),
        RealTypedOutput::MarineIk(value) => checksum_marine_ik(value),
        RealTypedOutput::Instruments(value) => checksum_instruments(value),
        RealTypedOutput::Canada(value) => checksum_canada(value),
        RealTypedOutput::Numbers(value) => checksum_numbers(value),
        RealTypedOutput::UnicodeBasic(value) => checksum_unicode_basic(value),
        RealTypedOutput::UnicodeMixed(value) => checksum_unicode_mixed(value),
        RealTypedOutput::DistinctValues(value) => checksum_distinct_values(value),
        RealTypedOutput::YStringUnicode(value) => checksum_y_string_unicode(value),
        RealTypedOutput::Random(value) => checksum_random(value),
    }
}

fn checksum_twitter(value: &TwitterSearch<'_>) -> u64 {
    let mut hash = mix(0x74776974746572, value.statuses.len() as u64);
    for status in &value.statuses {
        hash = mix(hash, checksum_tweet(status));
    }
    hash
}

fn checksum_tweet(value: &Tweet<'_>) -> u64 {
    let mut hash = 0x7477656574;
    hash = fold_opt_u64(hash, value.id);
    fold_opt_str(hash, &value.text)
}

fn checksum_apache_builds(value: &ApacheBuilds<'_>) -> u64 {
    let mut hash = 0x617061636865;
    hash = fold_opt_str(hash, &value.mode);
    hash = fold_opt_str(hash, &value.node_name);
    hash = mix(hash, value.jobs.len() as u64);
    for job in &value.jobs {
        hash = mix(hash, checksum_apache_job(job));
    }
    hash
}

fn checksum_apache_job(value: &ApacheJob<'_>) -> u64 {
    let mut hash = 0x6170616368656a6f;
    hash = fold_opt_str(hash, &value.name);
    hash = fold_opt_str(hash, &value.url);
    fold_opt_str(hash, &value.color)
}

fn checksum_citm_catalog(value: &CitmCatalog<'_>) -> u64 {
    let mut hash = mix(0x6369746d, value.events.len() as u64);
    for event in &value.events {
        hash = mix(hash, hash_str(event.key.as_ref()));
        hash = mix(hash, checksum_citm_event(&event.value));
    }
    hash
}

fn checksum_citm_event(value: &CitmEvent<'_>) -> u64 {
    let mut hash = 0x6369746d657665;
    hash = fold_opt_u64(hash, value.id);
    hash = fold_opt_str(hash, &value.name);
    hash = fold_u64_slice(hash, &value.sub_topic_ids);
    fold_u64_slice(hash, &value.topic_ids)
}

fn checksum_gsoc_2018(values: &[GsocProposalEntry<'_>]) -> u64 {
    let mut hash = mix(0x67736f6332303138, values.len() as u64);
    for entry in values {
        hash = mix(hash, entry.key as u64);
        hash = mix(hash, checksum_gsoc_proposal(&entry.value));
    }
    hash
}

fn checksum_gsoc_proposal(value: &GsocProposal<'_>) -> u64 {
    let mut hash = 0x67736f6370726f70;
    hash = mix(hash, value.context.id());
    hash = mix(hash, value.proposal_type.id());
    hash = fold_decoded_json_string(hash, &value.name);
    hash = fold_decoded_json_string(hash, &value.description);
    hash = mix(hash, checksum_gsoc_sponsor(&value.sponsor));
    mix(hash, checksum_gsoc_author(&value.author))
}

fn checksum_gsoc_sponsor(value: &GsocSponsor<'_>) -> u64 {
    let mut hash = 0x67736f6373706f6e;
    hash = mix(hash, value.sponsor_type.id());
    hash = fold_decoded_json_string(hash, &value.name);
    hash = fold_decoded_json_string(hash, &value.disambiguating_description);
    hash = fold_decoded_json_string(hash, &value.description);
    hash = fold_decoded_json_string(hash, &value.url);
    fold_decoded_json_string(hash, &value.logo)
}

fn checksum_gsoc_author(value: &GsocAuthor<'_>) -> u64 {
    let mut hash = 0x67736f6361757468;
    hash = mix(hash, value.author_type.id());
    fold_decoded_json_string(hash, &value.name)
}

fn checksum_update_center(value: &UpdateCenter<'_>) -> u64 {
    let mut hash = 0x757064617465;
    hash = fold_opt_str(hash, &value.connection_check_url);
    hash = fold_opt_str(hash, &value.id);
    hash = fold_opt_str(hash, &value.update_center_version);
    if let Some(core) = &value.core {
        hash = mix(hash, checksum_core(core));
    }
    hash = mix(hash, value.plugins.len() as u64);
    for plugin in &value.plugins {
        hash = mix(hash, hash_str(plugin.key.as_ref()));
        hash = mix(hash, checksum_plugin(&plugin.value));
    }
    hash
}

fn checksum_core(value: &UpdateCore<'_>) -> u64 {
    let mut hash = 0x636f7265;
    hash = fold_opt_str(hash, &value.name);
    fold_opt_str(hash, &value.version)
}

fn checksum_plugin(value: &Plugin<'_>) -> u64 {
    let mut hash = 0x706c7567696e;
    hash = fold_opt_str(hash, &value.name);
    hash = fold_opt_str(hash, &value.title);
    hash = fold_opt_str(hash, &value.url);
    fold_opt_str(hash, &value.version)
}

fn checksum_github_events(values: &[GithubEvent<'_>]) -> u64 {
    let mut hash = mix(0x676974687562, values.len() as u64);
    for value in values {
        hash = mix(hash, checksum_github_event(value));
    }
    hash
}

fn checksum_github_event(value: &GithubEvent<'_>) -> u64 {
    let mut hash = 0x67686576656e74;
    hash = fold_opt_str(hash, &value.event_type);
    hash = fold_opt_str(hash, &value.created_at);
    hash = fold_opt_str(hash, &value.id);
    hash = fold_opt_bool(hash, value.public);
    hash = match &value.actor {
        Some(actor) => mix(hash, checksum_github_actor(actor)),
        None => mix(hash, 0),
    };
    hash = match &value.repo {
        Some(repo) => mix(hash, checksum_github_repo(repo)),
        None => mix(hash, 0),
    };
    hash = match &value.org {
        Some(org) => mix(hash, checksum_github_actor(org)),
        None => mix(hash, 0),
    };
    match &value.payload {
        Some(payload) => mix(hash, checksum_github_payload(payload)),
        None => mix(hash, 0),
    }
}

fn checksum_github_actor(value: &GithubActor<'_>) -> u64 {
    let mut hash = 0x67686163746f72;
    hash = fold_opt_u64(hash, value.id);
    hash = fold_opt_str(hash, &value.login);
    hash = fold_opt_str(hash, &value.url);
    fold_opt_str(hash, &value.avatar_url)
}

fn checksum_github_repo(value: &GithubRepo<'_>) -> u64 {
    let mut hash = 0x67687265706f;
    hash = fold_opt_u64(hash, value.id);
    hash = fold_opt_str(hash, &value.name);
    fold_opt_str(hash, &value.url)
}

fn checksum_github_payload(value: &GithubPayload<'_>) -> u64 {
    let mut hash = 0x67687061796c;
    hash = fold_opt_str(hash, &value.action);
    hash = fold_opt_str(hash, &value.ref_name);
    hash = fold_opt_str(hash, &value.ref_type);
    hash = fold_opt_u64(hash, value.push_id);
    hash = fold_opt_u64(hash, value.size);
    hash = fold_opt_u64(hash, value.distinct_size);
    hash = fold_opt_str(hash, &value.head);
    hash = fold_opt_str(hash, &value.before);
    hash = fold_opt_str(hash, &value.description);
    fold_opt_str(hash, &value.master_branch)
}

fn checksum_canada(value: &CanadaFeatureCollection<'_>) -> u64 {
    let mut hash = 0x63616e616461;
    hash = fold_opt_str(hash, &value.collection_type);
    hash = mix(hash, value.features.len() as u64);
    for feature in &value.features {
        hash = mix(hash, checksum_canada_feature(feature));
    }
    hash
}

fn checksum_canada_feature(value: &CanadaFeature<'_>) -> u64 {
    let mut hash = 0x636166656174;
    hash = fold_opt_str(hash, &value.feature_type);
    hash = value.properties.as_ref().map_or_else(
        || mix(hash, 0),
        |value| mix(hash, checksum_canada_properties(value)),
    );
    value.geometry.as_ref().map_or_else(
        || mix(hash, 0),
        |value| mix(hash, checksum_canada_geometry(value)),
    )
}

fn checksum_canada_properties(value: &CanadaProperties<'_>) -> u64 {
    fold_opt_str(0x636170726f70, &value.name)
}

fn checksum_canada_geometry(value: &CanadaGeometry<'_>) -> u64 {
    let mut hash = 0x636167656f6d;
    hash = fold_opt_str(hash, &value.geometry_type);
    fold_three_deep_str_slice(hash, &value.coordinates)
}

fn checksum_mesh(value: &Mesh) -> u64 {
    let mut hash = mix(0x6d657368, value.batches.len() as u64);
    for batch in &value.batches {
        hash = mix(hash, checksum_mesh_batch(batch));
    }
    hash = fold_f64_slice(hash, &value.positions);
    hash = fold_f64_slice(hash, &value.tex0);
    hash = fold_u32_slice(hash, &value.colors);
    hash = fold_nested_f64_slice(hash, &value.influences);
    hash = fold_f64_slice(hash, &value.normals);
    fold_u32_slice(hash, &value.indices)
}

fn checksum_mesh_batch(value: &MeshBatch) -> u64 {
    let mut hash = 0x6d65736862617463;
    hash = fold_u32_slice(hash, &value.index_range);
    hash = fold_u32_slice(hash, &value.vertex_range);
    fold_u32_slice(hash, &value.used_bones)
}

fn checksum_marine_ik(value: &MarineIk) -> u64 {
    let mut hash = mix(0x6d6172696e65, value.geometries.len() as u64);
    for geometry in &value.geometries {
        hash = mix(hash, checksum_marine_geometry(geometry));
    }
    hash
}

fn checksum_numbers(values: &[f64]) -> u64 {
    fold_f64_slice(0x6e756d62657273, values)
}

fn checksum_unicode_basic(values: &[UnicodeBasicRecord<'_>]) -> u64 {
    let mut hash = mix(0x756e69636f6465, values.len() as u64);
    for value in values {
        hash = mix(hash, checksum_unicode_basic_record(value));
    }
    hash
}

fn checksum_unicode_basic_record(value: &UnicodeBasicRecord<'_>) -> u64 {
    let mut hash = 0x756e6962617369;
    hash = fold_opt_u64(hash, value.id);
    hash = fold_opt_str(hash, &value.script);
    hash = fold_opt_str(hash, &value.text);
    hash = fold_opt_u64(hash, value.len);
    fold_str_slice(hash, &value.tags)
}

fn checksum_unicode_mixed(value: &UnicodeMixedDocument<'_>) -> u64 {
    let mut hash = 0x756e696d69786564;
    if let Some(metadata) = &value.metadata {
        hash = fold_opt_str(hash, &metadata.purpose);
        hash = fold_class_slice(hash, &metadata.classes);
        hash = fold_opt_u64(hash, metadata.count);
    } else {
        hash = mix(hash, 0);
    }
    hash = mix(hash, value.records.len() as u64);
    for record in &value.records {
        hash = mix(hash, checksum_unicode_mixed_record(record));
    }
    hash
}

fn checksum_unicode_mixed_record(value: &UnicodeMixedRecord<'_>) -> u64 {
    let mut hash = 0x756e696d69787265;
    hash = fold_opt_u64(hash, value.id);
    hash = fold_opt_record_type(hash, value.class);
    hash = fold_opt_decoded_json_string(hash, &value.value);
    fold_opt_u64(hash, value.n)
}

fn checksum_distinct_values(values: &[DistinctValue<'_>]) -> u64 {
    let mut hash = mix(0x6469737476616c, values.len() as u64);
    for value in values {
        hash = mix(hash, checksum_distinct_value(value));
    }
    hash
}

fn checksum_y_string_unicode(values: &[YStringUnicodeToken]) -> u64 {
    let mut hash = mix(0x795f737472696e67, values.len() as u64);
    for value in values {
        hash = mix(hash, value.id());
    }
    hash
}

fn checksum_distinct_value(value: &DistinctValue<'_>) -> u64 {
    let mut hash = 0x646973747661;
    hash = fold_opt_str(hash, &value.timestamp);
    hash = fold_opt_u64(hash, value.seq);
    hash = fold_opt_str(hash, &value.status);
    hash = mix(hash, value.dynamic.len() as u64);
    for field in &value.dynamic {
        hash = mix(hash, hash_str(field.key.as_ref()));
        hash = mix(hash, hash_str(field.value.as_ref()));
    }
    hash
}

fn checksum_random(value: &RandomDocument<'_>) -> u64 {
    let mut hash = 0x72616e646f6d;
    hash = fold_opt_u64(hash, value.id);
    hash = fold_opt_str(hash, &value.jsonrpc);
    hash = fold_opt_u64(hash, value.total);
    hash = mix(hash, value.result.len() as u64);
    for user in &value.result {
        hash = mix(hash, checksum_random_user(user));
    }
    hash
}

fn checksum_random_user(value: &RandomUser<'_>) -> u64 {
    let mut hash = 0x72616e64757365;
    hash = fold_opt_u64(hash, value.id);
    hash = fold_opt_str(hash, &value.avatar);
    hash = fold_opt_u64(hash, value.age);
    hash = fold_opt_bool(hash, value.admin);
    hash = fold_opt_str(hash, &value.name);
    hash = fold_opt_str(hash, &value.company);
    hash = fold_opt_str(hash, &value.phone);
    hash = fold_opt_str(hash, &value.email);
    hash = fold_opt_str(hash, &value.birth_date);
    hash = mix(hash, value.friends.len() as u64);
    for friend in &value.friends {
        hash = mix(hash, checksum_random_friend(friend));
    }
    fold_opt_str(hash, &value.field)
}

fn checksum_random_friend(value: &RandomFriend<'_>) -> u64 {
    let mut hash = 0x72616e64667269;
    hash = fold_opt_u64(hash, value.id);
    hash = fold_opt_str(hash, &value.name);
    fold_opt_str(hash, &value.phone)
}

fn checksum_instruments(value: &InstrumentsDocument<'_>) -> u64 {
    let mut hash = 0x696e737472756d65;
    hash = fold_opt_str(hash, &value.name);
    hash = fold_opt_u64(hash, value.version.map(u64::from));
    hash = mix(hash, value.instruments.len() as u64);
    for instrument in &value.instruments {
        hash = mix(hash, checksum_instrument(instrument));
    }
    hash = mix(hash, value.patterns.len() as u64);
    for pattern in &value.patterns {
        hash = mix(hash, checksum_instrument_pattern(pattern));
    }
    hash = mix(hash, value.samples.len() as u64);
    for sample in &value.samples {
        hash = mix(hash, checksum_instrument_sample(sample));
    }
    hash
}

fn checksum_instrument(value: &Instrument<'_>) -> u64 {
    let mut hash = 0x696e737472;
    for scalar in [
        value.default_filter_cutoff,
        u32::from(value.default_filter_cutoff_enabled),
        value.default_filter_mode,
        value.default_filter_resonance,
        u32::from(value.default_filter_resonance_enabled),
        value.default_pan,
        value.duplicate_check_type,
        value.duplicate_note_action,
        value.fadeout,
        value.global_volume,
        value.graph_insert,
        value.midi_bank,
        value.midi_channel,
        value.midi_drum_set,
        value.midi_program,
        value.new_note_action,
        value.pitch_pan_center,
        value.pitch_pan_separation,
        value.pitch_to_tempo_lock,
        value.random_cutoff_weight,
        value.random_pan_weight,
        value.random_resonance_weight,
        value.random_volume_weight,
        value.volume_ramp_down,
        value.volume_ramp_up,
    ] {
        hash = mix(hash, scalar as u64);
    }
    hash = fold_opt_str(hash, &value.legacy_filename);
    hash = fold_opt_str(hash, &value.name);
    hash = fold_opt_envelope(hash, &value.panning_envelope);
    hash = fold_opt_envelope(hash, &value.pitch_envelope);
    fold_opt_envelope(hash, &value.volume_envelope)
}

fn fold_opt_envelope(hash: u64, value: &Option<InstrumentEnvelope>) -> u64 {
    value
        .as_ref()
        .map_or_else(|| mix(hash, 0), |value| mix(hash, checksum_envelope(value)))
}

fn checksum_envelope(value: &InstrumentEnvelope) -> u64 {
    let mut hash = 0x656e76656c6f70;
    hash = mix(hash, value.loop_end as u64);
    hash = mix(hash, value.loop_start as u64);
    hash = mix(hash, value.nodes.len() as u64);
    for node in &value.nodes {
        hash = mix(hash, node.tick as u64);
        hash = mix(hash, node.value as u64);
    }
    hash = mix(hash, value.release_node as u64);
    hash = mix(hash, value.sustain_end as u64);
    mix(hash, value.sustain_start as u64)
}

fn checksum_instrument_pattern(value: &InstrumentPattern<'_>) -> u64 {
    let mut hash = 0x7061747465726e;
    hash = fold_opt_str(hash, &value.name);
    hash = mix(hash, value.rows as u64);
    hash = mix(hash, value.rows_per_beat as u64);
    hash = mix(hash, value.rows_per_measure as u64);
    match &value.data {
        Some(events) => {
            hash = mix(hash, events.len() as u64);
            for event in events {
                hash = mix(hash, checksum_pattern_event(event));
            }
            hash
        }
        None => mix(hash, 0),
    }
}

fn checksum_pattern_event(value: &InstrumentPatternEvent) -> u64 {
    let mut hash = 0x6576656e74;
    for scalar in [
        value.channel,
        value.fxcmd,
        value.fxparam,
        value.instr,
        value.note,
        value.row,
        value.volcmd,
        value.volval,
    ] {
        hash = mix(hash, scalar as u64);
    }
    hash
}

fn checksum_instrument_sample(value: &InstrumentSample<'_>) -> u64 {
    let mut hash = 0x73616d706c65;
    for scalar in [
        value.c5_samplerate,
        value.global_volume,
        value.length,
        value.loop_end,
        value.loop_start,
        value.pan,
        value.sustain_end,
        value.sustain_start,
        value.vibrato_depth,
        value.vibrato_rate,
        value.vibrato_sweep,
        value.vibrato_type,
        value.volume,
    ] {
        hash = mix(hash, scalar as u64);
    }
    hash = fold_opt_str(hash, &value.legacy_filename);
    fold_opt_str(hash, &value.name)
}

fn checksum_marine_geometry(value: &MarineGeometry) -> u64 {
    match &value.data {
        Some(data) => mix(0x67656f6d, checksum_marine_geometry_data(data)),
        None => mix(0x67656f6d, 0),
    }
}

fn checksum_marine_geometry_data(value: &MarineGeometryData) -> u64 {
    let mut hash = 0x6d6172696e656461;
    hash = fold_nested_f64_slice(hash, &value.uvs);
    hash = fold_f64_slice(hash, &value.vertices);
    hash = fold_f64_slice(hash, &value.skin_weights);
    hash = fold_u32_slice(hash, &value.skin_indices);
    hash = fold_f64_slice(hash, &value.normals);
    fold_u32_slice(hash, &value.faces)
}

#[cfg(test)]
fn checksum_w5_array_events(values: &[W5ArrayEvent<'_>]) -> u64 {
    let mut hash = mix(0x77356172726179, values.len() as u64);
    for value in values {
        hash = fold_opt_u64(hash, value.id);
        hash = fold_opt_str(hash, &value.actor);
        hash = match value.public {
            Some(value) => mix(hash, value as u64),
            None => mix(hash, 0xff),
        };
    }
    hash
}

#[cfg(test)]
fn checksum_w5_map_entries(values: &[W5MapMetricEntry<'_>]) -> u64 {
    let mut hash = mix(0x77356d6170, values.len() as u64);
    for entry in values {
        hash = mix(hash, hash_str(entry.key.as_ref()));
        hash = fold_opt_u64(hash, entry.value.count);
        hash = fold_opt_str(hash, &entry.value.label);
    }
    hash
}

fn fold_opt_str(hash: u64, value: &Option<Cow<'_, str>>) -> u64 {
    match value {
        Some(value) => mix(hash, hash_str(value.as_ref())),
        None => mix(hash, 0),
    }
}

fn fold_decoded_json_string(hash: u64, value: &DecodedJsonString<'_>) -> u64 {
    let hash = mix(hash, value.fingerprint());
    mix(hash, value.decoded_len())
}

fn fold_opt_decoded_json_string(hash: u64, value: &Option<DecodedJsonString<'_>>) -> u64 {
    match value {
        Some(value) => fold_decoded_json_string(hash, value),
        None => mix(hash, 0),
    }
}

fn fold_opt_record_type(hash: u64, value: Option<UnicodeMixedRecordType>) -> u64 {
    value.map_or_else(|| mix(hash, 0), |value| mix(hash, value.id()))
}

fn fold_class_slice(mut hash: u64, values: &[UnicodeMixedClass]) -> u64 {
    hash = mix(hash, values.len() as u64);
    for value in values {
        hash = mix(hash, value.id());
    }
    hash
}

fn fold_opt_u64(hash: u64, value: Option<u64>) -> u64 {
    value.map_or_else(|| mix(hash, 0), |value| mix(hash, value))
}

fn fold_opt_bool(hash: u64, value: Option<bool>) -> u64 {
    value.map_or_else(|| mix(hash, 0xff), |value| mix(hash, value as u64))
}

fn fold_str_slice(mut hash: u64, values: &[Cow<'_, str>]) -> u64 {
    hash = mix(hash, values.len() as u64);
    for value in values {
        hash = mix(hash, hash_str(value.as_ref()));
    }
    hash
}

fn fold_u64_slice(mut hash: u64, values: &[u64]) -> u64 {
    hash = mix(hash, values.len() as u64);
    for value in values {
        hash = mix(hash, *value);
    }
    hash
}

fn fold_nested_f64_slice(mut hash: u64, values: &[Vec<f64>]) -> u64 {
    hash = mix(hash, values.len() as u64);
    for value in values {
        hash = fold_f64_slice(hash, value);
    }
    hash
}

fn fold_three_deep_str_slice(mut hash: u64, values: &[Vec<Vec<Cow<'_, str>>>]) -> u64 {
    hash = mix(hash, values.len() as u64);
    for value in values {
        hash = mix(hash, value.len() as u64);
        for point in value {
            hash = fold_str_slice(hash, point);
        }
    }
    hash
}

fn fold_f64_slice(mut hash: u64, values: &[f64]) -> u64 {
    hash = mix(hash, values.len() as u64);
    for value in values {
        hash = mix(hash, value.to_bits());
    }
    hash
}

impl<'de> Deserialize<'de> for DistinctValue<'de> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct DistinctValueVisitor;

        impl<'de> Visitor<'de> for DistinctValueVisitor {
            type Value = DistinctValue<'de>;

            fn expecting(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
                formatter.write_str("a distinct_values object")
            }

            fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
            where
                A: MapAccess<'de>,
            {
                let mut timestamp = None;
                let mut seq = None;
                let mut status = None;
                let mut dynamic = Vec::with_capacity(map.size_hint().unwrap_or(0));
                while let Some(key) = map.next_key::<Cow<'de, str>>()? {
                    match key.as_ref() {
                        "timestamp" => timestamp = map.next_value::<Option<Cow<'de, str>>>()?,
                        "seq" => seq = map.next_value::<Option<u64>>()?,
                        "status" => status = map.next_value::<Option<Cow<'de, str>>>()?,
                        _ => {
                            let value = map.next_value::<Cow<'de, str>>()?;
                            dynamic.push(DistinctField { key, value });
                        }
                    }
                }
                Ok(DistinctValue {
                    timestamp,
                    seq,
                    status,
                    dynamic,
                })
            }
        }

        deserializer.deserialize_map(DistinctValueVisitor)
    }
}

fn fold_u32_slice(mut hash: u64, values: &[u32]) -> u64 {
    hash = mix(hash, values.len() as u64);
    for value in values {
        hash = mix(hash, *value as u64);
    }
    hash
}

fn hash_str(value: &str) -> u64 {
    let mut hash = 0xcbf29ce484222325u64 ^ value.len() as u64;
    for byte in value.as_bytes() {
        hash = mix(hash, *byte as u64);
    }
    hash
}

fn decoded_json_string_fingerprint(value: &str) -> (u64, u64) {
    let mut hash = 0xcbf29ce484222325u64;
    for byte in value.as_bytes() {
        hash = mix(hash, *byte as u64);
    }
    (hash, value.len() as u64)
}

fn mix(seed: u64, value: u64) -> u64 {
    seed ^ value
        .wrapping_add(0x9e3779b97f4a7c15)
        .wrapping_add(seed << 6)
        .wrapping_add(seed >> 2)
}

fn deserialize_plugin_entries<'de, D>(deserializer: D) -> Result<Vec<PluginEntry<'de>>, D::Error>
where
    D: Deserializer<'de>,
{
    struct PluginEntriesVisitor;

    impl<'de> Visitor<'de> for PluginEntriesVisitor {
        type Value = Vec<PluginEntry<'de>>;

        fn expecting(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
            formatter.write_str("a plugin object map")
        }

        fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
        where
            A: MapAccess<'de>,
        {
            let mut entries = Vec::with_capacity(map.size_hint().unwrap_or(0));
            while let Some((key, value)) = map.next_entry::<Cow<'de, str>, Plugin<'de>>()? {
                entries.push(PluginEntry { key, value });
            }
            Ok(entries)
        }
    }

    deserializer.deserialize_map(PluginEntriesVisitor)
}

fn deserialize_citm_event_entries<'de, D>(
    deserializer: D,
) -> Result<Vec<CitmEventEntry<'de>>, D::Error>
where
    D: Deserializer<'de>,
{
    struct CitmEventEntriesVisitor;

    impl<'de> Visitor<'de> for CitmEventEntriesVisitor {
        type Value = Vec<CitmEventEntry<'de>>;

        fn expecting(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
            formatter.write_str("a CITM event object map")
        }

        fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
        where
            A: MapAccess<'de>,
        {
            let mut entries = Vec::with_capacity(map.size_hint().unwrap_or(0));
            while let Some((key, value)) = map.next_entry::<Cow<'de, str>, CitmEvent<'de>>()? {
                entries.push(CitmEventEntry { key, value });
            }
            Ok(entries)
        }
    }

    deserializer.deserialize_map(CitmEventEntriesVisitor)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn generated_twitter_typed_parser_matches_sidecars() {
        let input = br#"{"statuses":[{"created_at":"now","id":1,"id_str":"1","text":"hi","source":"web","truncated":false,"user":{"id":2,"id_str":"2","name":"n","screen_name":"s","followers_count":3,"friends_count":4,"verified":true}}],"search_metadata":{"count":1,"completed_in":0.1,"query":"q","refresh_url":"r"}}"#;
        let text = std::str::from_utf8(input).unwrap();
        assert_real_typed_parity(text, input, RealTypedFixture::Twitter);
    }

    #[test]
    fn generated_update_center_typed_parser_matches_sidecars() {
        let input = br#"{"connectionCheckUrl":"http://example.test/","core":{"buildDate":"today","name":"core","sha1":"abc","url":"http://u","version":"1"},"id":"default","plugins":{"p":{"buildDate":"today","dependencies":[{"name":"dep","optional":true,"version":"1"}],"developers":[{"developerId":"dev","email":"dev@example.test","name":"Dev"}],"excerpt":"e","gav":"g","labels":["a","b"],"name":"p","releaseTimestamp":"now","requiredCore":"1","scm":"git","sha1":"s","title":"P","url":"http://p","version":"1","wiki":"http://w"}},"signature":{"digest":"d","signature":"s"},"updateCenterVersion":"1"}"#;
        let text = std::str::from_utf8(input).unwrap();
        assert_real_typed_parity(text, input, RealTypedFixture::UpdateCenter);
    }

    #[test]
    fn generated_apache_builds_typed_parser_matches_sidecars() {
        let input = br#"{"mode":"NORMAL","nodeName":"","jobs":[{"name":"Abdera-trunk","url":"https://builds.apache.org/job/Abdera-trunk/","color":"blue"}],"overallLoad":{},"views":[]}"#;
        let text = std::str::from_utf8(input).unwrap();
        assert_real_typed_parity(text, input, RealTypedFixture::ApacheBuilds);
    }

    #[test]
    fn generated_citm_catalog_typed_parser_matches_sidecars() {
        let input = br#"{"events":{"138586341":{"id":138586341,"name":"30th Anniversary Tour","subTopicIds":[337184269],"topicIds":[324846099]}}}"#;
        let text = std::str::from_utf8(input).unwrap();
        assert_real_typed_parity(text, input, RealTypedFixture::CitmCatalog);
    }

    #[test]
    fn generated_gsoc_2018_typed_parser_matches_sidecars() {
        let input = br#"{"0":{"@context":"http://schema.org","@type":"SoftwareSourceCode","name":"Project","description":"Line one\nLine two","sponsor":{"@type":"Organization","name":"Org","disambiguatingDescription":"Open source org","description":"Builds things","url":"https://example.org","logo":"//example.org/logo.png"},"author":{"@type":"Person","name":"Ada"}}}"#;
        let text = std::str::from_utf8(input).unwrap();
        assert_real_typed_parity(text, input, RealTypedFixture::Gsoc2018);
    }

    #[test]
    fn generated_gsoc_2018_rejects_bad_tokens_and_keys() {
        let bad_token = r#"{"0":{"@context":"http://schema.org","@type":"Article","name":"Project","description":"Text","sponsor":{"@type":"Organization","name":"Org","disambiguatingDescription":"Org","description":"Text","url":"https://example.org","logo":"//example.org/logo.png"},"author":{"@type":"Person","name":"Ada"}}}"#;
        assert!(crate::generated_real_typed::parse_gsoc_2018(bad_token).is_err());
        let bad_key = r#"{"x":{"@context":"http://schema.org","@type":"SoftwareSourceCode","name":"Project","description":"Text","sponsor":{"@type":"Organization","name":"Org","disambiguatingDescription":"Org","description":"Text","url":"https://example.org","logo":"//example.org/logo.png"},"author":{"@type":"Person","name":"Ada"}}}"#;
        assert!(crate::generated_real_typed::parse_gsoc_2018(bad_key).is_err());
    }

    #[test]
    fn generated_github_events_typed_parser_matches_sidecars() {
        let input = br#"[{"type":"PushEvent","created_at":"2013-01-10T07:58:30Z","actor":{"id":138052,"login":"jathanism","url":"https://api.github.com/users/jathanism","avatar_url":"https://secure.gravatar.com/avatar/a"},"repo":{"id":6357414,"name":"jathanism/trigger","url":"https://api.github.com/repos/jathanism/trigger"},"public":true,"payload":{"distinct_size":1,"ref":"refs/heads/main","push_id":134107894,"head":"05570a","before":"000000","size":1},"id":"1234567890","org":{"id":1,"login":"org","url":"https://api.github.com/orgs/org","avatar_url":"https://secure.gravatar.com/avatar/o"}}]"#;
        let text = std::str::from_utf8(input).unwrap();
        assert_real_typed_parity(text, input, RealTypedFixture::GithubEvents);
    }

    #[test]
    fn w2_full_real_typed_fixtures_match_sidecars() {
        for (name, fixture) in [
            ("apache_builds", RealTypedFixture::ApacheBuilds),
            ("citm_catalog", RealTypedFixture::CitmCatalog),
            ("gsoc-2018", RealTypedFixture::Gsoc2018),
        ] {
            let bytes = std::fs::read(locate_fixture(name)).unwrap();
            let text = std::str::from_utf8(&bytes).unwrap();
            assert_real_typed_parity(text, &bytes, fixture);
        }
    }

    #[test]
    fn w6_full_github_events_typed_fixture_matches_sidecars() {
        let bytes = std::fs::read(locate_fixture("github_events")).unwrap();
        let text = std::str::from_utf8(&bytes).unwrap();
        assert_real_typed_parity(text, &bytes, RealTypedFixture::GithubEvents);
    }

    #[test]
    fn w5_generated_array_root_probe_matches_sidecars() {
        let input =
            br#"[{"id":1,"actor":"octo","public":true},{"id":2,"actor":"hub","public":false}]"#;
        let text = std::str::from_utf8(input).unwrap();
        let generated = crate::generated_real_typed::parse_w5_array_root_probe(text).unwrap();
        let serde = serde_json::from_slice::<Vec<W5ArrayEvent<'_>>>(input).unwrap();
        let sonic = sonic_rs::from_slice::<Vec<W5ArrayEvent<'_>>>(input).unwrap();
        let checksums = [
            checksum_w5_array_events(&generated),
            checksum_w5_array_events(&serde),
            checksum_w5_array_events(&sonic),
        ];
        assert_eq!(checksums[0], checksums[1], "generated/serde mismatch");
        assert_eq!(checksums[0], checksums[2], "generated/sonic mismatch");
    }

    #[test]
    fn w5_generated_map_entry_root_probe_matches_sidecars() {
        let input = br#"{"101":{"count":2,"label":"small"},"202":{"count":7,"label":"large"}}"#;
        let text = std::str::from_utf8(input).unwrap();
        let generated = crate::generated_real_typed::parse_w5_map_entry_root_probe(text).unwrap();
        let serde = serde_json::from_slice::<W5MapMetricEntries<'_>>(input)
            .unwrap()
            .0;
        let sonic = sonic_rs::from_slice::<W5MapMetricEntries<'_>>(input)
            .unwrap()
            .0;
        let checksums = [
            checksum_w5_map_entries(&generated),
            checksum_w5_map_entries(&serde),
            checksum_w5_map_entries(&sonic),
        ];
        assert_eq!(checksums[0], checksums[1], "generated/serde mismatch");
        assert_eq!(checksums[0], checksums[2], "generated/sonic mismatch");
    }

    #[test]
    fn generated_mesh_typed_parser_matches_sidecars() {
        let input = br#"{"batches":[{"indexRange":[0,3],"vertexRange":[0,1],"usedBones":[22]}],"positions":[1.0,2.5,-3.25],"tex0":[0.0,1.0],"colors":[4278190080],"influences":[[1.0,0.0]],"normals":[0.0,1.0,0.0],"indices":[0,1,2],"morphTargets":{}}"#;
        let text = std::str::from_utf8(input).unwrap();
        assert_real_typed_parity(text, input, RealTypedFixture::Mesh);
    }

    #[test]
    fn generated_marine_ik_typed_parser_matches_sidecars() {
        let input = br#"{"metadata":{"version":4.3},"geometries":[{"uuid":"g","type":"Geometry","data":{"uvs":[[0.0,1.0]],"vertices":[1.0,2.0,3.0],"skinWeights":[1.0,0.0],"skinIndices":[0,1],"normals":[0.0,1.0,0.0],"faces":[1,2,3],"animations":[]}}],"materials":[]}"#;
        let text = std::str::from_utf8(input).unwrap();
        assert_real_typed_parity(text, input, RealTypedFixture::MarineIk);
    }

    #[test]
    fn generated_numbers_typed_parser_matches_sidecars() {
        let input = br#"[0,1,-2,3.5,6.25e2,-7.125e-1]"#;
        let text = std::str::from_utf8(input).unwrap();
        assert_real_typed_parity(text, input, RealTypedFixture::Numbers);
    }

    #[test]
    fn w13_full_numbers_typed_fixture_matches_sidecars() {
        let bytes = std::fs::read(locate_fixture("numbers")).unwrap();
        let text = std::str::from_utf8(&bytes).unwrap();
        assert_real_typed_parity(text, &bytes, RealTypedFixture::Numbers);
    }

    #[test]
    fn generated_unicode_basic_typed_parser_matches_sidecars() {
        let input = br#"[{"id":0,"script":"latin","text":"hello, world","len":12,"tags":["latin","sample"]}]"#;
        let text = std::str::from_utf8(input).unwrap();
        assert_real_typed_parity(text, input, RealTypedFixture::UnicodeBasic);
    }

    #[test]
    fn w13_full_unicode_basic_typed_fixture_matches_sidecars() {
        let bytes = std::fs::read(locate_fixture("unicode_basic")).unwrap();
        let text = std::str::from_utf8(&bytes).unwrap();
        assert_real_typed_parity(text, &bytes, RealTypedFixture::UnicodeBasic);
    }

    #[test]
    fn generated_unicode_mixed_typed_parser_matches_sidecars() {
        let input = br#"{"metadata":{"purpose":"Unicode + escape stress corpus for skinny JSON parser","classes":["ascii","latin1","cjk","emoji","mixed_escapes"],"count":2},"records":[{"id":0,"type":"ascii","value":"plain","n":7},{"id":1,"type":"mixed","value":"a\uD83D\uDE80\nb","n":8}]}"#;
        let text = std::str::from_utf8(input).unwrap();
        let parsed = crate::generated_real_typed::parse_unicode_mixed(text).unwrap();
        assert!(!parsed.records[0].value.as_ref().unwrap().is_raw_escaped());
        assert!(parsed.records[1].value.as_ref().unwrap().is_raw_escaped());
        assert_real_typed_parity(text, input, RealTypedFixture::UnicodeMixed);
    }

    #[test]
    fn generated_unicode_mixed_rejects_bad_tokens_and_strings() {
        for input in [
            br#"{"metadata":{"classes":["mixed"]},"records":[]}"#.as_slice(),
            br#"{"metadata":{"classes":["ascii"]},"records":[{"id":0,"type":"mixed_escapes","value":"x","n":1}]}"#.as_slice(),
            br#"{"metadata":{"classes":["ascii"]},"records":[{"id":0,"type":"ascii","value":"\uD800","n":1}]}"#.as_slice(),
            br#"{"metadata":{"classes":["ascii"]},"records":[{"id":0,"type":"ascii","value":"\q","n":1}]}"#.as_slice(),
        ] {
            let text = std::str::from_utf8(input).unwrap();
            assert!(crate::generated_real_typed::parse_unicode_mixed(text).is_err());
        }
    }

    #[test]
    fn w14_full_unicode_mixed_typed_fixture_matches_sidecars() {
        let bytes = std::fs::read(locate_fixture("unicode_mixed")).unwrap();
        let text = std::str::from_utf8(&bytes).unwrap();
        assert_real_typed_parity(text, &bytes, RealTypedFixture::UnicodeMixed);
    }

    #[test]
    fn generated_distinct_values_typed_parser_matches_sidecars() {
        let input =
            br#"[{"key_0_0":"ignored","timestamp":"2026-05-12T00:00:00Z","seq":0,"status":"ok"}]"#;
        let text = std::str::from_utf8(input).unwrap();
        assert_real_typed_parity(text, input, RealTypedFixture::DistinctValues);
    }

    #[test]
    fn w14_full_distinct_values_typed_fixture_matches_sidecars() {
        let bytes = std::fs::read(locate_fixture("distinct_values")).unwrap();
        let text = std::str::from_utf8(&bytes).unwrap();
        assert_real_typed_parity(text, &bytes, RealTypedFixture::DistinctValues);
    }

    #[test]
    fn generated_y_string_unicode_typed_parser_matches_sidecars() {
        let input = br#"["\u00e0\u0303","\"","\uDBFF\uDFFE","\uD83F\uDFFE","\u2064","\uFFFE","\u20AC\uD834\uDD1E","\u0821","\uD83D\uDE80","\uD83C\uDF89","\uD83D\uDC68\u200D\uD83D\uDC69\u200D\uD83D\uDC67\u200D\uD83D\uDC66"]"#;
        let text = std::str::from_utf8(input).unwrap();
        assert_real_typed_parity(text, input, RealTypedFixture::YStringUnicode);
    }

    #[test]
    fn w14_full_y_string_unicode_typed_fixture_matches_sidecars() {
        let bytes = std::fs::read(locate_fixture("y_string_unicode")).unwrap();
        let text = std::str::from_utf8(&bytes).unwrap();
        assert_real_typed_parity(text, &bytes, RealTypedFixture::YStringUnicode);
    }

    #[test]
    fn generated_random_typed_parser_matches_sidecars() {
        let input = br#"{"id":1,"jsonrpc":"2.0","total":1,"result":[{"id":7,"avatar":"images/user_7.png","age":29,"admin":true,"name":"Ada","company":"Babbage","phone":"+15550107","email":"ada@example.com","birthDate":"Mon, 05 Jan 1998 15:59:20 GMT","friends":[{"id":1,"name":"Grace","phone":"+15550101"}],"field":"field value"}]}"#;
        let text = std::str::from_utf8(input).unwrap();
        assert_real_typed_parity(text, input, RealTypedFixture::Random);
    }

    #[test]
    fn w13_full_random_typed_fixture_matches_sidecars() {
        let bytes = std::fs::read(locate_fixture("random")).unwrap();
        let text = std::str::from_utf8(&bytes).unwrap();
        assert_real_typed_parity(text, &bytes, RealTypedFixture::Random);
    }

    #[test]
    fn generated_instruments_typed_parser_matches_sidecars() {
        let input = br#"{"name":"demo","version":1,"instruments":[{"default_filter_cutoff":127,"default_filter_cutoff_enabled":true,"default_filter_mode":2,"default_filter_resonance":12,"default_filter_resonance_enabled":false,"default_pan":32,"duplicate_check_type":1,"duplicate_note_action":0,"fadeout":256,"global_volume":64,"graph_insert":0,"legacy_filename":"lead.xi","midi_bank":1,"midi_channel":2,"midi_drum_set":0,"midi_program":33,"name":"lead","new_note_action":1,"panning_envelope":{"loop_end":2,"loop_start":0,"nodes":[{"tick":0,"value":32},{"tick":12,"value":48}],"release_node":0,"sustain_end":1,"sustain_start":1},"pitch_envelope":{"loop_end":0,"loop_start":0,"nodes":[],"release_node":0,"sustain_end":0,"sustain_start":0},"pitch_pan_center":60,"pitch_pan_separation":4,"pitch_to_tempo_lock":0,"random_cutoff_weight":3,"random_pan_weight":2,"random_resonance_weight":1,"random_volume_weight":5,"volume_envelope":{"loop_end":1,"loop_start":0,"nodes":[{"tick":0,"value":64}],"release_node":0,"sustain_end":1,"sustain_start":0},"volume_ramp_down":8,"volume_ramp_up":9}],"patterns":[{"data":[{"channel":1,"fxcmd":2,"fxparam":3,"instr":4,"note":48,"row":0,"volcmd":1,"volval":32}],"name":"p0","rows":64,"rows_per_beat":4,"rows_per_measure":16}],"samples":[{"c5_samplerate":44100,"global_volume":64,"legacy_filename":"s0.wav","length":2048,"loop_end":1024,"loop_start":128,"name":"sample","pan":32,"sustain_end":900,"sustain_start":100,"vibrato_depth":1,"vibrato_rate":2,"vibrato_sweep":3,"vibrato_type":4,"volume":64}]}"#;
        let text = std::str::from_utf8(input).unwrap();
        assert_real_typed_parity(text, input, RealTypedFixture::Instruments);
    }

    #[test]
    fn w13_full_instruments_typed_fixture_matches_sidecars() {
        let bytes = std::fs::read(locate_fixture("instruments")).unwrap();
        let text = std::str::from_utf8(&bytes).unwrap();
        assert_real_typed_parity(text, &bytes, RealTypedFixture::Instruments);
    }

    #[test]
    fn generated_canada_typed_parser_matches_sidecars() {
        let input = br#"{"type":"FeatureCollection","features":[{"type":"Feature","properties":{"name":"Canada"},"geometry":{"type":"Polygon","coordinates":[[[-65.61361699999998,43.420273],[-65.61972,43.418053]]]}}]}"#;
        let text = std::str::from_utf8(input).unwrap();
        assert_real_typed_parity(text, input, RealTypedFixture::Canada);
    }

    #[test]
    fn w14_full_canada_typed_fixture_matches_sidecars() {
        let bytes = std::fs::read(locate_fixture("canada")).unwrap();
        let text = std::str::from_utf8(&bytes).unwrap();
        assert_real_typed_parity(text, &bytes, RealTypedFixture::Canada);
    }
}
