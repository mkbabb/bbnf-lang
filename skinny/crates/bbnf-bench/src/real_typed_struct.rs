use serde::de::{MapAccess, Visitor};
use serde::{Deserialize, Deserializer};
use std::borrow::Cow;
use std::fmt;
use std::path::PathBuf;

use crate::direct_struct::DirectStructError;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RealTypedFixture {
    Twitter,
    ApacheBuilds,
    CitmCatalog,
    GithubEvents,
    UpdateCenter,
    Mesh,
    MarineIk,
    Numbers,
    UnicodeBasic,
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
    GithubEvents(Vec<GithubEvent<'a>>),
    UpdateCenter(UpdateCenter<'a>),
    Mesh(Mesh),
    MarineIk(MarineIk),
    Numbers(Vec<f64>),
    UnicodeBasic(Vec<UnicodeBasicRecord<'a>>),
}

pub fn fixture_for_name(name: &str) -> Option<RealTypedFixture> {
    match name {
        "twitter" => Some(RealTypedFixture::Twitter),
        "apache_builds" | "apache-builds" => Some(RealTypedFixture::ApacheBuilds),
        "citm_catalog" | "citm-catalog" => Some(RealTypedFixture::CitmCatalog),
        "github_events" | "github-events" => Some(RealTypedFixture::GithubEvents),
        "update_center" | "update-center" => Some(RealTypedFixture::UpdateCenter),
        "mesh" => Some(RealTypedFixture::Mesh),
        "marine_ik" | "marine-ik" => Some(RealTypedFixture::MarineIk),
        "numbers" => Some(RealTypedFixture::Numbers),
        "unicode_basic" | "unicode-basic" => Some(RealTypedFixture::UnicodeBasic),
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
        _ => [name, name],
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
        RealTypedFixture::Numbers => crate::generated_real_typed::parse_numbers(input)
            .map(RealTypedOutput::Numbers)
            .map_err(|error| DirectStructError::Parse(error.to_string())),
        RealTypedFixture::UnicodeBasic => crate::generated_real_typed::parse_unicode_basic(input)
            .map(RealTypedOutput::UnicodeBasic)
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
        RealTypedFixture::Numbers => serde_json::from_slice::<Vec<f64>>(bytes)
            .map(RealTypedOutput::Numbers)
            .map_err(|error| DirectStructError::Serde(error.to_string())),
        RealTypedFixture::UnicodeBasic => {
            serde_json::from_slice::<Vec<UnicodeBasicRecord<'a>>>(bytes)
                .map(RealTypedOutput::UnicodeBasic)
                .map_err(|error| DirectStructError::Serde(error.to_string()))
        }
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
        RealTypedFixture::Numbers => sonic_rs::from_slice::<Vec<f64>>(bytes)
            .map(RealTypedOutput::Numbers)
            .map_err(|error| DirectStructError::Sonic(error.to_string())),
        RealTypedFixture::UnicodeBasic => {
            sonic_rs::from_slice::<Vec<UnicodeBasicRecord<'a>>>(bytes)
                .map(RealTypedOutput::UnicodeBasic)
                .map_err(|error| DirectStructError::Sonic(error.to_string()))
        }
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
        RealTypedOutput::GithubEvents(value) => checksum_github_events(value),
        RealTypedOutput::UpdateCenter(value) => checksum_update_center(value),
        RealTypedOutput::Mesh(value) => checksum_mesh(value),
        RealTypedOutput::MarineIk(value) => checksum_marine_ik(value),
        RealTypedOutput::Numbers(value) => checksum_numbers(value),
        RealTypedOutput::UnicodeBasic(value) => checksum_unicode_basic(value),
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

fn fold_f64_slice(mut hash: u64, values: &[f64]) -> u64 {
    hash = mix(hash, values.len() as u64);
    for value in values {
        hash = mix(hash, value.to_bits());
    }
    hash
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
}
