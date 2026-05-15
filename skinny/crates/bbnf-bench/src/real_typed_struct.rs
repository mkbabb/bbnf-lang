use serde::de::{MapAccess, Visitor};
use serde::{Deserialize, Deserializer};
use std::borrow::Cow;
use std::fmt;
use std::path::PathBuf;

use crate::direct_struct::DirectStructError;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RealTypedFixture {
    Twitter,
    UpdateCenter,
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

pub enum RealTypedOutput<'a> {
    Twitter(TwitterSearch<'a>),
    UpdateCenter(UpdateCenter<'a>),
}

pub fn fixture_for_name(name: &str) -> Option<RealTypedFixture> {
    match name {
        "twitter" => Some(RealTypedFixture::Twitter),
        "update_center" | "update-center" => Some(RealTypedFixture::UpdateCenter),
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
        RealTypedFixture::UpdateCenter => crate::generated_real_typed::parse_update_center(input)
            .map(RealTypedOutput::UpdateCenter)
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
        RealTypedFixture::UpdateCenter => serde_json::from_slice::<UpdateCenter<'a>>(bytes)
            .map(RealTypedOutput::UpdateCenter)
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
        RealTypedFixture::UpdateCenter => sonic_rs::from_slice::<UpdateCenter<'a>>(bytes)
            .map(RealTypedOutput::UpdateCenter)
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
        RealTypedOutput::UpdateCenter(value) => checksum_update_center(value),
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

fn fold_opt_str(hash: u64, value: &Option<Cow<'_, str>>) -> u64 {
    match value {
        Some(value) => mix(hash, hash_str(value.as_ref())),
        None => mix(hash, 0),
    }
}

fn fold_opt_u64(hash: u64, value: Option<u64>) -> u64 {
    value.map_or_else(|| mix(hash, 0), |value| mix(hash, value))
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
}
