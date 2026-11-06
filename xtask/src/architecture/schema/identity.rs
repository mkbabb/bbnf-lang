use std::borrow::Cow;
use std::fmt;
use std::path::{Component, Path};

use anyhow::{Context, Result, bail};
use serde::{Deserialize, Deserializer, Serialize};

pub(crate) const PERCENT_DECODE_WORK_PER_BYTE: usize = 256;

pub(crate) fn percent_decode_work_bound(
    input_bytes: usize,
) -> std::result::Result<usize, crate::architecture::hash::BoundedKernelError> {
    use crate::architecture::hash::BoundedKernelError;

    fn multiply(
        count: usize,
        coefficient: usize,
    ) -> std::result::Result<usize, BoundedKernelError> {
        count
            .checked_mul(coefficient)
            .ok_or(BoundedKernelError::Arithmetic(
                "percent decoder work ledger overflowed",
            ))
    }

    fn add(left: usize, right: usize) -> std::result::Result<usize, BoundedKernelError> {
        left.checked_add(right)
            .ok_or(BoundedKernelError::Arithmetic(
                "percent decoder work ledger overflowed",
            ))
    }

    // D: every successful decode retires two distinct operand nodes. Nodes
    // created by lossy normalization are restricted below to EF/BF/BD, so
    // they are neither percent candidates nor ASCII-hex operands. Therefore
    // all retired operands are original input nodes and 2D <= n.
    let successful_decodes = input_bytes / 2;
    // I: an invalid decoded byte permanently retires one candidate plus its
    // two operands from decoder eligibility, so 3I <= n. Each such byte can
    // expand to the three-byte UTF-8 replacement and create at most two nodes.
    let invalid_decodes = input_bytes / 3;
    let created_nodes = multiply(invalid_decodes, 2)?;
    // A: initial nodes plus the two possible replacement-expansion nodes.
    let arena_nodes = add(input_bytes, created_nodes)?;
    // R/K: a successful candidate belongs to exactly one current-generation
    // run, and each nonempty run contains at least one such candidate.
    let generated_run_visits = successful_decodes;
    let generated_runs = successful_decodes;
    // U/W/T: UTF-8 plus equality inspect a raw byte at most twice; lossy output
    // is at most three bytes per raw byte; decode and normalization retirement
    // together retire at most two operands plus one raw node per decode.
    let utf8_and_equality = multiply(generated_run_visits, 2)?;
    let normalized_writes = multiply(generated_run_visits, 3)?;
    let retirements = add(multiply(successful_decodes, 2)?, generated_run_visits)?;
    // N: a run visits at most two predecessors, at most 3R normalized bytes,
    // and two trailing nodes. Summed over K <= D runs, N <= 3R + 4K.
    let neighborhood_visits = add(
        multiply(generated_run_visits, 3)?,
        multiply(generated_runs, 4)?,
    )?;
    // Q: at most n initial enqueues plus one enqueue per neighborhood visit;
    // the outer loop drains every enqueue exactly once, including its last
    // unsuccessful generation.
    let enqueues = add(input_bytes, neighborhood_visits)?;
    let queue_actions = multiply(enqueues, 2)?;
    // F: every decode removes two live bytes and every invalid normalization
    // adds at most two, so the final live string never exceeds n bytes.
    let final_copy = input_bytes;

    // Checked primitive-action ledger. The factors pay for every source scan,
    // arena/link read or write, branch/Option test, hex comparison, worklist
    // action, UTF-8/equality action, normalized write, retirement, generation
    // update, neighborhood action, and final output write. Substituting the
    // bounds above yields at most 1333n/6 < 256n.
    let terms = [
        multiply(input_bytes, 4)?,          // source and initialization scans
        multiply(arena_nodes, 7)?,          // A node-field writes and pushes
        queue_actions,                      // Q enqueue/dequeue actions
        multiply(enqueues, 24)?,            // candidate/link/hex inspection
        multiply(successful_decodes, 10)?,  // D successful-decode mutation
        multiply(successful_decodes, 8)?,   // D changed/run dispatch
        multiply(generated_run_visits, 6)?, // R raw-run reads and writes
        multiply(utf8_and_equality, 4)?,    // U scan/equality control
        multiply(normalized_writes, 12)?,   // W normalized arena/link writes
        multiply(retirements, 6)?,          // T retirement/link updates
        multiply(neighborhood_visits, 12)?, // N neighborhood/link/queue work
        multiply(final_copy, 4)?,           // F arena read and output copy
        successful_decodes,                 // generation increment/check
    ];
    let total = terms.into_iter().try_fold(0usize, add)?;
    let ceiling = multiply(input_bytes, PERCENT_DECODE_WORK_PER_BYTE)?;
    if total > ceiling {
        return Err(BoundedKernelError::Arithmetic(
            "percent decoder work proof exceeded its named ceiling",
        ));
    }
    Ok(total)
}

macro_rules! portable_id {
    ($name:ident) => {
        #[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd, Serialize)]
        #[serde(transparent)]
        pub struct $name(String);

        impl $name {
            pub fn new(value: impl Into<String>) -> Result<Self> {
                let value = value.into();
                validate_identity(&value, stringify!($name))?;
                Ok(Self(value))
            }

            pub fn as_str(&self) -> &str {
                &self.0
            }
        }

        impl fmt::Display for $name {
            fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
                formatter.write_str(&self.0)
            }
        }

        impl<'de> Deserialize<'de> for $name {
            fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
                let value = String::deserialize(deserializer)?;
                Self::new(value).map_err(serde::de::Error::custom)
            }
        }
    };
}

portable_id!(RepositoryId);
portable_id!(ProductId);
portable_id!(WorkspaceId);
portable_id!(PackageFaceId);
portable_id!(ContextId);
portable_id!(WorktreeId);
portable_id!(DocumentationId);
portable_id!(ExclusionId);
portable_id!(ResolverId);
portable_id!(ObligationSubject);

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd, Serialize)]
#[serde(transparent)]
pub struct CanonicalText(String);

impl CanonicalText {
    pub fn new(value: impl Into<String>) -> Result<Self> {
        let value = value.into();
        validate_portable_string(&value, "text")?;
        Ok(Self(value))
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

#[cfg(feature = "architecture-test-seams")]
#[doc(hidden)]
pub fn unchecked_canonical_text_for_test(value: String) -> CanonicalText {
    CanonicalText(value)
}

impl fmt::Display for CanonicalText {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str(&self.0)
    }
}

impl<'de> Deserialize<'de> for CanonicalText {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let value = String::deserialize(deserializer)?;
        Self::new(value).map_err(serde::de::Error::custom)
    }
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd, Serialize)]
#[serde(transparent)]
pub struct RepoPath(String);

impl RepoPath {
    pub fn new(value: impl Into<String>) -> Result<Self> {
        let value = value.into();
        validate_portable_string(&value, "repository path")?;
        if value.is_empty()
            || value.contains('\\')
            || value.starts_with('/')
            || (value != "."
                && value
                    .split('/')
                    .any(|part| part.is_empty() || part == "." || part == ".."))
        {
            bail!("non-canonical repository path `{value}`");
        }
        Ok(Self(value))
    }

    pub fn from_relative_path(path: &Path) -> Result<Self> {
        if path == Path::new(".") {
            return Ok(Self::root());
        }
        if path.as_os_str().is_empty() {
            bail!("non-canonical empty relative repository path");
        }
        let mut parts = Vec::new();
        for component in path.components() {
            let Component::Normal(part) = component else {
                bail!(
                    "non-canonical relative repository path `{}`",
                    path.display()
                );
            };
            parts.push(
                part.to_str()
                    .context("repository path component is not valid UTF-8")?,
            );
        }
        Self::new(parts.join("/"))
    }

    pub fn root() -> Self {
        Self(".".into())
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl fmt::Display for RepoPath {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str(&self.0)
    }
}

impl<'de> Deserialize<'de> for RepoPath {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let value = String::deserialize(deserializer)?;
        Self::new(value).map_err(serde::de::Error::custom)
    }
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum PhysicalKind {
    File,
    Directory,
    Symlink,
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct PhysicalIdentity {
    pub repository: RepositoryId,
    pub path: RepoPath,
    pub kind: PhysicalKind,
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd, Serialize, Deserialize)]
#[serde(tag = "kind", rename_all = "snake_case", deny_unknown_fields)]
pub enum Identity {
    Repository {
        repository: RepositoryId,
    },
    Product {
        product: ProductId,
    },
    Workspace {
        workspace: WorkspaceId,
    },
    PackageFace {
        package_face: PackageFaceId,
    },
    Context {
        context: ContextId,
    },
    Worktree {
        worktree: WorktreeId,
    },
    Documentation {
        documentation: DocumentationId,
    },
    Physical {
        physical: PhysicalIdentity,
    },
    Virtual {
        namespace: CanonicalText,
        key: CanonicalText,
    },
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd, Serialize)]
#[serde(transparent)]
pub struct Sha256Digest(String);

impl Sha256Digest {
    pub fn new(value: impl Into<String>) -> Result<Self> {
        let value = value.into();
        if value.len() != 64 || !value.bytes().all(|byte| byte.is_ascii_hexdigit()) {
            bail!("invalid SHA-256 digest `{value}`");
        }
        Ok(Self(value.to_ascii_lowercase()))
    }

    pub fn of(bytes: &[u8]) -> Self {
        Self(crate::architecture::hash::sha256(bytes))
    }

    pub(crate) fn from_bounded_hex(
        value: String,
    ) -> std::result::Result<Self, crate::architecture::hash::BoundedKernelError> {
        if value.len() != 64
            || !value
                .bytes()
                .all(|byte| byte.is_ascii_digit() || matches!(byte, b'a'..=b'f'))
        {
            return Err(crate::architecture::hash::BoundedKernelError::Canonical {
                code: "invalid bounded SHA-256 digest",
                first: value.len(),
                second: 64,
            });
        }
        Ok(Self(value))
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl fmt::Display for Sha256Digest {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str(&self.0)
    }
}

impl<'de> Deserialize<'de> for Sha256Digest {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let value = String::deserialize(deserializer)?;
        let digest = Self::new(value.clone()).map_err(serde::de::Error::custom)?;
        if digest.as_str() != value {
            return Err(serde::de::Error::custom(
                "SHA-256 digest is not canonical lowercase hexadecimal",
            ));
        }
        Ok(digest)
    }
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd, Serialize)]
#[serde(transparent)]
pub struct EdgeIdentity(String);

impl EdgeIdentity {
    pub fn from_digest(digest: &Sha256Digest) -> Self {
        Self(format!("edge:sha256:{digest}"))
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl<'de> Deserialize<'de> for EdgeIdentity {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let value = String::deserialize(deserializer)?;
        let encoded = value
            .strip_prefix("edge:sha256:")
            .ok_or_else(|| serde::de::Error::custom("invalid edge identity prefix"))?;
        let digest = Sha256Digest::new(encoded.to_owned()).map_err(serde::de::Error::custom)?;
        let identity = Self::from_digest(&digest);
        if identity.as_str() != value {
            return Err(serde::de::Error::custom(
                "edge identity is not canonical lowercase SHA-256",
            ));
        }
        Ok(identity)
    }
}

pub fn validate_identity(value: &str, field: &str) -> Result<()> {
    validate_portable_string(value, field)?;
    if value.trim().is_empty()
        || value.starts_with('.')
        || value.contains("..")
        || value.chars().any(char::is_whitespace)
    {
        bail!("invalid {field} `{value}`");
    }
    Ok(())
}

pub fn validate_portable_string(value: &str, field: &str) -> Result<()> {
    if value.len() > 1024 * 1024 {
        bail!("PRIVACY_VALIDATOR_INCOMPLETE: oversized {field}");
    }
    let decoded = percent_decode(value);
    if portable_violation(decoded.as_bytes()).0 {
        bail!("PRIVACY_VALIDATOR_INCOMPLETE: non-portable {field} `{value}`");
    }
    Ok(())
}

fn portable_string_is_valid(value: &str) -> bool {
    value.len() <= 1024 * 1024 && !portable_violation(percent_decode(value).as_bytes()).0
}

fn portable_violation(bytes: &[u8]) -> (bool, usize) {
    let boundary = |byte: u8| {
        !byte.is_ascii_alphanumeric() && !matches!(byte, b'_' | b'-' | b'.' | b'*' | b'?' | b']')
    };
    const ROOTS: [&[u8]; 5] = [b"/Users/", b"/home/", b"/private/", b"/tmp/", b"/var/"];
    let mut root_states = [0usize; ROOTS.len()];
    let mut file_state = 0usize;
    let mut previous: Option<u8> = None;
    let mut before_previous: Option<u8> = None;
    let mut before_before_previous: Option<u8> = None;
    let mut tilde_candidate = false;
    let mut operations = 0usize;

    for byte in bytes.iter().copied() {
        operations += 1;
        let begins_boundary_token = previous.is_none_or(boundary);
        let windows_drive = matches!(byte, b'/' | b'\\')
            && previous == Some(b':')
            && before_previous.is_some_and(|value| value.is_ascii_alphabetic())
            && before_before_previous.is_none_or(boundary);
        let forward_unc = byte == b'/'
            && previous == Some(b'/')
            && before_previous.is_none_or(|value| value != b':' && boundary(value));
        let posix_absolute = previous == Some(b'/')
            && byte != b'/'
            && before_previous
                .is_none_or(|value| value != b'/' && (value == b':' || boundary(value)));

        if byte == b'~' && begins_boundary_token {
            tilde_candidate = true;
        } else if matches!(byte, b'/' | b'\\') {
            if tilde_candidate {
                return (true, operations);
            }
            tilde_candidate = false;
        } else if tilde_candidate
            && !(byte.is_ascii_alphanumeric() || matches!(byte, b'_' | b'-' | b'.'))
        {
            tilde_candidate = false;
        }

        if file_state == 0 {
            file_state = usize::from(byte.eq_ignore_ascii_case(&b'f') && begins_boundary_token);
        } else if byte.eq_ignore_ascii_case(&b"file:"[file_state]) {
            file_state += 1;
        } else {
            file_state = usize::from(byte.eq_ignore_ascii_case(&b'f') && begins_boundary_token);
        }
        let file_uri = file_state == b"file:".len();
        if file_uri {
            file_state = 0;
        }
        let mut known_posix_root = false;
        for (state, root) in root_states.iter_mut().zip(ROOTS) {
            if byte == root[*state] {
                *state += 1;
            } else {
                *state = usize::from(byte == b'/');
            }
            if *state == root.len() {
                known_posix_root = true;
                *state = 1;
            }
        }
        if matches!(byte, b'\0' | b'\r' | b'\\')
            || windows_drive
            || forward_unc
            || posix_absolute
            || known_posix_root
            || file_uri
        {
            return (true, operations);
        }

        before_before_previous = before_previous;
        before_previous = previous;
        previous = Some(byte);
    }
    (bytes == b"/", operations)
}

#[cfg(feature = "architecture-test-seams")]
#[doc(hidden)]
pub fn portable_violation_for_test(value: &str) -> (bool, usize) {
    portable_violation(value.as_bytes())
}

#[derive(Clone, Copy)]
struct DecodeByte {
    byte: u8,
    previous: Option<usize>,
    next: Option<usize>,
    live: bool,
    generation: usize,
    queued: usize,
}

fn percent_decode(value: &str) -> Cow<'_, str> {
    percent_decode_counted(value).0
}

fn percent_decode_counted(value: &str) -> (Cow<'_, str>, usize) {
    #[cfg(feature = "architecture-test-seams")]
    observe_percent_decode(PercentDecodeOperation::Started);
    let input_len = value.len();
    // The source/initialization term pays this bounded source scan and branch.
    if !value.as_bytes().contains(&b'%') {
        return (Cow::Borrowed(value), input_len);
    }
    // A pays the checked arena-node derivation; each invalid decoded input byte
    // can add only two non-operand replacement nodes.
    let arena_capacity = input_len
        .checked_add(
            input_len
                .checked_div(3)
                .and_then(|decoded| decoded.checked_mul(2))
                .expect("decoder expansion is bounded by input length"),
        )
        .expect("decoder arena capacity is bounded by the string limit");
    let mut arena = Vec::with_capacity(arena_capacity);
    let mut candidates = Vec::with_capacity(input_len);
    // Source/initialization pays the byte/index tests, A pays all node-field
    // writes, and Q pays each initial candidate enqueue.
    for (index, byte) in value.bytes().enumerate() {
        if byte == b'%' {
            candidates.push(index);
        }
        arena.push(DecodeByte {
            byte,
            previous: index.checked_sub(1),
            next: (index + 1 < input_len).then_some(index + 1),
            live: true,
            generation: 0,
            queued: usize::from(byte == b'%'),
        });
    }
    // This sampled counter is diagnostic regression evidence only. Budget
    // authority comes from `percent_decode_work_bound`, not this subset.
    let mut observed_loop_steps = input_len
        .checked_mul(2)
        .expect("decoder initialization work is bounded by the string limit");
    let mut changed = Vec::with_capacity(input_len);
    let mut raw = Vec::with_capacity(input_len);
    let mut normalized = Vec::with_capacity(input_len);
    let mut live_len = input_len;
    let mut generation = 1usize;

    loop {
        changed.clear();
        // Q pays the dequeue. Candidate inspection pays every live/next/byte
        // arena read, Option branch, and both worst-case hex comparisons.
        for candidate in candidates.drain(..) {
            observed_loop_steps += 1;
            // Retire stale queued nodes in constant time through the live bit.
            if !arena[candidate].live {
                continue;
            }
            let Some(high_index) = arena[candidate].next else {
                continue;
            };
            let Some(low_index) = arena[high_index].next else {
                continue;
            };
            let (Some(high), Some(low)) = (hex(arena[high_index].byte), hex(arena[low_index].byte))
            else {
                continue;
            };
            // D pays the decoded byte/link/generation writes and changed push;
            // T pays retirement of the two distinct operand nodes.
            let after = arena[low_index].next;
            arena[candidate].byte = (high << 4) | low;
            arena[candidate].next = after;
            arena[candidate].generation = generation;
            if let Some(after) = after {
                arena[after].previous = Some(candidate);
            }
            arena[high_index].live = false;
            arena[low_index].live = false;
            live_len -= 2;
            changed.push(candidate);
        }
        if changed.is_empty() {
            break;
        }

        // D dispatch pays the start/live/generation/predecessor reads and
        // branches for every changed candidate, including skipped run members.
        for &start in &changed {
            observed_loop_steps += 1;
            let index = start;
            if !arena[index].live || arena[index].generation != generation {
                continue;
            }
            if arena[index]
                .previous
                .is_some_and(|previous| arena[previous].generation == generation)
            {
                continue;
            }
            raw.clear();
            let mut cursor = Some(start);
            let mut last = start;
            // R pays every raw-run loop/branch, arena read, scratch write, and
            // next-link traversal. A current-generation candidate is visited
            // by exactly one run leader.
            while let Some(index) = cursor {
                observed_loop_steps += 1;
                if !arena[index].live || arena[index].generation != generation {
                    break;
                }
                raw.push(arena[index].byte);
                last = index;
                cursor = arena[index].next;
            }
            normalized.clear();
            let mut remaining = raw.as_slice();
            // Match String::from_utf8_lossy without its hidden temporary allocation.
            // U pays each UTF-8 input-byte action and its error/remaining
            // control; W pays every normalized scratch-byte write.
            while !remaining.is_empty() {
                match std::str::from_utf8(remaining) {
                    Ok(_) => {
                        observed_loop_steps += remaining.len();
                        normalized.extend_from_slice(remaining);
                        remaining = &[];
                    }
                    Err(error) => {
                        let valid = error.valid_up_to();
                        observed_loop_steps +=
                            valid + error.error_len().unwrap_or(remaining.len() - valid);
                        normalized.extend_from_slice(&remaining[..valid]);
                        normalized.extend_from_slice("�".as_bytes());
                        let consumed = valid + error.error_len().unwrap_or(remaining.len() - valid);
                        remaining = &remaining[consumed..];
                    }
                }
            }
            // U also pays the raw/normalized equality byte comparisons.
            if normalized != raw {
                let before = arena[start].previous;
                let after = arena[last].next;
                let mut old = Some(start);
                let mut tail = before;
                // W pays this branch, each normalized byte write, and every
                // link action. Lossy UTF-8 adds exactly two bytes per
                // replacement. Spend those new-node slots only on bytes from
                // the EF/BF/BD replacement alphabet; none can become a `%`
                // candidate or an ASCII-hex operand in a later generation.
                let mut nodes_to_create = normalized
                    .len()
                    .checked_sub(raw.len())
                    .expect("lossy UTF-8 normalization does not contract");
                for byte in &normalized {
                    observed_loop_steps += 1;
                    // Reuse `start` for the first output byte so an expansion
                    // at the list head cannot move the decoder's fixed head.
                    let is_created_replacement_byte = nodes_to_create != 0
                        && old != Some(start)
                        && matches!(*byte, 0xef | 0xbf | 0xbd);
                    let index = if is_created_replacement_byte {
                        assert!(*byte != b'%' && !byte.is_ascii_hexdigit());
                        nodes_to_create -= 1;
                        let index = arena.len();
                        assert!(index < arena_capacity, "derived decoder arena bound");
                        arena.push(DecodeByte {
                            byte: *byte,
                            previous: tail,
                            next: None,
                            live: true,
                            generation,
                            queued: 0,
                        });
                        live_len += 1;
                        index
                    } else {
                        let index = old.expect("generated run ends at its recorded successor");
                        old = arena[index].next;
                        arena[index].byte = *byte;
                        arena[index].previous = tail;
                        arena[index].generation = generation;
                        arena[index].queued = 0;
                        index
                    };
                    if let Some(tail) = tail {
                        arena[tail].next = Some(index);
                    }
                    tail = Some(index);
                }
                assert_eq!(nodes_to_create, 0, "every expansion slot is non-operand");
                while old != after {
                    // T conservatively pays this retirement path. Lossy UTF-8
                    // does not contract, so the non-operand placement rule
                    // normally consumes every old run node before this loop.
                    let index = old.expect("generated run ends at its recorded successor");
                    observed_loop_steps += 1;
                    arena[index].live = false;
                    live_len -= 1;
                    old = arena[index].next;
                }
                let tail = tail.expect("lossy normalization is never empty");
                arena[tail].next = after;
                if let Some(after) = after {
                    arena[after].previous = Some(tail);
                }
            }

            let mut neighborhood = start;
            for _ in 0..2 {
                if let Some(previous) = arena[neighborhood].previous {
                    neighborhood = previous;
                }
            }
            let mut trailing = 0usize;
            let mut saw_generated = false;
            // N pays all predecessor/link/byte/generation/queued reads,
            // branches, and writes in this bounded neighborhood. Q pays each
            // candidate push, at most one per neighborhood visit.
            loop {
                observed_loop_steps += 1;
                if arena[neighborhood].byte == b'%' && arena[neighborhood].queued != generation + 1
                {
                    arena[neighborhood].queued = generation + 1;
                    candidates.push(neighborhood);
                }
                let generated = arena[neighborhood].generation == generation;
                if generated {
                    saw_generated = true;
                    trailing = 0;
                } else if saw_generated {
                    trailing += 1;
                }
                if saw_generated && trailing >= 2 {
                    break;
                }
                let Some(next) = arena[neighborhood].next else {
                    break;
                };
                neighborhood = next;
            }
        }
        // The generation term pays the checked counter update and branch.
        generation = generation
            .checked_add(1)
            .expect("decode generation is bounded by input length");
    }

    let mut decoded = Vec::with_capacity(live_len);
    let mut cursor = Some(0usize);
    // F pays each final loop/branch, arena byte/link read, and output write.
    while let Some(index) = cursor {
        observed_loop_steps += 1;
        decoded.push(arena[index].byte);
        cursor = arena[index].next;
    }
    debug_assert_eq!(arena.capacity(), arena_capacity);
    debug_assert_eq!(candidates.capacity(), input_len);
    debug_assert_eq!(changed.capacity(), input_len);
    debug_assert_eq!(raw.capacity(), input_len);
    debug_assert_eq!(normalized.capacity(), input_len);
    debug_assert_eq!(decoded.capacity(), live_len);
    (
        Cow::Owned(String::from_utf8(decoded).expect("each decode round normalized UTF-8")),
        observed_loop_steps,
    )
}

#[cfg(feature = "architecture-test-seams")]
#[doc(hidden)]
pub fn percent_decode_for_test(value: &str) -> (String, usize) {
    let (decoded, observed_loop_steps) = percent_decode_counted(value);
    (decoded.into_owned(), observed_loop_steps)
}

#[cfg(feature = "architecture-test-seams")]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[doc(hidden)]
pub enum PercentDecodeOperation {
    Started,
}

#[cfg(feature = "architecture-test-seams")]
thread_local! {
    static PERCENT_DECODE_OBSERVER: std::cell::Cell<Option<fn(PercentDecodeOperation)>> =
        const { std::cell::Cell::new(None) };
}

#[cfg(feature = "architecture-test-seams")]
fn observe_percent_decode(operation: PercentDecodeOperation) {
    PERCENT_DECODE_OBSERVER.with(|slot| {
        if let Some(observer) = slot.get() {
            observer(operation);
        }
    });
}

#[cfg(feature = "architecture-test-seams")]
#[doc(hidden)]
pub fn with_percent_decode_observer_for_test<T>(
    observer: fn(PercentDecodeOperation),
    operation: impl FnOnce() -> T,
) -> T {
    struct Restore(Option<fn(PercentDecodeOperation)>);

    impl Drop for Restore {
        fn drop(&mut self) {
            PERCENT_DECODE_OBSERVER.with(|slot| slot.set(self.0));
        }
    }

    let previous = PERCENT_DECODE_OBSERVER.with(|slot| slot.replace(Some(observer)));
    let restore = Restore(previous);
    let result = operation();
    drop(restore);
    result
}

pub(crate) fn percent_decode_allocation_bound(
    input_bytes: usize,
) -> std::result::Result<usize, crate::architecture::hash::BoundedKernelError> {
    use crate::architecture::hash::BoundedKernelError;

    let arena_nodes = input_bytes
        .checked_add(
            input_bytes
                .checked_div(3)
                .and_then(|decoded| decoded.checked_mul(2))
                .ok_or(BoundedKernelError::Arithmetic(
                    "percent decoder expansion overflowed",
                ))?,
        )
        .ok_or(BoundedKernelError::Arithmetic(
            "percent decoder arena overflowed",
        ))?;
    let arena = arena_nodes
        .checked_mul(std::mem::size_of::<DecodeByte>())
        .ok_or(BoundedKernelError::Arithmetic(
            "percent decoder arena bytes overflowed",
        ))?;
    let worklists = input_bytes
        .checked_mul(std::mem::size_of::<usize>() * 2)
        .ok_or(BoundedKernelError::Arithmetic(
            "percent decoder worklist bytes overflowed",
        ))?;
    let scratch = input_bytes
        .checked_mul(3)
        .ok_or(BoundedKernelError::Arithmetic(
            "percent decoder scratch bytes overflowed",
        ))?;
    arena
        .checked_add(worklists)
        .and_then(|bytes| bytes.checked_add(scratch))
        .ok_or(BoundedKernelError::Arithmetic(
            "percent decoder allocation overflowed",
        ))
}

#[cfg(feature = "architecture-test-seams")]
#[doc(hidden)]
pub fn percent_decode_allocation_bound_for_test(input_bytes: usize) -> Result<usize> {
    percent_decode_allocation_bound(input_bytes).map_err(anyhow::Error::new)
}

fn hex(byte: u8) -> Option<u8> {
    match byte {
        b'0'..=b'9' => Some(byte - b'0'),
        b'a'..=b'f' => Some(byte - b'a' + 10),
        b'A'..=b'F' => Some(byte - b'A' + 10),
        _ => None,
    }
}

pub fn validate_serialized_strings(value: &impl Serialize) -> Result<()> {
    validate_serialized_strings_bounded(value).map_err(anyhow::Error::new)
}

pub(crate) fn validate_serialized_strings_bounded(
    value: &impl Serialize,
) -> std::result::Result<(), crate::architecture::hash::BoundedKernelError> {
    crate::architecture::hash::walk_json_strings(value, &mut |text| {
        if portable_string_is_valid(text) {
            Ok(())
        } else {
            Err(crate::architecture::hash::BoundedKernelError::Canonical {
                code: "PRIVACY_VALIDATOR_INCOMPLETE",
                first: text.len(),
                second: 0,
            })
        }
    })
}
