use std::collections::BTreeMap;
use std::ffi::{OsStr, OsString};
use std::os::unix::ffi::{OsStrExt as _, OsStringExt as _};
use std::os::unix::process::ExitStatusExt as _;
use std::path::{Path, PathBuf};
use std::process::Output;
use std::sync::Arc;

use anyhow::{Context as _, Result};

use crate::architecture::schema::{CanonicalText, Sha256Digest};

use super::{canonical_child_policy, hash_file};

pub(super) const POLICY: &str = "bbnf.apple-arm-capability-launch.v2-direct-declared-arm64";
const LAUNCHER: &str = "/usr/bin/arch";
const REQUESTED_ARCHITECTURE: &str = "arm64";

struct BinaryFrame {
    bytes: Vec<u8>,
}

impl BinaryFrame {
    fn new(domain: &[u8]) -> Self {
        Self {
            bytes: domain.to_vec(),
        }
    }

    fn push(&mut self, tag: &[u8], value: &[u8]) {
        for bytes in [tag, value] {
            self.bytes.extend_from_slice(
                &u64::try_from(bytes.len())
                    .expect("frame length")
                    .to_be_bytes(),
            );
            self.bytes.extend_from_slice(bytes);
        }
    }

    fn push_os(&mut self, tag: &[u8], value: &OsStr) {
        self.push(tag, value.as_bytes());
    }
}
struct CommandObservation {
    output: Output,
    transcript: Vec<u8>,
}
#[derive(Debug)]
struct AppleArmRunner {
    launcher: PathBuf,
    launcher_hash: Sha256Digest,
}

impl AppleArmRunner {
    fn capture() -> Result<Self> {
        let launcher = PathBuf::from(LAUNCHER);
        if std::fs::canonicalize(&launcher).context("canonicalize Apple ARM launcher")? != launcher
        {
            anyhow::bail!("Apple ARM launcher is not a canonical path");
        }
        Ok(Self {
            launcher_hash: hash_file(&launcher)?,
            launcher,
        })
    }

    fn run(
        &self,
        role: &'static str,
        program: &Path,
        logical_args: &[&OsStr],
        environment: &[(OsString, OsString)],
    ) -> Result<CommandObservation> {
        if std::fs::canonicalize(program).context("canonicalize Apple child program")? != program
            || !program.is_file()
        {
            anyhow::bail!("Apple child program is not a canonical file");
        }
        let physical_argv = physical_argv(program.as_os_str(), logical_args);
        let mut command = crate::architecture::canonical_child_command(&self.launcher);
        command.args(&physical_argv);
        for (name, value) in environment {
            command.env(name, value);
        }
        let output = command
            .output()
            .with_context(|| format!("execute Apple ARM {role} child"))?;
        let mut frame = BinaryFrame::new(b"bbnf.apple-arm.command-observation.v2");
        frame.push(b"policy", POLICY.as_bytes());
        frame.push(b"role", role.as_bytes());
        frame.push(b"environment-clear", b"true");
        frame.push_os(b"launcher-path", self.launcher.as_os_str());
        frame.push(b"launcher-hash", self.launcher_hash.as_str().as_bytes());
        frame.push(b"program-hash", hash_file(program)?.as_str().as_bytes());
        for argument in &physical_argv {
            frame.push_os(b"physical-argv", argument);
        }
        for (name, value) in [
            (OsStr::new("LC_ALL"), OsStr::new("C")),
            (OsStr::new("TZ"), OsStr::new("UTC")),
        ] {
            frame.push_os(b"environment-name", name);
            frame.push_os(b"environment-value", value);
        }
        for (name, value) in environment {
            frame.push_os(b"environment-name", name);
            frame.push_os(b"environment-value", value);
        }
        let status = output.status.clone().into_raw().to_be_bytes();
        frame.push(b"status-raw", &status);
        frame.push(b"stdout", &output.stdout);
        frame.push(b"stderr", &output.stderr);
        let transcript = frame.bytes;
        Ok(CommandObservation { output, transcript })
    }
}

fn physical_argv(program: &OsStr, logical_args: &[&OsStr]) -> Vec<OsString> {
    [OsStr::new("-arm64"), program]
        .into_iter()
        .chain(logical_args.iter().copied())
        .map(OsStr::to_os_string)
        .collect()
}

#[derive(Debug)]
pub(super) struct AppleArmCaptureContext {
    runner: AppleArmRunner,
    selected: BTreeMap<&'static str, (PathBuf, Sha256Digest)>,
    context_transcript: Vec<u8>,
    context_hash: Sha256Digest,
}

pub(super) struct AppleToolCapture {
    pub(super) capability: CanonicalText,
    pub(super) capability_hash: Sha256Digest,
    pub(super) selected_hash: Sha256Digest,
    pub(super) execution_contract: Vec<CanonicalText>,
    pub(super) toolchain_selection_hash: Sha256Digest,
    pub(super) dynamic_runtime_hash: Sha256Digest,
}

impl AppleArmCaptureContext {
    pub(super) fn capture() -> Result<Arc<Self>> {
        if !cfg!(target_arch = "aarch64") {
            anyhow::bail!("Apple ARM capture requires a macOS/aarch64 compilation target");
        }
        let runner = AppleArmRunner::capture()?;

        let uname_machine = runner.run(
            "observed-arm-precondition",
            Path::new("/usr/bin/uname"),
            &[OsStr::new("-m")],
            &[],
        )?;
        require_success(&uname_machine, "Apple ARM observed-machine precondition")?;
        if parse_path_record(&uname_machine.output.stdout)?.as_bytes()
            != REQUESTED_ARCHITECTURE.as_bytes()
        {
            anyhow::bail!("Apple ARM observed-machine precondition requires `arm64`");
        }
        let mut context = BinaryFrame::new(b"bbnf.apple-arm.capture-context.v2");
        context.push(b"policy", POLICY.as_bytes());
        context.push(b"uname-machine", &uname_machine.transcript);

        let selector = runner.run(
            "selector",
            Path::new("/usr/bin/xcode-select"),
            &[OsStr::new("-p")],
            &[],
        )?;
        require_success(&selector, "selected Xcode developer directory")?;
        let selected_directory = PathBuf::from(parse_path_record(&selector.output.stdout)?);
        let developer_directory = std::fs::canonicalize(&selected_directory)
            .context("canonicalize selected Xcode developer directory")?;
        if !developer_directory.is_dir() {
            anyhow::bail!("selected Xcode developer directory is not a directory");
        }
        let selection_inputs = [developer_directory.as_os_str(); 3];
        let selection_environment =
            coherent_selection_environment(developer_directory.as_os_str(), &selection_inputs)?;

        context.push(b"selector", &selector.transcript);
        context.push_os(
            b"canonical-developer-directory",
            developer_directory.as_os_str(),
        );
        let mut selected = BTreeMap::new();
        for (logical_name, payload_name) in [("ar", "ar"), ("cc", "clang"), ("ld", "ld")] {
            let resolver = runner.run(
                "resolver",
                Path::new("/usr/bin/xcrun"),
                &[OsStr::new("--find"), OsStr::new(payload_name)],
                &selection_environment,
            )?;
            require_success(&resolver, "required Xcode-selected payload")?;
            let resolved = PathBuf::from(parse_path_record(&resolver.output.stdout)?);
            let path = std::fs::canonicalize(&resolved)
                .with_context(|| format!("canonicalize Xcode-selected {payload_name} payload"))?;
            if !path.is_file() {
                anyhow::bail!("required Xcode-selected {payload_name} payload is not a file");
            }
            let initial_hash = hash_file(&path)?;
            context.push(b"logical-tool", logical_name.as_bytes());
            context.push(b"payload-name", payload_name.as_bytes());
            context.push(b"resolver", &resolver.transcript);
            context.push_os(b"selected-payload-path", path.as_os_str());
            context.push(b"selected-payload-hash", initial_hash.as_str().as_bytes());
            selected.insert(logical_name, (path, initial_hash));
        }

        let uname_system = runner.run(
            "host-kernel-premise",
            Path::new("/usr/bin/uname"),
            &[OsStr::new("-srmv")],
            &[],
        )?;
        require_success(&uname_system, "exact kernel premise")?;
        let macos_version = runner.run("macos-premise", Path::new("/usr/bin/sw_vers"), &[], &[])?;
        require_success(&macos_version, "exact macOS build premise")?;

        context.push(b"uname-system", &uname_system.transcript);
        context.push(b"macos-version", &macos_version.transcript);
        let context_transcript = context.bytes;
        let context_hash = Sha256Digest::of(&context_transcript);
        let captured = Arc::new(Self {
            runner,
            selected,
            context_transcript,
            context_hash,
        });
        captured.verify_payloads_unchanged()?;
        Ok(captured)
    }

    pub(super) fn selected_path(&self, tool: &str) -> Result<&Path> {
        Ok(self.selected_tool(tool)?.0.as_path())
    }

    pub(super) fn launcher(&self) -> (&Path, &Sha256Digest) {
        (&self.runner.launcher, &self.runner.launcher_hash)
    }

    pub(super) fn require_capture_input(&self, tool: &str, path: &Path) -> Result<()> {
        let canonical = std::fs::canonicalize(path)
            .with_context(|| format!("canonicalize required `{tool}` invocation"))?;
        if path.as_os_str() != canonical.as_os_str()
            || canonical.as_os_str() != self.selected_path(tool)?.as_os_str()
        {
            anyhow::bail!("required `{tool}` invocation is not the exact Xcode-selected payload");
        }
        Ok(())
    }

    pub(super) fn capture_tool(&self, tool: &str, args: &[&str]) -> Result<AppleToolCapture> {
        let selected = self.selected_tool(tool)?;
        let logical_args = args.iter().map(OsStr::new).collect::<Vec<_>>();
        let capability = self
            .runner
            .run("selected-capability", &selected.0, &logical_args, &[])?;
        let (capability_fact, capability_hash) =
            super::capability_facts(tool, args, &capability.output)?;

        let listing = self.runner.run(
            "direct-dependency-observation",
            Path::new("/usr/bin/otool"),
            &[
                OsStr::new("-arch"),
                OsStr::new(REQUESTED_ARCHITECTURE),
                OsStr::new("-L"),
                selected.0.as_os_str(),
            ],
            &[],
        )?;
        require_success(&listing, "ARM64 direct dependency observation")?;
        let dependency_count = parse_direct_dependency_listing(
            selected.0.as_os_str().as_bytes(),
            &listing.output.stdout,
        )?
        .len();

        let mut runtime =
            BinaryFrame::new(b"bbnf.apple-arm.direct-declared-dependency-runtime-hash.v2");
        runtime.push(b"policy", POLICY.as_bytes());
        runtime.push(b"capture-context", &self.context_transcript);
        runtime.push_os(b"selected-payload-path", selected.0.as_os_str());
        runtime.push(b"selected-payload-hash", selected.1.as_str().as_bytes());
        runtime.push(b"direct-dependency-command", &listing.transcript);
        let dynamic_runtime_hash = Sha256Digest::of(&runtime.bytes);
        let capability_command_hash = Sha256Digest::of(&capability.transcript);
        let dependency_hash = Sha256Digest::of(&listing.transcript);
        let launcher_path_hash = canonical_os_digest(
            "launcher-path",
            0,
            self.runner.launcher.as_os_str().as_bytes(),
        );
        let selected_path_hash = canonical_os_digest(
            "selected-program-path",
            0,
            selected.0.as_os_str().as_bytes(),
        );

        let mut execution_contract = canonical_child_policy("apple-arm-capability-launch")?;
        execution_contract.extend(
            [
                format!("apple-arm-launch:policy-version={POLICY}"),
                format!(
                    "apple-arm-launch:capture-context-sha256={}",
                    self.context_hash
                ),
                format!("apple-arm-launch:capability-command-sha256={capability_command_hash}"),
                format!("apple-arm-launch:direct-dependency-command-sha256={dependency_hash}"),
                format!("apple-arm-launch:direct-dependency-count={dependency_count}"),
                format!("apple-arm-launch:launcher-path[0]-sha256={launcher_path_hash}"),
                format!("apple-arm-launch:selected-program-path[0]-sha256={selected_path_hash}"),
            ]
            .into_iter()
            .map(CanonicalText::new)
            .collect::<Result<Vec<_>>>()?,
        );
        Ok(AppleToolCapture {
            capability: capability_fact,
            capability_hash,
            selected_hash: selected.1.clone(),
            execution_contract,
            toolchain_selection_hash: self.context_hash.clone(),
            dynamic_runtime_hash,
        })
    }

    pub(super) fn verify_payloads_unchanged(&self) -> Result<()> {
        for (path, initial_hash) in self.selected.values() {
            if hash_file(path)? != *initial_hash {
                anyhow::bail!("required Xcode-selected payload changed during capture");
            }
        }
        Ok(())
    }

    pub(super) fn generic_runtime_hash(
        &self,
        path: &Path,
        executable_hash: &Sha256Digest,
    ) -> Sha256Digest {
        let mut frame = BinaryFrame::new(b"bbnf.apple-arm.generic-runtime-context.v2");
        frame.push(b"policy", POLICY.as_bytes());
        frame.push(b"capture-context", &self.context_transcript);
        frame.push_os(b"payload-path", path.as_os_str());
        frame.push(b"payload-hash", executable_hash.as_str().as_bytes());
        Sha256Digest::of(&frame.bytes)
    }

    pub(super) fn host_facts(&self) -> Result<Vec<CanonicalText>> {
        let hash = &self.context_hash;
        Ok(vec![CanonicalText::new(format!(
            "apple-arm-host:context-sha256={hash}"
        ))?])
    }

    fn selected_tool(&self, tool: &str) -> Result<&(PathBuf, Sha256Digest)> {
        self.selected
            .get(tool)
            .with_context(|| format!("`{tool}` is not an Xcode-selected build tool"))
    }
}

fn require_success(observation: &CommandObservation, name: &str) -> Result<()> {
    if !observation.output.status.success() {
        anyhow::bail!("{name} is unavailable");
    }
    Ok(())
}

fn parse_path_record(bytes: &[u8]) -> Result<OsString> {
    let record = bytes.strip_suffix(b"\n").unwrap_or(bytes);
    if bytes.contains(&0) || record.is_empty() || record.contains(&b'\n') {
        anyhow::bail!("Apple selector/resolver output is not exactly one nonempty path record");
    }
    Ok(OsString::from_vec(record.to_vec()))
}

fn coherent_selection_environment(
    selected: &OsStr,
    inputs: &[&OsStr],
) -> Result<Vec<(OsString, OsString)>> {
    if inputs.len() != 3 || inputs.iter().any(|input| *input != selected) {
        anyhow::bail!("Xcode resolver inputs split the selected developer directory");
    }
    let name = OsString::from("DEVELOPER_DIR");
    Ok(vec![(name, selected.to_os_string())])
}

fn parse_direct_dependency_listing(root: &[u8], stdout: &[u8]) -> Result<Vec<Vec<u8>>> {
    if stdout.is_empty() || !stdout.ends_with(b"\n") || stdout.contains(&0) {
        anyhow::bail!("otool direct dependency listing is empty or truncated");
    }
    let complete_records = &stdout[..stdout.len() - 1];
    let mut lines = complete_records.split(|byte| *byte == b'\n');
    let header = lines.next().context("otool dependency header is absent")?;
    let mut expected_header = root.to_vec();
    expected_header.push(b':');
    if header != expected_header {
        anyhow::bail!("otool dependency listing has a wrong root or file slice");
    }
    let mut dependencies = Vec::new();
    for line in lines {
        let row = line
            .strip_prefix(b"\t")
            .context("otool dependency listing contains an extra or unparsed row")?;
        let marker = b" (compatibility version ";
        let marker_index = row
            .windows(marker.len())
            .rposition(|window| window == marker)
            .context("otool dependency row lacks compatibility metadata")?;
        let path = &row[..marker_index];
        let metadata = &row[marker_index + marker.len()..];
        let current_marker = b", current version ";
        let current_index = metadata
            .windows(current_marker.len())
            .position(|window| window == current_marker)
            .context("otool dependency row lacks current-version metadata")?;
        let compatibility = &metadata[..current_index];
        let current = metadata[current_index + current_marker.len()..]
            .strip_suffix(b")")
            .context("otool dependency row is truncated")?;
        let current = [b", weak".as_slice(), b", reexport", b", upward", b", lazy"]
            .into_iter()
            .find_map(|modifier| current.strip_suffix(modifier))
            .unwrap_or(current);
        if path.is_empty() || !valid_version(compatibility) || !valid_version(current) {
            anyhow::bail!("otool dependency row is malformed");
        }
        dependencies.push(path.to_vec());
    }
    if dependencies.is_empty() {
        anyhow::bail!("otool dependency listing contains no dependency row");
    }
    Ok(dependencies)
}

fn valid_version(version: &[u8]) -> bool {
    let mut parts = version.split(|byte| *byte == b'.');
    let valid = [65_535u32, 255, 255].into_iter().all(|limit| {
        parts.next().is_some_and(|part| {
            !part.is_empty()
                && (part.len() == 1 || part[0] != b'0')
                && part.iter().all(u8::is_ascii_digit)
                && std::str::from_utf8(part)
                    .ok()
                    .and_then(|digits| digits.parse::<u32>().ok())
                    .is_some_and(|value| value <= limit)
        })
    });
    valid && parts.next().is_none()
}

fn canonical_os_digest(label: &str, index: usize, value: &[u8]) -> Sha256Digest {
    let mut frame = BinaryFrame::new(b"bbnf.apple-arm.canonical-text-field.v2");
    frame.push(b"field-label", label.as_bytes());
    frame.push(
        b"field-index",
        &u64::try_from(index).expect("field index").to_be_bytes(),
    );
    frame.push(b"field-value", value);
    Sha256Digest::of(&frame.bytes)
}

#[cfg(feature = "architecture-test-seams")]
pub fn apple_binary_frame_for_test(domain: &[u8], fields: &[(&[u8], &[u8])]) -> Vec<u8> {
    let mut frame = BinaryFrame::new(domain);
    for (tag, value) in fields {
        frame.push(tag, value);
    }
    frame.bytes
}
#[cfg(feature = "architecture-test-seams")]
pub fn apple_canonical_os_digest_for_test(label: &str, index: usize, value: &[u8]) -> Sha256Digest {
    canonical_os_digest(label, index, value)
}
#[cfg(feature = "architecture-test-seams")]
pub fn apple_physical_argv_for_test(program: &[u8], logical_args: &[&[u8]]) -> Vec<Vec<u8>> {
    let logical_args = logical_args
        .iter()
        .map(|argument| OsStr::from_bytes(argument))
        .collect::<Vec<_>>();
    physical_argv(OsStr::from_bytes(program), &logical_args)
        .into_iter()
        .map(OsString::into_vec)
        .collect()
}
#[cfg(feature = "architecture-test-seams")]
pub fn apple_selector_record_for_test(stdout: &[u8]) -> Result<Vec<u8>> {
    Ok(parse_path_record(stdout)?.into_vec())
}

#[cfg(feature = "architecture-test-seams")]
pub fn apple_selection_inputs_for_test(selected: &[u8], inputs: &[Vec<u8>]) -> Result<()> {
    let selected = OsStr::from_bytes(selected);
    let inputs = inputs
        .iter()
        .map(|input| OsStr::from_bytes(input))
        .collect::<Vec<_>>();
    coherent_selection_environment(selected, &inputs).map(|_| ())
}

#[cfg(feature = "architecture-test-seams")]
pub fn apple_direct_dependencies_for_test(root: &[u8], stdout: &[u8]) -> Result<Vec<Vec<u8>>> {
    parse_direct_dependency_listing(root, stdout)
}
