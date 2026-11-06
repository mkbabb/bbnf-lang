use xtask::architecture::schema::{
    Attribute, CanonicalText, Condition, ContextId, EdgeIdentity, EdgeKind, Fact, GraphArtifact,
    Identity, NodeKind, Obligation, ObligationCode, ObligationSubject, PipelineStage, ProductId,
    ProjectionName, RepoPath, RepositoryId, ResolverId, Sha256Digest, SourceAnchor, Truth,
    exact_edge, exact_node,
};
use xtask::architecture::test_support::{
    percent_decode_allocation_bound_for_test, percent_decode_for_test, portable_violation_for_test,
};

#[test]
fn digest_wire_types_reject_noncanonical_and_malformed_values() {
    let lower = "0123456789abcdef".repeat(4);
    let digest: Sha256Digest =
        serde_json::from_str(&format!("\"{lower}\"")).expect("canonical digest");
    assert_eq!(digest.as_str(), lower);
    assert!(
        serde_json::from_str::<Sha256Digest>(&format!("\"{}\"", lower.to_uppercase())).is_err()
    );
    assert!(serde_json::from_str::<Sha256Digest>("\"abcd\"").is_err());
    assert!(serde_json::from_str::<Sha256Digest>(&format!("\"{}g\"", &lower[..63])).is_err());

    let edge = format!("edge:sha256:{lower}");
    let identity: EdgeIdentity =
        serde_json::from_str(&format!("\"{edge}\"")).expect("canonical edge identity");
    assert_eq!(identity.as_str(), edge);
    for malformed in [
        lower.clone(),
        format!("edge:sha256:{}", lower.to_uppercase()),
        format!("edge:sha512:{lower}"),
        format!("edge:sha256:{lower}:suffix"),
    ] {
        assert!(
            serde_json::from_str::<EdgeIdentity>(&format!("\"{malformed}\"")).is_err(),
            "accepted malformed edge identity {malformed}"
        );
    }
}

#[test]
fn node_identity_kind_compatibility_is_total_at_construction_and_canonicalization() {
    let repository = Identity::Repository {
        repository: RepositoryId::new("repo").expect("repository"),
    };
    assert!(exact_node(repository.clone(), NodeKind::Product, "wrong", Vec::new()).is_err());
    assert!(
        exact_node(
            Identity::Product {
                product: ProductId::new("product").expect("product"),
            },
            NodeKind::Repository,
            "wrong",
            Vec::new(),
        )
        .is_err()
    );

    let mut forged = match exact_node(repository, NodeKind::Repository, "repo", Vec::new())
        .expect("valid node")
    {
        Fact::Node { node } => node,
        Fact::Edge { .. } => unreachable!("node constructor"),
    };
    forged.kind = NodeKind::Product;
    let mut graph = GraphArtifact::new();
    graph.exact(Fact::Node { node: forged });
    assert!(graph.canonicalize().is_err());
    assert!(graph.validate_canonical().is_err());
}

#[test]
fn canonicalize_normalizes_every_authorized_nested_identity_vector() {
    fn text(value: &str) -> CanonicalText {
        CanonicalText::new(value).expect("canonical text")
    }

    fn identity(value: &str) -> Identity {
        Identity::Virtual {
            namespace: text("nested-law"),
            key: text(value),
        }
    }

    fn feature(value: &str) -> Attribute {
        Attribute::Feature {
            feature: text(value),
        }
    }

    let unordered_attributes = vec![feature("z"), feature("a"), feature("a")];

    let mut node_graph = GraphArtifact::new();
    let Fact::Node { mut node } = exact_node(
        identity("node"),
        NodeKind::VirtualArtifact,
        "node",
        Vec::new(),
    )
    .expect("node") else {
        unreachable!("node constructor")
    };
    node.attributes = unordered_attributes.clone();
    node_graph.exact(Fact::Node { node });
    assert!(node_graph.validate_canonical().is_err());
    node_graph
        .canonicalize()
        .expect("normalize node attributes");
    assert_eq!(
        node_graph.nodes()[0].attributes,
        vec![feature("a"), feature("z")]
    );
    node_graph
        .validate_canonical()
        .expect("canonical node attributes");

    fn make_edge_graph(condition: Condition, attributes: Vec<Attribute>) -> GraphArtifact {
        let mut graph = GraphArtifact::new();
        for key in ["from", "to"] {
            graph.exact(
                exact_node(identity(key), NodeKind::VirtualArtifact, key, Vec::new())
                    .expect("endpoint"),
            );
        }
        graph.exact(
            exact_edge(
                identity("from"),
                identity("to"),
                EdgeKind::RustUses,
                condition,
                SourceAnchor::Synthesized {
                    authority: text("nested-law"),
                },
                ResolverId::new("nested-law").expect("resolver"),
                attributes,
            )
            .expect("edge"),
        );
        graph.canonicalize().expect("initial graph");
        graph
    }

    let mut edge_graph = make_edge_graph(Condition::Always, Vec::new());
    let edge = edge_graph
        .truth
        .iter_mut()
        .find_map(|truth| match truth {
            Truth::Exact {
                fact: Fact::Edge { edge },
            } => Some(edge),
            _ => None,
        })
        .expect("edge");
    edge.record.attributes = unordered_attributes.clone();
    assert!(edge_graph.validate_canonical().is_err());
    edge_graph
        .canonicalize()
        .expect("normalize edge attributes");
    let edge = edge_graph.edges()[0];
    assert_eq!(edge.record.attributes, vec![feature("a"), feature("z")]);
    edge.validate_identity().expect("normalized edge identity");
    edge_graph
        .validate_canonical()
        .expect("canonical edge attributes");

    let mut cargo_graph = make_edge_graph(
        Condition::Cargo {
            context: ContextId::new("nested-law").expect("context"),
            target: text("target"),
            profile: text("profile"),
            features: Vec::new(),
        },
        Vec::new(),
    );
    let cargo_edge = cargo_graph
        .truth
        .iter_mut()
        .find_map(|truth| match truth {
            Truth::Exact {
                fact: Fact::Edge { edge },
            } => Some(edge),
            _ => None,
        })
        .expect("Cargo edge");
    let Condition::Cargo { features, .. } = &mut cargo_edge.record.condition else {
        unreachable!("Cargo condition")
    };
    *features = vec![text("z"), text("a"), text("a")];
    assert!(cargo_graph.validate_canonical().is_err());
    cargo_graph
        .canonicalize()
        .expect("normalize Cargo features");
    let Condition::Cargo { features, .. } = &cargo_graph.edges()[0].record.condition else {
        unreachable!("Cargo condition")
    };
    assert_eq!(features, &vec![text("a"), text("z")]);
    cargo_graph
        .validate_canonical()
        .expect("canonical Cargo features");

    let mut frontend_graph = make_edge_graph(
        Condition::Frontend {
            context: ContextId::new("nested-law").expect("context"),
            export_conditions: Vec::new(),
            query: None,
        },
        Vec::new(),
    );
    let frontend_edge = frontend_graph
        .truth
        .iter_mut()
        .find_map(|truth| match truth {
            Truth::Exact {
                fact: Fact::Edge { edge },
            } => Some(edge),
            _ => None,
        })
        .expect("frontend edge");
    let Condition::Frontend {
        export_conditions, ..
    } = &mut frontend_edge.record.condition
    else {
        unreachable!("frontend condition")
    };
    *export_conditions = vec![text("z"), text("a"), text("a")];
    assert!(frontend_graph.validate_canonical().is_err());
    frontend_graph
        .canonicalize()
        .expect("normalize frontend export conditions");
    let Condition::Frontend {
        export_conditions, ..
    } = &frontend_graph.edges()[0].record.condition
    else {
        unreachable!("frontend condition")
    };
    assert_eq!(export_conditions, &vec![text("a"), text("z")]);
    frontend_graph
        .validate_canonical()
        .expect("canonical frontend conditions");

    let mut obligation_graph = GraphArtifact::new();
    let mut obligation = Obligation::required(
        ObligationCode::RustSemanticsUnproven,
        ObligationSubject::new("nested-law").expect("subject"),
        vec![ProjectionName::RustModule],
        PipelineStage::Rust,
        SourceAnchor::Synthesized {
            authority: text("nested-law"),
        },
        text("nested law"),
    )
    .expect("obligation");
    obligation.applicable_projections = vec![
        ProjectionName::RuntimeDelivery,
        ProjectionName::Call,
        ProjectionName::Call,
    ];
    obligation_graph.unknown(obligation);
    assert!(obligation_graph.validate_canonical().is_err());
    obligation_graph
        .canonicalize()
        .expect("normalize obligation projections");
    assert_eq!(
        obligation_graph.obligations()[0].applicable_projections,
        vec![ProjectionName::Call, ProjectionName::RuntimeDelivery]
    );
    obligation_graph
        .validate_canonical()
        .expect("canonical obligation projections");
}

#[test]
fn typed_identity_and_paths_reject_nonportable_values() {
    assert!(RepositoryId::new("/Users/private/repo").is_err());
    assert!(RepoPath::new("../escape").is_err());
    assert!(CanonicalText::new("/private/tmp/leak").is_err());
}

#[test]
fn repository_paths_have_one_strict_constructor_and_wire_form() {
    for malformed in [
        "",
        "\\",
        "src\\lib.rs",
        "./src/lib.rs",
        "src/./lib.rs",
        "src/../lib.rs",
        "src//lib.rs",
        "src/lib.rs/",
    ] {
        assert!(
            RepoPath::new(malformed).is_err(),
            "constructor accepted {malformed:?}"
        );
        assert!(
            serde_json::from_str::<RepoPath>(&format!("{malformed:?}")).is_err(),
            "wire accepted {malformed:?}"
        );
    }
    assert_eq!(RepoPath::root().as_str(), ".");
    assert_eq!(
        serde_json::from_str::<RepoPath>("\".\"")
            .expect("canonical root")
            .as_str(),
        "."
    );
    assert_eq!(
        RepoPath::new("src/lib.rs")
            .expect("canonical path")
            .as_str(),
        "src/lib.rs"
    );
}

#[test]
fn relative_path_ingress_is_strict_fallible_and_unicode_preserving() {
    use std::path::Path;

    assert_eq!(
        RepoPath::from_relative_path(Path::new(".")).expect("explicit root"),
        RepoPath::root()
    );
    assert_eq!(
        RepoPath::from_relative_path(Path::new("src/éclat.rs"))
            .expect("Unicode path")
            .as_str(),
        "src/éclat.rs"
    );
    assert!(RepoPath::from_relative_path(Path::new("../escape")).is_err());
    assert!(RepoPath::from_relative_path(Path::new("/absolute")).is_err());

    #[cfg(unix)]
    {
        use std::os::unix::ffi::OsStringExt;
        let invalid = [
            std::ffi::OsString::from_vec(vec![b'a', 0xff]),
            std::ffi::OsString::from_vec(vec![b'a', 0xfe]),
        ];
        assert_eq!(invalid[0].to_string_lossy(), invalid[1].to_string_lossy());
        for path in invalid.map(std::path::PathBuf::from) {
            assert!(RepoPath::from_relative_path(&path).is_err());
        }
    }
}

#[test]
fn f17_privacy_rejects_embedded_cross_platform_and_encoded_paths_but_keeps_https() {
    for private in [
        "/home/reviewer/secret.rs",
        "failed at /var/folders/reviewer/secret.rs",
        "source=C:/reviewer/secret.rs",
        "source C:\\reviewer\\secret.rs",
        "source \\\\server\\share\\secret.rs",
        "source //server/share/secret.rs",
        "source file:///etc/private-source",
        "source FILE:%2F%2F%2Fetc%2Fprivate-source",
        "source file%3A%2F%2F%2Fetc%2Fprivate-source",
        "source file%25253A%25252F%25252F%25252Fetc%25252Fshadow",
        "error>/home/reviewer/secret.rs",
        "error|C:/reviewer/secret.rs",
        "error<//server/share/secret.rs",
        "source ~/private-source",
        "/",
        "prefix/Users/reviewer/secret",
        "source ~reviewer/private-source",
    ] {
        let error = CanonicalText::new(private).expect_err(private);
        assert!(
            error.to_string().contains("PRIVACY_VALIDATOR_INCOMPLETE"),
            "{private}: {error:#}"
        );
    }
    CanonicalText::new("https://example.invalid/a/b?next=relative").expect("ordinary non-file URL");
    RepoPath::new("repository/relative/path.rs").expect("repository-relative path");
    CanonicalText::new("profile:dev").expect("non-URI file substring");
}

fn repeated_percent_decode(mut value: String) -> String {
    for _ in 0..=value.len() {
        let bytes = value.as_bytes();
        let mut decoded = Vec::with_capacity(bytes.len());
        let mut index = 0;
        let mut changed = false;
        while index < bytes.len() {
            let nibble = |byte| match byte {
                b'0'..=b'9' => Some(byte - b'0'),
                b'a'..=b'f' => Some(byte - b'a' + 10),
                b'A'..=b'F' => Some(byte - b'A' + 10),
                _ => None,
            };
            if bytes[index] == b'%'
                && index + 2 < bytes.len()
                && let (Some(high), Some(low)) =
                    (nibble(bytes[index + 1]), nibble(bytes[index + 2]))
            {
                decoded.push((high << 4) | low);
                index += 3;
                changed = true;
            } else {
                decoded.push(bytes[index]);
                index += 1;
            }
        }
        if !changed {
            break;
        }
        value = String::from_utf8_lossy(&decoded).into_owned();
    }
    value
}

// Diagnostic regression ceiling for the decoder's sampled loop counter. This
// is not the universal work admission proved by the source ledger.
const SAMPLED_PERCENT_DECODE_LOOP_STEPS_PER_BYTE: usize = 15;

#[test]
fn percent_fixpoint_is_exhaustively_equivalent_and_linear() {
    const ALPHABET: &[u8] = b"%25AFx";
    for length in 0usize..=6 {
        let cases = ALPHABET.len().pow(length as u32);
        for mut encoded in 0..cases {
            let mut bytes = vec![0; length];
            for byte in &mut bytes {
                *byte = ALPHABET[encoded % ALPHABET.len()];
                encoded /= ALPHABET.len();
            }
            let value = String::from_utf8(bytes).expect("ASCII fixture");
            let (actual, observed_loop_steps) = percent_decode_for_test(&value);
            assert_eq!(actual, repeated_percent_decode(value.clone()), "{value:?}");
            assert!(
                observed_loop_steps <= value.len() * SAMPLED_PERCENT_DECODE_LOOP_STEPS_PER_BYTE,
                "{value:?}: {observed_loop_steps}"
            );
        }
    }

    for value in [
        "%C2%25A9",
        "%FF",
        "%E2%82%AC",
        "%25%32%35",
        "%FF%FF%FF%25FF",
        "%25FF%2525%41%FF",
        "%FF%25%32%35",
        "%FF%2525",
    ] {
        let (decoded, observed_loop_steps) = percent_decode_for_test(value);
        assert_eq!(
            decoded,
            repeated_percent_decode(value.to_owned()),
            "{value:?}"
        );
        // This sampled counter is regression evidence only. The universal
        // admission proof is the checked D/I/A/Q/R/U/W/T/N/F source ledger.
        assert!(
            observed_loop_steps <= value.len() * SAMPLED_PERCENT_DECODE_LOOP_STEPS_PER_BYTE,
            "{value:?}: {observed_loop_steps}"
        );
    }

    for depth in [8, 64, 512, 4096] {
        let value = format!("%{}", "25".repeat(depth));
        let (decoded, observed_loop_steps) = percent_decode_for_test(&value);
        assert_eq!(decoded, "%");
        assert!(
            observed_loop_steps <= value.len() * SAMPLED_PERCENT_DECODE_LOOP_STEPS_PER_BYTE,
            "{observed_loop_steps} sampled steps for {} input bytes",
            value.len()
        );
    }

    let hostile = "%FF%25FF%C2%25A9".repeat(2048);
    let allocation_bound = percent_decode_allocation_bound_for_test(hostile.len())
        .expect("fixed decoder allocation bound");
    let (baseline, _) = crate::take_heap_usage();
    let (decoded, observed_loop_steps) = percent_decode_for_test(&hostile);
    let (_, peak) = crate::take_heap_usage();
    assert_eq!(decoded, repeated_percent_decode(hostile.clone()));
    assert!(observed_loop_steps <= hostile.len() * SAMPLED_PERCENT_DECODE_LOOP_STEPS_PER_BYTE);
    assert!(peak.saturating_sub(baseline) <= allocation_bound as u64);
}

#[test]
fn dense_adjacent_percent_triplets_have_one_linear_generation_ledger() {
    for groups in [64usize, 512, 4_096] {
        let encoded = "%41".repeat(groups);
        let (decoded, observed_loop_steps) = percent_decode_for_test(&encoded);
        assert_eq!(decoded, "A".repeat(groups));
        assert!(
            observed_loop_steps <= encoded.len() * SAMPLED_PERCENT_DECODE_LOOP_STEPS_PER_BYTE,
            "dense generation rescanned adjacent decoded nodes: groups={groups}, observed_loop_steps={observed_loop_steps}, sampled_bound={}",
            encoded.len() * SAMPLED_PERCENT_DECODE_LOOP_STEPS_PER_BYTE
        );
    }
}

fn repeated_portable_violation(value: &str) -> bool {
    let bytes = value.as_bytes();
    let boundary = |byte: u8| {
        !byte.is_ascii_alphanumeric() && !matches!(byte, b'_' | b'-' | b'.' | b'*' | b'?' | b']')
    };
    let windows_drive = bytes.windows(3).enumerate().any(|(index, triplet)| {
        triplet[0].is_ascii_alphabetic()
            && triplet[1] == b':'
            && matches!(triplet[2], b'/' | b'\\')
            && (index == 0 || boundary(bytes[index - 1]))
    });
    let tilde_path = bytes.iter().enumerate().any(|(index, byte)| {
        *byte == b'~'
            && (index == 0 || boundary(bytes[index - 1]))
            && bytes[index + 1..]
                .iter()
                .position(|byte| matches!(byte, b'/' | b'\\'))
                .is_some_and(|slash| {
                    bytes[index + 1..index + 1 + slash].iter().all(|byte| {
                        byte.is_ascii_alphanumeric() || matches!(byte, b'_' | b'-' | b'.')
                    })
                })
    });
    let posix_absolute = bytes.iter().enumerate().any(|(index, byte)| {
        *byte == b'/'
            && bytes.get(index + 1).is_some_and(|next| *next != b'/')
            && (index == 0
                || (bytes[index - 1] != b'/'
                    && (boundary(bytes[index - 1]) || bytes[index - 1] == b':')))
    });
    let forward_unc = bytes.windows(2).enumerate().any(|(index, pair)| {
        pair == b"//" && (index == 0 || (boundary(bytes[index - 1]) && bytes[index - 1] != b':'))
    });
    let known_posix_root = ["/Users/", "/home/", "/private/", "/tmp/", "/var/"]
        .iter()
        .any(|root| value.contains(root));
    let file_uri = bytes.windows(5).enumerate().any(|(index, token)| {
        token.eq_ignore_ascii_case(b"file:") && (index == 0 || boundary(bytes[index - 1]))
    });
    bytes.contains(&b'\0')
        || bytes.contains(&b'\r')
        || bytes.contains(&b'\\')
        || file_uri
        || windows_drive
        || tilde_path
        || value == "/"
        || posix_absolute
        || known_posix_root
        || forward_unc
}

#[test]
fn portable_scan_is_exhaustively_equivalent_and_one_pass() {
    const ALPHABET: &[u8] = b"aC:/~ f";
    for length in 0usize..=6 {
        let cases = ALPHABET.len().pow(length as u32);
        for mut encoded in 0..cases {
            let mut bytes = vec![0; length];
            for byte in &mut bytes {
                *byte = ALPHABET[encoded % ALPHABET.len()];
                encoded /= ALPHABET.len();
            }
            let value = String::from_utf8(bytes).expect("ASCII fixture");
            let (actual, operations) = portable_violation_for_test(&value);
            assert_eq!(actual, repeated_portable_violation(&value), "{value:?}");
            assert!(operations <= value.len(), "{value:?}");
        }
    }

    for value in [
        "/Users/owner",
        "/home/owner",
        "/private/item",
        "/tmp/item",
        "/var/item",
        "FILE:///item",
        "x~/relative",
        "x ~owner/private",
        "https://example.invalid/a",
    ] {
        assert_eq!(
            portable_violation_for_test(value).0,
            repeated_portable_violation(value),
            "{value:?}"
        );
    }

    for length in [64, 512, 4096, 32_768] {
        let value = " ~".repeat(length / 2);
        let (_, operations) = portable_violation_for_test(&value);
        assert_eq!(operations, value.len());
    }
}

#[test]
fn privacy_source_has_explicit_linear_dfa_and_checked_decoder_ledger() {
    let source = std::fs::read_to_string(
        std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("src/architecture/schema/identity.rs"),
    )
    .expect("identity source");
    for forbidden in [
        "copy_within",
        "windows(5)",
        "eq_ignore_ascii_case(b\"file:\")",
    ] {
        assert!(
            !source.contains(forbidden),
            "portable DFA retains repeated window work: {forbidden}"
        );
    }
    for required in [
        "PERCENT_DECODE_WORK_PER_BYTE",
        "percent_decode_work_bound",
        "successful_decodes",
        "invalid_decodes",
        "arena_nodes",
        "queue_actions",
        "generated_run_visits",
        "utf8_and_equality",
        "normalized_writes",
        "retirements",
        "neighborhood_visits",
        "final_copy",
        "is_created_replacement_byte",
        "checked_add",
        "checked_mul",
        "stale",
        "from_utf8_lossy",
    ] {
        assert!(
            source.contains(required),
            "decoder source proof is missing {required}"
        );
    }
}

#[test]
fn serialized_privacy_walk_is_total_for_nested_keys_values_and_escapes() {
    let private = serde_json::json!({
        "nested": [{"escaped": "source \\/private\\/artifact"}],
        "/private/key": "relative",
    });
    let error = xtask::architecture::schema::validate_serialized_strings(&private)
        .expect_err("nested private path");
    assert!(error.to_string().contains("PRIVACY_VALIDATOR_INCOMPLETE"));
    xtask::architecture::schema::validate_serialized_strings(&serde_json::json!({
        "url": "https://example.invalid/a/b",
        "path": "repository/relative.rs",
    }))
    .expect("portable nested strings");
}

#[test]
fn unknown_requires_and_expands_every_applicable_projection() {
    let obligation = Obligation::required(
        ObligationCode::RustSemanticsUnproven,
        ObligationSubject::new("fixture-rust").expect("subject"),
        vec![ProjectionName::RustModule, ProjectionName::Call],
        PipelineStage::Rust,
        SourceAnchor::Manifest {
            repository: RepositoryId::new("fixture").expect("repo"),
            path: RepoPath::new("Cargo.toml").expect("path"),
            key: CanonicalText::new("rust").expect("key"),
        },
        CanonicalText::new("bounded semantic plane").expect("reason"),
    )
    .expect("required Unknown");
    assert_eq!(obligation.applicable_projections.len(), 2);
    assert!(
        Obligation::required(
            ObligationCode::RustSemanticsUnproven,
            ObligationSubject::new("fixture-rust").expect("subject"),
            Vec::new(),
            PipelineStage::Rust,
            SourceAnchor::Synthesized {
                authority: CanonicalText::new("test").expect("authority"),
            },
            CanonicalText::new("silent").expect("reason"),
        )
        .is_err()
    );
    let _ = ContextId::new("typed-context").expect("context");
}
