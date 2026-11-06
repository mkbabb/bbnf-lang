use anyhow::{Context, Result};

use crate::architecture::inventory::{Inventory, ResolvedNomination, WorktreeRecord};
use crate::architecture::manifest::ConstellationManifest;
use crate::architecture::schema::{
    Attribute, CanonicalText, Condition, EdgeKind, GraphArtifact, Identity, NodeKind,
    ObligationCode, PhysicalKind, PipelineStage, ProjectionName, ResolverId, SourceAnchor,
    exact_edge, exact_node,
};

use super::unknown;

pub fn collect(
    manifest: &ConstellationManifest,
    inventory: &Inventory,
    worktrees: &[WorktreeRecord],
    nominations: &[ResolvedNomination],
    graph: &mut GraphArtifact,
) -> Result<()> {
    let resolver = ResolverId::new("constellation-manifest-v3.1")?;
    let synthesized = SourceAnchor::Synthesized {
        authority: CanonicalText::new("constellation-manifest-v3.1")?,
    };
    for repository in &manifest.repositories {
        graph.exact(exact_node(
            Identity::Repository {
                repository: repository.id.clone(),
            },
            NodeKind::Repository,
            repository.id.as_str(),
            Vec::new(),
        )?);
    }
    for product in &manifest.products {
        let identity = Identity::Product {
            product: product.id.clone(),
        };
        graph.exact(exact_node(
            identity.clone(),
            NodeKind::Product,
            product.id.as_str(),
            Vec::new(),
        )?);
        edge(
            graph,
            Identity::Repository {
                repository: product.repository.clone(),
            },
            identity,
            EdgeKind::RepositoryContainsProduct,
            &synthesized,
            &resolver,
        )?;
    }
    for workspace in &manifest.workspaces {
        let identity = Identity::Workspace {
            workspace: workspace.id.clone(),
        };
        graph.exact(exact_node(
            identity.clone(),
            NodeKind::Workspace,
            workspace.id.as_str(),
            Vec::new(),
        )?);
        edge(
            graph,
            Identity::Repository {
                repository: workspace.repository.clone(),
            },
            identity.clone(),
            EdgeKind::RepositoryContainsWorkspace,
            &synthesized,
            &resolver,
        )?;
        let product = manifest
            .products
            .iter()
            .find(|product| product.workspaces.contains(&workspace.id))
            .context("validated workspace owner")?;
        edge(
            graph,
            Identity::Product {
                product: product.id.clone(),
            },
            identity,
            EdgeKind::ProductContainsWorkspace,
            &synthesized,
            &resolver,
        )?;
    }
    for face in &manifest.package_faces {
        let identity = Identity::PackageFace {
            package_face: face.id.clone(),
        };
        graph.exact(exact_node(
            identity.clone(),
            NodeKind::PackageFace,
            face.id.as_str(),
            Vec::new(),
        )?);
        for (from, kind) in [
            (
                Identity::Workspace {
                    workspace: face.workspace.clone(),
                },
                EdgeKind::WorkspaceContainsPackageFace,
            ),
            (
                Identity::Product {
                    product: face.product.clone(),
                },
                EdgeKind::ProductContainsPackageFace,
            ),
        ] {
            edge(graph, from, identity.clone(), kind, &synthesized, &resolver)?;
        }
        for (path, kind) in [
            (&face.root, EdgeKind::PackageFaceRoot),
            (&face.manifest, EdgeKind::PackageFaceManifest),
        ] {
            let physical = inventory
                .find(&face.repository, path)
                .context("validated package-face physical identity")?;
            edge(
                graph,
                identity.clone(),
                Identity::Physical {
                    physical: physical.identity.clone(),
                },
                kind,
                &synthesized,
                &resolver,
            )?;
        }
    }
    for context in &manifest.contexts {
        let identity = Identity::Context {
            context: context.id.clone(),
        };
        graph.exact(exact_node(
            identity.clone(),
            NodeKind::Context,
            context.id.as_str(),
            vec![Attribute::ContextTarget {
                target: context.target.clone(),
                profile: context.profile.clone(),
            }],
        )?);
        for (from, kind) in [
            (
                Identity::Repository {
                    repository: context.repository.clone(),
                },
                EdgeKind::RepositoryContainsContext,
            ),
            (
                Identity::Workspace {
                    workspace: context.workspace.clone(),
                },
                EdgeKind::WorkspaceContainsContext,
            ),
        ] {
            edge(graph, from, identity.clone(), kind, &synthesized, &resolver)?;
        }
        for face in &context.package_faces {
            edge(
                graph,
                identity.clone(),
                Identity::PackageFace {
                    package_face: face.clone(),
                },
                EdgeKind::ContextUsesPackageFace,
                &synthesized,
                &resolver,
            )?;
        }
        for (path, kind) in std::iter::once((&context.manifest, EdgeKind::ContextUsesManifest))
            .chain(std::iter::once((&context.lock, EdgeKind::ContextUsesLock)))
            .chain(
                context
                    .config_inputs
                    .iter()
                    .map(|path| (path, EdgeKind::ContextUsesConfig)),
            )
        {
            let physical = inventory
                .find(&context.repository, path)
                .context("validated context physical input")?;
            edge(
                graph,
                identity.clone(),
                Identity::Physical {
                    physical: physical.identity.clone(),
                },
                kind,
                &synthesized,
                &resolver,
            )?;
        }
    }
    add_documentation(manifest, inventory, graph, &resolver)?;
    add_physical(inventory, graph, &synthesized)?;
    add_worktrees(worktrees, nominations, graph, &synthesized, &resolver)?;
    graph.unknown(unknown(
        ObligationCode::MovementAuthorizationReceiptAbsent,
        "movement-authorization",
        vec![
            ProjectionName::Containment,
            ProjectionName::RustModule,
            ProjectionName::CargoDeclared,
            ProjectionName::CargoResolved,
            ProjectionName::FrontendBuild,
            ProjectionName::RuntimeDelivery,
            ProjectionName::TestOverlay,
            ProjectionName::GeneratedProvenance,
            ProjectionName::Ownership,
            ProjectionName::Call,
            ProjectionName::Documentation,
        ],
        PipelineStage::Foundation,
        SourceAnchor::Synthesized {
            authority: CanonicalText::new("p3.1-global-movement-latch")?,
        },
        "no separate versioned hostile-review and adjudication authorization receipt exists",
    )?);
    Ok(())
}

fn add_documentation(
    manifest: &ConstellationManifest,
    inventory: &Inventory,
    graph: &mut GraphArtifact,
    resolver: &ResolverId,
) -> Result<()> {
    for document in &manifest.documentation {
        let identity = Identity::Documentation {
            documentation: document.id.clone(),
        };
        graph.exact(exact_node(
            identity.clone(),
            NodeKind::Documentation,
            document.id.as_str(),
            vec![Attribute::DocumentationRole {
                role: CanonicalText::new(format!("{:?}", document.role).to_ascii_lowercase())?,
            }],
        )?);
        let physical = inventory
            .find(&document.repository, &document.path)
            .context("validated documentation path")?;
        edge(
            graph,
            identity.clone(),
            Identity::Physical {
                physical: physical.identity.clone(),
            },
            EdgeKind::DocumentationPhysical,
            &SourceAnchor::Documentation {
                documentation: document.id.clone(),
            },
            resolver,
        )?;
        if let Some(authority) = &document.superseded_by {
            edge(
                graph,
                identity,
                Identity::Documentation {
                    documentation: authority.clone(),
                },
                EdgeKind::DocumentationSupersedes,
                &SourceAnchor::Documentation {
                    documentation: document.id.clone(),
                },
                resolver,
            )?;
        }
    }
    Ok(())
}

fn add_physical(
    inventory: &Inventory,
    graph: &mut GraphArtifact,
    source: &SourceAnchor,
) -> Result<()> {
    for record in &inventory.records {
        let kind = match record.identity.kind {
            PhysicalKind::File => NodeKind::PhysicalFile,
            PhysicalKind::Directory => NodeKind::PhysicalDirectory,
            PhysicalKind::Symlink => NodeKind::PhysicalSymlink,
        };
        let mut attributes = vec![Attribute::PhysicalKind {
            physical_kind: record.identity.kind.clone(),
        }];
        if let Some(digest) = &record.content_hash {
            attributes.push(Attribute::ContentHash {
                digest: digest.clone(),
            });
        }
        attributes.extend(
            record
                .semantic_faces
                .iter()
                .cloned()
                .map(|package_face| Attribute::PackageFace { package_face }),
        );
        let identity = Identity::Physical {
            physical: record.identity.clone(),
        };
        graph.exact(exact_node(
            identity.clone(),
            kind,
            record.identity.path.as_str(),
            attributes,
        )?);
        edge(
            graph,
            Identity::Repository {
                repository: record.identity.repository.clone(),
            },
            identity,
            EdgeKind::RepositoryOwnsPhysical,
            source,
            &ResolverId::new("filesystem-inventory-v3.1")?,
        )?;
    }
    Ok(())
}

fn add_worktrees(
    worktrees: &[WorktreeRecord],
    nominations: &[ResolvedNomination],
    graph: &mut GraphArtifact,
    source: &SourceAnchor,
    resolver: &ResolverId,
) -> Result<()> {
    for record in worktrees {
        let identity = Identity::Worktree {
            worktree: record.identity.clone(),
        };
        let mut attributes = vec![
            Attribute::ContentHash {
                digest: record.content_hash.clone(),
            },
            Attribute::WorktreeAdministrativeKey {
                key: record.administrative_key.clone(),
            },
            Attribute::WorktreeHead {
                head: record.head.clone(),
            },
            Attribute::WorktreeState {
                detached: record.detached,
                bare: record.bare,
                dirty: record.dirty,
            },
            Attribute::IgnoredContentPolicy {
                policy: record.ignored_content_policy.clone(),
            },
        ];
        if let Some(branch) = &record.branch {
            attributes.push(Attribute::WorktreeBranch {
                branch: branch.clone(),
            });
        }
        graph.exact(exact_node(
            identity.clone(),
            NodeKind::Worktree,
            record.identity.as_str(),
            attributes,
        )?);
        edge(
            graph,
            Identity::Repository {
                repository: record.repository.clone(),
            },
            identity,
            EdgeKind::RepositoryOwnsWorktree,
            source,
            &ResolverId::new("git-worktree-truth-v3.1")?,
        )?;
    }
    for nomination in nominations {
        graph.exact(exact_edge(
            Identity::Product {
                product: nomination.product.clone(),
            },
            Identity::Worktree {
                worktree: nomination.worktree.clone(),
            },
            EdgeKind::ProductNominatesWorktree,
            Condition::Always,
            SourceAnchor::Synthesized {
                authority: CanonicalText::new(format!(
                    "worktree-nomination:{}",
                    nomination.nomination
                ))?,
            },
            resolver.clone(),
            vec![Attribute::Nomination {
                nomination: nomination.nomination.clone(),
            }],
        )?);
    }
    Ok(())
}

fn edge(
    graph: &mut GraphArtifact,
    from: Identity,
    to: Identity,
    kind: EdgeKind,
    source: &SourceAnchor,
    resolver: &ResolverId,
) -> Result<()> {
    graph.exact(exact_edge(
        from,
        to,
        kind,
        Condition::Always,
        source.clone(),
        resolver.clone(),
        Vec::new(),
    )?);
    Ok(())
}
