use ls_types::*;

use crate::state::DocumentState;

/// Check if the cursor is over an @import directive or a selectively imported name.
pub(super) fn hover_import(state: &DocumentState, offset: usize) -> Option<Hover> {
    for imp in &state.info.imports {
        if offset < imp.span.0 || offset > imp.span.1 {
            continue;
        }

        // Check "@import" keyword (7 chars).
        let kw_end = imp.span.0 + 7;
        if offset >= imp.span.0 && offset < kw_end {
            let content = format!(
                "### `@import` — Module Import\n\n\
                 Imports rules from `\"{}\"`.\n\n\
                 - **Glob**: `@import \"path\" ;` imports all rules.\n\
                 - **Selective**: `@import {{ a, b }} from \"path\" ;` imports only named rules \
                 (transitive local deps are expanded automatically).\n",
                imp.path
            );
            return Some(Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: content,
                }),
                range: Some(state.line_index.span_to_range(imp.span.0, kw_end)),
            });
        }

        // Check selectively imported names.
        if let Some(ref items) = imp.items {
            for item in items {
                if offset >= item.span.0 && offset <= item.span.1 {
                    let content = format!("Imported rule `{}` from `\"{}\"`", item.name, imp.path);
                    return Some(Hover {
                        contents: HoverContents::Markup(MarkupContent {
                            kind: MarkupKind::Markdown,
                            value: content,
                        }),
                        range: Some(state.line_index.span_to_range(item.span.0, item.span.1)),
                    });
                }
            }
        }
    }
    None
}
