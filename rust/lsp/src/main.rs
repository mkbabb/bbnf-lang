mod server;

// Re-export bbnf-analysis modules for use within this crate.
pub(crate) mod analysis {
    pub use bbnf_analysis::analysis::*;
}
pub(crate) mod state {
    pub use bbnf_analysis::state::*;
}
pub(crate) mod features {
    pub use bbnf_analysis::features::*;
}

use server::BbnfLanguageServer;
use tower_lsp_server::{LspService, Server};

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(BbnfLanguageServer::new);
    Server::new(stdin, stdout, socket).serve(service).await;
}

#[cfg(test)]
mod tests {
    use bbnf_analysis::analysis::LineIndex;
    use bbnf_analysis::state::diagnostics::analyze;

    #[test]
    fn test_analyze_parse_error() {
        let text = "value = number";
        let parser = bbnf::grammar::BBNFGrammar::grammar();
        let (result, parser_state) = parser.parse_return_state(text);
        eprintln!("Result is_some: {}", result.is_some());
        eprintln!("Offset: {} / {}", parser_state.offset, text.len());
        eprintln!("Furthest: {}", parser_state.furthest_offset);
        if let Some(ref ast) = result {
            eprintln!("AST len: {}", ast.len());
        }
        let line_index = LineIndex::new(text);
        let info = analyze(text, &line_index);
        eprintln!("Diagnostics: {:?}", info.diagnostics);
        eprintln!("Rules: {:?}", info.rules.iter().map(|r| &r.name).collect::<Vec<_>>());
        // Should produce a diagnostic
        assert!(
            !info.diagnostics.is_empty(),
            "Expected diagnostics for missing semicolon"
        );
    }

    #[test]
    fn test_analyze_valid() {
        let text = "value = /[0-9]+/;";
        let line_index = LineIndex::new(text);
        let info = analyze(text, &line_index);
        eprintln!("Diagnostics: {:?}", info.diagnostics);
        eprintln!("Rules: {:?}", info.rules.iter().map(|r| &r.name).collect::<Vec<_>>());
        assert!(info.diagnostics.is_empty(), "Expected no diagnostics for valid grammar");
        assert_eq!(info.rules.len(), 1);
    }

    #[test]
    fn test_analyze_json_grammar() {
        let grammar = r#"null = "null";
bool = "true" | "false";
number = /\-?(0|[1-9][0-9]*)(\.[0-9]+)?([eE][\+\-]?[0-9]+)?/;
char = /[^"\\]/ | /\\["\\/bfnrt]/ | /\\u[0-9a-fA-F]{4}/;
string = "\"" , { char } , "\"";
array = "[" , [ value , { "," , value } ] , "]";
pair = string , ":" , value;
object = "{" , [ pair , { "," , pair } ] , "}";
value = string | number | object | array | bool | null;"#;
        let line_index = LineIndex::new(grammar);
        let info = analyze(grammar, &line_index);
        eprintln!("Diagnostics: {:?}", info.diagnostics);
        eprintln!("Rules: {:?}", info.rules.iter().map(|r| &r.name).collect::<Vec<_>>());
    }

    #[test]
    fn test_analyze_recover_directive() {
        let grammar = r#"@recover stmt /[^;]*;/ ;

stmt = /[a-z]+/ , "=" , /[a-z]+/ , ";" ;
program = stmt * ;"#;
        let line_index = LineIndex::new(grammar);
        let info = analyze(grammar, &line_index);
        eprintln!("Diagnostics: {:?}", info.diagnostics);
        eprintln!("Recovers: {:?}", info.recovers);
        assert_eq!(info.recovers.len(), 1, "Should have 1 @recover directive");
        assert_eq!(info.recovers[0].rule_name, "stmt");
        let undefined_warnings: Vec<_> = info.diagnostics.iter()
            .filter(|d| d.message.contains("undefined") || d.message.contains("Undefined"))
            .collect();
        assert!(undefined_warnings.is_empty(), "Should not warn about @recover target: {:?}", undefined_warnings);
    }

    #[test]
    fn test_analyze_recover_undefined_target() {
        let grammar = r#"@recover nonexistent /[^;]*;/ ;

stmt = /[a-z]+/ , ";" ;"#;
        let line_index = LineIndex::new(grammar);
        let info = analyze(grammar, &line_index);
        eprintln!("Diagnostics: {:?}", info.diagnostics);
        let recover_warnings: Vec<_> = info.diagnostics.iter()
            .filter(|d| d.message.contains("@recover"))
            .collect();
        assert!(!recover_warnings.is_empty(), "Should warn about @recover targeting undefined rule");
    }

    #[test]
    fn test_analyze_simple_json_grammar() {
        let grammar = r#"null = "null";
bool = "true" | "false";
number = /[0-9]+/;
string = /[a-z]+/;
value = string | number | bool | null;"#;
        let line_index = LineIndex::new(grammar);
        let info = analyze(grammar, &line_index);
        eprintln!("Diagnostics: {:?}", info.diagnostics);
        eprintln!("Rules: {:?}", info.rules.iter().map(|r| &r.name).collect::<Vec<_>>());
        assert_eq!(info.rules.len(), 5, "Should have 5 rules");
    }

    #[test]
    fn test_analyze_no_collapse_directive() {
        let grammar = "@no_collapse items ;\n\nitems = /[a-z]+/ * ;\nprogram = items ;";
        let line_index = LineIndex::new(grammar);
        let info = analyze(grammar, &line_index);
        eprintln!("Diagnostics: {:?}", info.diagnostics);
        eprintln!("No collapses: {:?}", info.no_collapses);
        assert_eq!(info.no_collapses.len(), 1, "Should have 1 @no_collapse directive");
        assert_eq!(info.no_collapses[0].rule_name, "items");
        let undefined_warnings: Vec<_> = info.diagnostics.iter()
            .filter(|d| d.message.contains("undefined") || d.message.contains("Undefined"))
            .collect();
        assert!(undefined_warnings.is_empty(), "Should not warn about @no_collapse target: {:?}", undefined_warnings);
    }

    #[test]
    fn test_analyze_no_collapse_undefined_target() {
        let grammar = "@no_collapse nonexistent ;\n\nstmt = /[a-z]+/ ;";
        let line_index = LineIndex::new(grammar);
        let info = analyze(grammar, &line_index);
        eprintln!("Diagnostics: {:?}", info.diagnostics);
        let nc_warnings: Vec<_> = info.diagnostics.iter()
            .filter(|d| d.message.contains("@no_collapse"))
            .collect();
        assert!(!nc_warnings.is_empty(), "Should warn about @no_collapse targeting undefined rule");
    }

    #[test]
    fn test_no_collapse_prevents_unused_warning() {
        let grammar = "entry = items ;\n@no_collapse items ;\nitems = /[a-z]+/ * ;";
        let line_index = LineIndex::new(grammar);
        let info = analyze(grammar, &line_index);
        let unused: Vec<_> = info.diagnostics.iter()
            .filter(|d| d.message.contains("Unused rule") && d.message.contains("items"))
            .collect();
        assert!(unused.is_empty(), "items should not be flagged as unused: {:?}", unused);
    }
}
