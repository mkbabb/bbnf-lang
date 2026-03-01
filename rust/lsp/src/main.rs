mod analysis;
mod server;
mod state;

mod features;

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
    use super::analysis::LineIndex;
    use super::state::analyze;

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
        // This grammar has regex patterns that currently cause a panic in the parser.
        // The analyze() function should catch the panic and return a diagnostic
        // instead of crashing.
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
        // Should not panic — either succeeds or produces diagnostics
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
        // Should have no "undefined rule" warnings for @recover target
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
        // Should have a warning about undefined rule target
        let recover_warnings: Vec<_> = info.diagnostics.iter()
            .filter(|d| d.message.contains("@recover"))
            .collect();
        assert!(!recover_warnings.is_empty(), "Should warn about @recover targeting undefined rule");
    }

    #[test]
    fn test_analyze_simple_json_grammar() {
        // Simplified JSON grammar without problematic regex patterns
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
        // null is first (entry), bool/number/string/value all referenced
        // Only unused rules should be value (not referenced by anything)
    }
}
