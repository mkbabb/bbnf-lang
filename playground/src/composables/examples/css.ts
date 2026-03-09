import type { Example } from "../useExamples";

export const cssExample: Example = {
    name: "CSS",
    entryRule: "stylesheet",
    grammar: `// CSS Stylesheet Grammar — Prettifier edition
// Models CSS nesting (CSS Nesting Module Level 1).
// Treats selectors and values as opaque spans for formatting.

// Comments + whitespace
cssComment = /\\/\\*[^*]*\\*+([^\\/*][^*]*\\*+)*\\// ;
ws         = (/\\s+/ | cssComment) * ;

// Opaque tokens
selectorSpan = /[^{};]+[^\\s{};]/ | /[^{};]/ ;
valueSpan    = /[^;{}!,]+/ ;

// Declarations
propertyName = /[a-zA-Z_][\\w-]*/ | /--[\\w-]+/ | /-[a-zA-Z][\\w-]*/ ;
important    = "!" ?w , "important" ;
optSemicolon = ";" ? ;
declaration  = propertyName ?w , ":" ?w , valueSpan , ("," ?w , valueSpan) * ,
               important ? , optSemicolon ?w ;

@recover declaration /[^;{}]+;/ ;

// Block contents: declarations and nested rules (CSS Nesting Level 1)
blockContent = (ws , (declaration | ruleItem) ?w) * ;
ruleBlock    = "{" ?w , blockContent ?w , "}" ;

// Qualified rules
qualifiedRule = selectorSpan ?w , ruleBlock ;

@recover qualifiedRule /[^}]+}/ ;

// At-rules
mediaRule      = "@media" , /[^{]+/ , ruleBlock ;
supportsRule   = "@supports" , /[^{]+/ , ruleBlock ;
fontFaceRule   = "@font-face" ?w , ruleBlock ;
importRule     = "@import" , valueSpan , ("," ?w , valueSpan) * , ";" ;
atRuleBody     = ruleBlock | ";" ;
genericAtRule  = /@[a-zA-Z][\\w-]*/ , /[^;{}]*/ , atRuleBody ;

atRule = mediaRule | supportsRule | fontFaceRule | importRule | genericAtRule ;

@recover atRule /[^;{}]+[;]|[^}]+}/ ;

// Rule item
ruleItem = qualifiedRule | atRule ;

// Rule list (top-level)
ruleList   = (ws , ruleItem ?w) * ;

// Entry point
stylesheet = ws , ruleList ?w ;

@pretty stylesheet block ;
@pretty ruleList block ;
@pretty blockContent block indent ;
@pretty qualifiedRule group indent ;
@pretty declaration group ;
@pretty atRule group indent ;`,
    input: `.container {
  display: flex;
  gap: 1rem;
  padding: 16px 24px;
}

.card {
  background: #fff;
  border-radius: 8px;
  box-shadow: 0 2px 8px rgba(0,0,0,0.1);
}

.card:hover {
  transform: translateY(-2px);
  box-shadow: 0 4px 16px rgba(0,0,0,0.15);
}

@media (max-width: 768px) {
  .container { flex-direction: column; }
  .card { margin-bottom: 1rem; }
}

h1, h2, h3 {
  font-family: "Instrument Serif", serif;
  color: var(--foreground);
}`,
};
