import MarkdownIt from "markdown-it";

const md = new MarkdownIt({
    html: true,
    linkify: true,
    typographer: false,
});

// Custom heading renderer: add id + anchor link
const defaultHeadingOpen = md.renderer.rules.heading_open ?? ((tokens: any, idx: any, options: any, _env: any, self: any) => self.renderToken(tokens, idx, options));

md.renderer.rules.heading_open = (tokens, idx, options, env, self) => {
    const token = tokens[idx]!;
    const nextToken = tokens[idx + 1];
    if (nextToken?.type === "inline" && nextToken.content) {
        const slug = nextToken.content.toLowerCase().replace(/[^a-z0-9]+/g, "-").replace(/(^-|-$)/g, "");
        token.attrSet("id", slug);
    }
    return defaultHeadingOpen(tokens, idx, options, env, self);
};

// -------------------------------------------------------------------
// Syntax highlighting — lightweight token colorization for code fences
// -------------------------------------------------------------------

/** Token pattern: regex → CSS class. Order matters (first match wins). */
interface HighlightRule {
    pattern: RegExp;
    className: string;
}

const sharedRules: HighlightRule[] = [
    // Strings (double/single/backtick quoted)
    { pattern: /(["'`])(?:\\.|(?!\1).)*\1/g, className: "hl-string" },
    // Line comments
    { pattern: /\/\/[^\n]*/g, className: "hl-comment" },
    // Block comments
    { pattern: /\/\*[\s\S]*?\*\//g, className: "hl-comment" },
    // Hash comments
    { pattern: /#[^\n]*/g, className: "hl-comment" },
    // Numbers
    { pattern: /\b\d+(\.\d+)?([eE][+-]?\d+)?\b/g, className: "hl-number" },
    // Booleans / null / undefined
    { pattern: /\b(true|false|null|undefined|None|nil)\b/g, className: "hl-keyword" },
];

const langRules: Record<string, HighlightRule[]> = {
    ts: [
        { pattern: /(["'`])(?:\\.|(?!\1).)*\1/g, className: "hl-string" },
        { pattern: /\/\/[^\n]*/g, className: "hl-comment" },
        { pattern: /\/\*[\s\S]*?\*\//g, className: "hl-comment" },
        { pattern: /\b(import|from|export|const|let|var|function|return|if|else|for|while|of|in|new|class|extends|type|interface|async|await|try|catch|throw|typeof|instanceof|as|readonly)\b/g, className: "hl-keyword" },
        { pattern: /\b(true|false|null|undefined|void|never|any|string|number|boolean|object|unknown)\b/g, className: "hl-builtin" },
        { pattern: /\b\d+(\.\d+)?([eE][+-]?\d+)?\b/g, className: "hl-number" },
        { pattern: /\b([A-Z]\w*)\b/g, className: "hl-type" },
    ],
    typescript: "ts" as any,
    js: "ts" as any,
    javascript: "ts" as any,
    rust: [
        { pattern: /(["'])(?:\\.|(?!\1).)*\1/g, className: "hl-string" },
        { pattern: /\/\/[^\n]*/g, className: "hl-comment" },
        { pattern: /\/\*[\s\S]*?\*\//g, className: "hl-comment" },
        { pattern: /\b(use|mod|pub|fn|let|mut|const|struct|enum|impl|trait|type|where|if|else|for|while|loop|match|return|self|Self|super|crate|async|await|move|unsafe|extern|ref|dyn|in|as)\b/g, className: "hl-keyword" },
        { pattern: /\b(true|false|None|Some|Ok|Err|Vec|Box|Arc|Rc|String|Option|Result|usize|u8|u16|u32|u64|i8|i16|i32|i64|f32|f64|bool|str|char)\b/g, className: "hl-builtin" },
        { pattern: /\b\d+(\.\d+)?([eE][+-]?\d+)?\b/g, className: "hl-number" },
        { pattern: /#\[[\s\S]*?\]/g, className: "hl-decorator" },
        { pattern: /\b([A-Z]\w*)\b/g, className: "hl-type" },
    ],
    bbnf: [
        { pattern: /(["'])(?:\\.|(?!\1).)*\1/g, className: "hl-string" },
        { pattern: /\/\/[^\n]*/g, className: "hl-comment" },
        { pattern: /\/[^/\n]+\//g, className: "hl-regex" },
        { pattern: /@\w+/g, className: "hl-decorator" },
        { pattern: /\b(group|indent|dedent|block|sep|split|compact|fast|off|nobreak|softbreak|hardbreak|blankline)\b/g, className: "hl-builtin" },
        { pattern: /[|*+?;=,]/g, className: "hl-operator" },
    ],
    toml: [
        { pattern: /(["'])(?:\\.|(?!\1).)*\1/g, className: "hl-string" },
        { pattern: /#[^\n]*/g, className: "hl-comment" },
        { pattern: /\[[\w.-]+\]/g, className: "hl-decorator" },
        { pattern: /\b(true|false)\b/g, className: "hl-keyword" },
        { pattern: /\b\d+(\.\d+)?\b/g, className: "hl-number" },
    ],
    bash: [
        { pattern: /(["'])(?:\\.|(?!\1).)*\1/g, className: "hl-string" },
        { pattern: /#[^\n]*/g, className: "hl-comment" },
        { pattern: /\b(npm|cargo|cd|mkdir|install|run|test|build|npx)\b/g, className: "hl-keyword" },
    ],
    json: [
        { pattern: /"(?:\\.|[^"\\])*"/g, className: "hl-string" },
        { pattern: /\b(true|false|null)\b/g, className: "hl-keyword" },
        { pattern: /-?\b\d+(\.\d+)?([eE][+-]?\d+)?\b/g, className: "hl-number" },
    ],
    css: [
        { pattern: /(["'])(?:\\.|(?!\1).)*\1/g, className: "hl-string" },
        { pattern: /\/\*[\s\S]*?\*\//g, className: "hl-comment" },
        { pattern: /@[\w-]+/g, className: "hl-decorator" },
        { pattern: /#[\w-]+/g, className: "hl-number" },
        { pattern: /\b\d+(\.\d+)?(px|em|rem|%|vh|vw|s|ms)?\b/g, className: "hl-number" },
        { pattern: /\b(inherit|initial|unset|none|auto|block|flex|grid|inline|relative|absolute|fixed|sticky)\b/g, className: "hl-keyword" },
    ],
};

function resolveRules(lang: string): HighlightRule[] {
    let rules = langRules[lang];
    // Follow aliases (e.g. "typescript" → "ts")
    if (typeof rules === "string") rules = langRules[rules as string];
    return (rules as HighlightRule[] | undefined) ?? sharedRules;
}

function highlightCode(code: string, lang: string): string {
    const rules = resolveRules(lang);
    // Build a list of non-overlapping token spans
    const spans: { start: number; end: number; cls: string }[] = [];

    for (const rule of rules) {
        const re = new RegExp(rule.pattern.source, rule.pattern.flags);
        let m: RegExpExecArray | null;
        while ((m = re.exec(code)) !== null) {
            const start = m.index;
            const end = start + m[0].length;
            // Skip if overlapping with an earlier span
            if (!spans.some((s) => start < s.end && end > s.start)) {
                spans.push({ start, end, cls: rule.className });
            }
        }
    }

    spans.sort((a, b) => a.start - b.start);

    // Build output
    let result = "";
    let cursor = 0;
    for (const span of spans) {
        if (span.start > cursor) {
            result += escapeHtml(code.slice(cursor, span.start));
        }
        result += `<span class="${span.cls}">${escapeHtml(code.slice(span.start, span.end))}</span>`;
        cursor = span.end;
    }
    if (cursor < code.length) {
        result += escapeHtml(code.slice(cursor));
    }

    return result;
}

function escapeHtml(s: string): string {
    return s.replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;").replace(/"/g, "&quot;");
}

// Custom fence renderer: card-styled container + language label + syntax highlighting
md.renderer.rules.fence = (tokens, idx) => {
    const token = tokens[idx]!;
    const lang = token.info.trim();
    const highlighted = highlightCode(token.content, lang);
    const langLabel = lang
        ? `<span class="code-lang-label">${escapeHtml(lang)}</span>`
        : "";
    return `<div class="code-card">${langLabel}<pre class="!mt-0"><code class="language-${lang}">${highlighted}</code></pre></div>`;
};

export function useMarkdown() {
    function renderMarkdown(source: string): string {
        return md.render(source);
    }
    return { renderMarkdown };
}
