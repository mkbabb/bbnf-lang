import MarkdownIt from "markdown-it";
import { getLanguageIcon } from "./languageIcons";

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

/** Escape for use inside single-quoted HTML attributes — also escapes single quotes. */
function escapeAttr(s: string): string {
    return escapeHtml(s).replace(/'/g, "&#39;");
}

// Helper: parse code-tabs fence content into per-language blocks
function parseCodeTabs(content: string): { lang: string; code: string }[] {
    const blocks: { lang: string; code: string }[] = [];
    const parts = content.split(/^---(\w+)---$/m);
    // parts: ["", "rust", "\ncode...\n", "typescript", "\ncode...\n", ...]
    for (let i = 1; i < parts.length; i += 2) {
        const lang = parts[i]!.trim();
        const code = (parts[i + 1] ?? "").trim();
        if (lang && code) blocks.push({ lang, code });
    }
    return blocks;
}

// Custom fence renderer: card-styled container + language label + syntax highlighting
// Intercepts special fence types (code-tabs, bench-chart, live-bench) before default handling.
md.renderer.rules.fence = (tokens, idx) => {
    const token = tokens[idx]!;
    const lang = token.info.trim();

    // Custom fence: code tabs (Rust/TS toggle)
    if (lang === "code-tabs") {
        const blocks = parseCodeTabs(token.content);
        const labelMap: Record<string, string> = {
            rust: "Rust", ts: "TypeScript", typescript: "TypeScript",
            js: "JavaScript", javascript: "JavaScript",
            bash: "Bash", toml: "TOML", json: "JSON", css: "CSS",
            bbnf: "BBNF", wasm: "WASM",
        };
        const data = JSON.stringify(blocks.map(b => ({
            lang: b.lang,
            label: labelMap[b.lang] ?? b.lang.charAt(0).toUpperCase() + b.lang.slice(1),
            code: b.code,
            highlighted: highlightCode(b.code, b.lang),
        })));
        return `<div class="code-tabs-block" data-tabs='${escapeAttr(data)}'></div>`;
    }

    // Custom fence: benchmark chart
    if (lang === "bench-chart") {
        return `<div class="bench-chart-block" data-chart='${escapeAttr(token.content.trim())}'></div>`;
    }

    // Custom fence: live benchmark
    if (lang === "live-bench") {
        return `<div class="live-bench-block" data-bench='${escapeAttr(token.content.trim())}'></div>`;
    }

    // Custom fence: flow chart
    if (lang === "flow-chart") {
        return `<div class="flow-chart-block" data-flow='${escapeAttr(token.content.trim())}'></div>`;
    }

    // Custom fence: runnable code example
    if (lang === "runnable-code") {
        try {
            const parsed = JSON.parse(token.content.trim());
            const data = {
                grammar: parsed.grammar ?? "",
                input: parsed.input ?? "",
                language: parsed.language ?? "bbnf",
                highlighted: parsed.grammar ? highlightCode(parsed.grammar, parsed.language ?? "bbnf") : "",
            };
            return `<div class="runnable-code-block" data-runnable='${escapeAttr(JSON.stringify(data))}'></div>`;
        } catch {
            return `<div class="code-card"><pre><code>${escapeHtml(token.content)}</code></pre></div>`;
        }
    }

    // Default fence rendering
    const highlighted = highlightCode(token.content, lang);
    const icon = getLanguageIcon(lang);
    const iconHtml = icon ? `<span class="code-lang-icon" style="color:${icon.color}">${icon.svg}</span>` : "";
    const langLabel = lang
        ? `<span class="code-lang-label">${iconHtml}${escapeHtml(icon?.label ?? lang)}</span>`
        : "";
    return `<div class="code-card">${langLabel}<pre class="!mt-0"><code class="language-${lang}">${highlighted}</code></pre></div>`;
};

export function useMarkdown() {
    function renderMarkdown(source: string): string {
        let html = md.render(source);
        // Post-process: colorize performance numbers (e.g. "1,234.5 MB/s", "42 ms")
        // Use alternation to skip HTML tags — first branch captures tags verbatim,
        // second branch captures perf patterns only in text content.
        html = html.replace(
            /(<[^>]*>)|(\d[\d,]*(?:\.\d+)?)\s*(MB\/s|GB\/s|ops\/s|ms|µs|KB|MB|GB|x\b)/g,
            (_match, tag, num, unit) => {
                if (tag) return tag;
                return `<span class="perf-number">${num}</span>\u00a0<span class="perf-unit">${unit}</span>`;
            }
        );
        return html;
    }
    return { renderMarkdown };
}
