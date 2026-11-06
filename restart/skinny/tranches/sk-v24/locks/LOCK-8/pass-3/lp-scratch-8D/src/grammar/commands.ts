/**
 * Command parsers: sectioning, formatting, refs, cites, symbols, skip/spacing.
 * Each parser handles \commandName followed by its arguments.
 *
 * Uses regex with negative lookahead (?![a-zA-Z]) for word boundaries,
 * since parse-that's .not() operates at the starting position.
 */

import { Parser, regex, string, any } from "@mkbabb/parse-that";
import type {
    LatexNode,
    SectionNode,
    CommandNode,
    LabelNode,
    TextNode,
} from "../types/ast";
import { braceBalanced, braceContent, bracketBalanced, ws } from "./primitives";

/** Create a regex that matches \commandName with word boundary. */
function cmd(name: string): Parser<string> {
    return regex(new RegExp(`\\\\${name}(?![a-zA-Z])`)).map(() => name);
}

/** Match \commandName* (optional star) with word boundary. */
function cmdStar(name: string): Parser<[string, boolean]> {
    return regex(new RegExp(`\\\\${name}(\\*)?(?![a-zA-Z])`)).map(
        (m) => [name, m.endsWith("*")] as [string, boolean],
    );
}

// ── Sectioning ──────────────────────────────────────────────────────

/** Parse \chapter{...}, \section{...}, \subsection{...}, \subsubsection{...} */
export const sectionCommand: Parser<SectionNode> = regex(
    /\\(chapter|section|subsection|subsubsection)\*?(?![a-zA-Z])/,
)
    .skip(ws)
    .then(braceContent())
    .map(([cmdMatch, titleNodes]) => {
        const starred = cmdMatch.includes("*");
        const level = cmdMatch.replace(/^\\/, "").replace("*", "") as SectionNode["level"];
        return {
            type: "section" as const,
            level,
            starred,
            title: titleNodes,
        };
    });

// ── Formatting ──────────────────────────────────────────────────────

/** Parse \textit{...}, \textbf{...}, \emph{...}, etc. */
export const formattingCommand: Parser<CommandNode> = regex(
    /\\(textit|textbf|emph|texttt|text|mathit|mathrm|mathbf|underline)(?![a-zA-Z])/,
)
    .skip(ws)
    .then(braceContent())
    .map(([cmdMatch, argNodes]) => {
        const name = cmdMatch.replace(/^\\/, "");
        return {
            type: "command" as const,
            name,
            args: [argNodes],
        };
    });

// ── References ──────────────────────────────────────────────────────

/** Parse \ref{...}, \eqref{...}, \cref{...}, \autoref{...} */
export const refCommand: Parser<CommandNode> = regex(
    /\\(eqref|cref|autoref|ref)(?![a-zA-Z])/,
)
    .skip(ws)
    .then(braceBalanced())
    .map(([cmdMatch, arg]) => ({
        type: "command" as const,
        name: cmdMatch.replace(/^\\/, ""),
        args: [[{ type: "text" as const, value: arg }]],
    }));

/** Parse \label{...} */
export const labelCommand: Parser<LabelNode> = cmd("label")
    .skip(ws)
    .next(braceBalanced())
    .map((key) => ({
        type: "label" as const,
        key,
    }));

/** Parse \hyperref[target]{text} */
export const hyperrefCommand: Parser<CommandNode> = string("\\hyperref")
    .skip(ws)
    .next(bracketBalanced()) // target key is opaque
    .skip(ws)
    .then(braceContent()) // display text is LaTeX
    .map(([target, textNodes]) => ({
        type: "command" as const,
        name: "hyperref",
        args: [textNodes],
        optArgs: [[{ type: "text" as const, value: target }]],
    }));

// ── Citations ───────────────────────────────────────────────────────

/** Parse \cite[opt]{key} or \cite{key} */
export const citeCommand: Parser<CommandNode> = cmd("cite")
    .skip(ws)
    .then(bracketBalanced().opt())
    .skip(ws)
    .then(braceBalanced())
    .map(([[_, opt], key]) => ({
        type: "command" as const,
        name: "cite",
        args: [[{ type: "text" as const, value: key }]],
        ...(opt != null && {
            optArgs: [[{ type: "text" as const, value: opt }]],
        }),
    }));

// ── URL ─────────────────────────────────────────────────────────────

/** Parse \url{url} */
export const urlCommand: Parser<CommandNode> = cmd("url")
    .skip(ws)
    .next(braceBalanced())
    .map((url) => ({
        type: "command" as const,
        name: "url",
        args: [[{ type: "text" as const, value: url }]],
    }));

// ── Href ────────────────────────────────────────────────────────────

/** Parse \href{url}{text} */
export const hrefCommand: Parser<CommandNode> = string("\\href")
    .skip(ws)
    .next(braceBalanced()) // URL is opaque
    .skip(ws)
    .then(braceContent()) // display text is LaTeX
    .map(([url, textNodes]) => ({
        type: "command" as const,
        name: "href",
        args: [
            [{ type: "text" as const, value: url }],
            textNodes,
        ],
    }));

// ── Paragraph ───────────────────────────────────────────────────────

/** Parse \paragraph{...} */
export const paragraphCommand: Parser<CommandNode> = cmd("paragraph")
    .skip(ws)
    .next(braceContent())
    .map((titleNodes) => ({
        type: "command" as const,
        name: "paragraph",
        args: [titleNodes],
    }));

/** Parse \bibliography{...} */
export const bibliographyCommand: Parser<CommandNode> = cmd("bibliography")
    .skip(ws)
    .next(braceBalanced())
    .map((value) => ({
        type: "command" as const,
        name: "bibliography",
        args: [[{ type: "text" as const, value }]],
    }));

// ── Skip/spacing commands ───────────────────────────────────────────

/** Parse skip/spacing commands (no arguments, just consume). */
export const skipCommand: Parser<null> = regex(
    /\\(medskip|smallskip|bigskip|vfill|hfill|noindent|newline|centering|newpage|clearpage|cleardoublepage|maketitle|tableofcontents|bibliographystyle|appendix|frontmatter|mainmatter|backmatter)(?![a-zA-Z])/,
)
    .skip(ws)
    .skip(braceBalanced().opt())
    .map(() => null);

/** Parse \vspace{...} and \hspace{...} */
export const vspaceCommand: Parser<null> = regex(/\\[vh]space\*?/)
    .skip(ws)
    .skip(braceBalanced())
    .map(() => null);

/** Parse \newtheorem{name}[shared]{display}[parent] */
export const newtheoremCommand: Parser<null> = string("\\newtheorem")
    .skip(ws)
    .skip(braceBalanced())
    .skip(ws)
    .skip(bracketBalanced().opt())
    .skip(ws)
    .skip(braceBalanced())
    .skip(ws)
    .skip(bracketBalanced().opt())
    .map(() => null);

/** Parse \input{...} or \include{...} */
export const inputCommand: Parser<null> = regex(
    /\\(input|include)(?![a-zA-Z])/,
)
    .skip(ws)
    .skip(braceBalanced())
    .map(() => null);

/** Parse \usepackage[opts]{pkg} and \documentclass[opts]{cls} */
export const preambleCommand: Parser<null> = regex(
    /\\(usepackage|documentclass|RequirePackage|PassOptionsToPackage|newcommand|renewcommand|providecommand|DeclareMathOperator|DeclarePairedDelimiter|theoremstyle|numberwithin|setcounter|definecolor|hypersetup|geometry|fancyhf|fancyhead|fancyfoot|pagestyle|thispagestyle|setlength|addtolength|title|author|date|thanks)(?![a-zA-Z])/,
)
    .skip(regex(/[^\n]*/))
    .map(() => null);

/** Parse \footnote{...} */
export const footnoteCommand: Parser<CommandNode> = cmd("footnote")
    .skip(ws)
    .next(braceContent())
    .map((nodes) => ({
        type: "command" as const,
        name: "footnote",
        args: [nodes],
    }));

/** Parse \@ (inter-sentence spacing) */
export const atCommand: Parser<null> = string("\\@").map(() => null);

/** Parse \item (with optional [...] arg) */
export const itemCommand: Parser<CommandNode> = cmd("item")
    .skip(ws)
    .then(bracketBalanced().opt())
    .map(([_, opt]) => ({
        type: "command" as const,
        name: "item",
        args: [],
        ...(opt != null && {
            optArgs: [[{ type: "text" as const, value: opt }]],
        }),
    }));

/** Parse \includegraphics[opts]{file} */
export const includegraphicsCommand: Parser<CommandNode> = string(
    "\\includegraphics",
)
    .skip(ws)
    .then(bracketBalanced().opt())
    .skip(ws)
    .then(braceBalanced())
    .map(([[_, opts], file]) => ({
        type: "command" as const,
        name: "includegraphics",
        args: [[{ type: "text" as const, value: file }]],
        ...(opts != null && {
            optArgs: [[{ type: "text" as const, value: opts }]],
        }),
    }));

/** Parse \caption{...} */
export const captionCommand: Parser<CommandNode> = cmd("caption")
    .skip(ws)
    .next(braceContent())
    .map((nodes) => ({
        type: "command" as const,
        name: "caption",
        args: [nodes],
    }));

/** Unknown command: consume \commandName and optional brace arg */
export const unknownCommand: Parser<CommandNode | null> = string("\\")
    .next(regex(/[a-zA-Z@]+\*?/))
    .skip(ws)
    .then(bracketBalanced().opt())
    .skip(ws)
    .then(braceContent().opt())
    .map(([[name, opt], argNodes]) => ({
        type: "command" as const,
        name,
        args: argNodes != null ? [argNodes] : [],
        ...(opt != null && {
            optArgs: [[{ type: "text" as const, value: opt }]],
        }),
    }));
