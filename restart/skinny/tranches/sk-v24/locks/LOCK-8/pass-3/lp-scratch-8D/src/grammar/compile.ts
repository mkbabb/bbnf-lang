/**
 * BBNF grammar compilation + .map() transforms for atomic tokens.
 * Compiles latex-tokens.bbnf into usable parse-that parsers.
 */

import { BBNFToParser } from "@mkbabb/bbnf-lang";
import type { Parser } from "@mkbabb/parse-that";
import { ACCENT_MAPS, SYMBOL_MAP } from "../utils/accents";

// Load BBNF grammars as raw text. tsup handles .bbnf as text via loader config.
// At runtime these are just strings.
import BIBTEX_BBNF from "../../grammar/bibtex.bbnf";
import LATEX_TOKENS_BBNF from "../../grammar/latex-tokens.bbnf";

export type Nonterminals = Record<string, Parser<any>>;

/** Compile the latex-tokens BBNF and apply .map() transforms. */
export function compileLatexTokens(): Nonterminals {
    const [nt] = BBNFToParser(LATEX_TOKENS_BBNF);

    // Map special characters to Unicode
    nt.emDash = nt.emDash.map(() => "\u2014");
    nt.enDash = nt.enDash.map(() => "\u2013");
    nt.leftDoubleQuote = nt.leftDoubleQuote.map(() => "\u201C");
    nt.rightDoubleQuote = nt.rightDoubleQuote.map(() => "\u201D");
    nt.tilde = nt.tilde.map(() => " ");

    // Spacing commands → whitespace
    nt.spacingCmd = nt.spacingCmd.map(() => " ");
    nt.quadCmd = nt.quadCmd.map(() => " ");
    nt.thinSpace = nt.thinSpace.map(() => " ");
    nt.lineBreak = nt.lineBreak.map(() => " ");
    nt.escapedSpace = nt.escapedSpace.map(() => " ");

    // Escaped specials → the character itself
    nt.escapedSpecial = nt.escapedSpecial.map((parts: any[]) => {
        const ch = typeof parts === "string" ? parts[1] : parts[1];
        return ch === "&" ? "&amp;" : ch;
    });

    // Comments → empty string (stripped)
    nt.comment = nt.comment.map(() => "");

    // Brace groups → joined content
    nt.braceGroup = nt.braceGroup.map((parts: any[]) => {
        return flatJoin(parts);
    });

    // Bracket groups → joined content
    nt.bracketGroup = nt.bracketGroup.map((parts: any[]) => {
        return flatJoin(parts);
    });

    // Accents → Unicode
    nt.bracedAccent = nt.bracedAccent.map((parts: any[]) => {
        const cmd = deepString(parts[1]);
        const char = deepString(parts[2]).replace(/[{}]/g, "");
        const map = ACCENT_MAPS[cmd];
        return map?.[char] ?? char;
    });

    nt.unbracedAccent = nt.unbracedAccent.map((parts: any[]) => {
        const cmd = deepString(parts[1]);
        const char = deepString(parts[2]);
        const map = ACCENT_MAPS[cmd];
        return map?.[char] ?? char;
    });

    nt.namedAccent = nt.namedAccent.map((parts: any[]) => {
        const cmd = deepString(parts[1]);
        const char = deepString(parts[2]).replace(/[{}]/g, "");
        const map = ACCENT_MAPS[cmd];
        return map?.[char] ?? char;
    });

    return nt;
}

/** Compile the bibtex BBNF and apply transforms. */
export function compileBibtex(): Nonterminals {
    const [nt] = BBNFToParser(BIBTEX_BBNF);

    // bracedValue → inner content joined
    nt.bracedValue = nt.bracedValue.map((parts: any[]) => {
        // parts = ["{", ...inner, "}"]
        return flatJoin(parts.slice(1, -1));
    });

    // quotedValue → inner string
    nt.quotedValue = nt.quotedValue.map((parts: any[]) => {
        return typeof parts === "string" ? parts : parts[1] ?? "";
    });

    // fieldValue → concatenated parts
    nt.fieldValuePart = nt.fieldValuePart.map((v: any) => deepString(v));
    nt.fieldValue = nt.fieldValue.map((parts: any) => {
        return flatJoin(parts);
    });

    // field → { name, value }
    nt.field = nt.field.map((parts: any[]) => ({
        name: deepString(parts[0]).toLowerCase(),
        value: deepString(parts[2]),
    }));

    // entry → { type, key, fields }
    nt.entry = nt.entry.map((parts: any[]) => {
        const type = deepString(parts[1]).toLowerCase();
        const key = deepString(parts[3]);
        const fieldListRaw = parts[5];
        const fields: Record<string, string> = {};
        if (Array.isArray(fieldListRaw)) {
            for (const f of flattenFields(fieldListRaw)) {
                if (f && typeof f === "object" && "name" in f) {
                    fields[f.name] = f.value;
                }
            }
        } else if (fieldListRaw && typeof fieldListRaw === "object" && "name" in fieldListRaw) {
            fields[fieldListRaw.name] = fieldListRaw.value;
        }
        return { type, key, fields };
    });

    return nt;
}

/** Deeply flatten and join arrays/strings into a single string. */
function flatJoin(v: any): string {
    if (typeof v === "string") return v;
    if (v == null) return "";
    if (Array.isArray(v)) return v.map(flatJoin).join("");
    return String(v);
}

/** Convert any nested parse result to a string. */
function deepString(v: any): string {
    return flatJoin(v);
}

/** Flatten nested field arrays. */
function flattenFields(arr: any[]): any[] {
    const result: any[] = [];
    for (const item of arr) {
        if (Array.isArray(item)) {
            result.push(...flattenFields(item));
        } else if (item && typeof item === "object" && "name" in item) {
            result.push(item);
        }
    }
    return result;
}
