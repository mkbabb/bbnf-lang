// SERVED MODEL: claude-opus-5-5
//
// facade.ts — compiled rules as ordinary parse-that 2.x `Parser`s, so they compose with hand-written
// combinators (`.sepBy`, `.map`, `all(…)`) and answer `.parse()`. The emitted rule runs inside; the
// parse-that side sees one leaf that moves the offset and sets the value. Host parsers go the other
// way: a parse-that `Parser` supplies a rule the emitted code calls.

import { Parser, ParserState, createParserContext, mergeErrorState } from "@mkbabb/parse-that";
import type { Compiled, HostRule } from "./compile.js";

/** Rule `name` of `compiled` as a parse-that `Parser`. */
export function toParser<T = any>(compiled: Compiled, name: string): Parser<T> {
    const k = compiled.rules[name];
    if (k === undefined) throw new Error(`bbnf: \`${name}\` is not an entry rule of this parser`);
    const names = compiled.emission.ruleNames;
    const leaf = (state: ParserState<T>): ParserState<T> => {
        const end = k(state.src, state.offset);
        if (compiled.recovered !== undefined) {
            // Each recovered span is a diagnostic, as parse-that's `recover` records one.
            const rc = compiled.recovered();
            for (let j = 0; j < rc.length; j += 3) diagnose(state as ParserState<unknown>, names[rc[j]], rc[j + 1]);
        }
        if (end < 0) {
            mergeErrorState(state as ParserState<unknown>, name);
            state.isError = true;
            return state;
        }
        state.offset = end;
        state.value = compiled.value() as T;
        state.isError = state.fault !== undefined;
        return state;
    };
    return new Parser<T>(leaf, createParserContext("lazy", undefined, name));
}

function diagnose(state: ParserState<unknown>, rule: string, offset: number): void {
    const src = state.src;
    const before = src.slice(0, offset);
    const nl = before.lastIndexOf("\n");
    state.diagnostics.push({
        offset, furthestOffset: offset,
        line: nl === -1 ? 1 : before.split("\n").length,
        column: nl === -1 ? offset : offset - nl - 1,
        expected: [rule], suggestions: [], secondarySpans: [],
        found: src.slice(offset, offset + 20).replace(/\n/g, "\\n"),
    });
}

/** A parse-that `Parser` as a host rule the emitted code calls. */
export function fromParser(p: Parser<unknown>): HostRule {
    return (source, offset, box) => {
        const state = new ParserState<unknown>(source, undefined, offset);
        p.parser(state);
        if (state.isError) return -1;
        box.v = state.value;
        return state.offset;
    };
}
