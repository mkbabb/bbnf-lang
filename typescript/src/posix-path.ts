// SERVED MODEL: claude-opus-5-5
/**
 * The three path operations the `@import` loader needs, over `/`-separated
 * module IDs, with no `node:path`.
 *
 * A module ID is a path in the grammar's own namespace: a files-map key, a URL
 * path, or a filesystem path a node host supplies. It is never resolved against
 * a process working directory, so the loader runs unchanged in a browser.
 */

/** `a/./b/../c` → `a/c`; a leading `/` is kept. */
function normalize(p: string): string {
    const abs = p.startsWith("/");
    const out: string[] = [];
    for (const seg of p.split("/")) {
        if (seg === "" || seg === ".") continue;
        if (seg === "..") {
            if (out.length > 0 && out[out.length - 1] !== "..") out.pop();
            else if (!abs) out.push("..");
            continue;
        }
        out.push(seg);
    }
    return (abs ? "/" : "") + out.join("/");
}

/** Join `parts` left to right; an absolute part restarts the path. */
export function resolve(...parts: string[]): string {
    let acc = "";
    for (const p of parts) acc = p.startsWith("/") ? p : acc === "" ? p : `${acc}/${p}`;
    return normalize(acc);
}

/** The directory part of `p` (`.` for a bare name, `/` for a root child). */
export function dirname(p: string): string {
    const n = normalize(p);
    const i = n.lastIndexOf("/");
    return i < 0 ? "." : i === 0 ? "/" : n.slice(0, i);
}

/** The extension of `p`'s last segment, with its dot (`""` when none). */
export function extname(p: string): string {
    const base = p.slice(p.lastIndexOf("/") + 1);
    const i = base.lastIndexOf(".");
    return i <= 0 ? "" : base.slice(i);
}
