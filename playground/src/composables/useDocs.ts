import { computed } from "vue";

export interface DocHeading {
    id: string;
    text: string;
    level: number;
}

interface DocMeta {
    title: string;
    order: number;
    section: string;
    slug: string;
    content: string;
    headings: DocHeading[];
}

interface DocSection {
    name: string;
    docs: DocMeta[];
}

const modules = import.meta.glob("@docs/**/*.md", { query: "?raw", import: "default", eager: true }) as Record<string, string>;

function parseFrontmatter(raw: string): { meta: Record<string, string>; content: string } {
    const match = raw.match(/^---\r?\n([\s\S]*?)\r?\n---\r?\n([\s\S]*)$/);
    if (!match) return { meta: {}, content: raw };
    const meta: Record<string, string> = {};
    for (const line of match[1]!.split("\n")) {
        const colon = line.indexOf(":");
        if (colon > 0) {
            meta[line.slice(0, colon).trim()] = line.slice(colon + 1).trim();
        }
    }
    return { meta, content: match[2]! };
}

/** Extract ## and ### headings from markdown content. */
function extractHeadings(content: string): DocHeading[] {
    const headings: DocHeading[] = [];
    for (const line of content.split("\n")) {
        const match = line.match(/^(#{2,3})\s+(.+)$/);
        if (match) {
            const text = match[2]!.trim();
            // Generate a slug-friendly id from the heading text
            const id = text
                .toLowerCase()
                .replace(/[^a-z0-9]+/g, "-")
                .replace(/^-|-$/g, "");
            headings.push({ id, text, level: match[1]!.length });
        }
    }
    return headings;
}

const allDocs: DocMeta[] = Object.entries(modules).map(([path, raw]) => {
    // Support nested dirs: /docs/bbnf/foo.md → "bbnf/foo"
    const slug = path.split("/docs/")[1]!.replace(/\.md$/, "");
    const { meta, content } = parseFrontmatter(raw);
    return {
        title: meta.title ?? slug,
        order: parseInt(meta.order ?? "99", 10),
        section: meta.section ?? "General",
        slug,
        content,
        headings: extractHeadings(content),
    };
}).sort((a, b) => a.order - b.order);

// BBNF first, Performance last. "General" excluded.
const sectionOrder = ["BBNF", "parse-that", "pprint", "gorgeous", "Performance"];

const sections = computed<DocSection[]>(() => {
    const map = new Map<string, DocMeta[]>();
    for (const doc of allDocs) {
        if (doc.section === "General") continue; // Exclude "General" section
        if (!map.has(doc.section)) map.set(doc.section, []);
        map.get(doc.section)!.push(doc);
    }
    return Array.from(map.entries())
        .map(([name, docs]) => ({ name, docs }))
        .sort((a, b) => {
            const ai = sectionOrder.indexOf(a.name);
            const bi = sectionOrder.indexOf(b.name);
            return (ai === -1 ? sectionOrder.length : ai) - (bi === -1 ? sectionOrder.length : bi);
        });
});

const allSlugs = allDocs.map((d) => d.slug);

function getDoc(slug: string): DocMeta | undefined {
    return allDocs.find((d) => d.slug === slug);
}

export function useDocs() {
    return { sections, allDocs, allSlugs, getDoc };
}
