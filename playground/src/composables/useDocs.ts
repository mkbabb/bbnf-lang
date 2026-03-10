import { computed } from "vue";

interface DocMeta {
    title: string;
    order: number;
    section: string;
    slug: string;
    content: string;
}

interface DocSection {
    name: string;
    docs: DocMeta[];
}

const modules = import.meta.glob("@/docs/**/*.md", { query: "?raw", import: "default", eager: true }) as Record<string, string>;

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
    };
}).sort((a, b) => a.order - b.order);

const sections = computed<DocSection[]>(() => {
    const map = new Map<string, DocMeta[]>();
    for (const doc of allDocs) {
        if (!map.has(doc.section)) map.set(doc.section, []);
        map.get(doc.section)!.push(doc);
    }
    return Array.from(map.entries()).map(([name, docs]) => ({ name, docs }));
});

const allSlugs = allDocs.map((d) => d.slug);

function getDoc(slug: string): DocMeta | undefined {
    return allDocs.find((d) => d.slug === slug);
}

export function useDocs() {
    return { sections, allDocs, allSlugs, getDoc };
}
