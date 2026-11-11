export interface BibEntry {
    key: string;
    type: string;
    author: string;
    shortAuthor: string;
    year: string;
    title: string;
    fields: Record<string, string>;
}
