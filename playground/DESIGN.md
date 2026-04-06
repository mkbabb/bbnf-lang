# BBNF Playground Design Language

> Extends [glass-ui DESIGN.md](../../glass-ui/DESIGN.md)

## Token Overrides

Instrument Serif (display) + Fira Code (mono). Pastel accent palette: green, blue, purple, amber, pink, cyan—used for syntax highlighting and grammar node coloring.

Cartoon-style shadows with hard offsets (`3px 3px 0`, `5px 5px 0`) instead of glass-ui's softer defaults. Custom glass opacity/blur for the playground's split-pane layout. Custom easing tokens for panel resize transitions.

## Local Utilities

Defined as `@utility` in `main.css`:

- `btn-cta` — primary call-to-action (filled + scale press)
- `btn-ghost` — transparent hover-reveal
- `card-base` / `card-subtle` / `card-elevated` — three-tier card system
- `shadow-card` / `shadow-card-hover` — cartoon shadow pair with hover lift
- `tapered-rule` — decorative HR (tapered horizontal rule)

## Migration Tasks

- [ ] Replace custom TabBar.vue with glass-ui Tabs/TabsList/TabsTrigger/TabsContent
- [ ] Replace attribution card scoped CSS in NavBar.vue with glass-ui HoverCard or Popover
- [ ] Replace `text-[0.625rem]` with `text-micro` or `text-2xs`
- [ ] Evaluate `card-base`/`card-subtle`/`card-elevated` — use glass-ui `glass-subtle`/`glass-default`/`glass-elevated` directly
- [ ] Add focus-visible ring to all non-dock buttons (use `.focus-ring` utility or inline `focus-visible:shadow-[var(--focus-ring-shadow)]`)
