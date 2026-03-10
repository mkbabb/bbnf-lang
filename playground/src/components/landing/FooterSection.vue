<script setup lang="ts">
import { ExternalLink } from "lucide-vue-next";
import { BbnfLogo } from "@/components/custom/bbnf-logo";
</script>

<template>
    <footer class="border-t border-border/30 bg-card/40 backdrop-blur-sm py-8 mt-16">
        <div class="max-w-4xl mx-auto px-8 flex flex-col items-center gap-4 text-center">
            <BbnfLogo size="sm" />
            <div class="flex items-center gap-4 text-sm text-muted-foreground">
                <a
                    href="https://www.npmjs.com/package/@mkbabb/bbnf-lang"
                    target="_blank"
                    rel="noopener"
                    class="hover:text-foreground transition-colors flex items-center gap-1"
                >
                    npm <ExternalLink class="h-3 w-3" />
                </a>
                <a
                    href="https://github.com/mkbabb/bbnf-lang"
                    target="_blank"
                    rel="noopener"
                    class="hover:text-foreground transition-colors flex items-center gap-1"
                >
                    GitHub <ExternalLink class="h-3 w-3" />
                </a>
            </div>
            <p class="text-xs text-muted-foreground/60">
                Built with <a href="https://github.com/mkbabb/parse-that" target="_blank" rel="noopener" class="underline underline-offset-2 hover:text-muted-foreground">parse-that</a>
            </p>
        </div>
    </footer>
</template>
