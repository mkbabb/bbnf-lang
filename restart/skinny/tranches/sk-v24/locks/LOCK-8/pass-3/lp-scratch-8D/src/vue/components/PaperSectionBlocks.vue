<script setup lang="ts">
import { inject, provide, useSlots, reactive } from "vue";
import type {
    MathBlockData,
    PaperNestedBlock,
    PaperSectionData,
} from "../../types/output";
import { PAPER_CONTEXT } from "../context";
import { CONTENT_SLOTS } from "./paperSectionSlots";
import CodeBlock from "./CodeBlock.vue";
import MathBlock from "./MathBlock.vue";
import Theorem from "./Theorem.vue";

const props = defineProps<{
    section: PaperSectionData;
}>();

const ownSlots = useSlots();
const parentSlots = inject(CONTENT_SLOTS, null);
const effectiveSlots = parentSlots ?? ownSlots;
provide(CONTENT_SLOTS, effectiveSlots);

const ctx = inject(PAPER_CONTEXT)!;

const failedImages = reactive(new Set<string>());

function onImageError(filename: string) {
    failedImages.add(filename);
}

function renderParagraph(text: string): string {
    return ctx.renderTitle(text);
}

function isBlockHtml(text: string): boolean {
    return /^<(ol|ul|dl|blockquote|div)\b/.test(text.trim());
}

function renderNestedText(text: string): string {
    return ctx.renderTitle(text);
}

function nestedBlockKey(prefix: string, index: number, block: PaperNestedBlock): string {
    if (typeof block === "string") return `${prefix}-text-${index}`;
    if ("figure" in block) return `${prefix}-figure-${block.figure.label ?? index}`;
    if ("code" in block) return `${prefix}-code-${index}`;
    const math = block as MathBlockData;
    return `${prefix}-math-${math.anchorId ?? math.id ?? index}`;
}

function figureId(label?: string): string | undefined {
    return label ? label.replace(/:/g, "-") : undefined;
}
</script>

<template>
    <template v-for="(block, bi) in section.content" :key="bi">
        <template v-if="typeof block === 'string'">
            <div
                v-if="isBlockHtml(block)"
                v-html="renderParagraph(block)"
            />
            <p v-else v-html="renderParagraph(block)" />
        </template>

        <Theorem
            v-else-if="'theorem' in block"
            :type="block.theorem.type"
            :name="block.theorem.name"
            :number="block.theorem.number"
            :label="block.theorem.label"
        >
            <template
                v-for="(nested, ni) in block.theorem.content"
                :key="nestedBlockKey(`theorem-${bi}`, ni, nested)"
            >
                <div
                    v-if="typeof nested === 'string' && isBlockHtml(nested)"
                    v-html="renderNestedText(nested)"
                />
                <p
                    v-else-if="typeof nested === 'string'"
                    v-html="renderNestedText(nested)"
                />
                <figure
                    v-else-if="'figure' in nested"
                    :id="figureId(nested.figure.label)"
                >
                    <component
                        v-if="effectiveSlots.figure"
                        :is="() => effectiveSlots.figure!({ figure: nested.figure, index: ni })"
                    />
                    <div
                        v-else-if="failedImages.has(nested.figure.filename)"
                        class="paper-figure-placeholder"
                    />
                    <img
                        v-else
                        :src="`${ctx.assetBase}${nested.figure.filename}`"
                        :alt="nested.figure.caption"
                        loading="lazy"
                        @error="onImageError(nested.figure.filename)"
                    />
                    <figcaption v-if="nested.figure.caption || nested.figure.number">
                        <strong v-if="nested.figure.number">Figure {{ nested.figure.number }}: </strong>
                        <span v-if="nested.figure.caption" v-html="renderNestedText(nested.figure.caption)" />
                    </figcaption>
                </figure>
                <CodeBlock
                    v-else-if="'code' in nested"
                    :code="nested.code.code"
                    :caption="nested.code.caption"
                    :language="nested.code.language"
                />
                <MathBlock
                    v-else
                    :tex="nested.tex"
                    :id="nested.anchorId || nested.id"
                    :number="nested.number"
                    :numbered="nested.numbered"
                />
            </template>
        </Theorem>

        <figure
            v-else-if="'figure' in block"
            :id="figureId(block.figure.label)"
        >
            <component
                v-if="effectiveSlots.figure"
                :is="() => effectiveSlots.figure!({ figure: block.figure, index: bi })"
            />
            <div
                v-else-if="failedImages.has(block.figure.filename)"
                class="paper-figure-placeholder"
            />
            <img
                v-else
                :src="`${ctx.assetBase}${block.figure.filename}`"
                :alt="block.figure.caption"
                loading="lazy"
                @error="onImageError(block.figure.filename)"
            />
            <figcaption v-if="block.figure.caption || block.figure.number">
                <strong v-if="block.figure.number">Figure {{ block.figure.number }}: </strong>
                <span v-if="block.figure.caption" v-html="renderParagraph(block.figure.caption)" />
            </figcaption>
        </figure>

        <CodeBlock
            v-else-if="'code' in block"
            :code="block.code.code"
            :caption="block.code.caption"
            :language="block.code.language"
        />

        <div v-else-if="'proof' in block" class="paper-proof-block">
            <div class="paper-proof-label">
                <span
                    class="paper-proof-title"
                    v-html="block.proof.name || 'Proof'"
                />
                .
            </div>
            <div class="paper-proof-body">
                <template
                    v-for="(nested, ni) in block.proof.content"
                    :key="nestedBlockKey(`proof-${bi}`, ni, nested)"
                >
                    <div
                        v-if="typeof nested === 'string' && isBlockHtml(nested)"
                        v-html="renderNestedText(nested)"
                    />
                    <p
                        v-else-if="typeof nested === 'string'"
                        v-html="renderNestedText(nested)"
                    />
                    <figure
                        v-else-if="'figure' in nested"
                        :id="figureId(nested.figure.label)"
                    >
                        <component
                            v-if="effectiveSlots.figure"
                            :is="() => effectiveSlots.figure!({ figure: nested.figure, index: ni })"
                        />
                        <img
                            v-else
                            :src="`${ctx.assetBase}${nested.figure.filename}`"
                            :alt="nested.figure.caption"
                            loading="lazy"
                        />
                        <figcaption v-if="nested.figure.caption || nested.figure.number">
                            <strong v-if="nested.figure.number">Figure {{ nested.figure.number }}: </strong>
                            <span v-if="nested.figure.caption" v-html="renderNestedText(nested.figure.caption)" />
                        </figcaption>
                    </figure>
                    <CodeBlock
                        v-else-if="'code' in nested"
                        :code="nested.code.code"
                        :caption="nested.code.caption"
                        :language="nested.code.language"
                    />
                    <MathBlock
                        v-else
                        :tex="nested.tex"
                        :id="nested.anchorId || nested.id"
                        :number="nested.number"
                        :numbered="nested.numbered"
                    />
                </template>
            </div>
        </div>

        <MathBlock
            v-else
            :tex="block.tex"
            :id="block.anchorId || block.id"
            :number="block.number"
            :numbered="block.numbered"
        />
    </template>

    <template v-if="section.callout">
        <component
            v-if="effectiveSlots.callout"
            :is="() => effectiveSlots.callout!({ callout: section.callout!, section })"
        />
        <div v-else class="paper-callout">
            <a :href="section.callout.link">{{ section.callout.text }}</a>
        </div>
    </template>
</template>
