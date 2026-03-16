<script setup lang="ts">
import { ref, computed } from "vue";
import { getLanguageIcon } from "@/lib/languageIcons";
import TabBar from "./TabBar.vue";

interface TabData {
    lang: string;
    label: string;
    code: string;
    highlighted: string;
}

const props = defineProps<{
    tabs: TabData[];
}>();

const activeKey = ref(props.tabs[0]?.lang ?? "");

const tabItems = computed(() =>
    props.tabs.map((t) => ({
        key: t.lang,
        label: t.label,
        icon: getLanguageIcon(t.lang),
    }))
);

const activeIndex = computed(() => {
    const idx = props.tabs.findIndex((t) => t.lang === activeKey.value);
    return idx >= 0 ? idx : 0;
});
</script>

<template>
    <div class="code-card code-tabs">
        <TabBar :tabs="tabItems" v-model="activeKey" />
        <pre
            v-for="(tab, i) in props.tabs"
            v-show="i === activeIndex"
            :key="tab.lang"
            class="!mt-0 !rounded-t-none"
        ><code :class="`language-${tab.lang}`" v-html="tab.highlighted" /></pre>
    </div>
</template>
