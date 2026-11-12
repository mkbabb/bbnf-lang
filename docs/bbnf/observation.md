---
title: Observation and Failure
order: 6
section: BBNF
---

# Observation and Failure

Two implementations of a grammar agree when they accept the same inputs, report the same observation of each accepted input, and report the same failure contract for each rejected input. This page defines both. The executable form is `crates/oracle/src/obs.rs`.

## The observation tree

An accepted input is observed as a tree. A node is a grouping: a rule's result, a sequence or a repetition. A leaf is a scalar the input spelled. Each node and leaf may carry a label, the implementation's name for it (a rule name or a typed kind), and a span, the byte range of the input it covers. A grouping with no label is anonymous.

## Observation classes

Two trees are compared through `obs(class, tree)`, never directly. There are exactly four classes. A class belongs to a rewrite or to a pair of implementations, never to a grammar, and each class is a projection: applying it twice gives the same tree as applying it once.

- **Exact.** The lossless Syntax layer is compared by exact equality: labels, spans, leaf text and shape all count, and nothing is normalised.
- **Semantic.** The semantic projection compares trees after the declared CST normalisation: every span is erased, every trivia leaf is dropped, and every anonymous node below the root is replaced by its children in order; labels and leaf text count.
- **Scalar events.** The scalar-event observation is the sequence, in document order, of the text of every leaf that is not trivia; labels and shape do not count.
- **Left fold.** The left-associative fold is the semantic projection followed by re-association: a labelled node whose children form an odd chain `x0 o1 x1 … on xn` with n ≥ 2 becomes nested binary nodes `((x0 o1 x1) o2 x2) …` with the same label, so a left fold and a flat chain agree and a right-nested tree does not.

A trivia leaf is a leaf whose text is empty or made only of the whitespace that the default `?w` skips (space, tab, line feed, vertical tab, form feed, carriage return).

The classes are ordered by containment: two trees equal under Exact are equal under Semantic, and two trees equal under Semantic are equal under Scalar events and under Left fold.

## The failure contract

A rejected input is observed as its failure contract: the furthest byte offset the parse reached, and the set of labels the parse expected at that offset, sorted and deduplicated. When two failures of one parse meet, the one at the larger offset wins and a failure at a smaller offset never replaces it; at an equal offset their expected sets are merged. An expected set that is not sorted and deduplicated is not a failure contract.
