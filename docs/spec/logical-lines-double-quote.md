# Logical lines: double-quoted lexical content

This specification covers ordinary double-quoted grouping in `LogicalLines`
and its use by the ordinary `LogicalBlocks` document parser. Its paired
[executable specification](../../src/test/scala/org/goldenport/parser/LogicalLinesDoubleQuoteSpec.scala)
defines the observable line, section and prologue results.

## Lexical preservation and termination

When double-quote grouping is enabled, an ordinary literal retains every
authored character, including quotes, backslashes, whitespace, Unicode and
escape spelling. Grouping performs no escape decoding, trimming or Unicode
normalization.

A quote preceded by an odd consecutive run of backslashes is literal content;
the quote is appended and the ordinary literal remains open. A quote preceded
by an even run, including zero, terminates the literal through the existing
parent-state transition. Runs of one, three and five backslashes therefore
retain an embedded quote; runs of zero, two and four permit closure.

After a properly closed literal, a following ordinary physical line or quoted
property remains a separate logical line. In ordinary
`LogicalBlocks.Config.default.forLisp` and `Config.noLocation.forLisp`
documents, subsequent LIST and DETAIL sibling sections retain declaration
order and their own direct prologue line vectors. Location-enabled parsing
retains physical section/property locations; location-disabled parsing omits
those locations.

## Configuration and existing grouping compatibility

The rule applies to ordinary double-quoted literals in the script and lisp
configurations and inside existing JSON and S-expression grouping. Empty
literals and valid multiline quoted grouping retain their existing meaning.
Quote-disabled `LogicalLines.Config.raw` treats quotes and backslashes as
ordinary physical-line content.

Triple-quoted raw opening and termination keep their existing behavior. A
backslash before a terminating raw triple delimiter does not suppress that
delimiter. Single quotes, back quotes, state/API shapes and existing EOF
handling are outside this change.

## Decoding ownership and executable properties

Logical grouping preserves encoded spelling. Downstream JSON interpretation
owns decoding of JSON-compatible literals; `LogicalTokens` retains its
separate token-decoding contract. This specification introduces no CML grammar
or malformed-EOF policy.

The executable specification checks actual lexical line/block content and,
where applicable, Typesafe Config JSON decoded values. Its ScalaCheck
properties require at least 100 successful cases each, varying slash parity
and valid rendered display literals with forced escaped quotes, Unicode,
backslashes, control escapes and padding. Library-level evidence does not
establish downstream Cozy generation or CNCF generated-output acceptance.
