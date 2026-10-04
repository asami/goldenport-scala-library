# 2026-10-04: Ordinary logical-line escaped quotes

## Dependency and admitted scope

Cozy Phase 76 ordinary Logical UI CML properties require JSON-compatible
double-quoted literals while preserving decoded titles and labels. The
ordinary Kaleidox model builder routes documents through
`LogicalBlocks.Config.default.forLisp` or `Config.noLocation.forLisp` before
building its section AST. This library prerequisite is QUOTE-01/QUOTE-01A
for CNCF Phase 96 P96-03; it does not create another Phase.

The user authorized correcting this small upstream dependency directly in
the existing `scala12` primary checkout. The admitted implementation covers
the logical-line source, its new executable specification, the
[paired lexical contract](../../../spec/logical-lines-double-quote.md), and
this dated record.

## Source-based cause and correction

The ordinary branch of
[LogicalLines.DoubleQuoteState](../../../../src/main/scala/org/goldenport/parser/LogicalLines.scala)
previously closed on every double quote, including a quote escaped by an odd
trailing backslash run. Premature close and subsequent reopen could absorb a
following physical property or section boundary before ordinary CML parsing.

The frozen correction counts consecutive trailing backslashes only in the
nonraw quote branch. Odd runs append the quote verbatim; even or zero runs use
the existing closing transition. Existing triple-quoted raw transitions,
character accumulation, EOF handling and public fields remain unchanged.

## Authored specification scope and initial evidence

[LogicalLinesDoubleQuoteSpec](../../../../src/test/scala/org/goldenport/parser/LogicalLinesDoubleQuoteSpec.scala)
specifies parity, repeated/empty/Unicode/padded literals, lexical escapes,
script/lisp/raw configuration behavior, valid multiline and nested grouping,
raw triple termination, ordered ordinary document sections/prologues and
location behavior. Three ScalaCheck properties request at least 100 successful
cases each and inspect actual lexical and applicable decoded results.

At the authoring checkpoint, focused validation and independent acceptance
were pending. The selected parser accumulator consists of
LogicalLinesDoubleQuoteSpec, LogicalLinesSpec, LogicalBlocksSpec and
LogicalTokensSpec. No test execution or passing receipt had yet been recorded.
The validation result below does not establish independent acceptance. Local
upstream commit and dependency refresh, followed by real Cozy producer and
CNCF generated-output evidence, remain later parent-owned work; neither Step
nor Phase is closed by these edits.

The parent-recorded HYG-P96-PARSER-SIZE-001 remains separate follow-up debt:
LogicalLines exceeds the ordinary source-size threshold, while its exposed
nested states, sealed hierarchy and initialization make extraction nonlocal
to this four-path lexical correction. No source split is undertaken here.

## Focused validation result

The parent confirmed invocation `phase96-quote01a-test-001` for the exact
logical command:

```text
sbt --batch 'testOnly org.goldenport.parser.LogicalLinesDoubleQuoteSpec org.goldenport.parser.LogicalLinesSpec org.goldenport.parser.LogicalBlocksSpec org.goldenport.parser.LogicalTokensSpec'
```

Compile and Test compilation succeeded. Four suites completed with zero
aborted; 103 tests succeeded with zero failed, canceled, ignored or pending.
SBT and wrapper exits were both 0, and the shared lock was released. The three
new ScalaCheck properties each ran with `minSuccessful=100` as specified.

The observed runtime was SBT 1.9.7 with Adoptium Java 21.0.2. The existing
warning that `javaOptions` is ignored when `fork=false` is not a compilation
or test failure.

The parent admitted this result through `verify-typed-command-validation.py`
with status `accepted` for request `phase96-quote01a-command-request-001` r1
and subject `phase96-quote01a-validation-subject-001` r1.

This is focused validation, not a full-suite result or real Cozy producer/CNCF
generated-output proof. Independent protected-focused Step acceptance, local
upstream commit and dependency refresh remain pending. This result authorizes
no Step or Phase closure, commit or publication.
