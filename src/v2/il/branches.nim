
import il


proc newElif*(cond: Expression, suite: Suite): ElifBranch =
    (cond: cond, suite: suite)
proc newElse*(suite: Suite): ElseBranch =
    suite
