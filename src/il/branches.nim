
import il


proc newElif*(cond: Expression, suite: Suite): ElifBranch =
    (cond: cond, suite: suite)
proc newOf*(pat: Pattern, suite: Suite): OfBranch =
    (pat: pat, suite: suite)
proc newElse*(suite: Suite): ElseBranch =
    suite
