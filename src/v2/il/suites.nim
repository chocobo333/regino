
import il


proc newSuite*(stmts: openArray[Statement]): Suite =
    Suite(stmts: @stmts)

converter toSuite*(stmts: seq[Statement]): Suite =
    newSuite(stmts)

import stmts
proc typ*(self: Suite): Value =
    self.stmts[^1].typ
