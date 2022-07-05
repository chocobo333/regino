
import il
import values

import ../../lineinfos


proc newSuite*(stmts: openArray[Statement]): Suite =
    Suite(stmts: @stmts)

converter toSuite*(stmts: seq[Statement]): Suite =
    newSuite(stmts)

proc isFailed*(self: Suite): bool =
    self.stmts.len == 1 and self.stmts[0].kind == StatementKind.Fail

proc typ*(self: Suite|Program): Value =
    if self.stmts.len == 0:
        Value.Unit
    else:
        self.stmts[^1].typ

proc loc*(self: Suite): Location =
    let
        s = self.stmts[0].loc
        e = self.stmts[^1].loc
    newLocation(s.uri, s.range.a, e.range.b)
