
import il


proc newSuite*(stmts: openArray[Statement]): Suite =
    Suite(stmts: @stmts)

converter toSuite*(stmts: seq[Statement]): Suite =
    newSuite(stmts)
