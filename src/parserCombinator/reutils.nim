
import nre


proc rest*(self: RegexMatch): string =
    self.str[self.matchBounds.b+1..^1]