
import il


proc newComment*(s: string, isDoc: bool = false): Comment =
    Comment(s: s, isDoc: isDoc)
