
import parsers
from combinator import alt
import utils


proc recover*[I, O, E](parser: Parser[I, O, E], rcvr: Parser[I, O, E]): Parser[I, O, E] {.inline.} =
    alt(parser, rcvr).overwrite(
        genGraph("Recover", rcvr)
    )