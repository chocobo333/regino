
import parsers
import utils


proc drop*[I, O, E](parser: Parser[I, O, E]): Parser[I, O, E] =
    parser.dropped = true
    return parser.overwrite(genGraph("Drop", parser))