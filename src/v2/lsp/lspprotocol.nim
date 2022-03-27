
import strutils
import strformat
import streams
import json
import sugar

import eat

import lspschema


let
    jsonrpc* = "2.0"

let
    sp0 = sp(0)
    number = p"[0-9]+"
    untilcolon = p".*?:"

proc readMessage*(s: Stream): JsonNode =
    var
        contentLength: int = -1
        line: string
    while true:
        line = s.readLine
        if line.len != 0:
            break
    while true:
        let span = terminated(untilcolon, sp0)(line)
        case span.get.fragment
        of "Content-Length:":
            contentLength = (number @ (it => it.fragment.parseInt))(span.getSrc.fragment).get
        else:
            discard
        line = s.readLine
        if line.len == 0:
            break
    if contentLength != -1:
        s.readStr(contentLength).parseJson
    else:
        raise newException(IllformedError, "Missing Content-Length header")

proc sendMessage*(s: Stream, n: JsonNode) =
    var msg = newStringOfCap(1024)
    toUgly(msg, n)
    s.write &"Content-Length: {msg.len}\r\n\r\n{msg}"
    s.flush
