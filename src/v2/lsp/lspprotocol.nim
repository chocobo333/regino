
import strutils
import strformat
import streams
import json
import sugar
import options

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

proc id(self: RequestMessage): int =
    let id = self["id"]
    case id.kind
    of JString:
        id.getStr.parseInt
    of JInt:
        id.getInt
    else:
        raise newException(IllformedError, "")

proc `method`*(self: RequestMessage or NotificationMessage): string =
    self["method"].getStr
proc `params`*(self: RequestMessage or NotificationMessage): JsonNode =
    let
        params = self["params"]
    if params.isSome:
        params.get
    else:
        newJNull()

proc response*(self: RequestMessage, n: JsonNode): JsonNode =
    ResponseMessage.create(jsonrpc, self.id, some n, none ResponseError).JsonNode
proc respond*(s: Stream, msg: RequestMessage, n: JsonNode) =
    s.sendMessage msg.response(n)
proc notify*(s: Stream, `method`: string) =
    s.sendMessage NotificationMessage.create(jsonrpc, `method`, none JsonNode).JsonNode
proc notify*(s: Stream, `method`: string, params: JsonNode) =
    s.sendMessage NotificationMessage.create(jsonrpc, `method`, some params).JsonNode
proc notify*(s: Stream, p: (string, JsonNode)) =
    s.notify(p[0], p[1])
