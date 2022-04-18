
import json

import window
import ../lspschema
import ../lspprotocol


proc `window/logMessage`*(msg: string, msgtype: MessageType = MessageType.Log): (string, JsonNode) =
    (
        "window/logMessage",
        ShowMessageParams.create(
            msgtype.int,
            msg
        ).JsonNode
    )
proc logMessage*(window: Window, msg: string, msgtype: MessageType = MessageType.Log) =
    window.s.notify `window/logMessage`("[regino]: " & msg, msgtype)
