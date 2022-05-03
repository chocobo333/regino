
import json

import window
import ../lspschema
import ../lspprotocol


proc `window/showMessage`*(msg: string, msgtype: MessageType = MessageType.Log): (string, JsonNode) =
    (
        "window/showMessage",
        ShowMessageParams.create(
            msgtype.int,
            msg
        ).JsonNode
    )
proc showMessage*(window: Window, msg: string, msgtype: MessageType = MessageType.Log) =
    window.s.notify `window/showMessage`("[regino]: " & msg, msgtype)
