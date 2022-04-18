
import streams

type
    Window* = object
        s*: Stream

proc window*(s: Stream): Window = Window(s: s)
