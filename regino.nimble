# Package

version       = "0.1.0"
author        = "chocobo333"
description   = "A new awesome nimble package"
license       = "MIT"
srcDir        = "src"
bin           = @["regino"]


# Dependencies

requires "nim >= 1.4.2"
requires "https://github.com/chocobo333/eat-nim"
requires "https://github.com/chocobo333/llvm-nim"
requires "https://github.com/chocobo333/nim-coloredString"
requires "rts"