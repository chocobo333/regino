
import sequtils


type
    FileInfo = object
        filename*: string
        phantom*: bool
        id*: FileId
    FileNamePool* = seq[FileInfo]
    FileId* = int

proc newFileNamePool*(): FileNamePool = @[]
var pool: FileNamePool = newFileNamePool()

proc getFileInfo*(id: FileId, pool: FileNamePool = pool): FileInfo =
    pool[id]

proc newFileId*(filename: string, pool: var FileNamePool = pool): FileId =
    let idx = pool.mapIt(it.filename).find(filename)
    if idx == -1:
        result = pool.len
        pool.add FileInfo(filename: filename, id: result)
    else:
        assert idx == pool[idx].id
        result = idx

