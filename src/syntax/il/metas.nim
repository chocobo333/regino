
import il

proc Link*(_: typedesc[Metadata], params: seq[Expression] = @[]): Metadata =
    Metadata(kind: MetadataKind.Link, params: params)
proc ImportLL*(_: typedesc[Metadata], params: seq[Expression] = @[]): Metadata =
    Metadata(kind: MetadataKind.ImportLL, params: params)
proc Subtype*(_: typedesc[Metadata], params: seq[Expression] = @[]): Metadata =
    Metadata(kind: MetadataKind.Subtype, params: params)
proc Userdef*(_: typedesc[Metadata], name: string, params: seq[Expression] = @[]): Metadata =
    Metadata(kind: MetadataKind.Userdef, name: name, params: params)
