
import il

proc Global*(_: typedesc[Region]): Region =
    Region(kind: RegionKind.Global)
proc Param*(_: typedesc[Region], nth: Natural): Region =
    Region(kind: RegionKind.Param, nth)
proc Return*(_: typedesc[Region]): Region =
    Region(kind: RegionKind.Return)
proc Stack*(_: typedesc[Region]): Region =
    Region(kind: RegionKind.Stack)
proc Heap*(_: typedesc[Region]): Region =
    Region(kind: RegionKind.Heap)
proc Var*(_: typedesc[Region], ub: Region): Region =
    Region(kind: RegionKind.Var, ub: Region)
proc Link*(_: typedesc[Region], ub: Region): Region =
    Region(kind: RegionKind.Link, ub: Region)
