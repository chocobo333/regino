
import il

proc Static*(_: typedesc[Region]): Region =
    Region(kind: RegionKind.Static)
proc Global*(_: typedesc[Region]): Region =
    Region(kind: RegionKind.Global)
proc Param*(_: typedesc[Region], nth: Natural): Region =
    Region(kind: RegionKind.Param, nth: nth)
proc Return*(_: typedesc[Region]): Region =
    Region(kind: RegionKind.Return)
proc Suite*(_: typedesc[Region], parent: Region = Region.Global): Region =
    Region(kind: RegionKind.Suite, parent: parent)
proc Var*(_: typedesc[Region], lb: Region): Region =
    Region(kind: RegionKind.Var, lb: lb)
proc Pair*(_: typedesc[Region], first, second: Region): Region =
    Region(kind: RegionKind.Pair, first: first, second: second)
proc Link*(_: typedesc[Region], to: Region): Region =
    Region(kind: RegionKind.Link, to: Region)
