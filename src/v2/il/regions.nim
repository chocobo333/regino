
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

proc Pair*(_: typedesc[Region], first, second: Region): Region =
    Region(kind: RegionKind.Pair, first: first, second: second)
proc Link*(_: typedesc[Region], to: Region): Region =
    Region(kind: RegionKind.Link, to: Region)

var
    rid = 0

proc Var*(_: typedesc[Region], lb: Region): Region =
    result = Region(kind: RegionKind.Var, id: rid, lb: lb)
    inc rid

import hashes
proc hash*(self: Region): Hash =
    0
proc `==`*(self, other: Region): bool =
    if self.kind == other.kind:
        case self.kind
        of RegionKind.Static:
            true
        of RegionKind.Global:
            true
        of RegionKind.Param:
            self.nth == other.nth
        of RegionKind.Return:
            true
        of RegionKind.Suite:
            self.parent == other.parent
        of RegionKind.Var:
            self.id == other.id
        of RegionKind.Link:
            self.to == other.to
    elif self.kind == RegionKind.Link:
        self.to == other
    elif other.kind == RegionKind.Link:
        self == other.to
    else:
        false
