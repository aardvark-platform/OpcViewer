namespace Rabbyte.Drawing

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.UI
open Aardvark.UI.Primitives
open Aardvark.Base.IndexedGeometryPrimitives
open Aardvark.Geometry
open Aardvark.Base

type LineStyle = 
    | Solid
    | Dashed
    
type AreaStyle = 
    | Pattern
    | Filled
    | Empty

type PrimitiveType = 
    | Empty
    | Point
    | Line
    | PolyLine
    | Polygon

type SegmentCreation =
    | NoSegement
    | ProjDir of float * V3d    // SamplingRate // Projection Dir
    //| ViewPoint   // -> pro3d
    //| Sky         // -> pro3d
    //| Axis        // -> dibit

[<DomainType>]
type BrushStyle = 
    {
        primary     : ColorInput // use for lines and planes
        secondary   : ColorInput // use for vertices
        lineStyle   : Option<LineStyle>
        areaStyle   : Option<AreaStyle>
        thickness   : float
        samplingRate: float 
    }

type Segment = 
    {
        startPoint : V3d
        endPoint   : V3d 
        points     : plist<V3d> 
    }

[<DomainType>]
type DrawingModel = 
    {
        points          : plist<V3d>
        segments        : plist<Segment>
        style           : BrushStyle
        segmentCreation : SegmentCreation
        [<TreatAsValue>]
        past            : Option<DrawingModel>
        [<TreatAsValue>]
        future          : Option<DrawingModel>
        primitiveType   : PrimitiveType
        areaStyleNames  : hmap<AreaStyle, string>
        lineStyleNames  : hmap<LineStyle, string>
    
        //showOutline          : bool 
        //showDetailOutline    : bool 
        //alpha                : float
    }

type DrawingAction =
    | ChangeColorPrimary    of ColorPicker.Action
    | ChangeColorSecondary  of ColorPicker.Action
    | ChangeColorAuto       of ColorPicker.Action
    | ChangeThickness       of float
    | ChangeSamplingRate    of float
    | ChangeLineStyle       of Option<LineStyle>
    | ChangeAreaStyle       of Option<AreaStyle>
    | AddPoint              of V3d * (Option<V3d -> Option<V3d>>)
    | RecalculateSegments   of (V3d -> Option<V3d>)
    | RemoveLastPoint  
    | Clear
    | Finish 
    | FinishClose           of (Option<V3d -> Option<V3d>>)
    | Undo
    | Redo

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module DrawingModel =

    let defaultStyle = 
        {
            primary = { c = C4b.VRVisGreen }
            secondary = { c = C4b.Yellow }
            lineStyle = Some Solid
            areaStyle = Some Filled
            thickness = 3.0
            samplingRate = 0.2
        }

    let initial = 
        {                 
            style           = defaultStyle
            points          = plist.Empty
            segments        = plist.Empty
            segmentCreation = NoSegement
            past            = None
            future          = None
            primitiveType   = PrimitiveType.Empty
            areaStyleNames = HMap.ofList [Pattern, "Pattern"; Filled, "Filled"; AreaStyle.Empty, "Empty";]
            lineStyleNames = HMap.ofList [Solid, "Solid"; Dashed, "Dashed"]
        }