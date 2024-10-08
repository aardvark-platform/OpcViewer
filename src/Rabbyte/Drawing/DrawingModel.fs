namespace Rabbyte.Drawing

open Aardvark.Base
open FSharp.Data.Adaptive
open Aardvark.UI

open Adaptify

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

[<ModelType>]
type BrushStyle = 
    {
        primary     : C4b
        secondary   : C4b
        lineStyle   : LineStyle
        areaStyle   : AreaStyle
        thickness   : float
        samplingRate: float 
    }

type Segment = 
    {
        startPoint : V3d
        innerPoints: IndexList<V3d> 
        endPoint   : V3d 
    }

[<ModelType>]
type DrawingModel = 
    {
        points          : IndexList<V3d>
        segments        : IndexList<Segment>
        style           : BrushStyle
        segmentCreation : SegmentCreation
        [<TreatAsValue>]
        past            : Option<DrawingModel>
        [<TreatAsValue>]
        future          : Option<DrawingModel>
        primitiveType   : PrimitiveType
        areaStyleNames  : HashMap<AreaStyle, string>
        lineStyleNames  : HashMap<LineStyle, string>
    }

type DrawingAction =
    | ChangeColorPrimary    of C4b
    | ChangeColorSecondary  of C4b
    | ChangeColorAuto       of C4b
    | ChangeThickness       of float
    | ChangeSamplingRate    of float
    | ChangeLineStyle       of LineStyle
    | ChangeAreaStyle       of AreaStyle
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
            primary = C4b.VRVisGreen
            secondary = C4b.Yellow
            lineStyle = Solid
            areaStyle = Filled
            thickness = 3.0
            samplingRate = 0.2
        }

    let initial = 
        {                 
            style           = defaultStyle
            points          = IndexList.Empty
            segments        = IndexList.Empty
            segmentCreation = NoSegement
            past            = None
            future          = None
            primitiveType   = PrimitiveType.Empty
            areaStyleNames = HashMap.ofList [Pattern, "Pattern"; Filled, "Filled"; AreaStyle.Empty, "Empty";]
            lineStyleNames = HashMap.ofList [Solid, "Solid"; Dashed, "Dashed"]
        }

    let reset m =
        { m with
            points          = IndexList.Empty
            segments        = IndexList.Empty
            past            = None
            future          = None
            primitiveType   = PrimitiveType.Empty
        }