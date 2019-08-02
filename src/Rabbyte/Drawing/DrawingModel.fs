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

type PrimitiveStatus = 
    | Empty
    | InProgress
    | Point
    | Line
    | PolyLine
    | Polygon

type SegmentCreation =
    | NoSegement
    | Linear of float           // SamplingRate -> linear subsampled
    | ProjDir of float * V3d    // SamplingRate // Projection Dir
    //| ViewPoint   // -> pro3d
    //| Sky         // -> pro3d
    //| Axis        // -> dibit

type BrushStyle = {
    primary     : ColorInput // use for lines and planes
    secondary   : ColorInput // use for vertices
    lineStyle   : LineStyle
    areaStyle   : AreaStyle
    thickness   : NumericInput
    samplingRate: NumericInput
}

type Segment = {
    startPoint : V3d
    endPoint   : V3d 
    points     : plist<V3d> 
}

[<DomainType>]
type DrawingModel = {
    points          : plist<V3d>
    segments        : plist<Segment>
    style           : BrushStyle
    segmentCreation : SegmentCreation
    [<TreatAsValue>]
    past            : Option<DrawingModel>
    [<TreatAsValue>]
    future          : Option<DrawingModel>
    status          : PrimitiveStatus
}

type DrawingAction =
    | ChangeColorPrimary    of ColorPicker.Action
    | ChangeColorSecondary  of ColorPicker.Action
    | ChangeColorAuto       of ColorPicker.Action
    | ChangeThickness       of Numeric.Action
    | ChangeSamplingRate    of Numeric.Action
    | ChangeLineStyle       of LineStyle
    | ChangeAreaStyle       of AreaStyle
    | AddPoint              of V3d * (Option<V3d -> Option<V3d>>)
    | RecalculateSegments   of (V3d -> Option<V3d>)
    | RemoveLastPoint  
    | Clear
    | Finish 
    | FinishClose
    | Undo
    | Redo

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module DrawingModel =
    let defaultThickness    : NumericInput = { value = 3.0; min = 1.0; max = 8.0;step = 1.0;format = "{0:0}" }
    let defaultSamplingRate : NumericInput = { value = 0.2; min = 0.02; max = 10.0;step = 0.02;format = "{0:00}" }

    let defaultStyle = 
        {
            primary = { c = C4b.VRVisGreen }
            secondary = { c = C4b.Yellow }
            lineStyle = Solid
            areaStyle = Filled
            thickness = defaultThickness
            samplingRate = defaultSamplingRate
        }

    let inital = {                 
        style           = defaultStyle
        points          = plist.Empty
        segments        = plist.Empty
        segmentCreation = NoSegement
        past            = None
        future          = None
        status          = PrimitiveStatus.Empty
    }
