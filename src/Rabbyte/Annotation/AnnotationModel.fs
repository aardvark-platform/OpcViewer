namespace Rabbyte.Annotation

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.UI

open Rabbyte.Drawing

[<DomainType>]
type Annotation = {
    version     : int

    [<PrimaryKey; NonIncremental>]
    key         : Guid

    modelTrafo  : Trafo3d

    //geometry    : Geometry   // Pro3D
    //projection  : Projection // Pro3D
    //semantic    : Semantic   // Pro3D

    points          : plist<V3d>
    segments        : plist<Segment>
    style           : BrushStyle
    primitiveType   : PrimitiveType // similar to Geometry of Pro3D

    //color       : ColorInput       // use Style...
    //thickness   : NumericInput     // use Style...

    //results     : Option<AnnotationResults'>      // Pro3D
    //dnsResults  : Option<DipAndStrikeResults'>    // Pro3D

    visible     : bool
    //showDns     : bool  // Pro3d
    text        : string
    textsize    : float //NumericInput

    surfaceName : string
    //view        : CameraView // Pro3d

    //semanticId   : SemanticId      // Pro3D
    //semanticType : SemanticType    // Pro3D
} with 
    static member current = 1


[<DomainType>]
type AnnotationModel = {
    annotations : plist<Annotation>
    annotationsGrouped : hmap<C4b, plist<Annotation>>
}

type AnnotationAction = 
    | AddAnnotation of DrawingModel
    //| RemoveDrawing of DrawingModel
    //| EditDrawing of DrawingModel

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module AnnotationModel = 
    
    let initAnnotation =
        {
            version     = Annotation.current
            key         = Guid.NewGuid()
            modelTrafo  = Trafo3d.Identity
            primitiveType = PrimitiveType.Empty
            //geometry    = geometry
            //semantic    = Semantic.None
            points      = plist.Empty
            segments    = plist.Empty //[]
            style       = DrawingModel.defaultStyle
            //color       = color
            //thickness   = thickness
            //results     = None
            //dnsResults  = None            
            //projection  = projection
            visible     = true
            text        = ""
            textsize    = 4.0
            //showDns     = 
            //    match geometry with 
            //      | Geometry.DnS -> true 
            //      | _ -> false 
            surfaceName = ""
            //view = FreeFlyController.initial.view
        }

    let convertDrawingToAnnotation (drawingModel:DrawingModel) = 
        { 
            initAnnotation with 
                points = drawingModel.points
                segments = drawingModel.segments
                style = drawingModel.style
                primitiveType = drawingModel.primitiveType
        }

    let convertAnnotationToDrawing (annotation:Annotation) =
        { 
            DrawingModel.initial with
                style = annotation.style
                points = annotation.points
                segments = annotation.segments
                primitiveType = annotation.primitiveType
        }        

    let initial =  
        {
            annotations = plist.Empty
            annotationsGrouped = hmap.Empty
        }