namespace Rabbyte.Annotation

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.UI

open Rabbyte.Drawing

type ClippingVolumeType = 
    | Direction of V3d
    | Point of V3d
    | Points of plist<V3d>

[<DomainType>]
type Annotation = 
    {
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
        clippingVolume  : ClippingVolumeType
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
type AnnotationModel = 
    {
        annotations         : plist<Annotation>
        annotationsGrouped  : hmap<C4b, plist<Annotation>>
        showDebug           : bool
        extrusionOffset     : float
    }

type AnnotationAction = 
    | AddAnnotation of DrawingModel*Option<ClippingVolumeType>
    | ChangeExtrusionOffset of float
    | ShowDebugVis
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
            clippingVolume = Direction V3d.ZAxis //Points ([V3d.IOO; V3d.OIO; -V3d.OIO; -V3d.IOO] |> PList.ofList)//Point V3d.Zero// Direction (V3d.ZAxis)
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

    let convertDrawingToAnnotation (drawingModel:DrawingModel) (clippingVolumeType:Option<ClippingVolumeType>) = 
        let defaultClippingVolume = 
            { initAnnotation with 
                points = drawingModel.points
                segments = drawingModel.segments
                style = drawingModel.style
                primitiveType = drawingModel.primitiveType
            }

        match clippingVolumeType with
        | Some t -> { defaultClippingVolume with clippingVolume = t }
        | None -> defaultClippingVolume

    let convertAnnotationToDrawing (annotation:Annotation) = 
        { DrawingModel.initial with
            style = annotation.style
            points = annotation.points
            segments = annotation.segments
            primitiveType = annotation.primitiveType
        }        

    let initial = 
        {
            annotations = plist.Empty
            annotationsGrouped = hmap.Empty
            showDebug   = false
            extrusionOffset = 10.0
        }