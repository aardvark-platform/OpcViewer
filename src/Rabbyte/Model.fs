namespace SimpleDrawingModel

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.UI.Primitives
open Rabbyte.Drawing
open Rabbyte.Annotation

[<DomainType>]
type SimpleDrawingModel = {
    camera        : CameraControllerState
    drawingEnabled: bool 
    hoverPosition : option<Trafo3d>
    drawing       : DrawingModel
    annotations   : AnnotationModel
}