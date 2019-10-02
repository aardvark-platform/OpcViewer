namespace ExampleModel

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.UI.Primitives
open Rabbyte.Drawing
open Rabbyte.Annotation

[<DomainType>]
type ExampleModel = {
    camera        : CameraControllerState
    drawingEnabled: bool 
    hoverPosition : option<Trafo3d>
    drawing       : DrawingModel
    annotations   : AnnotationModel
}