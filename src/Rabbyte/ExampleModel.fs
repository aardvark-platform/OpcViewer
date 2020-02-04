namespace ExampleModel

open Aardvark.Base
open FSharp.Data.Adaptive
open Aardvark.UI.Primitives
open Rabbyte.Drawing
open Rabbyte.Annotation

[<ModelType>]
type ExampleModel = {
    camera        : CameraControllerState
    drawingEnabled: bool 
    hoverPosition : option<Trafo3d>
    drawing       : DrawingModel
    annotations   : AnnotationModel
}