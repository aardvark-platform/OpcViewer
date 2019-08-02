namespace SimpleDrawingModel

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.UI.Primitives
open Rabbyte.Drawing

[<DomainType>]
type SimpleDrawingModel = {
    camera        : CameraControllerState
    draw          : bool 
    hoverPosition : option<Trafo3d>
    drawing       : DrawingModel
}