namespace OpcViewer.Base.Attributes

open Aardvark.Base
open Aardvark.Base.Rendering
open FSharp.Data.Adaptive
open Adaptify

open OpcViewer.Base.FalseColors

type TextureLayer = {
    label : string
    index : int
}

[<ModelType>]
type ScalarLayer = {
    label        : string
    actualRange  : Range1d
    definedRange : Range1d
    index        : int
    colorLegend  : FalseColorsModel
}
type AttributeLayer = 
    | ScalarLayer of ScalarLayer 
    | TextureLayer of TextureLayer

type AttributeAction =
  | SetScalarMap     of Option<ScalarLayer>
  | ScalarsColorLegendMessage of FalseColorLegendApp.Action



[<ModelType>]
type AttributeModel =
    {
        scalarLayers         : HashMap<string, ScalarLayer> 
        selectedScalar       : option<ScalarLayer>
        textureLayers        : IndexList<TextureLayer>
    }


    

   