namespace OpcViewer.Base.Attributes

open Aardvark.Base
open Aardvark.Base.Rendering
open Aardvark.Base.Incremental

open OpcViewer.Base.FalseColors

type TextureLayer = {
    label : string
    index : int
}

[<DomainType>]
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



[<DomainType>]
type AttributeModel =
    {
        scalarLayers         : hmap<string, ScalarLayer> 
        selectedScalar       : option<ScalarLayer>
        textureLayers        : plist<TextureLayer>
    }


    

   