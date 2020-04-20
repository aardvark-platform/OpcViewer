namespace OpcViewer.Base.Attributes

open Aardvark.Base
open Aardvark.Base.Incremental

open OpcViewer.Base.FalseColors

[<DomainType>]
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
    | SetScalarMap              of ScalarLayer option
    | SetTexture                of TextureLayer option
    | ScalarsColorLegendMessage of FalseColorLegendApp.Action

[<DomainType>]
type AttributeModel =
    {
        scalarLayers        : ScalarLayer plist
        selectedScalar      : ScalarLayer option
        textureLayers       : TextureLayer plist
        selectedTexture     : TextureLayer option
    }