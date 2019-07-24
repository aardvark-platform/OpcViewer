namespace OpcViewer.Base.FalseColors

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.UI


[<DomainType>]
type FalseColorsModel = {
    useFalseColors  : bool
    lowerBound      : NumericInput
    upperBound      : NumericInput
    interval        : NumericInput
    invertMapping   : bool
    lowerColor      : ColorInput 
    upperColor      : ColorInput 
}

module FalseColorsModel = 
    let scalarsInterv  = {
        value   = 5.0
        min     = 0.0
        max     = 90.0
        step    = 0.0001
        format  = "{0:0.0000}"
    } 
    let initlb (range: Range1d) = {
        value   = range.Min
        min     = range.Min
        max     = range.Max
        step    = 0.0001
        format  = "{0:0.0000}"
    }
    let initub (range: Range1d) = {
        value   = range.Max
        min     = range.Min
        max     = range.Max
        step    = 0.0001
        format  = "{0:0.0000}"
    }
    let initDefinedScalarsLegend (range: Range1d) = 
        {
            useFalseColors  = false
            lowerBound      = initlb range
            upperBound      = initub range 
            interval        = scalarsInterv 
            invertMapping   = false
            lowerColor      = { c = C4b.Blue }
            upperColor      = { c = C4b.Red }
        }

   