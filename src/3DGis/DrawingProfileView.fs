namespace  ElevationProfileViewer

open System
open Aardvark.UI
open Aardvark.Base
open OpcViewer.Base
open Aardvark.Base.Incremental
open OpcViewer.Base.Picking
open Rabbyte.Drawing

module DrawingProfileView = 

    let px = "px"
    let strokeWidthMainRect = "0.1px"
    let strokeColor = "rgb(255,255,255)"
    let lineOpacity = "0.15"
    let polygonOpacity = "0.25"
    let polygonColor = "rgb( 132,226,255)"

    let inline (=>) a b = Attributes.attribute a b
 
    let mainBoxRect (m : MModel)=
        Incremental.Svg.rect (
            amap {                     
                let! xOffset = m.offsetUIDrawX
                let! yOffset = m.offsetUIDrawY

                let! dimensions = m.cutViewDim
                let xWidth = (float) dimensions.X
                let yWidth = (float) dimensions.Y

                let sX = (sprintf "%f" (xOffset * xWidth)) + px
                let sY = (sprintf "%f" (yOffset * yWidth)) + px

                let wX = (sprintf "%f" ((1.0-xOffset*2.0) * xWidth)) + px
                let wY = (sprintf "%f" ((1.0-yOffset*2.0) * yWidth)) + px

                yield attribute "x" sX
                yield attribute "y" sY 
                yield attribute "rx" "1px"
                yield attribute "ry" "1px" 
                yield attribute "width" wX
                yield attribute "height" wY
                yield attribute "fill" "url(#grad2)"
                yield attribute "opacity" "0.25"
                yield attribute "stroke" strokeColor
                yield attribute "stroke-width" strokeWidthMainRect
                yield onMouseLeave (fun _ -> HoveredCircleLeave)
            } |> AttributeMap.ofAMap
        )
             
    let contourLine (m : MModel) (order : float) (strokeWidth : float) (opacity : string) =
        Incremental.Svg.line ( 
            amap {
                let! xOffset = m.offsetUIDrawX
                let! yOffset = m.offsetUIDrawY
                        
                let! dimensions = m.cutViewDim
                let xWidth = (float) dimensions.X
                let yWidth = (float) dimensions.Y

                let heightRectH = (1.0-yOffset*2.0)/5.0

                let sX = (sprintf "%f" ((xOffset) * xWidth-5.0)) + px
                let sY = (sprintf "%f" ((yOffset + heightRectH * order) * yWidth)) + px

                let wX = (sprintf "%f" ((1.0-xOffset) * xWidth+5.0)) + px

                yield attribute "x1" sX
                yield attribute "y1" sY  
                yield attribute "x2" wX
                yield attribute "y2" sY 
                yield attribute "stroke" strokeColor
                yield attribute "stroke-width" (sprintf "%f" strokeWidth)
                yield attribute "stroke-opacity" opacity
            } |> AttributeMap.ofAMap
        )

    let verticalLine (m : MModel) (i : int) (numofPointsinLineList : int list) (coordArray : string[])=
        Incremental.Svg.line ( 
            amap {
                let rec countElemOffset i acc = 
                    if i >= 1 then
                        countElemOffset (i-1) (acc + numofPointsinLineList.Item(i))
                    else 
                        acc + numofPointsinLineList.Item(0)
                        
                let numElemOffset = countElemOffset i 0

                let xy1 = coordArray.[numElemOffset-1].Split([|","|], StringSplitOptions.None)
                let xy2 = coordArray.[numElemOffset].Split([|","|], StringSplitOptions.None)
                        
                let avgX = ((xy1.[0] |> float) + (xy2.[0] |> float)) / 2.0

                let! yOffset = m.offsetUIDrawY

                let! dimensions = m.cutViewDim
                let yWidth = (float) dimensions.Y
                            
                let sX = sprintf "%f" avgX
                        
                let sY = (sprintf "%f" ((yOffset - 0.025) * yWidth)) + px

                let wY = (sprintf "%f" ((1.0-yOffset+0.025) * yWidth)) + px

                yield attribute "x1" sX
                yield attribute "y1" sY
                yield attribute "x2" sX
                yield attribute "y2" wY
                yield attribute "stroke" strokeColor
                yield attribute "stroke-width" "1.0"
                yield attribute "stroke-opacity" "0.6"
            } |> AttributeMap.ofAMap
        )           

    let containerAttribs (m : MModel) = 
        amap {
            let! dimensions = m.cutViewDim

            let widthValue = (sprintf "%i" (dimensions.X))

            yield onWheelPrevent true (fun x -> id (Message.MouseWheel x))
            yield clazz "mySvg"
            yield style ("width: " + widthValue + "px; height: 100%; user-select: none;")
        } |> AttributeMap.ofAMap

    let drawSVGElements (m : MModel) = 
        Incremental.Svg.svg (containerAttribs m) <|
            alist {            
                //Gradient Color
                yield Svg.defs[][                
                    Svg.linearGradient [attribute "id" "grad2"; attribute "x1" "0%"; attribute "y1" "0%"; attribute "x2" "0%"; attribute "y2" "100%"]   [
                        Svg.stop [attribute "offset" "0%"; attribute "style" "stop-color:rgb(255,255,255);stop-opacity:1.0" ]
                        Svg.stop [attribute "offset" "100%"; attribute "style" "stop-color:rgb(16,16,16);stop-opacity:1.0"] 
                    ]
                ]
                         
                //Box
                yield mainBoxRect m                           

                //contour line (very high)
                yield contourLine m 0.0 2.5 lineOpacity   

                //contour line (high)
                yield contourLine m 1.0 1.5 lineOpacity                       

                //contour line (medium high)
                yield contourLine m 2.0 1.5 lineOpacity      

                //contour line (medium low)
                yield contourLine m 3.0 1.5 lineOpacity      

                //contour line (low)
                yield contourLine m 4.0 1.5 lineOpacity      

                //contour line (very low)
                yield contourLine m 5.0 2.5 lineOpacity     
                            
                yield Incremental.Svg.text ( 
                    amap {
                        let! xOffset = m.offsetUIDrawX
                        let! yOffset = m.offsetUIDrawY

                        let! dimensions = m.cutViewDim
                        let xWidth = (float) dimensions.X
                        let yWidth = (float) dimensions.Y

                        let sX = (sprintf "%f" (xOffset * 0.8 * xWidth)) 
                        let sY = (sprintf "%f" ((1.0-yOffset) * yWidth) )

                        yield clazz "label"
                        yield attribute "x" sX
                        yield attribute "y" sY     
                        yield attribute "transform" ("rotate(-90" + "," + sX + "," + sY + ")")
                        yield attribute "font-size" "16"
                        yield attribute "fill" "#ffffff"
                        yield attribute "font-family" "Roboto Mono, sans-serif"

                                    
                    } |> AttributeMap.ofAMap
                ) (Mod.constant "Elevation [m]")

                yield Incremental.Svg.text ( 
                    amap {
                        let! xOffset = m.offsetUIDrawX
                        let! yOffset = m.offsetUIDrawY

                        let! dimensions = m.cutViewDim
                        let xWidth = (float) dimensions.X
                        let yWidth = (float) dimensions.Y

                        let sX = (sprintf "%f" (xOffset * 1.0 * xWidth)) 
                        let sY = (sprintf "%f" ((1.0-yOffset) * yWidth+30.0) ) 

                        yield clazz "label"
                        yield attribute "x" sX
                        yield attribute "y" sY     
                        yield attribute "font-size" "16"
                        yield attribute "fill" "#ffffff"
                        yield attribute "font-family" "Roboto Mono, sans-serif"

                                    
                    } |> AttributeMap.ofAMap
                ) (Mod.constant "Distance [m]")

                yield Incremental.Svg.text ( 
                    amap {
                        let! xOffset = m.offsetUIDrawX
                        let! yOffset = m.offsetUIDrawY

                        let! dimensions = m.cutViewDim
                        let xWidth = (float) dimensions.X
                        let yWidth = (float) dimensions.Y

                        let sX = (sprintf "%f" (xOffset * 0.9 * xWidth)) 
                        let sY = (sprintf "%f" ((yOffset) * yWidth + 6.0) )

                        yield clazz "label"
                        yield attribute "x" sX
                        yield attribute "y" sY     
                        yield attribute "text-anchor" "end"     
                        yield attribute "font-size" "12"
                        yield attribute "font-style" "italic"
                        yield attribute "fill" "#ffffff"
                        yield attribute "font-family" "Roboto Mono, sans-serif"                         
                    } |> AttributeMap.ofAMap
                ) ( Mod.map2 (fun f g -> 
                                 if not (f = 0.0) && not (g = 0.0) then
                                     let diff = Math.Round((f : float) - (g : float), 2)
                                     diff.ToString() + " m"
                                 else 
                                     ""
                              ) m.maxHeight m.minHeight )

                yield Incremental.Svg.text ( 
                    amap {
                        let! xOffset = m.offsetUIDrawX
                        let! yOffset = m.offsetUIDrawY

                        let! dimensions = m.cutViewDim
                        let xWidth = (float) dimensions.X
                        let yWidth = (float) dimensions.Y

                        let sX = (sprintf "%f" ((1.0-xOffset) * xWidth+5.0))
                        let sY = (sprintf "%f" ((1.0-yOffset) * yWidth+18.0))

                        yield clazz "label"
                        yield attribute "x" sX
                        yield attribute "y" sY     
                        yield attribute "text-anchor" "end"     
                        yield attribute "font-size" "12"
                        yield attribute "font-style" "italic"
                        yield attribute "fill" "#ffffff"
                        yield attribute "font-family" "Roboto Mono, sans-serif"
                           
                    } |> AttributeMap.ofAMap
                ) (m.linearDistance |> Mod.map (fun f ->  if not (f = 0.0) then Math.Round(f,2).ToString() + " m" else ""))

                //chart curve 
                yield Incremental.Svg.polyline ( 
                    amap {
                        let! drawingCoord = m.svgPointsCoord

                        yield attribute "points" drawingCoord
                        yield attribute "fill" "none"
                        yield attribute "opacity" "0.8"
                        yield attribute "stroke" "rgb(50,208,255)"
                        yield attribute "stroke-width" "2.0"
                    } |> AttributeMap.ofAMap
                )

                let! drawingErrorCoord = m.svgPointsErrorCoord                                  
                let errorCoordArray = drawingErrorCoord.Split([|"  "|], StringSplitOptions.None)
                for i in 0 .. errorCoordArray.Length - 1 do
                    //chart curve error
                    yield Incremental.Svg.polyline ( 
                        amap {
                            let drawingCoord = errorCoordArray.[i]

                            yield attribute "points" drawingCoord
                            yield attribute "fill" "none"
                            yield attribute "opacity" "0.8"
                            yield attribute "stroke" "rgb(255,71,50)"
                            yield attribute "stroke-width" "2.0"
                        } |> AttributeMap.ofAMap
                    )

                //chart surface under curve
                yield Incremental.Svg.polygon ( 
                    amap {
                        let! drawingCoord = m.svgSurfaceUnderLineCoord

                        yield attribute "points" drawingCoord
                        yield attribute "fill" polygonColor
                        yield attribute "opacity" polygonOpacity
                        yield attribute "stroke-width" "0.0"
                    } |> AttributeMap.ofAMap
                )

                //chart surface under curve error
                yield Incremental.Svg.polygon ( 
                    amap {
                        let! drawingCoord = m.svgSurfaceUnderLineErrorCoord
                                   
                        yield attribute "points" drawingCoord
                        yield attribute "fill" "rgb(255,132,132)"
                        yield attribute "opacity" polygonOpacity
                        yield attribute "stroke-width" "0.0"
                    } |> AttributeMap.ofAMap
                )    

                let! circleSize = m.svgCircleSize
                let! dim = m.cutViewDim
                            
                let r = sprintf "%f" (Math.Min(circleSize/4.0 * float dim.X/100.0, 6.8))                              
                let stw = 
                    sprintf "%f" (Math.Min(circleSize/12.0 * float dim.X/100.0, 2.25) 
                    |> (fun x -> if x >= 1.0 then x else 0.0)) + "px"       

                //draw correct circles
                let! drawingCoord = m.svgPointsCoord                                  
                let coordArray = drawingCoord.Split([|" "|], StringSplitOptions.None)                                
                for i in 0 .. coordArray.Length - 2 do
                    let xy = coordArray.[i].Split([|","|], StringSplitOptions.None)
                    if xy.Length > 1 then
                        let x = xy.[0]
                        let y = xy.[1]
                        yield Incremental.Svg.circle 
                            (
                            [attribute "cx" x;
                            attribute "cy" y;
                            attribute "r" r;
                            attribute "stroke" "black";
                            attribute "stroke-width" stw;
                            attribute "fill" "rgb(50,208,255)";
                            onMouseEnter (fun _ -> HoveredCircleEnter i) ]
                            |> AttributeMap.ofList
                            )      

                        let boxX = x |> float
                                    
                        let boxR = r |> float
                        let! yOffset = m.offsetUIDrawY
                        let! dimensions = m.cutViewDim
                        let yWidth = (float) dimensions.Y

                        let sY = (sprintf "%f" (yOffset * yWidth)) + px

                        let wY = (sprintf "%f" ((1.0-yOffset*2.0) * yWidth)) + px

                        yield Incremental.Svg.rect 
                            (
                            [attribute "x" (sprintf "%f" (boxX-boxR)); 
                            attribute "y" sY; 
                            attribute "width" (sprintf "%f" (boxR * 2.0)); 
                            attribute "height" wY; 
                            attribute "stroke-width" "0"; 
                            attribute "fill" "rgba(50,208,255,0)"; 
                            clazz "hoverRect" ;
                            onMouseEnter (fun _ -> HoveredCircleEnter i) ] 
                            |> AttributeMap.ofList
                            )      
                                    
                // draw separating vertical lines
                let! numofPointsinLineList = m.numofPointsinLineList
                for i in 0 .. numofPointsinLineList.Length-2 do
                    yield verticalLine m i numofPointsinLineList coordArray

                // draw error circles
                let! drawingErrorCoord = m.svgPointsErrorCoord                                  
                let coordArray = drawingErrorCoord.Split([|" "|], StringSplitOptions.None)
                for i in 0 .. coordArray.Length - 2 do
                    let xy = coordArray.[i].Split([|","|], StringSplitOptions.None)
                    if xy.Length > 1 then
                        let x = xy.[0]
                        let y = xy.[1]
                        yield Incremental.Svg.circle 
                            (
                            [attribute "cx" x; 
                            attribute "cy" y; 
                            attribute "r" r; 
                            attribute "stroke" "black"; 
                            attribute "stroke-width" stw; 
                            attribute "fill" "rgb(255,71,50)" ] 
                            |> AttributeMap.ofList)                              

                let! hoverCircle = m.hoveredCircleIndex
                            
                if hoverCircle.IsSome then 
                    let! yOffset = m.offsetUIDrawY

                    let! dimensions = m.cutViewDim
                    let xWidth = (float) dimensions.X
                    let yWidth = (float) dimensions.Y

                    let! aL = m.altitudeList

                    let! samplingDistance = m.samplingDistance

                    let coordArray = drawingCoord.Split([|" "|], StringSplitOptions.None) 

                    let xy = coordArray.[hoverCircle.Value].Split([|","|], StringSplitOptions.None)
                    if xy.Length > 1 then
                        let x = xy.[0]
                        let y = xy.[1]
                                    
                        let boxR = r |> float
                        let sY = (sprintf "%f" (yOffset * yWidth)) + px
                        let wY = (sprintf "%f" ((1.0-yOffset*2.0) * yWidth)) + px
                        yield Incremental.Svg.rect 
                            (
                            [attribute "x" (sprintf "%f" ((x |> float)-boxR)); 
                            attribute "y" sY; 
                            attribute "width" (sprintf "%f" (boxR * 2.0)); 
                            attribute "height" wY; attribute "stroke-width" "0"; 
                            attribute "fill" "rgba(237,55,66,0.9)" ] 
                            |> AttributeMap.ofList
                            )      
                                    
                        yield Incremental.Svg.circle 
                            (
                            [attribute "cx" x; 
                            attribute "cy" y; 
                            attribute "r" r; 
                            attribute "stroke" "black"; 
                            attribute "stroke-width" stw; 
                            attribute "fill" "rgb(225,225,225)"; 
                            onMouseLeave (fun _ -> HoveredCircleLeave) ] 
                            |> AttributeMap.ofList
                            )                              
                                    
                        let textAnchor = 
                            let textPosition = (x |> float) / xWidth
                            if textPosition < 0.15 then
                                "begin"
                            elif textPosition < 0.85 then
                                "middle"
                            else
                                "end"

                        yield Incremental.Svg.text ( 
                            amap {
                                yield clazz "label"
                                yield attribute "x" x
                                yield attribute "y" (sprintf "%f" (yOffset * yWidth - 10.0) + px)     
                                yield attribute "font-size" "16"; 
                                yield attribute "fill" "#ffffff"; 
                                yield attribute "font-family" "Raleway, sans-serif"; 
                                yield attribute "font-weight" "600"; 
                                yield attribute "font-stretch" "expanded"; 
                                yield clazz "UIText"; 
                                yield attribute "text-anchor" textAnchor
                           
                            } |> AttributeMap.ofAMap
                        ) ( Mod.constant ("Altitude: " + sprintf "%.2f" (aL.Item(hoverCircle.Value)) + " m | Distance: " + sprintf "%.2f" (samplingDistance * float (hoverCircle.Value)) + " m") )

            }
        
        