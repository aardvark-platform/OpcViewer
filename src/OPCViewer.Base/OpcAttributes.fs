namespace OpcViewer.Base.Attributes

open System
open System.IO
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Base.Rendering
open Aardvark.SceneGraph
open Aardvark.Rendering.Text
open FShade
open Aardvark.UI
open Aardvark.UI.Operators
open Aardvark.UI.Trafos
open Aardvark.SceneGraph.Opc

open OpcViewer.Base.FalseColors //OpcSelectionViewer.Picking

module UI =
    let dropDown'' (values: alist<'a>) (selected: IMod<'a option>) (change: Option<'a> -> 'msg) (f: 'a -> string) =

        let attributes (name: string) =
            AttributeMap.ofListCond
                [ always (attribute "value" (name))
                  onlyWhen
                      (selected
                       |> Mod.map (fun x ->
                           match x with
                           | Some s -> name = f s
                           | None -> name = "-None-")) (attribute "selected" "selected") ]

        let customOnChange =
            let cb (i: int) =
                let currentState = values.Content |> Mod.force
                change (PList.tryAt (i - 1) currentState)

            onEvent "onchange" [ "event.target.selectedIndex" ] (fun x ->
                x
                |> List.head
                |> Int32.Parse
                |> cb)

        Incremental.select
            (AttributeMap.ofList
                [ customOnChange
                  style "color:black" ])
            (alist {
                yield Incremental.option (attributes "-None-") (AList.ofList [ text "-None-" ])
                yield! values
                       |> AList.mapi
                           (fun i x -> Incremental.option (attributes (f x)) (AList.ofList [ text (f x) ]))
             })

module SurfaceAttributes =
    open System.Xml

    let get (name: string) (node: XmlNode) = node.SelectSingleNode(name).InnerText.Trim()

    let parseMap (index: int) (node: XmlNode): ScalarLayer =
        let definedRange =
            node
            |> get "ChannelsDefinedRange"
            |> Range1d.Parse
        { label = node |> get "Label"
          actualRange =
              node
              |> get "ChannelsActualRange"
              |> Range1d.Parse
          definedRange = definedRange
          index = index
          colorLegend = (FalseColorsModel.initDefinedScalarsLegend definedRange) }

    let parseTexture (index: int) (node: XmlNode): TextureLayer =
        { label = node |> get "Label"
          index = index }

    let parseLayer (index: int) (node: XmlNode): AttributeLayer =
        let typ = node |> get "Type"
        match typ with
        | "Texture" -> TextureLayer(parseTexture index node)
        | "Map" -> ScalarLayer(parseMap index node)
        | _ -> failwith "type not supported"

    let layers (doc: XmlDocument) =
        let nodes = doc.SelectNodes "/Aardvark/SurfaceAttributes/AttributeLayers/AttributeLayer"
        nodes
        |> Seq.cast<XmlNode>
        |> Seq.mapi parseLayer

    let read (path: string) =
        let doc = new XmlDocument()
        doc.Load path
        doc |> layers

    let getTextures (layers: seq<AttributeLayer>) =
        layers
        |> Seq.choose (fun x ->
            match x with
            | TextureLayer l -> Some l
            | _ -> None)
        |> Seq.mapi (fun i x -> { x with index = i })
        |> PList.ofSeq

    let getScalars (layers: seq<AttributeLayer>) =
        layers
        |> Seq.choose (fun x ->
            match x with
            | ScalarLayer l -> Some l
            | _ -> None)
        |> Seq.mapi (fun i x -> { x with index = i })
        |> PList.ofSeq

    let getScalarsHmap (layers: seq<AttributeLayer>) =
        layers
        |> Seq.choose (fun x ->
            match x with
            | ScalarLayer l -> Some l
            | _ -> None)
        //|> Seq.mapi(fun i x -> { x with index = i})
        |> Seq.map (fun x -> x.label, x)
        |> HMap.ofSeq

    let getOPCxPath (surfacePath: string) =
        let parent = Path.GetFileName surfacePath
        let name = Path.GetFileName surfacePath
        //let path = surfacePath + "\" + parent + ".opcx"
        Path.ChangeExtension(Path.Combine(surfacePath, parent), ".opcx")

    let addSurfaceAttributes (path: string) =
        match (System.IO.File.Exists path) with
        | true ->
            let layers = read path
            let scalars = layers |> getScalars
            let textures = layers |> getTextures
            scalars, textures
        | false -> (PList.empty, PList.empty)


    let mapTolist (input: amap<_, 'a>): alist<'a> =
        input
        |> AMap.toASet
        |> AList.ofASet
        |> AList.map snd

    let scalarLayerList (layers: amap<string, MScalarLayer>) = (layers |> mapTolist)


    let initModel (dir: string) =
        let opcxPath = dir |> getOPCxPath
        let scalarLayers, textureLayers = addSurfaceAttributes opcxPath

        { scalarLayers = scalarLayers
          selectedScalar = None
          textureLayers = textureLayers
          selectedTexture = textureLayers |> PList.tryFirst }

    let update (model: AttributeModel) (msg: AttributeAction) =
        match msg with
        | SetScalarMap s ->
            { model with selectedScalar = s }

        | SetTexture t ->
            { model with selectedTexture = t}

        | ScalarsColorLegendMessage msg ->
            match model.selectedScalar with
            | Some s ->
                let sc = { s with colorLegend = FalseColorLegendApp.update s.colorLegend msg }

                let scalars =
                    model.scalarLayers
                    |> PList.toArray
                    |> Array.tryFindIndex (fun x -> x.label = sc.label)
                    |> Option.map (fun idx -> model.scalarLayers |> PList.updateAt idx (fun _ -> sc))
                    |> Option.defaultValue model.scalarLayers

                { model with
                      selectedScalar = Some sc
                      scalarLayers = scalars }
            | None -> model

    let showColorLegend (model: MAttributeModel) =
        alist {
            let! scalar = model.selectedScalar
            match scalar with
            | Some s -> yield FalseColorLegendApp.Draw.createColorLegendScalars s.colorLegend
            | None -> yield div [ style "font-style:italic" ] [ text "no scalar selected" ]
        }

    let scalarsColorLegend (m: MAttributeModel) =
        let attr =
            AttributeMap.ofList
                [ "display" => "block"
                  "width" => "90px"
                  "height" => "75%" //100%
                  "preserveAspectRatio" => "xMidYMid meet"
                  "style" => "position:absolute; right: 5px; top: 25%" ]
        Incremental.Svg.svg attr (showColorLegend m)

    let viewColorLegendTools (scalar: IMod<Option<MScalarLayer>>) =
        adaptive {
            let! scalar = scalar
            match scalar with
            | Some s ->
                return FalseColorLegendApp.UI.viewDefinedScalarsLegendTools s.colorLegend
                       |> UI.map ScalarsColorLegendMessage
            | None ->
                return div [ style "font-style:italic" ] [ text "no scalar in properties selected" ]
                       |> UI.map ScalarsColorLegendMessage
        }

    let view (m: MAttributeModel) =
        //require FalseColorLegendApp.UI.myCss (
        body [ style "width: 100%; height:100%; background: transparent" ] [
            div [ style "color:white; margin: 5px 15px 5px 5px" ] [
                h3 [] [ text "FalseColors" ]
                div [ style "margin: 10px" ] [
                    text "Scalars: "
                    UI.dropDown'' m.scalarLayers m.selectedScalar (fun x ->
                        SetScalarMap(x |> Option.map (fun y -> y.Current |> Mod.force))) (fun x ->
                        x.label |> Mod.force)
                ]

                div [ style "margin: 10px" ] [
                    text "Textures: "
                    UI.dropDown'' m.textureLayers m.selectedTexture (fun x ->
                        SetTexture(x |> Option.map (fun y -> y.Current |> Mod.force))) (fun x ->
                        x.label |> Mod.force)
                ]

                Incremental.div AttributeMap.empty (AList.ofModSingle (viewColorLegendTools m.selectedScalar))
            ]
        ]
//)
