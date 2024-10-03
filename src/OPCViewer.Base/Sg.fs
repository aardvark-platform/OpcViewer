namespace OpcViewer.Base

open System
open Aardvark.Base
open Aardvark.Base.Fonts
open FSharp.Data.Adaptive
open Aardvark.Rendering
open Aardvark.SceneGraph
open Aardvark.Rendering.Text
open FShade
open Aardvark.UI.``F# Sg``
open Aardvark.UI.Trafos
open Aardvark.Data.Opc
open Aardvark.SceneGraph.Opc

open Adaptify.FSharp.Core

//open OpcSelectionViewer.Picking
//open OpcOutlineTest
module Patch =
    let load (opcPaths : OpcPaths) (mode : ViewerModality) (p : PatchFileInfo) (selAttribute:string) =
        let sw = System.Diagnostics.Stopwatch()
        sw.Start()
        let patch_DirAbsPath = opcPaths.Patches_DirAbsPath +/ p.Name

        let pos = 
          match mode, p.Positions2d with
          | ViewerModality.SvBR, Some p2 -> p2
          | _ -> p.Positions          
        
        let positions   = patch_DirAbsPath +/ pos |> Aara.fromFile<V3f>
        let coordinates = patch_DirAbsPath +/ (List.head p.Coordinates) |> Aara.fromFile<V2f>

        let scl = 
            let x = p.Attributes |> List.tryFind( fun x -> x = (selAttribute + ".aara")) 
            match x with
            | Some name ->
                let data =                    
                    let a = Path.combine [patch_DirAbsPath; name] |> Aara.fromFile<double>
                    a.Data |> Array.map (fun a -> float32 a)                     
                
                data :> Array
            | None -> [|V4f.OOOO|] :> Array
                
        sw.Stop()

        let coordinates = coordinates.Data |> Array.map (fun v -> V2f(v.X, 1.0f-v.Y))

        let index = Aara.createIndex (positions.AsMatrix())

        let a : float = 0.0

        let indexAttributes =
            let def = [
                DefaultSemantic.Positions, positions.Data :> Array
                DefaultSemantic.DiffuseColorCoordinates, coordinates :> Array
                Sym.ofString("Scalar"), scl 
            ]
            
            def |> SymDict.ofList 

      
        let geometry =
            IndexedGeometry(
                Mode              = IndexedGeometryMode.TriangleList,
                IndexArray        = index,
                IndexedAttributes = indexAttributes
            )
                                        
        geometry, sw.MicroTime 


module Sg = 
  open Aardvark.UI
  open OpcViewer.Base.Picking
  open OpcViewer.Base.Attributes
  
  let addAttributeFalsecolorMappingParameters  (selectedScalar:aval<Option<AdaptiveScalarLayer>>) (isg:ISg<'a>) =
            
        let isSelected = selectedScalar |> AVal.map( function Some _ -> true | _ -> false )

        //let upperBound = 
        //    adaptive {
        //        let! scalar = selectedScalar
            
        //        match scalar with 
        //         | Some s -> 
        //            let! range = s.actualRange
        //            return range.Max
        //         | None -> return 1.0
        //    }

        //let lowerBound = 
        //    adaptive {
        //        let! scalar = selectedScalar
            
        //        match scalar with 
        //         | Some s -> 
        //            let! range = s.actualRange
        //            return range.Min
        //         | None -> return 1.0
        //    }

        let interval = selectedScalar |> AVal.bind ( fun x ->
                        match x with 
                            | Some s -> s.colorLegend.interval.value
                            | _   -> AVal.constant(1.0)
                        )

        let inverted = selectedScalar |> AVal.bind ( fun x ->
                        match x with 
                            | Some s -> s.colorLegend.invertMapping
                            | _   -> AVal.constant(false)
                        )     
        
        let upperB = selectedScalar |> AVal.bind ( fun x ->
                        match x with 
                            | Some s -> s.colorLegend.upperBound.value
                            | _   -> AVal.constant(1.0)
                        )

        let lowerB = selectedScalar |> AVal.bind ( fun x ->
                        match x with 
                            | Some s -> s.colorLegend.lowerBound.value
                            | _   -> AVal.constant(1.0)
                        )

        let showcolors = selectedScalar |> AVal.bind ( fun x ->
                            match x with 
                                | Some s -> s.colorLegend.showColors
                                | _   -> AVal.constant(false)
                            )     

        let upperC = 
          selectedScalar 
            |> AVal.bind (fun x ->
               match x with 
                 | Some (s : AdaptiveScalarLayer) -> 
                   s.colorLegend.upperColor
                     |> AVal.map(fun x -> 
                       let t = x.ToC3f()
                       let t1 = HSVf.FromC3f(t)
                       let t2 = (float)t1.H
                       t2)
                 | _ -> AVal.constant(1.0)
               )
        let lowerC = 
          selectedScalar 
            |> AVal.bind ( fun x ->
              match x with 
                | Some s -> 
                  s.colorLegend.lowerColor
                    |> AVal.map(fun x -> ((float)(HSVf.FromC3f (x.ToC3f())).H))
                | _   -> AVal.constant(0.0)
              )
              
            
        isg
            |> Sg.uniform "falseColors"    isSelected
            //|> Sg.uniform "lowerBound"     lowerBound
            //|> Sg.uniform "upperBound"     upperBound
            |> Sg.uniform "startC"         lowerC  
            |> Sg.uniform "endC"           upperC
            |> Sg.uniform "interval"       interval
            |> Sg.uniform "inverted"       inverted
            |> Sg.uniform "lowerBound"     lowerB
            |> Sg.uniform "upperBound"     upperB
            |> Sg.uniform "useColors"     showcolors
            
   
  
  //open Aardvark.Physics.Sky
  let transparent = RenderPass.after "transparent" RenderPassOrder.BackToFront RenderPass.main 

  let font = Font("Consolas")
  let border = { left = 0.01; right = 0.01; top = 0.01; bottom = 0.01 }
  
  let pickable' (pick :aval<Pickable>) (sg: ISg) =
    Sg.PickableApplicator (pick, AVal.constant sg) :> ISg

  let opcSg loadedHierarchies (selectedScalar:aval<Option<_>>) (picking : aval<bool>) (bb : Box3d) = 
    
    let config = { wantMipMaps = true; wantSrgb = false; wantCompressed = false }
    let sg = 
      loadedHierarchies
        |> List.map (fun (g,dir,info) ->         
          let texPath = Patch.extractTexturePath (OpcPaths dir) info 0
          let tex = FileTexture(texPath,config) :> ITexture
                    
          Sg.ofIndexedGeometry g
              |> Sg.trafo (AVal.constant info.Local2Global)
              |> Sg.diffuseTexture (AVal.constant tex)       
              //|> Sg.andAlso(Sg.wireBox (AVal.constant C4b.VRVisGreen) (AVal.constant info.GlobalBoundingBox) |> Sg.noEvents)
          )
        |> Sg.ofList   
    
    let pickable = 
      adaptive {
       // let! bb = opcData.globalBB
        return { shape = PickShape.Box bb; trafo = Trafo3d.Identity }
      }       
    
    sg    
      |> addAttributeFalsecolorMappingParameters selectedScalar
      |> pickable' pickable
      |> Sg.noEvents      
      |> Sg.withEvents [
          SceneEventKind.Down, (
            fun sceneHit -> 
              let intersect = picking |> AVal.force
              if intersect then              
                Log.line "hit an opc? %A" bb
                true, Seq.ofList[(HitSurface (bb,sceneHit))] //, fun a -> a))]
              else 
                false, Seq.ofList[]
          )      
      ]

  let boxSg loadedHierarchies =
    let sg = 
      loadedHierarchies
        |> List.map (fun (_,_,info) -> Sg.wireBox (AVal.constant C4b.VRVisGreen) (AVal.constant info.GlobalBoundingBox) |> Sg.noEvents)
        |> Sg.ofList
        |> Sg.effect [ 
            toEffect Shader.stableTrafo
            toEffect DefaultSurfaces.vertexColor       
        ]    
    sg

  ///probably move to a shader
  let screenAligned (forw : V3d) (up : V3d) (modelt: Trafo3d) =
     let right = up.Cross forw
     let rotTrafo = 
         new Trafo3d(
             new M44d(
                 right.X, up.X, forw.X, 0.0,
                 right.Y, up.Y, forw.Y, 0.0,
                 right.Z, up.Z, forw.Z, 0.0,
                 0.0,     0.0,  0.0,    1.0
             ),
             new M44d(
                 right.X, right.Y, right.Z, 0.0,
                 up.X,    up.Y,    up.Z,    0.0,
                 forw.X,  forw.Y,  forw.Z,  0.0,
                 0.0,     0.0,     0.0,     1.0
             )
     )
     rotTrafo * modelt
    
  let linePass = RenderPass.after "lines" RenderPassOrder.Arbitrary RenderPass.main

  let billboardText (view : aval<CameraView>) modelTrafo text =
                     
      let billboardTrafo = 
          adaptive {
              let! v = view
              let! modelt = modelTrafo
      
              return screenAligned v.Forward v.Up modelt
          }           

      // TODO: Don't use system font, use a font provider
      Sg.text (Font.create "Consolas" FontStyle.Regular) C4b.White text
          |> Sg.noEvents
          |> Sg.effect [
            Shader.stableTrafo |> toEffect
          ]         
          |> Sg.trafo (0.05 |> Trafo3d.Scale |> AVal.constant )
          |> Sg.trafo billboardTrafo  

  let textSg loadedHierarchies (c:aval<CameraView>) =
    let sg = 
      loadedHierarchies
        |> List.map (fun (_,_,info) -> 
           billboardText c (info.GlobalBoundingBox.Center |> Trafo3d.Translation |> AVal.constant) (info.Name |> AVal.constant)
         )
        |> Sg.ofList        
    sg
    
  let createSingleOpcSg (selectedScalar:aval<Option<AdaptiveScalarLayer>>) (picking : aval<bool>) (view : aval<CameraView>) (data : Box3d*AdaptiveOpcData) =
    adaptive {
        let boundingBox, opcData = data
    
        let leaves = 
          opcData.patchHierarchy.tree
            |> QTree.getLeaves 
            |> Seq.toList 
            |> List.map(fun y -> (opcData.patchHierarchy.opcPaths.Opc_DirAbsPath, y))

        let! scalar = selectedScalar
        let! attribute = 
            match scalar with
                | Some s -> s.label
                | _ -> AVal.constant ""
            
        let loadedPatches = 
            leaves 
              |> List.map(fun (dir,patch) -> (Patch.load (OpcPaths dir) ViewerModality.XYZ patch.info attribute, dir, patch.info)) 
              |> List.map(fun ((a,_),c,d) -> (a,c,d)) //|> List.skip 2 |> List.take 1

        //let globalBB = 
        //  Sg.wireBox (AVal.constant C4b.Red) (AVal.constant boundingBox) 
        //    |> Sg.noEvents 
        //    |> Sg.effect [ 
        //      toEffect Shader.stableTrafo
        //      toEffect DefaultSurfaces.vertexColor       
        //    ]  

        return [
          opcSg loadedPatches selectedScalar picking boundingBox
          //boxSg  loadedPatches m boundingBox;
          textSg loadedPatches view
          //globalBB
        ] |> Sg.ofList 
    } |> Sg.dynamic 
      
      