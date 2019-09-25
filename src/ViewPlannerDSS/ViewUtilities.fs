
namespace ViewPlanner

open Aardvark.Base
open Aardvark.Base.Incremental

open Aardvark.UI
open Aardvark.UI.Primitives

open ViewPlanner.Rover

module ViewUtilities = 

    let accordionContentPositions (r:MRoverModel) = 
        
        let itemAttributes =
              amap {
                  yield clazz "ui divided list inverted segment"
              } |> AttributeMap.ofAMap

        Incremental.div itemAttributes (
            alist { 
              //yield Incremental.i itemAttributes AList.empty
              let items = r.positionsList
              let! selected = r.selectedPosition
              for item in items do
               
                let id = item.id
                let! s = item.id
                let t = Incremental.text (Mod.map (fun id  -> "Position " + id.ToString()) id)

                let! color = 
                    match selected with
                    | Some placement -> 
                        let pid = placement.id
                        let equal = Mod.map2 (fun a b -> a = b) pid id
                        equal |> Mod.map (fun e -> if e then C4b.Cyan else C4b.White)
                    | None -> Mod.constant C4b.White
                let bgc = color |> Html.ofC4b |> sprintf "color: %s"

                yield div [clazz "item"] [
                    Incremental.i itemAttributes AList.empty
                    i [
                        clazz "small cube left aligned icon"; 
                        style bgc
                        onClick (fun _ -> SetRoverPosAndTarget s)
                    ][]
                    div [clazz "content"] [
                        Incremental.i itemAttributes AList.empty
                        t      
                    ]
                ]
            }
          )


    let rec modulo (time:float) (counter:int) (current:float) = 
                            
        match current with
        | c when ((c + 60.0) > time) -> 
               let v = c + 60.0
               let remainder = v - time
               (counter, remainder)
                            
        | _ -> modulo time (counter+1) (current+60.0)



    let viewPlanDetails (vp:MViewPlan) (same:bool) = 
        
         Incremental.div AttributeMap.Empty (
        
                alist{
                
                    match same with
                    | true -> 
                        let plan = vp
                        let outputvars = plan.outputParams
                        let! instrument = plan.instrument
                        let! pan = plan.panOverlap
                        let! tilt = plan.tiltOverlap
                        let! numsamples = outputvars.numberOfSamples
                        let! energy = outputvars.energyRequired
                        let! time = outputvars.timeRequired
                        let! datasize = outputvars.datasize
                        let! spatialRes = outputvars.spatialRes

                        let m = modulo time 0 0.0

                        let minutes = fst m
                        let seconds = snd m

        
                        //text
                        let ins = instrument.ToString()
                        let p = "" + pan.ToString() + " %"
                        let t = "" + tilt.ToString() + " %"
                        let samples = "" + numsamples.ToString()
                        let e = "" + energy.ToString() + " %"
                        let ti = "" + minutes.ToString() + " min " + seconds.ToString() + " sec"
                        let ds = "" + datasize.ToString() + " MB"
                        let sr = "" + spatialRes.ToString() + "cm/pixel"
                         
                        yield table [clazz "ui celled unstackable inverted table"; style "border-radius: 0;"] [
                            
                            tr [] [
                                td [attribute "colspan" "2"] [
                                   text "Input parameters"
                                ]
                            ]

                            tr [] [
                               td [] [text "Instrument"]
                               td [] [text ins]
                                ]
                            
                            tr [] [
                               td [] [text "pan overlap"]
                               td [] [text p]
                                ]
                            
                            tr [] [
                               td [] [text "tilt overlap"]
                               td [] [text t]
                                ]
                            
                            tr [] [
                                td [attribute "colspan" "2"] [
                                   text "Output parameters"
                                ]
                            ]

                            tr [] [
                               td [] [text "# of samples"]
                               td [] [text samples]
                                ]
                            
                            tr [] [
                               td [] [text "required energy"]
                               td [] [text e]
                                ]

                            tr [] [
                               td [] [text "required time"]
                               td [] [text ti]
                                ]

                            tr [] [
                               td [] [text "data volume"]
                               td [] [text ds]
                                ]
                            
                            tr [] [
                               td [] [text "spatial resolution"]
                               td [] [text sr]
                                ]
                          ]
                        
                        yield button [clazz "ui inverted labeled basic icon button"; onClick (fun _ -> RoverAction.RotateToPoint)]  [
                            i [clazz "icon play"] []
                            text "walk through" 
                            ] //|> UI.map RoverAction


                 
                    | false -> yield div[][]

                }
            )

      
      

    let accordionContentViewPlans (r:MRoverModel) = 
        
        let itemAttributes =
              amap {
                  yield clazz "ui divided list inverted segment"
              } |> AttributeMap.ofAMap

        Incremental.div itemAttributes (
            alist { 

              let items = r.viewplans
              let! selected = r.selectedViewPlan
              for item in items do
               
                let id = item.id
                let! s = item.id
                let t = Incremental.text (Mod.map (fun id  -> "ViewPlan " + id.ToString()) id)

                let! color,same, ic = 
                    match selected with
                    | Some placement -> 
                        let pid = placement.id
                        let equal = Mod.map2 (fun a b -> a = b) pid id
                        equal |> Mod.map (fun e -> if e then (C4b.Cyan,true, "icon folder open outline") else (C4b.White,false, "icon folder outline"))
                    | None -> Mod.constant (C4b.White, false, "icon folder outline")
                let bgc = color |> Html.ofC4b |> sprintf "color: %s"
               

                yield div [clazz "item"] [
                    Incremental.i itemAttributes AList.empty

                    div[onClick (fun _ -> ShowViewPlan s); style "margin-top:5px"][
                        
                        
                        i [
                            clazz ic // "small cube left aligned icon"; 
                            style bgc
                            onClick (fun _ -> ShowViewPlan s)
                        
                        ][]
                    
                        t
                    
                    ]
                    
                    div [clazz "content"; style "margin-bottom:5px"] [
                        //Incremental.i itemAttributes AList.empty
                        //t
                        viewPlanDetails item same
                        
                    ]
                ]

            }
          )
    

    let overlayText (t:IMod<string>) = 
         div [js "oncontextmenu" "event.preventDefault();"] [ 
           let style' = "color: white; font-family:Consolas;"
    
           yield div [clazz "ui"; style "position: absolute; top: 10px; left: 10px; float:right" ] [          
              yield table [] [
                tr[][
                    td[style style'][Incremental.text(t |> Mod.map(fun x -> x))]
                ]
              ]
           ]
        ]
    

    //if criteria is met, show button
    let visibleButton (criteria:IMod<bool>) (func: unit -> RoverAction) (cl:string) (icon:string) (txt:string) = 
        
        let attributes = 
              amap {
                  //if criteria then yield clazz cl else yield clazz "ui disabled button"
                  //yield onClick (func)
                  yield clazz icon
              } |> AttributeMap.ofAMap

        


        Incremental.button attributes (
            alist {
              
             let! vis,fu = 
                criteria |> Mod.map (fun c ->
                match c with
                | true -> 
                    let f = onClick (func)
                    (cl, Some f)

                | false -> 
                    let visual = cl
                    (visual, None)
                )
              
             let b = 
                match fu with
                | Some f ->
                    button [clazz vis; f]  
                        [
                        text txt
                        ]
                | None ->
                    button [clazz vis]  
                        [
                        text txt
                        ]

             //let b = 
             //   button [clazz vis]  
             //       [
             //       text txt
             //       ]
            
             yield b

            }
        )
             
   
            
    //---CONTROL MENUS---
    let rPModeMenu (m:MModel) =
        
              div[style "color:white; margin: 5px 15px 5px 5px"][

                h5[] [text "Press Strg + left click to pick a point"]
                h5[] [text "First hit = rover position; second hit = rover target"]

                Html.SemUi.accordion "Rover positions" "map pin" true [
                      accordionContentPositions m.rover |> UI.map RoverAction
                      ]  
                                
              ]
            
    

    let sampleModeMenu (m:MModel) = 
        
              div[style "color:white; margin: 5px 15px 5px 5px"][

                //h4[][text "Rover Controls"]
                //p[][div[][Incremental.text (m.rover.pan.current |>Mod.map (fun f -> "Panning - current value: " + f.ToString())); slider { min = -180.0; max = 180.0; step = 1.0 } [clazz "ui blue slider"] m.rover.pan.current RoverAction.ChangePan]] |> UI.map RoverAction 
                //p[][div[][Incremental.text (m.rover.tilt.current |> Mod.map (fun f -> "Tilting - current value: " + f.ToString())); slider { min = 0.0; max = 180.0; step = 1.0 } [clazz "ui blue slider"] m.rover.tilt.current RoverAction.ChangeTilt]] |> UI.map RoverAction  
                
                h4[][text "Input Parameters"]
                table [clazz "ui celled unstackable inverted table"; style "border-radius: 0;"] [
                            tr [] [
                               td [] [text "Instrument"]
                               td [] [dropdown { allowEmpty = false; placeholder = "" } [ clazz "ui inverted selection dropdown" ] (m.rover.cameraOptions |> AMap.map (fun k v -> text v)) m.rover.currentCamType RoverAction.SwitchCamera]|> UI.map RoverAction
                            ]
                            tr [] [
                                td [] [text "pan overlap"]
                                td [] [dropdown { allowEmpty = false; placeholder = "" } [ clazz "ui inverted selection dropdown" ] (m.rover.panOverlapOptions |> AMap.map (fun k v -> text v)) m.rover.currentPanOverlap RoverAction.ChangePanOverlap] |> UI.map RoverAction
                            ]

                            tr [] [
                                td [] [text "tilt overlap"]
                                td [] [dropdown { allowEmpty = false; placeholder = "" } [ clazz "ui inverted selection dropdown" ] (m.rover.tiltOverlapOptions |> AMap.map (fun k v -> text v)) m.rover.currentTiltOverlap RoverAction.ChangeTiltOverlap ] |> UI.map RoverAction
                            ]

                            tr [] [
                                td [attribute "colspan" "2"] [
                                 Html.SemUi.accordion "Rover positions" "map pin" true [
                                    accordionContentPositions m.rover |> UI.map RoverAction
                                    ]  
                                ]
                            ]



                ]

                button [clazz "ui inverted labeled basic icon button"; onClick (fun _ -> RoverAction.CalculateAngles)]  [
                i [clazz "icon camera"] []
                text "sample"] |> UI.map RoverAction

                button [clazz "ui inverted labeled basic icon button"; onClick (fun _ -> RoverAction.SampleAllCombinations)]  [
                i [clazz "icon camera"] []
                text "sample all"] |> UI.map RoverAction
                                
              ]
            
    
   


    let viewPlanModeMenu (m: MModel) = 
         
        div[style "color:white; margin: 5px 15px 5px 5px"][

            Html.SemUi.accordion "ViewPlans" "bookmark" true [
                accordionContentViewPlans m.rover |> UI.map RoverAction
            ]  
                                  
        ] 
    

    let standardModeMenu  = 
         
        div[style "color:white; margin: 5px 15px 5px 5px"][

            h4[][text "How to use"]

            table [clazz "ui celled unstackable inverted table"; style "border-radius: 0;"] [
                    tr [] [
                        td [] [i [clazz "icon map marker alternate"][]]
                        td [] [text "Step 1: Rover Placement Mode - pick rover positions"]
                    ]

                    tr [] [
                        td [] [i [clazz "icon camera"][]]
                        td [] [text "Step 2: Sample Mode - choose input parameters,set region of interest and sample"]
                    ]

                    tr [] [
                        td [] [i [clazz "icon bookmark"][]]
                        td [] [text "Step 3: ViewPlan Mode - select a viewplan"]
                    ]   
                ]   
            ]
            

    let selectMode (curr:IMod<Option<ModeOption>>) (m:MModel) =
        
        Incremental.div AttributeMap.Empty (
        
            alist{
        
                let! d = curr |> Mod.map (fun f -> 
        
       
                    match f with
                    | Some StandardMode -> standardModeMenu  
                    | Some RoverPlacementMode -> rPModeMenu  m
                    | Some ViewPlanMode -> viewPlanModeMenu  m
                    | Some SampleMode -> sampleModeMenu  m
                    | None -> standardModeMenu  

                )

            yield d

            }
        )
    

    let renderControl (vars:IMod<MCamVariables>) (idx:IMod<int>) (scene: ISg<Action>) (att:AttributeMap<Action>) =
 
          let viewListMod = vars |> Mod.map(fun v -> v.viewList) 
          let fr = vars |> Mod.map(fun v -> v.frustum) |> Mod.bind(fun f -> f)
          let v = viewListMod |> Mod.bind(fun a -> a.Content)
          let activeView = 
              Mod.map2( fun (x:plist<CameraView>) index -> 
                  let arr = x |> PList.toArray
                  arr.[index]
                  ) v idx
          let camera = Mod.map2(fun view frustum -> Camera.create view frustum) activeView fr
          DomNode.RenderControl(att, camera, scene, RenderControlConfig.standard, None)


    let view (scene: ISg<Action>) (side:string) (m:MModel) =
        
        let att = 
            (AttributeMap.ofList [ 
            style "width: 100%; height:100%"; 
            attribute "showFPS" "false";      
            attribute "data-renderalways" "false"
            attribute "data-samples" "4"
            ]) 
        
      
        
        Incremental.div AttributeMap.Empty (
        
            alist {
                
                let! s = m.rover.selectedViewPlan
                
                match s with
                | Some plan ->

                    match side with
                    | "left" ->

                        let views = plan.cameraVariables |> AList.toMod
                        let view = views |> Mod.map(fun f -> f |> PList.first)
                        let dom = renderControl view m.rover.walkThroughIdx scene att
                        yield div [clazz "ui"; style "background: #1B1C1E"] [dom]

                    | "right" -> 
                       
                       let instrument = plan.instrument
  
                       let m = 
                            adaptive {
                                let! st = instrument
                                match st with
                                | "High Resolution Camera" -> return div[][] 
 
                                | "WACLR" -> 
                                    let! viewsMod = plan.cameraVariables |> AList.toMod
                                    let view = viewsMod |> PList.last |> Mod.constant
                                    return renderControl view m.rover.walkThroughIdx scene att
                                   
                                | _ -> return div[][]
                            }

                           
                       let a = m |> Mod.toAList
                       for item in a do
                        yield item

                       


                    | _ -> yield div[][]

      
                | None -> yield div[][]

           

            }
        )


    
    let selectView (curr:IMod<Option<ModeOption>>) (scene: ISg<Action>) (side:string) (m:MModel) = 
          
        
          let instrumentViewAttributes =
                amap {
                    
                    let! horz = m.rover.horzRes
                    let! vert = m.rover.vertRes
                    let height = "height:" + (vert/uint32(2)).ToString() + ";" 
                    let width = "width:" + (horz/uint32(2)).ToString() + ";"
                    yield style ("background: #1B1C1E;" + height + width)
                } |> AttributeMap.ofAMap
        
          Incremental.div instrumentViewAttributes (
        
            alist{
        
                let! d = curr |> Mod.map (fun f -> 
        
                    match f with
                    | Some ViewPlanMode -> view scene side m
                    | _ -> div[][]
                )

            yield d

            }
        )



    
    //---
