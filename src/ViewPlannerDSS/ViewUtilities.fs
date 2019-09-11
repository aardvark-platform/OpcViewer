
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

    let accordionContentViewPlans (r:MRoverModel) = 
        
        let itemAttributes =
              amap {
                  yield clazz "ui divided list inverted segment"
              } |> AttributeMap.ofAMap

        Incremental.div itemAttributes (
            alist { 
              //yield Incremental.i itemAttributes AList.empty
              let items = r.viewplans
              let! selected = r.selectedViewPlan
              for item in items do
               
                let id = item.id
                let! s = item.id
                let t = Incremental.text (Mod.map (fun id  -> "ViewPlan " + id.ToString()) id)

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
                        onClick (fun _ -> ShowViewPlan s)
                    ][]
                    div [clazz "content"] [
                        Incremental.i itemAttributes AList.empty
                        t      
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

                h4[][text "Rover Controls"]
                p[][div[][Incremental.text (m.rover.pan.current |>Mod.map (fun f -> "Panning - current value: " + f.ToString())); slider { min = -180.0; max = 180.0; step = 1.0 } [clazz "ui blue slider"] m.rover.pan.current RoverAction.ChangePan]] |> UI.map RoverAction 
                p[][div[][Incremental.text (m.rover.tilt.current |> Mod.map (fun f -> "Tilting - current value: " + f.ToString())); slider { min = 0.0; max = 180.0; step = 1.0 } [clazz "ui blue slider"] m.rover.tilt.current RoverAction.ChangeTilt]] |> UI.map RoverAction  
                
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
                                
              ]
            
    
    let viewPlanDetails (vp:IMod<Option<MViewPlan>>)  =

        Incremental.div AttributeMap.Empty (
        
            alist{
        
                let! d = vp |> Mod.map (fun plan -> 
 
                    match plan with
                    | Some p -> 
         
                        let outputvars = p.outputParams
                        let instrument = p.instrument.ToString()
                        let pan = "" + (p.panOverlap).ToString() + "°"
                        let tilt = "" + (p.tiltOverlap).ToString() + "°"
                        let numsamples = "" + outputvars.numberOfSamples.ToString()
                        let energy = "" + outputvars.energyRequired.ToString() + "%"
                        let time = "" + outputvars.timeRequired.ToString() + "sec"
                        let bandwidth = "" + outputvars.bandwidthRequired.ToString()
                         
                        table [clazz "ui celled unstackable inverted table"; style "border-radius: 0;"] [
                            tr [] [
                               td [] [text "Instrument"]
                               td [] [text instrument]
                                ]
                            
                            tr [] [
                               td [] [text "pan overlap"]
                               td [] [text pan]
                                ]
                            
                            tr [] [
                               td [] [text "tilt overlap"]
                               td [] [text tilt]
                                ]
                            
                            tr [] [
                               td [] [text "# of samples"]
                               td [] [text numsamples]
                                ]
                            
                            tr [] [
                               td [] [text "required energy"]
                               td [] [text energy]
                                ]

                            tr [] [
                               td [] [text "required time"]
                               td [] [text time]
                                ]

                            tr [] [
                               td [] [text "required bandwidth"]
                               td [] [text bandwidth]
                                ]
                          ]
                         
                    | None -> h5[][text "Select a viewplan to view its details"]
                  
                )

            yield d

            }
        )


    let viewPlanModeMenu (m: MModel) = 
         
        div[style "color:white; margin: 5px 15px 5px 5px"][

            Html.SemUi.accordion "ViewPlans" "bookmark" true [
                accordionContentViewPlans m.rover |> UI.map RoverAction
            ]  
                
            br[]
            viewPlanDetails m.rover.selectedViewPlan
                
            br[]

            button [clazz "ui inverted labeled basic icon button"; onClick (fun _ -> RoverAction.RotateToPoint)]  [
                i [clazz "icon play"] []
                text "walk through" 
            ] |> UI.map RoverAction
                                
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
    
    //---
