
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
             
   
            
       
        
        
     
        
        


