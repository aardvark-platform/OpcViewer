
namespace ViewPlanner

open Aardvark.Base
open Aardvark.Base.Incremental

open Aardvark.UI
open Aardvark.UI.Primitives

open ViewPlanner.Rover

module ViewUtilities = 
    

    let accordionContent (r:MRoverModel) = 
        
        let itemAttributes =
              amap {
                  yield clazz "ui divided list inverted segment"
                  //yield style "overflow-y : visible"
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
    
    
     
        
        


