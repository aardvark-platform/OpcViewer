
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
              for item in items do
                let id = item.id
                let! s = item.id
                let t = Incremental.text (Mod.map (fun id  -> "Position " + id.ToString()) id)

                yield div [clazz "item"] [
                    Incremental.i itemAttributes AList.empty
                    i [
                        clazz "small cube left aligned icon"; 
                        //style bgc
                        onClick (fun _ -> SetRoverPosAndTarget s)
                    ][]
                    div [clazz "content"] [
                        Incremental.i itemAttributes AList.empty
                        t      
                    ]
                ]
            }
          )
        


