namespace OpcSelectionViewer

#nowarn "8989"

module Serialization =    
    open System.IO
    open MBrace.FsPickler
        
    let registry = new CustomPicklerRegistry()    
    let cache = PicklerCache.FromCustomPicklerRegistry registry    

    let binarySerializer = FsPickler.CreateBinarySerializer(picklerResolver = cache)    

    let save path (d : 'a) =    
        let arr =  binarySerializer.Pickle d
        File.WriteAllBytes(path, arr);
        d
    
    let loadAs<'a> path : 'a =
        let arr = File.ReadAllBytes(path)
        binarySerializer.UnPickle arr

    let loadAsType<'a> path : 'a = 
        let arr = File.ReadAllBytes(path)
        let pickle = new Pickle<'a>(arr)
        binarySerializer.UnPickleTyped pickle

    let writeToFile path (contents : string) =
        System.IO.File.WriteAllText(path, contents)  

    let fileExists filePath =
        match System.IO.File.Exists filePath with
          | true  -> Some filePath
          | false -> None

    module Chiron =
        let writeToFile path (contents : string) =
            System.IO.File.WriteAllText(path, contents)
        
        let readFromFile path =
            System.IO.File.ReadAllText(path)  

module Lenses = 
    open Aardvark.Base

    let get    (lens : Lens<'s,'a>) (s:'s) : 'a              = lens.Get(s)
    let set    (lens : Lens<'s,'a>) (v : 'a) (s:'s) : 's     = lens.Set(s,v)
    let set'   (lens : Lens<'s,'a>) (s:'s) (v : 'a)  : 's    = lens.Set(s,v)
    let update (lens : Lens<'s,'a>) (f : 'a->'a) (s:'s) : 's = lens.Update(s,f)