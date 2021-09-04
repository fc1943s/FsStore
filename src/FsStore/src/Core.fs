namespace FsStore

open System.Collections.Generic


module Map =
    let inline tryFindDictionary key (map: Dictionary<_, _>) =
        match map.TryGetValue key with
        | true, value -> Some value
        | _ -> None
