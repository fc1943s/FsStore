namespace FsStore.Utils

open FsStore
open FsStore.Model
open FsStore.State
open Microsoft.FSharp.Core.Operators
open FsCore
open FsJs

module TempValue =
    let rec tempValue = Atoms.Join.createJoinAtom (nameof tempValue)

    let inline getAtomField (atom: InputAtom<'TValue> option) (inputScope: AtomScope) =
        match atom with
        | Some (InputAtom atomReference) ->
            let current =
                match atomReference with
                | AtomReference.Atom atom -> Some atom
                | _ -> Some (unbox Atom.empty)

            let temp =
                //                    Dom.log
                //                        (fun () -> $"getAtomField atomPath={atomPath} queryAtomPath atomPath={queryAtomPath atomPath}")

                try
                    match Atom.query atomReference, inputScope with
                    | storeAtomPath, AtomScope.Temp -> Some (tempValue storeAtomPath)
                    | _ -> None
                with
                | ex ->
                    let getLocals () = $"ex={ex} {getLocals ()}"
                    Logger.logError (fun () -> "TempValue.getAtomField") getLocals
                    None

            current, temp
        | _ -> None, None

    let inline set<'TValue9, 'TKey> (setter: Setter<obj>) (atom: AtomConfig<'TValue9>) (value: 'TValue9) =
        let _, tempAtom = getAtomField (Some (InputAtom (AtomReference.Atom atom))) AtomScope.Temp

        match tempAtom with
        | Some atom ->
            let newValueJson =
                match Json.encode<'TValue9> value with
                | String.Valid json -> Some json
                | _ -> None

            Atom.set setter atom newValueJson
        | _ -> ()

    let inline setWithScope<'TValue10, 'TKey>
        (setter: Setter<obj>)
        (atomScope: AtomScope)
        (atom: 'TKey -> AtomConfig<'TValue10>, key: 'TKey, value: 'TValue10)
        =
        match atomScope with
        | AtomScope.Current -> Atom.set setter (atom key) value
        | AtomScope.Temp -> set<'TValue10, 'TKey> setter (atom key) value

    let inline reset<'TValue8, 'TKey> (setter: Setter<obj>) (atom: AtomConfig<'TValue8>) =
        let _, tempAtom = getAtomField (Some (InputAtom (AtomReference.Atom atom))) AtomScope.Temp

        match tempAtom with
        | Some atom -> Atom.set setter atom None
        | _ -> ()

    let rec ___emptyTempAtom = nameof ___emptyTempAtom

    let inline get<'TValue11, 'TKey> getter (atom: AtomConfig<'TValue11>) =
        let _, tempAtom = getAtomField (Some (InputAtom (AtomReference.Atom atom))) AtomScope.Temp

        match tempAtom with
        | Some tempAtom ->
            let result = Atom.get getter tempAtom

            match result with
            | Some result when result = ___emptyTempAtom -> unbox null
            | None -> Atom.get getter atom
            | Some result -> Json.decode<'TValue11> result
        | _ -> Atom.get getter atom
