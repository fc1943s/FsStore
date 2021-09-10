namespace FsStore.State.Atoms

open System
open FsCore.BaseModel
open FsStore
open FsStore.Model
open Microsoft.FSharp.Core.Operators
open FsCore
open FsJs
open FsStore.Bindings


module rec Join =
    let collection = Collection (nameof Join)


    let inline createJoinAtom name =
        let rec internalAtomFamily =
            Atom.Primitives.atomFamily
                (fun (atomPathGuidHash: Guid) ->
                    Engine.createAtomWithSubscription
                        (ValueAtomPath (
                            FsStore.storeRoot,
                            collection,
                            [
                                Gun.AtomKeyFragment (string atomPathGuidHash)
                            ],
                            AtomName name
                        ))
                        (None: string option))

        Atom.Primitives.atomFamily
            (fun (storeAtomPath: StoreAtomPath) ->
                let atomPath = storeAtomPath |> StoreAtomPath.AtomPath

                let guidHash = Crypto.getTextGuidHash (atomPath |> AtomPath.Value)
                let atom = internalAtomFamily guidHash

                let getLocals () =
                    $"atomPath={atomPath} guidHash={guidHash} {getLocals ()}"

                Logger.logTrace (fun () -> $"{nameof FsStore} | Atoms.Join.joinAtom") getLocals

                Atom.Primitives.selector
                    (fun getter ->
                        let value = Atom.get getter atom
                        let getLocals () = $"value={value} {getLocals ()}"
                        Logger.logTrace (fun () -> $"{nameof FsStore} | Join.createJoinAtom / joinAtom get()") getLocals

                        value
                        |> Option.map
                            (fun value ->
                                match Json.decode<AtomPath * string option> value with
                                | _, Some value -> value
                                | _ -> null))
                    (fun _ setter newValue ->

                        let newValueJson =
                            match Json.encode (atomPath, newValue) with
                            | String.Valid json -> Some json
                            | _ -> None

                        let getLocals () =
                            $"newValue={newValue} newValueJson={newValueJson} {getLocals ()}"

                        Logger.logTrace (fun () -> $"{nameof FsStore} | Join.createJoinAtom / joinAtom set()") getLocals
                        Atom.set setter atom newValueJson))
