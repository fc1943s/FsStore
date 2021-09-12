namespace FsStore.State.Selectors

open Fable.Core.JsInterop
open Fable.Core
open FsCore
open FsCore.BaseModel
open FsStore.Bindings.Gun
open FsStore.Model
open FsStore
open FsStore.State
open Microsoft.FSharp.Core.Operators
open FsJs
open FsStore.Bindings

#nowarn "40"


module rec Gun =
    let collection = Collection (nameof Gun)

    let rec readSelector name fn =
        Atom.readSelector (ValueAtomPath (FsStore.storeRoot, collection, [], AtomName name)) fn

    let rec gunPeers =
        readSelector
            (nameof gunPeers)
            (fun getter ->
                let gunOptions = Atom.get getter Atoms.gunOptions

                match gunOptions with
                | GunOptions.Minimal -> [||]
                | GunOptions.Sync gunPeers ->
                    gunPeers
                    |> Array.filter
                        (function
                        | GunPeer (String.Valid _) -> true
                        | _ -> false))


    let rec gun =
        readSelector
            (nameof gun)
            (fun getter ->
                //                    let deviceInfo = Atom.value getter deviceInfo
                let gunPeers = Atom.get getter gunPeers

                let gun =
                    //                        if deviceInfo.IsTesting then
                    //                            Bindings.Gun.gun
                    //                                {
                    //                                    GunProps.peers = None
                    //                                    GunProps.radisk = Some false
                    //                                    GunProps.localStorage = Some false
                    //                                    GunProps.multicast = None
                    //                                }
                    //                        else
                    Bindings.Gun.gun
                        {
                            GunProps.peers = Some gunPeers
                            GunProps.radisk = Some true
                            GunProps.localStorage = Some false
                            GunProps.multicast = None
                        }

                let getLocals () =
                    $"gunPeers={gunPeers}. gun={gun} {getLocals ()}"

                Logger.logDebug (fun () -> $"{nameof FsStore} | Selectors.Gun.gun. returning...") getLocals

                gun)

    let rec gunUser =
        readSelector
            (nameof gunUser)
            (fun getter ->
                let logger = Atom.get getter Store.logger
                let _gunTrigger = Atom.get getter Atoms.gunTrigger
                let gun = Atom.get getter gun

                let getLocals () =
                    $"keys={gun.user().__.sea |> Js.objectKeys} {getLocals ()}"

                logger.Debug (fun () -> "Selectors.Gun.gunUser") getLocals

                gun.user ())

    let rec gunNamespace =
        readSelector
            (nameof gunNamespace)
            (fun getter ->
                let logger = Atom.get getter Store.logger
                let _gunTrigger = Atom.get getter Atoms.gunTrigger
                let gunUser = Atom.get getter gunUser

                let getLocals () =
                    $"gunUser.is={JS.JSON.stringify gunUser.is} {getLocals ()}"

                logger.Debug (fun () -> "Selectors.Gun.gunNamespace") getLocals

                gunUser :> Types.IGunNode)

    let rec alias =
        readSelector
            (nameof alias)
            (fun getter ->
                let logger = Atom.get getter Store.logger
                let _gunTrigger = Atom.get getter Atoms.gunTrigger
                let gunUser = Atom.get getter gunUser

                match gunUser.is with
                | Some {
                           alias = Some (GunUserAlias.Alias (Alias (String.Valid alias)))
                       } ->
                    let getLocals () =
                        $"alias={alias} keys={gunUser.__.sea |> Js.objectKeys} {getLocals ()}"

                    logger.Debug (fun () -> "Selectors.Gun.alias") getLocals

                    Some (Alias alias)
                | _ ->
                    match gunUser.__.sea with
                    | Some { priv = Some (Priv (String.Valid _)) } ->
                        let internalAlias = Atom.get getter Atoms.internalAlias

                        let getLocals () =
                            $"internalAlias={internalAlias} {getLocals ()}"

                        logger.Debug (fun () -> "Selectors.Gun.alias") getLocals
                        internalAlias
                    | _ -> None)

    let rec privateKeys =
        readSelector
            (nameof privateKeys)
            (fun getter ->
                let logger = Atom.get getter Store.logger
                let _gunTrigger = Atom.get getter Atoms.gunTrigger
                let gunUser = Atom.get getter gunUser

                let getLocals () =
                    $"keys={gunUser.__.sea |> Js.objectKeys} {getLocals ()}"

                logger.Debug (fun () -> "Selectors.Gun.keys") getLocals
                gunUser.__.sea)


    let getRecursiveNode (gunNode: Types.IGunNode) (nodes: AtomKeyFragment list) getter alias =
        let withToString (node: Types.IGunChainReference) =
            node?toString <- fun () -> $"alias={alias} nodes={nodes}"
            node

        match nodes with
        | [] -> None
        | [ root ] -> Some (gunNode.get root |> withToString)
        | nodes ->
            let lastNode = nodes |> List.last

            let parentAtomPath =
                AtomPath (
                    nodes.[0..nodes.Length - 2]
                    |> List.map AtomKeyFragment.Value
                    |> String.concat "/"
                )

            let node = Atom.get getter (gunAtomNode (alias, parentAtomPath))

            node
            |> Option.map (fun (node: Types.IGunChainReference) -> node.get lastNode |> withToString)

    let rec gunAtomNode =
        Atom.Primitives.atomFamily
            (fun (alias: Alias option, AtomPath atomPath) ->
                Atom.Primitives.readSelector
                    (fun getter ->
                        let gunNode =
                            match alias with
                            | Some _ -> Atom.get getter gunNamespace
                            | None -> Atom.get getter gun :> Types.IGunNode

                        let nodes =
                            atomPath
                            |> String.split "/"
                            |> Array.toList
                            |> List.map AtomKeyFragment

                        //                    let getNodeOld () =
                        //                        (Some (gunNamespace.get nodes.Head), nodes.Tail)
                        //                        ||> List.fold
                        //                                (fun result node ->
                        //                                    result
                        //                                    |> Option.map (fun result -> result.get node))


                        getRecursiveNode gunNode nodes getter alias))

    let rec adapterOptions =
        readSelector
            (nameof adapterOptions)
            (fun getter ->
                let alias = Atom.get getter alias
                let gunOptions = Atom.get getter Atoms.gunOptions

                let getLocals () =
                    $"alias={alias} gunOptions={Json.encodeWithNull gunOptions}"

                Profiling.addTimestamp (fun () -> $"{nameof FsStore} | Selectors.Gun.adapterOptions get()") getLocals

                match gunOptions, alias with
                | GunOptions.Sync peers, Some alias -> Some (Atom.AdapterOptions.Gun (peers, alias))
                | _ -> None)
