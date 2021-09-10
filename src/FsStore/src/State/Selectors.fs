namespace FsStore.State

open Fable.Core.JsInterop
open System.Collections.Generic
open Fable.Core
open FsBeacon.Shared
open FsCore
open FsCore.BaseModel
open FsStore.Bindings.Gun
open FsStore.Model
open FsStore
open Microsoft.FSharp.Core.Operators
open FsJs
open FsStore.Bindings

#nowarn "40"


[<AutoOpen>]
module SelectorsMagic =
    module Sync =
        [<RequireQualifiedAccess>]
        type Request =
            | Connect of alias: string
            | Set of alias: string * atomPath: string * value: string
            | Get of alias: string * atomPath: string
            | Filter of alias: string * atomPath: string

        [<RequireQualifiedAccess>]
        type Response =
            | ConnectResult
            | SetResult of ok: bool
            | GetResult of value: string option
            | GetStream of alias: string * atomPath: string * value: string option
            | FilterResult of keys: string []
            | FilterStream of alias: string * atomPath: string * keys: string []


    module Selectors =
        let rec deviceInfo =
            Atom.readSelector (RootAtomPath (FsStore.storeRoot, AtomName (nameof deviceInfo))) (fun _ -> Dom.deviceInfo)

        let rec logger =
            Atom.readSelector
                (RootAtomPath (FsStore.storeRoot, AtomName (nameof logger)))
                (fun getter ->
                    let logLevel = Atom.get getter Atoms.logLevel
                    let logger = Logger.Logger.Create logLevel
                    Logger.State.lastLogger <- logger
                    logger)

        let rec store =
            let mutable lastValue = 0
            let valueAtom = Atom.Primitives.atom lastValue
            let accessorsAtom = Atom.Primitives.atom (None: (Getter<_> * Setter<_>) option)

            Profiling.addTimestamp (fun () -> $"{nameof FsStore} | Selectors.store [ constructor ]") getLocals

            let rec valueWrapper =
                Atom.Primitives.selector
                    (fun getter ->
                        let result = Atom.get getter valueAtom

                        let getLocals () = $"result={result} {getLocals ()}"

                        Profiling.addTimestamp
                            (fun () -> $"{nameof FsStore} | Selectors.store [ valueWrapper.read(getter) ]")
                            getLocals

                        result)
                    (fun getter setter newValue ->
                        let getLocals () = $"newValue={newValue} {getLocals ()}"

                        Profiling.addTimestamp
                            (fun () ->
                                $"{nameof FsStore} | Selectors.store [ valueWrapper.set(getter,setter,newValue) ]")
                            getLocals

                        Atom.set setter accessorsAtom (Some (getter, setter))
                        Atom.set setter valueAtom newValue)
                |> Atom.addSubscription
                    false
                    (fun setAtom ->
                        promise {
                            let getLocals () = $"lastValue={lastValue} {getLocals ()}"

                            Profiling.addTimestamp
                                (fun () -> $"{nameof FsStore} | Selectors.store [ valueWrapper.onMount() ]")
                                getLocals

                            lastValue <- lastValue + 1
                            setAtom lastValue
                        })
                    (fun () ->
                        let getLocals () = $"lastValue={lastValue} {getLocals ()}"

                        Profiling.addTimestamp
                            (fun () -> $"{nameof FsStore} | Selectors.store [ valueWrapper.onUnmount() ] ")
                            getLocals)

            Atom.readSelector
                (RootAtomPath (FsStore.storeRoot, AtomName (nameof store)))
                (fun getter ->
                    let value = Atom.get getter valueWrapper
                    let accessors = Atom.get getter accessorsAtom

                    let getLocals () =
                        $"value={value} accessors={accessors.IsSome} {getLocals ()}"

                    Profiling.addTimestamp
                        (fun () -> $"{nameof FsStore} | Selectors.store [ wrapper.read(getter) ]")
                        getLocals

                    accessors)



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
                        let logger = Atom.get getter logger
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
                        let logger = Atom.get getter logger
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
                        let logger = Atom.get getter logger
                        let _gunTrigger = Atom.get getter Atoms.gunTrigger
                        let gunUser = Atom.get getter Gun.gunUser

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
                        let logger = Atom.get getter logger
                        let _gunTrigger = Atom.get getter Atoms.gunTrigger
                        let gunUser = Atom.get getter Gun.gunUser

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

                        Profiling.addTimestamp
                            (fun () -> $"{nameof FsStore} | Selectors.Gun.adapterOptions get()")
                            getLocals

                        match gunOptions, alias with
                        | GunOptions.Sync peers, Some alias -> Some (Atom.AdapterOptions.Gun (peers, alias))
                        | _ -> None)


        module Hub =
            open Fable.SignalR

            let collection = Collection (nameof Hub)

            let inline readSelector name fn =
                Atom.readSelector (ValueAtomPath (FsStore.storeRoot, collection, [], AtomName name)) fn

            let hubKeySubscriptionMap = Dictionary<Gun.Alias * StoreRoot * Collection, string [] -> unit> ()
            let hubAtomSubscriptionMap = Dictionary<Gun.Alias * StoreAtomPath, string option -> JS.Promise<unit>> ()

            let rec hubConnection =
                readSelector
                    (nameof hubConnection)
                    (fun getter ->
                        let timeout = 2000

                        let hubUrl = Atom.get getter Atoms.hubUrl
                        let alias = Atom.get getter Gun.alias

                        let getLocals () =
                            $"alias={alias} hubUrl={hubUrl} {getLocals ()}"

                        Logger.logDebug (fun () -> $"{nameof FsStore} | Selectors.Hub.hubConnection. start") getLocals

                        match alias, hubUrl with
                        | Some _, Some (String.Valid hubUrl) ->
                            let connection =
                                SignalR.connect<Sync.Request, Sync.Request, obj, Sync.Response, Sync.Response>
                                    (fun hub ->
                                        hub
                                            .withUrl($"{hubUrl}{Sync.endpoint}")
                                            //                    .useMessagePack()
                                            .withAutomaticReconnect(
                                                {
                                                    nextRetryDelayInMilliseconds =
                                                        fun _context ->
                                                            Logger.logDebug
                                                                (fun () ->
                                                                    $"{nameof FsStore} | Selectors.Hub.hubConnection. SignalR.connect(). withAutomaticReconnect")
                                                                getLocals

                                                            Some timeout
                                                }
                                            )
                                            .onReconnecting(fun ex ->
                                                let getLocals () = $"ex={ex} {getLocals ()}"

                                                Logger.logDebug
                                                    (fun () ->
                                                        $"{nameof FsStore} | Selectors.Hub.hubConnection. SignalR.connect(). onReconnecting ")
                                                    getLocals)
                                            .onReconnected(fun ex ->
                                                let getLocals () = $"ex={ex} {getLocals ()}"

                                                Logger.logDebug
                                                    (fun () ->
                                                        $"{nameof FsStore} | Selectors.Hub.hubConnection. SignalR.connect(). onReconnected")
                                                    getLocals)
                                            .onClose(fun ex ->
                                                let getLocals () = $"ex={ex} {getLocals ()}"

                                                Logger.logDebug
                                                    (fun () ->
                                                        $"{nameof FsStore} | Selectors.Hub.hubConnection. SignalR.connect(). onClose")
                                                    getLocals)
                                            .configureLogging(LogLevel.Debug)
                                            .onMessage (fun msg ->
                                                match msg with
                                                | Sync.Response.ConnectResult ->
                                                    Logger.logDebug
                                                        (fun () ->
                                                            $"{nameof FsStore} | Selectors.Hub.hubConnection. Sync.Response.ConnectResult")
                                                        getLocals
                                                | Sync.Response.SetResult result ->
                                                    let getLocals () = $"result={result} {getLocals ()}"

                                                    Logger.logDebug
                                                        (fun () ->
                                                            $"{nameof FsStore} | Selectors.Hub.hubConnection. Sync.Response.SetResult")
                                                        getLocals
                                                | Sync.Response.GetResult value ->
                                                    let getLocals () = $"value={value} {getLocals ()}"

                                                    Logger.logDebug
                                                        (fun () ->
                                                            $"{nameof FsStore} | Selectors.Hub.hubConnection. Sync.Response.GetResult")
                                                        getLocals
                                                | Sync.Response.GetStream (alias, atomPath, value) ->
                                                    let getLocals () =
                                                        $"alias={alias} atomPath={atomPath} value={value} {getLocals ()}"

                                                    Logger.logDebug
                                                        (fun () ->
                                                            $"{nameof FsStore} | Selectors.Hub.hubConnection / GetStream")
                                                        getLocals

                                                    let storeAtomPath =
                                                        match atomPath |> String.split "/" |> Array.toList with
                                                        | storeRoot :: tail ->
                                                            let collection, keys, name =
                                                                match tail with
                                                                | [ name ] -> None, [], name
                                                                | collection :: [ name ] -> Some collection, [], name
                                                                | collection :: tail ->
                                                                    let name = tail |> List.last
                                                                    let keys = tail |> List.take (tail.Length - 1)
                                                                    Some collection, keys, name
                                                                | _ ->
                                                                    failwith
                                                                        $"{nameof FsStore} | Selectors.Hub.hubConnection / GetStream / invalid file event {getLocals ()}"

                                                            match collection with
                                                            | Some collection ->
                                                                StoreAtomPath.ValueAtomPath (
                                                                    StoreRoot storeRoot,
                                                                    Collection collection,
                                                                    keys |> List.map AtomKeyFragment,
                                                                    AtomName name
                                                                )
                                                                |> Some
                                                            | None ->
                                                                StoreAtomPath.RootAtomPath (
                                                                    StoreRoot storeRoot,
                                                                    AtomName name
                                                                )
                                                                |> Some
                                                        | _ -> None

                                                    match storeAtomPath with
                                                    | Some storeAtomPath ->
                                                        match
                                                            hubAtomSubscriptionMap
                                                            |> Map.tryFindDictionary ((Alias alias, storeAtomPath))
                                                            with
                                                        | Some fn ->
                                                            let getLocals () = $"msg={msg} {getLocals ()}"

                                                            Logger.logDebug
                                                                (fun () ->
                                                                    $"{nameof FsStore} | Selectors.Hub.hubConnection. Selectors.hub onMsg. triggering ")
                                                                getLocals

                                                            fn value |> Promise.start
                                                        | None ->
                                                            let getLocals () = $"msg={msg} {getLocals ()}"

                                                            Logger.logDebug
                                                                (fun () ->
                                                                    $"{nameof FsStore} | Selectors.Hub.hubConnection. onMsg. skipping. not in map")
                                                                getLocals
                                                    | None ->
                                                        let getLocals () =
                                                            $"storeAtomPath={storeAtomPath} msg={msg} {getLocals ()}"

                                                        Logger.logDebug
                                                            (fun () ->
                                                                $"{nameof FsStore} | Selectors.Hub.hubConnection. onMsg. skipping. invalid atom ")
                                                            getLocals

                                                | Sync.Response.FilterResult keys ->
                                                    let getLocals () = $"keys={keys} {getLocals ()}"

                                                    Logger.logDebug
                                                        (fun () ->
                                                            $"{nameof FsStore} | Selectors.Hub.hubConnection. Sync.Response.FilterResult")
                                                        getLocals
                                                | Sync.Response.FilterStream (alias, atomPath, keys) ->
                                                    let getLocals () =
                                                        $"alias={alias} atomPath={atomPath} keys={keys} {getLocals ()}"

                                                    Logger.logDebug
                                                        (fun () ->
                                                            $"{nameof FsStore} | Selectors.Hub.hubConnection. Sync.Response.FilterStream")
                                                        getLocals

                                                match msg with
                                                | Sync.Response.FilterStream (alias, atomPath, keys) ->
                                                    let storeRoot, collection =
                                                        match atomPath |> String.split "/" |> Array.toList with
                                                        | storeRoot :: collection :: _ -> storeRoot, collection
                                                        | _ ->
                                                            failwith
                                                                $"{nameof FsStore} | Selectors.Hub.hubConnection / FilterStream / invalid atom path {getLocals ()}"

                                                    match
                                                        hubKeySubscriptionMap
                                                        |> Map.tryFindDictionary
                                                            ((Alias alias, StoreRoot storeRoot, Collection collection))
                                                        with
                                                    | Some fn ->
                                                        let getLocals () = $"msg={msg} {getLocals ()}"

                                                        Logger.logDebug
                                                            (fun () ->
                                                                $"{nameof FsStore} | Selectors.Hub.hubConnection. Selectors.hub. FilterStream. onMsg. triggering")
                                                            getLocals

                                                        fn keys
                                                    | None ->
                                                        let getLocals () = $"msg={msg} {getLocals ()}"

                                                        Logger.logDebug
                                                            (fun () ->
                                                                $"{nameof FsStore} | Selectors.Hub.hubConnection. onMsg. FilterStream. skipping. not in map")
                                                            getLocals
                                                | _ ->
                                                    let getLocals () = $"msg={msg} {getLocals ()}"

                                                    Logger.logDebug
                                                        (fun () ->
                                                            $"{nameof FsStore} | Selectors.Hub.hubConnection. onMsg. FilterStream. skipping. not handled")
                                                        getLocals))

                            let getLocals () =
                                $"alias={alias} hubUrl={hubUrl} {getLocals ()}"

                            Logger.logDebug
                                (fun () ->
                                    $"{nameof FsStore} | Selectors.Hub.hubConnection. end. starting connection...")
                                getLocals

                            connection.startNow ()
                            Some connection
                        | _ -> None)


            let rec hub =
                readSelector
                    (nameof hub)
                    (fun getter ->
                        let logger = Atom.get getter logger
                        let _hubTrigger = Atom.get getter Atoms.hubTrigger
                        let hubConnection = Atom.get getter hubConnection

                        match hubConnection with
                        | Some hubConnection ->
                            let getLocals () =
                                $"_hubTrigger={_hubTrigger} hubConnection.connectionId={hubConnection.connectionId} {getLocals ()}"

                            logger.Debug (fun () -> $"{nameof FsStore} | Selectors.Hub.hub.") getLocals

                            Some hubConnection
                        //                        match Atom.value getter atomAccessors with
                        //                        | Some (getter, setter) ->
                        //                            Some hubConnection
                        //                        | None -> None
                        | _ -> None)

            let rec adapterOptions =
                readSelector
                    (nameof adapterOptions)
                    (fun getter ->
                        let alias = Atom.get getter Gun.alias
                        let hubUrl = Atom.get getter Atoms.hubUrl

                        let getLocals () = $"alias={alias} hubUrl={hubUrl}"

                        Profiling.addTimestamp
                            (fun () -> $"{nameof FsStore} | Selectors.Hub.adapterOptions get()")
                            getLocals

                        match alias, hubUrl with
                        | Some alias, Some (String.Valid hubUrl) -> Some (Atom.AdapterOptions.Hub (hubUrl, alias))
                        | _ -> None)
