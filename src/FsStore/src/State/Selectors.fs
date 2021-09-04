namespace FsStore.State

open Fable.Core.JsInterop
open System.Collections.Generic
open Fable.Core
open FsCore
open FsCore.BaseModel
open FsStore.Bindings.Gun
open FsStore.Model
open FsStore
open Microsoft.FSharp.Core.Operators
open FsJs
open FsStore.Bindings

#nowarn "40"


module rec Sync =
    [<RequireQualifiedAccess>]
    type Request =
        | Connect of alias: string
        | Set of alias: string * atomPath: string * value: string
        | Get of alias: string * atomPath: string
        | Filter of alias: string * storeRoot: string * collection: string

    [<RequireQualifiedAccess>]
    type Response =
        | ConnectResult
        | SetResult of ok: bool
        | GetResult of value: string option
        | GetStream of atomPath: string * value: string option
        | FilterResult of atomPathArray: string []
        | FilterStream of (string * string * string) * atomPathArray: string []

    let endpoint = $"/{nameof Sync}"


[<AutoOpen>]
module SelectorsMagic =
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

            Profiling.addTimestamp (fun () -> $"{nameof FsStore} | Selectors.store [ constructor ]")

            let rec valueWrapper =
                Atom.Primitives.selector
                    (fun getter ->
                        let result = Atom.get getter valueAtom

                        Profiling.addTimestamp
                            (fun () ->
                                $"{nameof FsStore} | Selectors.store [ valueWrapper.read(getter) ] result={result}")

                        result)
                    (fun getter setter newValue ->
                        Profiling.addTimestamp
                            (fun () ->
                                $"{nameof FsStore} | Selectors.store [ valueWrapper.set(getter,setter,newValue) ] newValue={newValue}")

                        Atom.set setter accessorsAtom (Some (getter, setter))
                        Atom.set setter valueAtom newValue)
                |> Atom.addSubscription
                    false
                    (fun setAtom ->
                        promise {
                            Profiling.addTimestamp
                                (fun () ->
                                    $"{nameof FsStore} | Selectors.store [ valueWrapper.onMount() ] lastValue={lastValue}")

                            lastValue <- lastValue + 1
                            setAtom lastValue
                        })
                    (fun () ->
                        Profiling.addTimestamp
                            (fun () ->
                                $"{nameof FsStore} | Selectors.store [ valueWrapper.onUnmount() ] lastValue={lastValue}"))

            Atom.readSelector
                (RootAtomPath (FsStore.storeRoot, AtomName (nameof store)))
                (fun getter ->
                    let value = Atom.get getter valueWrapper
                    let accessors = Atom.get getter accessorsAtom

                    Profiling.addTimestamp
                        (fun () ->
                            $"{nameof FsStore} | Selectors.store [ wrapper.read(getter) ] value={value} accessors={accessors.IsSome}")

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

                        Logger.logDebug (fun () -> $"Selectors.Gun.gun. gunPeers={gunPeers}. gun={gun} returning...")

                        gun)

            let rec gunUser =
                readSelector
                    (nameof gunUser)
                    (fun getter ->
                        let logger = Atom.get getter logger
                        let _gunTrigger = Atom.get getter Atoms.gunTrigger
                        let gun = Atom.get getter gun

                        logger.Debug (fun () -> $"Selectors.Gun.gunUser. keys={gun.user().__.sea |> Js.objectKeys}")

                        gun.user ())

            let rec gunNamespace =
                readSelector
                    (nameof gunNamespace)
                    (fun getter ->
                        let logger = Atom.get getter logger
                        let _gunTrigger = Atom.get getter Atoms.gunTrigger
                        let gunUser = Atom.get getter gunUser

                        logger.Debug
                            (fun () -> $"Selectors.Gun.gunNamespace. gunUser.is={JS.JSON.stringify gunUser.is}")

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
                            logger.Debug
                                (fun () ->
                                    $"Selectors.Gun.alias. alias={alias}  keys={gunUser.__.sea |> Js.objectKeys}")

                            Some (Alias alias)
                        | _ ->
                            match gunUser.__.sea with
                            | Some { priv = Some (Priv (String.Valid _)) } ->
                                let internalAlias = Atom.get getter Atoms.internalAlias
                                logger.Debug (fun () -> $"Selectors.Gun.alias. internalAlias={internalAlias}")
                                internalAlias
                            | _ -> None)

            let rec privateKeys =
                readSelector
                    (nameof privateKeys)
                    (fun getter ->
                        let logger = Atom.get getter logger
                        let _gunTrigger = Atom.get getter Atoms.gunTrigger
                        let gunUser = Atom.get getter Gun.gunUser
                        logger.Debug (fun () -> $"Selectors.Gun.keys. keys={gunUser.__.sea |> Js.objectKeys}")
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

                        Profiling.addTimestamp (fun () -> $"Selectors.Gun.adapterOptions get() {getLocals ()}")

                        match gunOptions, alias with
                        | GunOptions.Sync peers, Some alias -> Some (Atom.AdapterOptions.Gun (peers, alias))
                        | _ -> None)


        module Hub =
            open Fable.SignalR

            let collection = Collection (nameof Hub)

            let inline readSelector name fn =
                Atom.readSelector (ValueAtomPath (FsStore.storeRoot, collection, [], AtomName name)) fn

            let hubSubscriptionMap = Dictionary<Gun.Alias * StoreRoot * Collection, string [] -> unit> ()

            let rec hubConnection =
                readSelector
                    (nameof hubConnection)
                    (fun getter ->
                        let timeout = 2000

                        let hubUrl = Atom.get getter Atoms.hubUrl
                        let alias = Atom.get getter Gun.alias

                        Logger.logDebug (fun () -> $"Selectors.Hub.hubConnection. start. alias={alias} hubUrl={hubUrl}")

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
                                                                    "Selectors.Hub.hubConnection. SignalR.connect(). withAutomaticReconnect")

                                                            Some timeout
                                                }
                                            )
                                            .onReconnecting(fun ex ->
                                                Logger.logDebug
                                                    (fun () ->
                                                        $"Selectors.Hub.hubConnection. SignalR.connect(). onReconnecting ex={ex}"))
                                            .onReconnected(fun ex ->
                                                Logger.logDebug
                                                    (fun () ->
                                                        $"Selectors.Hub.hubConnection. SignalR.connect(). onReconnected ex={ex}"))
                                            .onClose(fun ex ->
                                                Logger.logDebug
                                                    (fun () ->
                                                        $"Selectors.Hub.hubConnection. SignalR.connect(). onClose ex={ex}"))
                                            .configureLogging(LogLevel.Debug)
                                            .onMessage (fun msg ->
                                                match msg with
                                                | Sync.Response.ConnectResult ->
                                                    Logger.logDebug
                                                        (fun () ->
                                                            "Selectors.Hub.hubConnection. Sync.Response.ConnectResult")
                                                | Sync.Response.SetResult result ->
                                                    Logger.logDebug
                                                        (fun () ->
                                                            $"Selectors.Hub.hubConnection. Sync.Response.SetResult result={result}")
                                                | Sync.Response.GetResult value ->
                                                    Logger.logDebug
                                                        (fun () ->
                                                            $"Selectors.Hub.hubConnection. Sync.Response.GetResult value={value}")
                                                | Sync.Response.GetStream (key, value) ->
                                                    Logger.logDebug
                                                        (fun () ->
                                                            $"Selectors.Hub.hubConnection. Sync.Response.GetStream key={key} value={value}")
                                                | Sync.Response.FilterResult keys ->
                                                    Logger.logDebug
                                                        (fun () ->
                                                            $"Selectors.Hub.hubConnection. Sync.Response.FilterResult keys={keys}")
                                                | Sync.Response.FilterStream (key, keys) ->
                                                    Logger.logDebug
                                                        (fun () ->
                                                            $"Selectors.Hub.hubConnection. Sync.Response.FilterStream key={key} keys={keys}")

                                                match msg with
                                                | Sync.Response.FilterStream ((alias, storeRoot, collection), keys) ->
                                                    match
                                                        hubSubscriptionMap.TryGetValue
                                                            ((Alias alias, StoreRoot storeRoot, Collection collection))
                                                        with
                                                    | true, fn ->
                                                        Logger.logDebug
                                                            (fun () ->
                                                                $"Selectors.Hub.hubConnection. Selectors.hub onMsg msg={msg}. triggering ")

                                                        fn keys
                                                    | _ ->
                                                        Logger.logDebug
                                                            (fun () ->
                                                                $"Selectors.Hub.hubConnection. onMsg msg={msg}. skipping. not in map ")
                                                | _ ->
                                                    Logger.logDebug
                                                        (fun () ->
                                                            $"Selectors.Hub.hubConnection.  onMsg msg={msg}. skipping. not handled ")))

                            Logger.logDebug
                                (fun () ->
                                    $"Selectors.Hub.hubConnection. end. alias={alias} hubUrl={hubUrl}. starting connection...")

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
                            logger.Debug
                                (fun () ->
                                    $"Selectors.Hub.hub. _hubTrigger={_hubTrigger} hubConnection.connectionId={hubConnection.connectionId}")

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

                        Profiling.addTimestamp (fun () -> $"Selectors.Hub.adapterOptions get() {getLocals ()}")

                        match alias, hubUrl with
                        | Some alias, Some (String.Valid hubUrl) -> Some (Atom.AdapterOptions.Hub (hubUrl, alias))
                        | _ -> None)
