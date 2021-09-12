namespace FsStore.State

open System.Collections.Generic
open Fable.Core
open FsBeacon.Shared
open FsCore
open FsCore.BaseModel
open FsStore.Bindings.Gun
open FsStore.Model
open FsStore
open FsStore.State
open Microsoft.FSharp.Core.Operators
open FsJs
open FsStore.Bindings


[<AutoOpen>]
module SelectorsHubMagic =
    module Selectors =
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
                        let alias = Atom.get getter Selectors.Gun.alias

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
                        let logger = Atom.get getter Selectors.logger
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
                        let alias = Atom.get getter Selectors.Gun.alias
                        let hubUrl = Atom.get getter Atoms.hubUrl

                        let getLocals () = $"alias={alias} hubUrl={hubUrl}"

                        Profiling.addTimestamp
                            (fun () -> $"{nameof FsStore} | Selectors.Hub.adapterOptions get()")
                            getLocals

                        match alias, hubUrl with
                        | Some alias, Some (String.Valid hubUrl) -> Some (Atom.AdapterOptions.Hub (hubUrl, alias))
                        | _ -> None)
