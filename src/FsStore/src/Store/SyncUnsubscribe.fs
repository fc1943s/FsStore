namespace FsStore.Store

open Microsoft.FSharp.Core.Operators
open FsCore
open FsJs
open FsStore.Bindings


module SyncUnsubscribe =
    module Store =
        let inline syncUnsubscribe getLocals gunAtomNode subscription success =
            match subscription with
            | Some ticks when DateTime.ticksDiff ticks < 1000. ->
                Logger.logTrace
                    (fun () ->
                        $"Store.syncUnsubscribe. skipping unsubscribe. jotai resubscribe glitch. gunAtomNode={gunAtomNode} {getLocals ()} ")
            | Some _ ->
                match gunAtomNode with
                | Some (key, gunAtomNode: Gun.Types.IGunChainReference) ->

                    Profiling.addCount (fun () -> $"{nameof FsStore} | {key} unsubscribe")

                    Logger.logTrace
                        (fun () -> $"Store.syncUnsubscribe. {key} gunAtomNode={gunAtomNode} {getLocals ()} ")

                    gunAtomNode.off () |> ignore
                    success ()
                | None ->
                    Logger.logTrace
                        (fun () ->
                            $"Store.syncUnsubscribe. skipping unsubscribe, no gun atom node. gunAtomNode={gunAtomNode} {getLocals ()} ")

            | None ->
                Logger.logTrace
                    (fun () ->
                        $"Store.syncUnsubscribe. skipping unsubscribe. no last subscription found. gunAtomNode={gunAtomNode} {getLocals ()} ")
