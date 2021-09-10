namespace FsStore.Hooks

open FsCore
open FsJs
open FsStore
open FsStore.Model
open FsStore.Bindings


module Messaging =
    let inline appUpdate getter setter state command =
        promise {
            let! result =
                promise {
                    match command with
                    | AppCommand.Init state -> return state, []
                    | AppCommand.QueueNotification notification ->
                        return
                            { state with
                                NotificationQueue = notification :: state.NotificationQueue
                            },
                            []

                    | AppCommand.SignInPair keys ->
                        let! result = Auth.signIn getter setter ("", keys |> Json.encode<Gun.GunKeys>)

                        let message =
                            match result with
                            | Ok (alias, _keys) -> Message.Event (AppEvent.UserSignedIn alias)
                            | Error error ->
                                Message.Command (
                                    AppCommand.QueueNotification (Notification.Error ("Sign In Error", error, None))
                                )

                        return
                            state,
                            [
                                message
                            ]
                }

            let getLocals () =
                $"command={command} result={result} {getLocals ()}"

            Profiling.addCount (fun () -> $"{nameof FsStore} | Messaging.appUpdate") getLocals
            return result
        }

    let inline atomUpdate _getter _setter state command =
        promise {
            let! result =
                promise {
                    match command with
                    | AtomCommand.Init state -> return state, []
                    | AtomCommand.Subscribe -> return state, []
                    | AtomCommand.Unsubscribe -> return state, []
                }

            let getLocals () =
                $"command={command} result={result} {getLocals ()}"

            Profiling.addCount (fun () -> $"{nameof FsStore} | Messaging.atomUpdate") getLocals
            return result
        }
