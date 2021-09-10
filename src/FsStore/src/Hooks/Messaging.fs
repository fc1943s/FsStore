namespace FsStore.Hooks

open FsJs
open FsStore
open FsStore.Model
open FsStore.Bindings
open FsStore.State


module Messaging =
    let inline appUpdate getter setter state command =
        promise {
            let logger = Atom.get getter Selectors.logger

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
                            | Error error -> Message.Command (AppCommand.QueueNotification (Notification.Error error))

                        return
                            state,
                            [
                                message
                            ]
                }

            Profiling.addCount (fun () -> $"{nameof FsStore} | Messaging.appUpdate. command={command} result={result}")
            logger.Trace (fun () -> $"Messaging.appUpdate. command={command} result={result}")
            return result
        }

    let inline atomUpdate getter _setter state command =
        promise {
            let logger = Atom.get getter Selectors.logger

            let! result =
                promise {
                    match command with
                    | AtomCommand.Init state -> return state, []
                    | AtomCommand.Subscribe -> return state, []
                    | AtomCommand.Unsubscribe -> return state, []
                }

            Profiling.addCount (fun () -> $"{nameof FsStore} | Messaging.atomUpdate. command={command} result={result}")
            logger.Trace (fun () -> $"Messaging.atomUpdate. command={command} result={result}")
            return result
        }
