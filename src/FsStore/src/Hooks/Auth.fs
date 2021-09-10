namespace FsStore.Hooks

open Fable.Extras
open Fable.Core
open Feliz
open FsJs
open FsStore
open FsStore.Bindings
open FsStore.Model
open FsStore.State


module Auth =
    module Utils =
        let inline hydrateAppMessage setter fn result =
            match result with
            | Ok value -> Hydrate.hydrateAppMessage setter (fn value)
            | Error error ->
                Hydrate.hydrateAppMessage
                    setter
                    (Message.Command (AppCommand.QueueNotification (Notification.Error error)))


    let signIn =
        fun getter _setter (alias, password) ->
            promise {
                let gunUser = Atom.get getter Selectors.Gun.gunUser

                let! ack =
                    match alias, password with
                    | "", keys ->
                        printfn "keys sign in"

                        let keys =
                            try
                                keys |> Json.decode<Gun.GunKeys>
                            with
                            | ex ->
                                printfn $"keys decode error: {ex.Message}"
                                Gun.GunKeys.Default

                        Gun.authKeys gunUser keys

                    | alias, password ->
                        printfn "user/pass sign in"
                        Gun.authUser gunUser (Gun.Alias alias) (Gun.Pass password)

                return
                    match ack with
                    | { err = None } ->
                        let keys = gunUser.__.sea

                        match keys with
                        | Some keys ->
                            //                        do! Promise.sleep 100
                            //                            Store.change setter Atoms.gunTrigger ((+) 1)
                            //                            Store.change setter Atoms.hubTrigger ((+) 1)
                            Ok (Gun.Alias alias, keys)
                        | None -> Error $"No keys found for user {alias} after sign in"
                    | { err = Some error } -> Error error
            }

    module Actions =
        let logout =
            Atom.Primitives.setSelector
                (fun getter setter () ->
                    Profiling.addTimestamp
                        (fun () -> $"{nameof FsStore} | Auth.Actions.logout. invoking gun.user().leave()")

                    let gunUser = Atom.get getter Selectors.Gun.gunUser
                    gunUser.leave ()
                    Atom.change setter Atoms.gunTrigger ((+) 1)
                    Atom.change setter Atoms.hubTrigger ((+) 1))


        let changePassword =
            Atom.Primitives.asyncSetSelector
                (fun getter setter (password, newPassword) ->
                    promise {
                        let alias = Atom.get getter Selectors.Gun.alias
                        let gunUser = Atom.get getter Selectors.Gun.gunUser

                        let! result =
                            promise {
                                match alias with
                                | Some alias ->
                                    let! ack =
                                        Gun.changeUserPassword gunUser alias (Gun.Pass password) (Gun.Pass newPassword)

                                    return!
                                        promise {
                                            match ack with
                                            | { ok = Some 1; err = None } ->
                                                Atom.change setter Atoms.gunTrigger ((+) 1)
                                                Atom.change setter Atoms.hubTrigger ((+) 1)
                                                return Ok alias
                                            | { err = Some error } -> return Error error
                                            | _ -> return Error $"invalid ack {JS.JSON.stringify ack}"
                                        }
                                | _ -> return Error "Invalid alias"
                            }

                        result
                        |> Utils.hydrateAppMessage
                            setter
                            (fun alias -> Message.Event (AppEvent.UserPasswordChanged alias))
                        |> ignore
                    })

        let deleteUser =
            Atom.Primitives.asyncSetSelector
                (fun getter setter password ->
                    promise {
                        let alias = Atom.get getter Selectors.Gun.alias

                        let! result =
                            promise {
                                match alias with
                                | Some alias ->
                                    let gunUser = Atom.get getter Selectors.Gun.gunUser

                                    let! ack = Gun.deleteUser gunUser alias (Gun.Pass password)
                                    printfn $"ack={JS.JSON.stringify ack}"

                                    return!
                                        promise {
                                            match ack with
                                            | { ok = Some 0; err = None } ->
                                                Atom.set setter logout ()
                                                return Ok alias
                                            | { err = Some error } -> return Error error
                                            | _ -> return Error $"invalid ack {JS.JSON.stringify ack}"
                                        }
                                | _ -> return Error "Invalid alias"

                            }

                        result
                        |> Utils.hydrateAppMessage setter (fun alias -> Message.Event (AppEvent.UserDeleted alias))
                        |> ignore

                    })

        let signUp =
            Atom.Primitives.asyncSetSelector
                (fun getter setter (alias, password) ->
                    promise {
                        let! result =
                            promise {
                                if alias = "" || password = "" then
                                    return Error "Required fields"
                                elif (JSe.RegExp @"^[a-zA-Z0-9.!#$%&’*+/=?^_`{|}~-]+@[a-zA-Z0-9-]+(?:\.[a-zA-Z0-9-]+)*$")
                                         .Test alias
                                     |> not then
                                    return Error "Invalid email address"
                                else
                                    let gun = Atom.get getter Selectors.Gun.gun
                                    let logger = Atom.get getter Selectors.logger
                                    let user = gun.user ()
                                    logger.Debug (fun () -> $"Auth.useSignUp. gunUser.is={user.is |> Js.objectKeys}")

                                    let! ack = Gun.createUser user (Gun.Alias alias) (Gun.Pass password)

                                    logger.Debug
                                        (fun () -> $"Auth.useSignUp. Gun.createUser signUpAck={JS.JSON.stringify ack}")

                                    return!
                                        promise {
                                            match ack with
                                            | {
                                                  err = None
                                                  ok = Some 0
                                                  pub = Some _
                                              } ->
                                                match! signIn getter setter (alias, password) with
                                                | Ok (alias, keys) ->
                                                    do! Gun.putPublicHash gun alias
                                                    return Ok (alias, keys)
                                                | Error error -> return Error error
                                            | { err = Some err } -> return Error err
                                            | _ -> return Error $"Invalid ack: {JS.JSON.stringify ack}"
                                        }
                            }

                        result
                        |> Utils.hydrateAppMessage
                            setter
                            (fun (alias, _keys) -> Message.Event (AppEvent.UserRegistered alias))
                        |> ignore
                    })

    let inline useGunAliasLoader () =
        let setInternalAlias = Store.useSetState Atoms.internalAlias
        let gun = Store.useValue Selectors.Gun.gun
        let privateKeys = Store.useValue Selectors.Gun.privateKeys
        let logger = Store.useValue Selectors.logger

        let getLocals () =
            $"privateKeys.IsSome={privateKeys.IsSome}"


        React.useEffect (
            (fun () ->
                promise {
                    let logDebug fn getLocals =
                        logger.Debug (fun () -> $"Auth.useGunAliasLoader {fn ()} {getLocals ()}")

                    match privateKeys with
                    | Some privateKeys ->
                        let! data = Gun.aliasRadQuery gun
                        let! newAlias = Gun.userDecode<Gun.Alias> privateKeys data
                        let getLocals () = $"data={data} newAlias={newAlias}"

                        match newAlias with
                        | Some alias ->
                            logDebug (fun () -> "setting internal alias") getLocals
                            setInternalAlias (Some alias)
                        | None ->
                            logDebug (fun () -> "setting none. no alias") getLocals
                            setInternalAlias None
                    | None ->
                        logDebug (fun () -> "setting none. no keys") getLocals
                        setInternalAlias None
                }
                |> Promise.start),
            [|
                box setInternalAlias
                box gun
                box privateKeys
                box logger
            |]
        )
