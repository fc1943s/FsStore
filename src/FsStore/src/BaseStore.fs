namespace FsStore

open System.Collections.Generic
open Fable.Extras
open System
open FsStore.Model
open Microsoft.FSharp.Core.Operators
open FsCore
open FsJs
open FsStore.Bindings

#nowarn "40"


module BaseStore =
    module Store =
        let testKeysCache = Dictionary<string, Set<string>> ()

        let inline splitAtomPath (AtomPath atomPath) =
            let matches =
                (JSe.RegExp @"(.*?)\/([\w-]{36})\/\w+.*?")
                    .Match atomPath
                |> Option.ofObj
                |> Option.defaultValue Seq.empty
                |> Seq.toList

            match matches with
            | _match :: root :: guid :: _key -> Some (root, guid)
            | _ -> None






        //    type Msg =
        //        | Internal of int64 option * obj
        //        | Gun of int64 * obj
        //        | Hub of int64 * obj


        type Command2 =
            | UnregisterAdapter
            | CreateUser
            | SignInAlias
            | SignInPair
            | SignOutUser


        type Event2<'T> =
            //            | AdapterRegistered
            | AdapterUnregistered
            | UserCreated
            | UserSignedIn
            | UserSignedOut
            | AtomMount
            | AtomUnmount
            | AdapterEnable
            | AdapterSubscribe
            | AdapterValue of Atom.AdapterType * 'T
            | AdapterUnsubscribe
            | AdapterDisable

        let adapterValueMap = Map<TicksGuid, Atom.AdapterType * 'T>
        type Z = Map<Atom.AdapterType, TicksGuid * Gun.EncryptedSignedValue> //selector, defaultValue
        type R = TicksGuid -> Gun.EncryptedSignedValue
        type F = Gun.EncryptedSignedValue

        //    type AckMap = Map<AdapterValue<'T>, > //selector, defaultValue

        type Y =
            | Y //of Msg
            | W

        type AtomSyncState<'T> = { Value: 'T }


        type SyncState<'TValue> () =
            let mutable lastAdapterValueMapByType: Map<Atom.AdapterType, (TicksGuid * 'TValue) option> option = None
            let mutable lastGunSubscription = None
            let mutable lastHubSubscription = None
            let mutable syncPaused = false

            member this.AdapterValueMapByType
                with get () = lastAdapterValueMapByType
                and set value = lastAdapterValueMapByType <- value

            member this.GunSubscription
                with get () = lastGunSubscription
                and set (value: int64 option) = lastGunSubscription <- value

            member this.HubSubscription
                with get () = lastHubSubscription
                and set (value: IDisposable option) = lastHubSubscription <- value

            member this.SyncPaused
                with get () = syncPaused
                and set value = syncPaused <- value

