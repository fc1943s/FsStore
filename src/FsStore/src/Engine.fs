namespace FsStore

open System
open System.Collections.Generic
open Fable.Core.JsInterop
open Fable.Core
open FsBeacon.Shared
open FsCore.BaseModel
open FsJs
open FsStore.Bindings
open FsStore.Bindings.Batcher
open FsStore.Bindings.Gun
open FsStore.Model
open FsCore
open FsStore.State

#nowarn "40"


module Engine =
    type UpdateFn<'State, 'Command, 'Event> =
        Getter<obj> -> Setter<obj> -> 'State -> 'Command -> JS.Promise<'State * Message<'Command, 'Event> list>

    let inline getLocals () = ""

    let inline consumeCommands (updateFn: UpdateFn<_, _, _>) state getter setter commands =
        promise {
            //            let logger = Atom.get getter Selectors.logger

            let rec loop state commands processedMessages =
                promise {
                    match commands with
                    | command :: commands ->
                        let! state, processedMessages' = updateFn getter setter state command

                        let commands', events =
                            processedMessages'
                            |> List.partition
                                (function
                                | Message.Command _ -> true
                                | _ -> false)

                        let newCommands =
                            commands'
                            |> List.choose
                                (function
                                | Message.Command command -> Some command
                                | _ -> None)

                        return! loop state (newCommands @ commands) (processedMessages @ events)
                    | [] -> return state, processedMessages
                }

            let! newState, processedMessages = loop state commands []


            let getLocals () =
                $"commands={commands} newState={newState} processedMessages={processedMessages} {getLocals ()}"

            let addTimestamp fn getLocals =
                Profiling.addTimestamp (fun () -> $"{nameof FsStore} | Engine.consumeCommands {fn ()}") getLocals

            addTimestamp (fun () -> "[ ](_1)") getLocals

            return
                newState,
                processedMessages
                |> List.choose
                    (function
                    | Message.Event event -> Some event
                    | _ -> None)
        }


    let mutable lastStore: (Getter<obj> * Setter<obj>) option = None

    type MountFn<'A> = Getter<obj> -> Setter<obj> -> ('A -> unit) -> JS.Promise<unit>
    type UnmountFn = Getter<obj> -> Setter<obj> -> unit
    type StateFn<'S> = Getter<obj> -> 'S option
    type StateMountFn<'A, 'S> = Getter<obj> -> Setter<obj> -> 'S -> ('A -> unit) -> JS.Promise<unit>
    type StateUnmountFn<'S> = Getter<obj> -> Setter<obj> -> 'S -> unit



    let inline wrapAtomWithState<'A, 'S when 'A: equality>
        (stateFn: StateFn<'S>)
        (mount: StateMountFn<'A, 'S>)
        (unmount: StateUnmountFn<'S>)
        (atom: AtomConfig<'A>)
        =
        let mutable lastState: 'S option = None

        let mutable mounted = false

        let getLocals () =
            $"atom={atom} mounted={mounted} lastState.IsSome={lastState.IsSome} {getLocals ()}"

        let addTimestamp fn getLocals =
            Profiling.addTimestamp (fun () -> $"{nameof FsStore} | Engine.wrapAtomWithState {fn ()}") getLocals

        addTimestamp (fun () -> "[ constructor ](g1)") getLocals

        let mutable lastSetAtom = None

        let inline getState () =
            match lastStore with
            | Some (getter, setter) ->
                let state =
                    match stateFn getter with
                    | Some state -> Some state
                    | None -> lastState

                lastState <- state

                match state with
                | Some state -> Some (getter, setter, state)
                | None -> None
            | _ -> None

        let inline newMount () =
            promise {
                match getState () with
                | Some (getter, setter, state) ->
                    addTimestamp (fun () -> "[ newMount ](g2) invoking mount") getLocals

                    mounted <- true
                    do! mount getter setter state lastSetAtom.Value
                | None -> addTimestamp (fun () -> "[ newMount ](g3) skipping, no state") getLocals
            }

        let inline newUnmount () =
            if mounted then
                match getState () with
                | Some (getter, setter, state) ->
                    addTimestamp (fun () -> "[ newUnmount ](g4) invoking unmount") getLocals

                    mounted <- false
                    lastState <- None
                    unmount getter setter state
                | None ->
                    addTimestamp
                        (fun () -> "[ newUnmount ](g5) invoking unmount skipping, no state. (should unmount here???)")
                        getLocals

        let inline refreshInternalState getter =
            if lastStore.IsNone then
                lastStore <- Atom.get getter Selectors.Store.store

            let _logger = Atom.get getter Selectors.Store.logger

            let newState = stateFn getter

            let getLocals () =
                $"newState.IsSome={newState.IsSome} lastSetAtom.IsSome={lastSetAtom.IsSome} {getLocals ()}"

            if lastSetAtom.IsNone then
                addTimestamp
                    (fun () -> "[ refreshInternalState ](g5) skipping mount/unmount. lastSetAtom not found")
                    getLocals
            else
                match newState with
                | Some _ ->
                    addTimestamp (fun () -> "[ refreshInternalState ](g6) invoking newMount") getLocals
                    lastState <- newState
                    newMount () |> Promise.start
                | None ->
                    addTimestamp (fun () -> "[ refreshInternalState ](g7) invoking newUnmount") getLocals
                    newUnmount ()

        atom
        |> Atom.wrap
            (fun getter ->
                refreshInternalState getter

                let result = Atom.get getter atom
                let getLocals () = $"result={result}  {getLocals ()}"

                addTimestamp (fun () -> "[ wrapper.get() ](g8)") getLocals

                result)
            (fun _getter setter newValue ->
                //                    refreshInternalState getter

                let getLocals () = $"newValue={newValue}  {getLocals ()}"

                addTimestamp (fun () -> "[ wrapper.set() ](g9)") getLocals
                Atom.set setter atom newValue)
        |> Atom.addSubscription
            false
            (fun setAtom ->
                addTimestamp (fun () -> "[ addSubscription mount ](g10) invoking newMount") getLocals
                lastSetAtom <- Some setAtom
                newMount ())
            (fun () ->
                addTimestamp (fun () -> "[ addSubscription unmount ](g11) invoking newUnmount") getLocals
                newUnmount ())



    let inline wrapAtom<'A when 'A: equality> (mount: MountFn<'A>) (unmount: UnmountFn) (atom: AtomConfig<'A>) =
        wrapAtomWithState
            (fun _ -> Some ())
            (fun getter setter _ -> mount getter setter)
            (fun getter setter _ -> unmount getter setter)
            atom

    let inline wrapAtomWithInterval defaultValue interval atom =
        let mutable intervalHandle = -1
        let mutable lastValue = None

        let getLocals () =
            $"interval={interval} defaultValue={defaultValue} lastValue={lastValue} timeout={intervalHandle}  {getLocals ()}"

        let addTimestamp fn getLocals =
            Profiling.addTimestamp (fun () -> $"{nameof FsStore} | Engine.wrapAtomWithInterval {fn ()}") getLocals

        let cache = Atom.Primitives.atom defaultValue

        let inline getFn getter setAtom =
            addTimestamp (fun () -> "[ wrapper.mount.fn() ](h2) interval fn") getLocals

            if intervalHandle >= 0 then
                let atomValue = Atom.get getter atom

                if Some atomValue |> Object.compare lastValue |> not then
                    let getLocals () = $"atomValue={atomValue} {getLocals ()}"

                    addTimestamp (fun () -> "[ wrapper.mount.fn() ](h3) interval fn. triggering new value") getLocals

                    lastValue <- Some atomValue
                    setAtom atomValue

        let debouncedGetFn = Js.debounce getFn 0

        let wrapper =
            cache
            |> wrapAtom
                (fun getter _ setAtom ->
                    promise {
                        addTimestamp (fun () -> "[ wrapper.mount() ](h1)") getLocals

                        if intervalHandle = -1 then debouncedGetFn getter setAtom
                        intervalHandle <- JS.setInterval (fun () -> debouncedGetFn getter setAtom) interval
                    })
                (fun _getter _setter ->
                    //                let logger = Logger.State.lastLogger
                    addTimestamp (fun () -> "[ wrapper.unmount() ](h4) ") getLocals

                    if intervalHandle >= 0 then JS.clearTimeout intervalHandle
                    intervalHandle <- -1)

        wrapper?init <- defaultValue

        wrapper


    [<Erase>]
    type GroupRef = GroupRef of IComparable

    [<Erase>]
    type AtomValueRef = AtomValueRef of IComparable

    [<Erase>]
    type KeyRef = KeyRef of IComparable

    let typeMetadataMap =
        Dictionary<DataType * Type, {| DefaultValue: AtomValueRef
                                       Decode: GunKeys
                                           -> EncryptedSignedValue
                                           -> JS.Promise<(TicksGuid * AtomValueRef) option>
                                       Encode: GunKeys -> TicksGuid * AtomValueRef -> JS.Promise<EncryptedSignedValue>
                                       OnFormat: AtomKeyFragment [] -> KeyRef option |}>
            ()

    let collectionTypeMap = Dictionary<StoreRoot * Collection, Type> ()


    let inline batchGunPutFromUi atomType (gunAtomNode, privateKeys, ticks, newValue: AtomValueRef, onPut) =
        let getLocals () =
            $"atomType={atomType} ticks={ticks} newValue={newValue} {getLocals ()}"

        let typeMetadata = typeMetadataMap.[atomType]

        let addTimestamp fn getLocals =
            Profiling.addTimestamp (fun () -> $"{nameof FsStore} | Engine.batchPutFromUi {fn ()}") getLocals

        batch (
            BatchType.Set (
                ticks,
                (fun ticks ->
                    promise {
                        let! newValueJson =
                            promise {
                                if newValue |> Js.ofNonEmptyObj |> Option.isNone then
                                    return null
                                else
                                    let! (EncryptedSignedValue encrypted) =
                                        typeMetadata.Encode privateKeys (ticks, newValue)

                                    return encrypted
                            }

                        let! putResult =
                            put gunAtomNode (GunValue.EncryptedSignedValue (EncryptedSignedValue newValueJson))

                        let getLocals () = $"putResult={putResult} {getLocals ()}"

                        addTimestamp (fun () -> "[ batchSetFn ](i1)") getLocals
                        if putResult then onPut ()
                    })
            )
        )


    let memoryAdapterOptions = Some Atom.AdapterOptions.Memory

    let inline getAdapterOptions getter adapterType =
        match adapterType with
        | Atom.AdapterType.Gun -> Atom.get getter Selectors.Gun.adapterOptions
        | Atom.AdapterType.Hub -> Atom.get getter Selectors.Hub.adapterOptions
        | Atom.AdapterType.Memory -> memoryAdapterOptions

    type FromUi =
        | FromUi
        | NotFromUi

    let collectionKeysFamily =
        Atom.atomFamilyAtom
            (fun (_alias: Gun.Alias option, _storeRoot: StoreRoot, _collection: Collection) -> Set.empty: Set<KeyRef>)



    type Transaction = Transaction of fromUi: FromUi * ticks: TicksGuid * value: AtomValueRef


    let lastCollectionKeyMap = Dictionary<StoreAtomPath, Set<AtomKeyFragment>> ()

    let inline batchKeyResult storeAtomPath keysBatch =
        let lastKeySet =
            lastCollectionKeyMap
            |> Map.tryFindDictionary storeAtomPath
            |> Option.defaultValue Set.empty

        let newKeySet =
            (lastKeySet, keysBatch)
            ||> Array.fold
                    (fun newKeySet (kind, _ticks, key) ->
                        let setFn =
                            match kind with
                            | BatchKind.RemoveItem -> Set.remove
                            | BatchKind.UnionItem -> Set.add

                        newKeySet |> setFn key)

        lastCollectionKeyMap.[storeAtomPath] <- newKeySet
        let newKeys = newKeySet |> Set.toArray


        //                    let newItems =
        //                        itemsArray
        //                        |> Seq.collect snd
        //                        |> Seq.filter (lastSet.Contains >> not)
        //                        |> Seq.toArray
        //
        //                    let items =
        //                        itemsArray
        //                        |> Array.collect snd
        //                        |> Array.append newItems
        //                        |> Array.distinct
        //
        //                    lastValue <- Some (newItems |> Set.ofArray |> Set.union lastSet)
        let getLocals () =
            $"keysBatch={keysBatch} newKeys={newKeys} lastKeySet={lastKeySet} {getLocals ()}"

        let addTimestamp fn getLocals =
            Profiling.addTimestamp (fun () -> $"{nameof FsStore} | Engine.batchKeyResult {fn ()}") getLocals

        addTimestamp (fun () -> "[ ] ") getLocals

        newKeys

    let inline batchKey storeAtomPath atomType kind data (setAtom: AtomKeyFragment [] -> JS.Promise<unit>) =

        let getLocals () =
            $"atomType={atomType} atomPath={storeAtomPath |> StoreAtomPath.AtomPath} {getLocals ()}"

        let addTimestamp fn getLocals =
            Profiling.addTimestamp (fun () -> $"{nameof FsStore} | Engine.batchKey {fn ()}") getLocals

        addTimestamp (fun () -> "[ body ]") getLocals

        batchKey atomType kind setAtom data (batchKeyResult storeAtomPath)

    let inline batchAtomKey setter atomType onFormat kind (alias, storeRoot, collection) (fromUi, ticks, value) =
        let storeAtomPath = CollectionAtomPath (storeRoot, collection)

        let getLocals () =
            $"atomType={atomType} kind={kind} alias={alias} storeRoot={storeRoot} collection={collection} fromUi={fromUi} ticks={ticks} value={value} {getLocals ()}"

        let addTimestamp fn getLocals =
            Profiling.addTimestamp (fun () -> $"{nameof FsStore} | Engine.batchKeysAtom {fn ()}") getLocals

        addTimestamp (fun () -> "[ body ]") getLocals

        batchKey
            storeAtomPath
            atomType
            kind
            (ticks, value)
            (fun keys ->
                promise {
                    let newSet =
                        keys
                        |> Array.map Array.singleton
                        |> Array.choose onFormat
                        |> Set.ofArray

                    let getLocals () =
                        $"keys={keys} newSet={newSet} {getLocals ()}"

                    addTimestamp (fun () -> "[ batchKeysAtom ](y2) setting collectionKeysFamily ") getLocals

                    Atom.set setter (collectionKeysFamily (alias, storeRoot, collection)) newSet
                })

    let inline newHashedDisposable (ticks: TicksGuid) =
        promise {
            let getLocals () = $"ticks={ticks} {getLocals ()}"
            Logger.logDebug (fun () -> $"{nameof FsStore} | Engine.newHashedDisposable / constructor") getLocals

            return
                Object.newDisposable
                    (fun () ->
                        Logger.logDebug
                            (fun () -> $"{nameof FsStore} | Engine.newHashedDisposable / Dispose()")
                            getLocals)
        }

    let inline getAdapterSubscription atomType adapterType =

        let typeMetadata = typeMetadataMap.[atomType]

        let debouncedGunBatchPutFromUi = Js.debounce (batchGunPutFromUi atomType) 0

        let getLocals () =
            $"atomType={atomType} adapterType={adapterType} {getLocals ()}"

        let addTimestamp fn getLocals =
            Profiling.addTimestamp (fun () -> $"{nameof FsStore} | Engine.getAdapterSubscription {fn ()}") getLocals

        let mount, unmount =
            match adapterType with
            | Atom.AdapterType.Gun ->
                (fun storeAtomPath getter setter adapterOptions (adapterSetAtom: Transaction -> unit) ->
                    match adapterOptions with
                    | Atom.AdapterOptions.Gun (_peers, alias) ->
                        let atomPath = storeAtomPath |> StoreAtomPath.AtomPath
                        let getLocals () = $"atomPath={atomPath}  {getLocals ()}"

                        let privateKeys = Atom.get getter Selectors.Gun.privateKeys
                        let gunAtomNode = Atom.get getter (Selectors.Gun.gunAtomNode (Some alias, atomPath))

                        let getLocals () =
                            $" adapterOptions={adapterOptions} {getLocals ()}"

                        addTimestamp (fun () -> "[ ====> mountFn ](j1) gun") getLocals

                        match gunAtomNode, privateKeys with
                        | Some gunAtomNode, Some privateKeys ->
                            let subscriptionTicks = Guid.newTicksGuid ()

                            let mountResult =
                                match storeAtomPath with
                                | ValueAtomPath (_, _, _, name)
                                | RootAtomPath (_, name) ->
                                    addTimestamp
                                        (fun () -> $"[ |--| mount ] invoking indexed subscribe. name={name} ")
                                        getLocals

                                    let subscription =
                                        batchSubscribe
                                            gunAtomNode
                                            subscriptionTicks
                                            (fun (subscriptionTicks, gunValue, key) ->
                                                promise {
                                                    let getLocals () =
                                                        $"gunValue={gunValue} key={key} {getLocals ()}"

                                                    try
                                                        addTimestamp
                                                            (fun () ->
                                                                "[ ||==> Gun.batchSubscribe.on() ](j4-1(1)) will decode. inside gun.on() ")
                                                            getLocals

                                                        let! newValue = typeMetadata.Decode privateKeys gunValue

                                                        let getLocals () = $"newValue={newValue} {getLocals ()}"

                                                        match newValue with
                                                        | Some (ticks, value) ->
                                                            addTimestamp
                                                                (fun () ->
                                                                    "[ ||==> Gun.batchSubscribe.on() ](j4-1(2)) invoking debouncedSetAtom. inside gun.on() ")
                                                                getLocals

                                                            adapterSetAtom (Transaction (NotFromUi, ticks, value))
                                                        | None ->
                                                            addTimestamp
                                                                (fun () ->
                                                                    "[ ||==> Gun.batchSubscribe.on() ](j4-1(3)) skipped set from gun ")
                                                                getLocals
                                                    with
                                                    | ex ->
                                                        let getLocals () =
                                                            $"ex={ex} subscriptionTicks={subscriptionTicks} {getLocals ()}"

                                                        Logger.logError
                                                            (fun () ->
                                                                $"{nameof FsStore} | Engine.getAtomAdapter. gun subscribe data error")
                                                            getLocals
                                                })

                                        newHashedDisposable subscriptionTicks


                                    let inline setAdapterValue (Transaction (fromUi, lastTicks, lastValue)) =
                                        let getLocals () =
                                            $"fromUi={fromUi} lastTicks={lastTicks} lastValue={lastValue} {getLocals ()}"

                                        addTimestamp
                                            (fun () ->
                                                "[ ||==> setAdapterValue ](j4-2) invoking debouncedBatchPutFromUi  ")
                                            getLocals

                                        if fromUi = FromUi then
                                            debouncedGunBatchPutFromUi (
                                                gunAtomNode,
                                                privateKeys,
                                                lastTicks,
                                                lastValue,
                                                (fun () ->
                                                    addTimestamp
                                                        (fun () ->
                                                            "[ ||==> setAdapterValue ](j4) invoking debouncedSetAtom. gun inside setAtom passed to debouncedBatchPutFromUi  ")
                                                        getLocals

                                                    adapterSetAtom (Transaction (NotFromUi, lastTicks, lastValue)))
                                            )

                                    Some (subscription, setAdapterValue)
                                | CollectionAtomPath (storeRoot, collection) ->
                                    addTimestamp (fun () -> "[ |--| mount ] invoking collection subscribe  ") getLocals

                                    let subscription =
                                        batchSubscribe
                                            (gunAtomNode.map ())
                                            subscriptionTicks
                                            (fun (subscriptionTicks, gunValue, rawKey) ->
                                                promise {
                                                    let getLocals () =
                                                        $"rawKey={rawKey} gunValue={gunValue} subscriptionTicks={subscriptionTicks} {getLocals ()}"

                                                    addTimestamp
                                                        (fun () ->
                                                            "[ ||==>X Gun.batchSubscribe.on() ](j4-1) inside gun.map().on() ")
                                                        getLocals

                                                    batchAtomKey
                                                        setter
                                                        atomType
                                                        typeMetadata.OnFormat
                                                        (match gunValue |> Option.ofObjUnbox with
                                                         | Some _ -> BatchKind.UnionItem
                                                         | _ -> BatchKind.RemoveItem)
                                                        (Some alias, storeRoot, collection)
                                                        (NotFromUi, Guid.newTicksGuid (), rawKey)
                                                })

                                        newHashedDisposable subscriptionTicks

                                    let inline setAdapterValue
                                        (Transaction (fromUi, lastTicks, AtomValueRef lastValue))
                                        =
                                        let getLocals () =
                                            $"fromUi={fromUi} lastTicks={lastTicks} lastValue={lastValue} {getLocals ()}"

                                        addTimestamp
                                            (fun () ->
                                                "[ ||==> collection setAdapterValue ](j4-2) calling batchKeysAtom  ")
                                            getLocals

                                        if lastValue <> null then
                                            batchAtomKey
                                                setter
                                                atomType
                                                typeMetadata.OnFormat
                                                BatchKind.UnionItem
                                                (Some alias, storeRoot, collection)
                                                (NotFromUi, Guid.newTicksGuid (), AtomKeyFragment (string lastValue))

                                    Some (subscription, setAdapterValue)
                                | _ ->
                                    addTimestamp (fun () -> "[ mountFn ](j2-2) invalid atom path ") getLocals
                                    None


                            addTimestamp (fun () -> "[ ||||||||| mountFn ](j2) gun. will batch subscribe") getLocals

                            match mountResult with
                            | Some (subscription, setAdapterValues) ->
                                let subscriptionId = SubscriptionId subscriptionTicks
                                Some (subscriptionId, subscription, setAdapterValues)
                            | None -> None

                        | _ -> failwith $"{nameof FsStore} | invalid gun atom node {getLocals ()}"
                    | _ -> None),
                (fun storeAtomPath getter (_setter: Setter<obj>) adapterOptions ->
                    let atomPath = storeAtomPath |> StoreAtomPath.AtomPath

                    let getLocals () = $"atomPath={atomPath} {getLocals ()}"

                    match adapterOptions with
                    | Atom.AdapterOptions.Gun (_peers, alias) ->
                        let gunAtomNode = Atom.get getter (Selectors.Gun.gunAtomNode (Some alias, atomPath))

                        match gunAtomNode with
                        | Some _gunAtomNode ->
                            // TODO: delayed unsub (free after 10 seconds)
//                            match storeAtomPath with
//                            | IndexedAtomPath _
//                            | RootAtomPath _ -> gunAtomNode.off () |> ignore
//                            | CollectionAtomPath _ -> gunAtomNode.map().off () |> ignore
                            ()
                        | _ -> ()

                        let getLocals () =
                            $"adapterOptions={adapterOptions} {getLocals ()}"

                        addTimestamp (fun () -> "[ <==== unmountFn ](j5) gun unmount (# skipped, gun bug? ) ") getLocals

                    | _ -> ())
            | Atom.AdapterType.Hub ->
                (fun storeAtomPath getter setter adapterOptions adapterSetAtom ->
                    match adapterOptions with
                    | Atom.AdapterOptions.Hub (_hubUrl, alias) ->
                        let hub = Atom.get getter Selectors.Hub.hub
                        let privateKeys = Atom.get getter Selectors.Gun.privateKeys

                        match hub, privateKeys with
                        | Some hub, Some privateKeys ->
                            let atomPath =
                                storeAtomPath
                                |> StoreAtomPath.AtomPath
                                |> AtomPath.Value

                            let getLocals () =
                                $" alias={alias} _hubUrl={_hubUrl} adapterOptions={adapterOptions} atomPath={atomPath} {getLocals ()}"

                            addTimestamp (fun () -> $"+09B ====> getAtomAdapter hub mount  {getLocals ()}") getLocals

                            let mountResult =
                                match storeAtomPath with
                                | ValueAtomPath (_, _, _, name)
                                | RootAtomPath (_, name) ->
                                    addTimestamp
                                        (fun () -> $"[ |--| mount ] invoking indexed subscribe. name={name} ")
                                        getLocals

                                    let handle value =
                                        promise {
                                            let! newValue =
                                                match value |> Option.defaultValue null with
                                                | null -> unbox null |> Promise.lift
                                                | result ->
                                                    typeMetadata.Decode privateKeys (EncryptedSignedValue result)

                                            let getLocals () = $"newValue={newValue} {getLocals ()}"

                                            match newValue with
                                            | Some (ticks, value) ->
                                                addTimestamp
                                                    (fun () ->
                                                        "[ ||==> Hub.batchSubscribe.on() ](j4-1(2)) invoking debouncedAdapterSetAtom. inside hub.on() ")
                                                    getLocals

                                                adapterSetAtom (Transaction (NotFromUi, ticks, value))
                                            | None ->
                                                addTimestamp
                                                    (fun () ->
                                                        "[ ||==> Hub.batchSubscribe.on() ](j4-1(3)) skipped set from hub ")
                                                    getLocals
                                        }

                                    Selectors.Hub.hubAtomSubscriptionMap.[(alias, storeAtomPath)] <- handle

                                    let subscription =
                                        promise {
                                            let! subscription =
                                                hubSubscribe
                                                    hub
                                                    (Sync.Request.Get (alias |> Alias.Value, atomPath))
                                                    (fun (msg: Sync.Response) ->
                                                        promise {

                                                            match msg with
                                                            | Sync.Response.GetResult result ->
                                                                let getLocals () =
                                                                    $"msg={msg} atomPath={atomPath} {getLocals ()}"

                                                                Logger.logTrace
                                                                    (fun () ->
                                                                        $"{nameof FsStore} | Store.syncSubscribe. Sync.Response.GetResult")
                                                                    getLocals

                                                                do! handle result
                                                            | _ ->
                                                                let getLocals () = $"msg={msg} {getLocals ()}"

                                                                Logger.logTrace
                                                                    (fun () ->
                                                                        $"{nameof FsStore} | Store.syncSubscribe. wrapper.next() HUB stream subscribe] skipped")
                                                                    getLocals
                                                        }
                                                        |> Promise.start)
                                                    (fun ex ->
                                                        let getLocals () = $"ex={ex} {getLocals ()}"

                                                        Logger.logError
                                                            (fun () ->
                                                                $"{nameof FsStore} | Store.syncSubscribe. onError...")
                                                            getLocals

                                                        Selectors.Hub.hubAtomSubscriptionMap.Remove (
                                                            (alias, storeAtomPath)
                                                        )
                                                        |> ignore)

                                            let getLocals () =
                                                $"subscription={subscription} {getLocals ()}"

                                            addTimestamp (fun () -> "[ Hub.subscribe.on() ] ") getLocals
                                            return subscription
                                        }

                                    let inline setAdapterValue (Transaction (fromUi, lastTicks, lastValue)) =
                                        promise {
                                            let getLocals () =
                                                $"fromUi={fromUi} lastTicks={lastTicks} lastValue={lastValue} {getLocals ()}"

                                            addTimestamp
                                                (fun () ->
                                                    "[ ||==> setAdapterValue ](j4-2) HUB invoking debouncedBatchPutFromUi  ")
                                                getLocals

                                            if fromUi = FromUi then
                                                try
                                                    let! newValueJson =
                                                        promise {
                                                            if lastValue |> Js.ofNonEmptyObj |> Option.isNone then
                                                                return null
                                                            else
                                                                let! (EncryptedSignedValue encrypted) =
                                                                    typeMetadata.Encode
                                                                        privateKeys
                                                                        (lastTicks, lastValue)

                                                                return encrypted
                                                        }

                                                    let! response =
                                                        hub.invokeAsPromise (
                                                            Sync.Request.Set (
                                                                alias |> Alias.Value,
                                                                atomPath,
                                                                newValueJson
                                                            )
                                                        )

                                                    match response with
                                                    | Sync.Response.SetResult result ->

                                                        let getLocals () = $"result={result} {getLocals ()}"

                                                        addTimestamp
                                                            (fun () ->
                                                                "[ ||==> setAdapterValue ](j4-2) HUB . inside setAdapterValue  ")
                                                            getLocals

                                                        if result then
                                                            adapterSetAtom (
                                                                Transaction (NotFromUi, lastTicks, lastValue)
                                                            )
                                                    | response ->
                                                        let getLocals () = $"response={response} {getLocals ()}"

                                                        Logger.logError
                                                            (fun () -> $"{nameof FsStore} | Store.putFromUi. #90592")
                                                            getLocals
                                                with
                                                | ex ->
                                                    let getLocals () = $"ex={ex} {getLocals ()}"

                                                    Logger.logError
                                                        (fun () -> $"{nameof FsStore} | Store.putFromUi. hub.set")
                                                        getLocals
                                        }
                                        |> Promise.start

                                    Some (subscription, setAdapterValue)
                                | CollectionAtomPath (storeRoot, collection) as storeAtomPath ->
                                    addTimestamp (fun () -> "[ |--| mount ] invoking collection subscribe  ") getLocals

                                    let atomPath =
                                        storeAtomPath
                                        |> StoreAtomPath.AtomPath
                                        |> AtomPath.Value

                                    let getLocals () = $"atomPath={atomPath} {getLocals ()}"

                                    let handle (items: string []) =
                                        let lastKeySet =
                                            lastCollectionKeyMap
                                            |> Map.tryFindDictionary storeAtomPath
                                            |> Option.defaultValue Set.empty

                                        let newKeysSet = items |> Array.map AtomKeyFragment |> Set.ofArray

                                        let keysToAddSet =
                                            newKeysSet
                                            |> Set.filter (lastKeySet.Contains >> not)

                                        let keysToRemoveSet =
                                            lastKeySet
                                            |> Set.filter (newKeysSet.Contains >> not)

                                        let addArray =
                                            keysToAddSet
                                            |> Set.toArray
                                            |> Array.map (fun key -> BatchKind.UnionItem, key)

                                        let removeArray =
                                            keysToRemoveSet
                                            |> Set.toArray
                                            |> Array.map (fun key -> BatchKind.RemoveItem, key)

                                        let getLocals () =
                                            $"items={items} lastKeySet={lastKeySet} keysToAddSet={keysToAddSet} keysToRemoveSet={keysToRemoveSet} {getLocals ()}"

                                        addTimestamp
                                            (fun () -> "CollectionAtomPath hub FilterResult stream handle")
                                            getLocals

                                        addArray
                                        |> Array.append removeArray
                                        |> Array.iter
                                            (fun (kind, key) ->
                                                batchAtomKey
                                                    setter
                                                    atomType
                                                    typeMetadata.OnFormat
                                                    kind
                                                    (Some alias, storeRoot, collection)
                                                    (NotFromUi, Guid.newTicksGuid (), key))

                                    Selectors.Hub.hubKeySubscriptionMap.[(alias, storeRoot, collection)] <- handle

                                    let subscription =
                                        hubSubscribe
                                            hub
                                            (Sync.Request.Filter (alias |> Alias.Value, atomPath))
                                            (fun (response: Sync.Response) ->
                                                let getLocals () = $"response={response}  {getLocals ()}"

                                                addTimestamp
                                                    (fun () ->
                                                        "[ ||==>X Hub.batchSubscribe.on() ](j4-1) inside hub.map().on() ")
                                                    getLocals

                                                match response with
                                                | Sync.Response.FilterResult keys -> handle keys
                                                | response ->
                                                    let getLocals () = $"response={response} {getLocals ()}"

                                                    Logger.logError
                                                        (fun () ->
                                                            $"{nameof FsStore} | Store.selectAtomSyncKeys Gun.batchHubSubscribe invalid")
                                                        getLocals)
                                            (fun ex ->
                                                let getLocals () = $"ex={ex} {getLocals ()}"

                                                Logger.logError
                                                    (fun () -> $"{nameof FsStore} | hub.map().on() error")
                                                    getLocals

                                                Selectors.Hub.hubKeySubscriptionMap.Remove (
                                                    (alias, storeRoot, collection)
                                                )
                                                |> ignore)

                                    let inline setAdapterValue
                                        (Transaction (fromUi, lastTicks, AtomValueRef lastValue))
                                        =
                                        let getLocals () =
                                            $"fromUi={fromUi} lastTicks={lastTicks} lastValue={lastValue} {getLocals ()}"


                                        failwith $"{nameof FsStore} | invalid adapter assign {getLocals ()}"

                                    //                                        addTimestamp
//                                            (fun () ->
//                                                "[ ||==> collection setAdapterValue ](j4-2) calling batchKeysAtom  ")
//                                            getLocals
//
//                                        batchAtomKey
//                                            setter
//                                            atomType
//                                            typeMetadata.OnFormat
//                                            BatchKind.Union
//                                            (Some alias, storeRoot, collection)
//                                            (NotFromUi, Guid.newTicksGuid (), AtomKeyFragment (string lastValue))

                                    Some (subscription, setAdapterValue)
                                | _ ->
                                    addTimestamp (fun () -> "[ mountFn ](j2-2) invalid atom path ") getLocals
                                    None

                            addTimestamp (fun () -> "[ ||||||||| mountFn ](j2) hub. will batch subscribe") getLocals

                            match mountResult with
                            | Some (subscription, setAdapterValues) ->
                                let subscriptionTicks = Guid.newTicksGuid ()
                                let subscriptionId = SubscriptionId subscriptionTicks
                                Some (subscriptionId, subscription, setAdapterValues)
                            | None -> None
                        | _ -> None
                    | _ -> None),
                (fun _storeAtomPath _getter _setter adapterOptions ->
                    match adapterOptions with
                    | Atom.AdapterOptions.Hub (_alias, _hubUrl) ->
                        //                let gunAtomNode = Atom.get getter (Selectors.Gun.gunAtomNode (alias, atomPath))
                        addTimestamp (fun () -> $"+08B <==== getAtomAdapter hub unmount") getLocals
                    | _ -> ()

                    )
            | Atom.AdapterType.Memory ->
                (fun _storeAtomPath _getter _setter adapterOptions _setValue ->
                    match adapterOptions with
                    | Atom.AdapterOptions.Memory ->
                        //                let gunAtomNode = Atom.get getter (Selectors.Gun.gunAtomNode (alias, atomPath))

                        let getLocals () =
                            $"adapterOptions={adapterOptions} {getLocals ()}"

                        addTimestamp (fun () -> "[ ====> mountFn ](j6) memory mount ") getLocals

                        let subscriptionTicks = Guid.newTicksGuid ()
                        let subscriptionId = SubscriptionId subscriptionTicks
                        let subscription = newHashedDisposable subscriptionTicks

                        Some (
                            subscriptionId,
                            subscription,
                            (fun (Transaction (_fromUi, _lastTicks, _lastValue)) ->
                                let getLocals () =
                                    $"_lastTicks={_lastTicks} _lastValue={_lastValue} {getLocals ()}"

                                addTimestamp
                                    (fun () -> "[ ¨¨ setAdapterValue ](j8) memory inside debouncedPutFromUi setAtom ")
                                    getLocals)
                        )
                    | _ -> None),
                (fun _storeAtomPath _getter _setter adapterOptions ->
                    match adapterOptions with
                    | Atom.AdapterOptions.Memory ->
                        //                let gunAtomNode = Atom.get getter (Selectors.Gun.gunAtomNode (alias, atomPath))

                        let getLocals () =
                            $"adapterOptions={adapterOptions} {getLocals ()}"

                        addTimestamp (fun () -> "[ <==== unmountFn ](j7) memory unmount ") getLocals
                    | _ -> ())

        mount, unmount



    type AtomId = AtomId of adapterType: Atom.AdapterType * alias: Gun.Alias * storeAtomPath: StoreAtomPath

    let gunSubscriptionMap = Dictionary<AtomId, (SubscriptionId * IDisposable * (Transaction -> unit)) option> ()
    //    let collectionSubscriptionMap = Dictionary<StoreRoot * Collection, unit -> unit> ()


    let inline createAtomWithAdapter
        (AtomId (adapterType, alias, storeAtomPath) as atomId)
        (mount: Getter<obj>
                    -> (AtomConfig<obj> -> obj -> unit)
                    -> Atom.AdapterOptions
                    -> (Transaction -> unit)
                    -> (SubscriptionId * JS.Promise<IDisposable> * (Transaction -> unit)) option)
        (unmount: Getter<obj> -> (AtomConfig<obj> -> obj -> unit) -> Atom.AdapterOptions -> unit)
        : AtomConfig<Transaction option> =
        let atom = Atom.Primitives.create (AtomType.Atom None)

        let mutable setAdapterValue: (Transaction -> unit) option = None

        let getLocals () =
            $"storeAtomPath={storeAtomPath} alias={alias} adapterType={adapterType} atom={atom} setAdapterValue.IsSome={setAdapterValue.IsSome}  {getLocals ()}"

        let addTimestamp fn getLocals =
            Profiling.addTimestamp (fun () -> $"{nameof FsStore} | Engine.createAtomWithAdapter {fn ()}") getLocals

        addTimestamp (fun () -> "[ constructor ](f1)") getLocals

        Atom.Primitives.selector
            (fun getter ->
                let result = Atom.get getter atom
                let getLocals () = $"result={result} {getLocals ()}"
                addTimestamp (fun () -> "[ wrapper.get() ](f2)") getLocals
                result)
            (fun _ setter newValue ->
                match newValue with
                | Some (Transaction (newFromUi, newTicks, newValue)) ->
                    Atom.change
                        setter
                        atom
                        (fun oldValue ->
                            let getLocals () =
                                $"oldValue={oldValue} newTicks={newTicks} newValue={newValue} newFromUi={newFromUi} {getLocals ()}"

                            match setAdapterValue with
                            | Some setAdapterValue when
                                (match oldValue with
                                 | None when newFromUi = FromUi -> true
                                 | None -> false
                                 | Some (Transaction (_, _, oldValue)) when oldValue |> Object.compare newValue |> not ->
                                     true
                                 | _ -> false)
                                ->
                                addTimestamp
                                    (fun () -> "[ (^^^^2) wrapper.set() ](f3) triggering new adapter value")
                                    getLocals

                                // gunPut
                                setAdapterValue (Transaction (newFromUi, newTicks, newValue))
                            | _ ->
                                addTimestamp (fun () -> "[ wrapper.set() ](f3-1) skipping new adapter assign") getLocals

                            Some (Transaction (newFromUi, newTicks, newValue)))
                | None -> failwith $"{nameof FsStore} | invalid newValue {getLocals ()}")
        |> wrapAtomWithState
            (fun getter ->
                let adapterOptions = getAdapterOptions getter adapterType

                let getLocals () =
                    $"adapterOptions={Json.encodeWithNull adapterOptions} {getLocals ()}"

                addTimestamp (fun () -> "[ state.read() ](f5)") getLocals

                match adapterOptions with
                | Some adapterOptions -> Some adapterOptions
                | None -> None)
            (fun getter setter adapterOptions setAtom ->
                promise {
                    //                let gunAtomNode = Atom.get getter (Selectors.Gun.gunAtomNode (alias, atomPath))

                    let getLocals () =
                        $"adapterOptions={Json.encodeWithNull adapterOptions} {getLocals ()}"

                    addTimestamp (fun () -> "[ @@> mount ](f6)") getLocals

                    if setAdapterValue.IsNone then
                        let adapterSetAtom (Transaction (fromUi, ticksGuid, value)) =
                            let getLocals () =
                                $"fromUi={fromUi} ticksGuid={ticksGuid} value={value} {getLocals ()}"

                            addTimestamp
                                (fun () -> "[ setAdapterValue / setAtom ](f6+1) after debounced put?")
                                getLocals

                            if setAdapterValue.IsNone then
                                addTimestamp
                                    (fun () ->
                                        "[ setAdapterValue / setAtom ](f6+2) skipping assign from adapter. unmounted ")
                                    getLocals
                            else
                                setAtom (Some (Transaction (fromUi, ticksGuid, value)))

                        let debouncedAdapterSetAtom =
                            Js.debounce
                                (fun value ->
                                    let getLocals () = $"value={value} {getLocals ()}"

                                    addTimestamp
                                        (fun () ->
                                            "[ ********> mountFn / debouncedAdapterSetAtom ](j3) gun. debounced on subscribe data")
                                        getLocals

                                    adapterSetAtom value)
                                0


                        let mountResult = mount getter setter adapterOptions debouncedAdapterSetAtom

                        match mountResult with
                        | Some (subscriptionId, subscription, setAdapterValues) ->
                            setAdapterValue <- Some setAdapterValues

                            let! subscription = subscription
                            gunSubscriptionMap.[atomId] <- Some (subscriptionId, subscription, setAdapterValues)
                        | _ ->
                            gunSubscriptionMap.Remove atomId |> ignore
                            setAdapterValue <- None

                    addTimestamp (fun () -> "[ after mount ](f6)") getLocals

                })
            (fun getter setter adapterOptions ->
                let getLocals () =
                    $"adapterOptions={Json.encodeWithNull adapterOptions} {getLocals ()}"

                addTimestamp (fun () -> "[ <@@ unmount ](f7)") getLocals

                if setAdapterValue.IsSome then
                    unmount getter setter adapterOptions
                    gunSubscriptionMap.Remove atomId |> ignore
                    setAdapterValue <- None

                let getLocals () = $" {getLocals ()}"

                addTimestamp (fun () -> "[ after unmount ](f7)") getLocals)

    let adapterAtomMap = Dictionary<AtomId, AtomConfig<Transaction option>> ()

    let inline getAdapterValues atomType getter storeAtomPath =
        let alias = Atom.get getter Selectors.Gun.alias

        let atomPath = storeAtomPath |> StoreAtomPath.AtomPath

        let getLocals () =
            $"atomType={atomType} alias={alias} atomPath={atomPath} {getLocals ()}"

        let addTimestamp fn getLocals =
            Profiling.addTimestamp (fun () -> $"{nameof FsStore} | Engine.getAdapterValues {fn ()}") getLocals

        Reflection.unionCases<Atom.AdapterType>
        |> List.choose
            (fun adapterType ->
                match alias, getAdapterOptions getter adapterType with
                | Some alias, Some adapterOptions ->
                    let getLocals () =
                        $"adapterType={adapterType} adapterOptions={adapterOptions}  {getLocals ()}"

                    let atomId = AtomId (adapterType, alias, storeAtomPath)

                    let mount, unmount = getAdapterSubscription atomType adapterType

                    let adapterAtom =
                        match adapterAtomMap |> Map.tryFindDictionary atomId with
                        | Some value ->
                            addTimestamp (fun () -> "[ getAdapterValues ](a4) returning cached adapter") getLocals
                            value
                        | None ->
                            let mount, unmount = mount storeAtomPath, unmount storeAtomPath
                            let newAtom = createAtomWithAdapter atomId mount unmount

                            addTimestamp
                                (fun () -> "[ getAdapterValues ](a4) returning newly created adapter")
                                getLocals

                            adapterAtomMap.Add (atomId, newAtom)

                            newAtom

                    let adapterValue = Atom.get getter adapterAtom

                    let getLocals () =
                        $"adapterType={adapterType} adapterAtom={adapterAtom} adapterOptions={adapterOptions} adapterValue={adapterValue} {getLocals ()}"

                    addTimestamp (fun () -> "[ getAdapterValues ](a4) returning valid adapter") getLocals
                    Some (adapterType, adapterOptions, adapterAtom, adapterValue)
                | _ -> None)


    let inline subscribeCollection<'TKey, 'A4 when 'TKey: equality and 'A4: equality and 'TKey :> IComparable>
        storeRoot
        collection
        (onFormat: AtomKeyFragment [] -> 'TKey option)
        //        (_onFormat: string -> 'TKey)
        =
        let collectionAtomType = typeof<'TKey []>
        let atomType = DataType.Key, collectionAtomType

        let storeAtomPath = CollectionAtomPath (storeRoot, collection)

        let getLocals () =
            $"atomType={atomType} atomPath={storeAtomPath |> StoreAtomPath.AtomPath} {getLocals ()}"

        let addTimestamp fn getLocals =
            Profiling.addTimestamp (fun () -> $"{nameof FsStore} | Engine.subscribeCollection {fn ()}") getLocals

        if typeMetadataMap.ContainsKey atomType |> not then
            collectionTypeMap.[(storeRoot, collection)] <- collectionAtomType

            typeMetadataMap.[atomType] <-
                {|
                    DefaultValue = AtomValueRef (([||]: 'TKey []) |> unbox<IComparable>)
                    OnFormat =
                        (fun keys ->
                            let result =
                                keys
                                |> Array.map Array.singleton
                                |> Array.choose onFormat
                                |> function
                                    | [| key |] -> Some (KeyRef key)
                                    | _ -> None

                            let getLocals () =
                                $"keys={keys} result={result} {getLocals ()}"

                            addTimestamp (fun () -> "[ OnFormat ]") getLocals
                            result)
                    Decode = unbox null
                    Encode = unbox null
                |}

        addTimestamp (fun () -> "[ constructor ](z1)") getLocals

        let inline sync _adapterValues =
            match lastStore with
            | Some (getter, _setter) ->
                let adapterValues = getAdapterValues atomType getter storeAtomPath

                let getLocals () =
                    $"adapterValues={adapterValues} {getLocals ()}"

                addTimestamp (fun () -> "[ sync ](z4)") getLocals
            | _ -> ()

        let debouncedSync =
            Js.debounce
                (fun adapterValues ->
                    addTimestamp (fun () -> "[ debouncedSync ](z2)") getLocals
                    sync adapterValues)
                0

        let inline refreshAdapterValues getter =
            let adapterValues = getAdapterValues atomType getter storeAtomPath

            let getLocals () =
                $"adapterValues={adapterValues} {getLocals ()}"

            addTimestamp (fun () -> "[ refreshAdapterValues ](z3)") getLocals

            debouncedSync adapterValues

        Atom.readSelector
            storeAtomPath
            (fun getter ->
                refreshAdapterValues getter
                let alias = Atom.get getter Selectors.Gun.alias
                let collectionKeys = Atom.get getter (collectionKeysFamily (alias, storeRoot, collection))

                let getLocals () =
                    $"alias={alias} collectionKeys={collectionKeys} {getLocals ()}"

                addTimestamp (fun () -> "[ wrapper.get() ](z3) ") getLocals

                collectionKeys
                |> Set.toArray
                |> Array.map (fun (KeyRef key) -> key |> unbox<'TKey>))
        |> Atom.split

    let inline groupValues groupMap =
        groupMap
        |> Seq.groupBy fst
        |> Seq.map
            (fun (group, values) ->
                let newItem =
                    values
                    |> Seq.sortByDescending fst
                    |> Seq.map snd
                    |> Seq.head

                group, newItem)
        |> Seq.sortByDescending (fun (_, (ticks, _)) -> ticks)
        |> Seq.toList


    let groupMapFamily: (GroupRef * Gun.Alias option * StoreAtomPath -> AtomConfig<(GroupRef * (TicksGuid * AtomValueRef)) list>) =
        Atom.atomFamilyAtom
            (fun (groupRef: GroupRef, alias: Gun.Alias option, storeAtomPath: StoreAtomPath) ->
                let getLocals () =
                    $"groupRef={groupRef} alias={alias} storeAtomPath={storeAtomPath |> StoreAtomPath.AtomPath} {getLocals ()}"

                let addTimestamp fn getLocals =
                    Profiling.addTimestamp (fun () -> $"{nameof FsStore} | Engine.groupMapFamily {fn ()}") getLocals

                addTimestamp (fun () -> "[ constructor ](e3)") getLocals
                [])


    let userAtomFamily =
        Atom.Primitives.readSelectorFamily
            (fun (groupRef: GroupRef, alias: Gun.Alias option, storeAtomPath: StoreAtomPath) getter ->
                let groupMap = Atom.get getter (groupMapFamily (groupRef, alias, storeAtomPath))

                let getLocals () =
                    $"groupRef={groupRef} alias={alias} storeAtomPath={storeAtomPath |> StoreAtomPath.AtomPath} {getLocals ()}"

                let addTimestamp fn getLocals =
                    Profiling.addTimestamp (fun () -> $"{nameof FsStore} | Engine.userAtomFamily {fn ()}") getLocals

                addTimestamp (fun () -> "[ readSelectorFamily.read() ](e2)") getLocals

                match groupMap with
                | [] -> []
                | _ -> groupMap |> groupValues)

    let inline createAtomWithGroup<'TGroup, 'A6 when 'A6: equality and 'TGroup :> IComparable and 'A6 :> IComparable>
        (storeAtomPath: StoreAtomPath)
        (defaultGroup: 'TGroup, defaultValue: 'A6)
        : AtomConfig<('TGroup * (TicksGuid * 'A6)) list> =

        let getLocals () =
            $"atomPath={storeAtomPath |> StoreAtomPath.AtomPath} defaultGroup={defaultGroup} defaultValue={defaultValue} {getLocals ()}"

        let addTimestamp fn getLocals =
            Profiling.addTimestamp (fun () -> $"{nameof FsStore} | Engine.createAtomWithGroup {fn ()}") getLocals

        addTimestamp (fun () -> "[ constructor ](d1)") getLocals

        let wrapper =
            Atom.Primitives.selector
                (fun getter ->
                    let alias = Atom.get getter Selectors.Gun.alias
                    let userAtom = userAtomFamily (GroupRef (unbox defaultGroup), alias, storeAtomPath)
                    let lastSyncValueByType = Atom.get getter userAtom

                    let result =
                        lastSyncValueByType
                        |> Seq.sortByDescending (fun (_, (ticks, _)) -> ticks)
                        |> Seq.toList

                    let filteredResult =
                        result
                        |> List.filter
                            (fun (_, (group', _)) ->
                                result.Length = 1
                                || group' <> (result |> List.head |> snd |> fst))
                    //                        |> unbox<(TicksGuid * ('TGroup * 'A)) list>


                    let finalResult =
                        filteredResult
                        |> List.map
                            (fun (groupRef, (ticks, atomValueRef: AtomValueRef)) ->
                                groupRef |> unbox<'TGroup>, (ticks, atomValueRef |> unbox<'A6>))
                        |> function
                            | [] ->
                                [
                                    defaultGroup, (Guid.Empty, defaultValue)
                                ]
                            | result -> result

                    let getLocals () =
                        $"alias={alias} userAtom={userAtom} result={result} filteredResult={Json.encodeWithNull filteredResult} finalResult={finalResult} {getLocals ()} "

                    addTimestamp (fun () -> "[ wrapper.get() ](d2)") getLocals

                    finalResult)
                (fun getter setter (newValue: ('TGroup * (TicksGuid * 'A6)) list) ->
                    let alias = Atom.get getter Selectors.Gun.alias
                    let userAtom = groupMapFamily (GroupRef defaultGroup, alias, storeAtomPath)

                    let newValue =
                        newValue
                        |> List.map (fun (group, (ticks, value: 'A6)) -> GroupRef group, (ticks, AtomValueRef value))

                    let getLocals () =
                        $"newValue={newValue} alias={alias} userAtom={userAtom} {getLocals ()} "

                    addTimestamp (fun () -> "[ wrapper.set() ](d3)") getLocals

                    Atom.set setter userAtom newValue)
            |> Atom.register storeAtomPath

        wrapper?init <- defaultValue

        wrapper

    let inline sync
        (adapterValues: (Atom.AdapterType * Atom.AdapterOptions * AtomConfig<Transaction option> * Transaction option) list)
        (atom: AtomConfig<(Atom.AdapterType * Transaction) list>)
        : unit =
        match lastStore with
        | Some (getter, setter) ->
            let getLocals () = $"atom={atom} {getLocals ()}"

            let addTimestamp fn getLocals =
                Profiling.addTimestamp (fun () -> $"{nameof FsStore} | Engine.sync {fn ()}") getLocals

            let localAdapters = Atom.get getter atom

            let lastAdapterType, Transaction (lastFromUi, lastTicks, lastValue) = localAdapters |> List.head

            let inline setAdapterAtom adapterAtom =
                fun (Transaction (fromUi, ticks, newValue)) ->
                    let getLocals () =
                        $"adapterAtom={adapterAtom} fromUi={fromUi} ticks={ticks} newValue={newValue} {getLocals ()} "

                    addTimestamp (fun () -> "[ (:::::) setAdapterAtom ](c1) ") getLocals

                    Atom.set setter adapterAtom (Some (Transaction (fromUi, ticks, newValue)))

            let values =
                adapterValues
                |> List.map
                    (function
                    | adapterType, _adapterOptions, adapterAtom, Some (Transaction (fromUi, adapterTicks, adapterValue)) ->
                        setAdapterAtom adapterAtom, adapterType, Some fromUi, Some adapterTicks, Some adapterValue
                    | adapterType, _adapterOptions, adapterAtom, None ->
                        setAdapterAtom adapterAtom, adapterType, None, None, None)

            let getLocals () =
                $" lastAdapterType={lastAdapterType} lastFromUi={lastFromUi} lastTicks={lastTicks} lastValue={lastValue} == [ localAdapters={Json.encodeWithNull localAdapters} ] {getLocals ()} "


            let inline setUiAtom (Transaction (fromUi, ticks, newValue)) =
                addTimestamp (fun () -> "[ (:::::) lastAdapter.write() ](c2) invoking Atom.change ") getLocals

                Atom.change
                    setter
                    atom
                    (fun oldValue ->
                        let getLocals () =
                            $"lastAdapterType={lastAdapterType} oldValue={oldValue} ticks={ticks} newValue={newValue} {getLocals ()} "

                        addTimestamp (fun () -> "[ (:::::) lastAdapter.write() ](c2) inside Atom.change ") getLocals

                        (lastAdapterType, (Transaction (fromUi, ticks, newValue)))
                        :: (oldValue
                            |> List.filter (fun (adapterType, _) -> adapterType <> lastAdapterType)))

            let newValues =
                values
                |> List.append [
                    setUiAtom, lastAdapterType, Some lastFromUi, Some lastTicks, Some lastValue
                   ]
                |> List.sortByDescending (fun (_, _, _, ticks, _) -> ticks)

            let valuesfmt =
                Json.encodeWithNull (
                    newValues
                    |> List.map
                        (fun (_setAdapterAtom, adapterType, fromUi, ticksGuid, value) ->
                            adapterType, fromUi, ticksGuid, value)
                )

            let getLocals () = $"  {getLocals ()} values={valuesfmt}"

            let _lastAdapterSetAtom, lastAdapterType, lastFromUi, lastTicks, lastValue = newValues.Head
            //                | _ -> ()
//            let lastAdapterSetAtom, lastAdapterType, (lastFromUi, lastTicks, lastValue) = newValues.Head

            addTimestamp (fun () -> "[ sync ](c1-1) init. from debounce. will fan out") getLocals

            newValues
            |> List.skip 1
            |> List.map
                (function
                | setAdapterAtom, adapterType, fromUi, ticks, value when value |> Object.compare lastValue |> not ->
                    promise {
                        let getLocals () =
                            $" adapterFromUi={fromUi} lastFromUi={lastFromUi} adapterType={adapterType} lastAdapterType={lastAdapterType} lastTicks={lastTicks} ticks={ticks} lastValue={lastValue} value={value} {getLocals ()} "

                        match lastFromUi, lastTicks, lastValue with
                        | Some lastFromUi, Some lastTicks, Some lastValue when ticks.IsNone || lastTicks > ticks.Value ->
                            // set adapter value from local atom
                            addTimestamp (fun () -> "[ (%%%%) invalidAdapter.write() ](c4)") getLocals

                            let newLastFromUi =
                                match lastFromUi with
                                | NotFromUi when
                                    adapterType <> Atom.AdapterType.Memory
                                    && lastAdapterType <> Atom.AdapterType.Memory
                                    ->
                                    FromUi
                                | _ -> lastFromUi

                            setAdapterAtom (Transaction (newLastFromUi, lastTicks, lastValue))
                        | _ ->
                            addTimestamp
                                (fun () -> "[ (%%%%) invalidAdapter.write() ](c3) same ticks. skipping")
                                getLocals
                    }
                | _ -> Promise.lift ())
            |> List.toArray
            |> Promise.all
            |> Promise.ignore
            |> Promise.start
        | _ -> ()

    let inline createAtomWithSubscription<'A8 when 'A8: equality and 'A8 :> IComparable>
        storeAtomPath
        (defaultValue: 'A8)
        : AtomConfig<'A8> =
        let atomType = DataType.Data, typeof<'A8>

        if typeMetadataMap.ContainsKey atomType |> not then
            typeMetadataMap.[atomType] <-
                {|
                    DefaultValue = AtomValueRef defaultValue
                    Decode = unbox userDecode<TicksGuid * 'A8>
                    Encode = unbox userEncode<TicksGuid * 'A8>
                    OnFormat = unbox null
                |}

        let atomPath = storeAtomPath |> StoreAtomPath.AtomPath

        let getLocals () =
            $"atomType={atomType} atomPath={atomPath} defaultValue={defaultValue} {getLocals ()}"

        let addTimestamp fn getLocals =
            Profiling.addTimestamp (fun () -> $"{nameof FsStore} | Engine.createAtomWithSubscription {fn ()}") getLocals

        let localAdaptersAtom =
            createAtomWithGroup storeAtomPath (Atom.AdapterType.Memory, (NotFromUi, defaultValue))
            |> Atom.map
                (fun _getter value ->
                    let result =
                        value
                        |> List.map
                            (fun (adapterType, (ticksGuid, (fromUi, value: 'A8))) ->
                                adapterType, (Transaction (fromUi, ticksGuid, AtomValueRef value)))

                    let getLocals () =
                        $"value={value} result={result} {getLocals ()}"

                    addTimestamp (fun () -> "[ <((A))> wrapper.get() ](a6)") getLocals
                    result)
                (fun _getter _setter (newValue: (Atom.AdapterType * Transaction) list) ->
                    let newValue =
                        newValue
                        |> List.map
                            (fun (adapterType, Transaction (ticksGuid, fromUi, value)) ->
                                adapterType, (fromUi, (ticksGuid, value |> unbox<'A8>)))

                    let getLocals () = $"newValue={newValue} {getLocals ()}"

                    addTimestamp (fun () -> "[ <((A))> wrapper.set() ](a6)") getLocals
                    newValue)

        let getLocals () =
            $"localAdaptersAtom={localAdaptersAtom} {getLocals ()}"

        addTimestamp (fun () -> "[ constructor ](a1)") getLocals

        let debouncedSync =
            Js.debounce
                (fun adapterValues ->
                    addTimestamp (fun () -> "[ debouncedSync ](a2)") getLocals
                    sync adapterValues localAdaptersAtom)
                0

        let inline refreshAdapterValues getter =
            let adapterValues = getAdapterValues atomType getter storeAtomPath

            let getLocals () =
                $"adapterValues={adapterValues} {getLocals ()}"

            addTimestamp (fun () -> "[ refreshAdapterValues ](a3)") getLocals
            debouncedSync adapterValues

        let rec wrapper =
            Atom.Primitives.selector
                (fun getter ->
                    refreshAdapterValues getter
                    let localAdapters = Atom.get getter localAdaptersAtom
                    let _adapterType, Transaction (_fromUi, _ticks, value) = localAdapters |> List.head


                    let getLocals () =
                        $"_ticks={_ticks} _adapterType={_adapterType} value={value} {getLocals ()}"

                    addTimestamp (fun () -> "[ wrapper.get() ](a5)") getLocals

                    value |> unbox<'A8>)
                (fun getter setter (newValue: 'A8) ->
                    //                    refreshAdapterValues getter

                    let getLocals () = $"newValue={newValue} {getLocals ()}"

                    addTimestamp (fun () -> "[ <&¨¨&> wrapper.set() ](a6)") getLocals

                    Atom.change
                        setter
                        localAdaptersAtom
                        (fun localAdapters ->
                            let newItem =
                                Atom.AdapterType.Memory,
                                Transaction (FromUi, Guid.newTicksGuid (), AtomValueRef newValue)

                            let getLocals () =
                                $"localAdapters={Json.encodeWithNull localAdapters} newItem={newItem} {getLocals ()}"

                            addTimestamp (fun () -> "[ <&¨<->¨&> wrapper.set() ](a6)") getLocals

                            localAdapters
                            |> List.filter (fun (adapterType, _) -> adapterType <> Atom.AdapterType.Memory)
                            |> List.append (newItem |> List.singleton))


                    let alias = Atom.get getter Selectors.Gun.alias
                    let collectionPath = storeAtomPath |> StoreAtomPath.CollectionPath

                    let getLocals () =
                        $"collectionPath={collectionPath} {getLocals ()}"

                    match collectionPath with
                    | Some (storeRoot, collection) when
                        collectionTypeMap.ContainsKey ((storeRoot, collection))
                        && newValue <> unbox null
                        ->
                        let collectionAtomType = collectionTypeMap.[(storeRoot, collection)]
                        let collectionType = DataType.Key, collectionAtomType

                        if typeMetadataMap.ContainsKey collectionType then
                            let typeMetadata = typeMetadataMap.[collectionType]

                            let key =
                                storeAtomPath
                                |> StoreAtomPath.Keys
                                |> Option.map Array.toList

                            let getLocals () = $"key={key} {getLocals ()}"

                            match key with
                            | Some (key :: _) ->
                                addTimestamp
                                    (fun () -> "[ updateKey ](a6-1) saving key. invoking collection Atom.change ")
                                    getLocals

                                batchAtomKey
                                    setter
                                    collectionType
                                    typeMetadata.OnFormat
                                    BatchKind.UnionItem
                                    (alias, storeRoot, collection)
                                    (NotFromUi, Guid.newTicksGuid (), key)
                            | _ ->
                                addTimestamp
                                    (fun () -> "[ updateKey ](a6-1) skipping key. skipping Atom.change ")
                                    getLocals
                        else
                            addTimestamp
                                (fun () -> "[ updateKey ](a6-3) skipping key2. skipping Atom.change ")
                                getLocals
                    | _ -> addTimestamp (fun () -> "[ updateKey ](a6-2) skipping collection Atom.change ") getLocals)

        wrapper?init <- defaultValue

        wrapper |> Atom.register storeAtomPath

    let inline createFamilyWithSubscription storeRoot collection name defaultValueFn formatFn =
        Atom.Primitives.atomFamily
            (fun param ->
                createAtomWithSubscription
                    (StoreAtomPath.ValueAtomPath (storeRoot, collection, formatFn param, AtomName name))
                    (defaultValueFn param))

    let inline bindAtom<'A9 when 'A9: equality> atom1 (atom2: Jotai.AtomConfig<_>) =
        let mutable lastSetAtom: ('A9 option -> unit) option = None
        let mutable lastValue = None

        let getLocals () =
            $"atom1={atom1} atom2={atom2} lastValue={lastValue} {getLocals ()}"

        let addTimestamp fn getLocals =
            Profiling.addTimestamp (fun () -> $"{nameof FsStore} | Engine.bindAtom {fn ()}") getLocals

        addTimestamp (fun () -> "[ constructor ](b1)") getLocals

        atom1
        |> Atom.wrap
            (fun getter ->
                match Atom.get getter atom1, Atom.get getter atom2 with
                | value1, value2 when
                    value1
                    |> Object.compare (atom1.init |> Option.defaultValue (unbox null))
                    && (value2
                        |> Object.compare (atom2.init |> Option.defaultValue (unbox null))
                        || lastValue.IsNone
                        || (Atom.get getter Selectors.Gun.alias).IsNone)
                    ->
                    let getLocals () =
                        $"value1={value1} value2={value2} {getLocals ()}"

                    addTimestamp (fun () -> "[ wrapper.get() ](b2) choosing value2") getLocals
                    value2
                | value1, value2 ->
                    let getLocals () =
                        $"value1={value1} value2={value2} {getLocals ()}"

                    match lastSetAtom with
                    | Some lastSetAtom when
                        lastValue.IsNone
                        || lastValue |> Object.compare (Some value1) |> not
                        ->
                        addTimestamp (fun () -> "[ wrapper.get() ](b3) different. triggering additional") getLocals

                        lastValue <- Some value1
                        lastSetAtom (Some value1)
                    | _ -> ()

                    addTimestamp (fun () -> "[ wrapper.get() ](b4) choosing value1") getLocals

                    value1)
            (fun _get setter newValue ->
                let getLocals () = $"newValue={newValue} {getLocals ()}"

                if lastValue.IsNone
                   || lastValue |> Object.compare (Some newValue) |> not then
                    lastValue <- Some newValue
                    Atom.set setter atom1 newValue

                    addTimestamp (fun () -> "[ wrapper.set() ](b5) setting atom1 and atom2") getLocals
                else
                    addTimestamp (fun () -> "[ wrapper.set() ](b6) setting atom2 only") getLocals

                Atom.set setter atom2 newValue)


    let inline createAtomWithSubscriptionStorage storeAtomPath (defaultValue: 'A10) =
        let storageAtom = Atom.createWithStorage<'A10> storeAtomPath defaultValue
        let syncAtom = createAtomWithSubscription storeAtomPath defaultValue
        bindAtom<'A10> syncAtom storageAtom

    let inline getKeysFormatter fn id =
        id
        |> Option.ofObjUnbox
        |> Option.map fn
        |> Option.defaultValue []

    let inline parseGuidKey fn keys =
        match keys |> Array.toList with
        | AtomKeyFragment guid :: _ ->
            match Guid.TryParse guid with
            | true, guid -> Some (fn guid)
            | _ -> None
        | _ -> None

    let inline delete getter storeAtomPath =
        promise {
            let alias = Atom.get getter Selectors.Gun.alias
            let atomPath = storeAtomPath |> StoreAtomPath.AtomPath
            let gunAtomNode = Atom.get getter (Selectors.Gun.gunAtomNode (alias, atomPath))

            let getLocals () = ""

            let getLocals () =
                $"alias={alias} atomPath={atomPath} {getLocals ()}"

            match gunAtomNode with
            | Some gunAtomNode ->
                let! putResult = put gunAtomNode (unbox null)
                let getLocals () = $"putResult={putResult} {getLocals ()}"
                Logger.logDebug (fun () -> $"{nameof FsStore} | Engine.delete") getLocals
            | None -> failwith $"{nameof FsStore} | Engine.delete. invalid gun atom node"

            match alias with
            | Some (Alias alias) ->
                let hub = Atom.get getter Selectors.Hub.hub

                match hub with
                | Some hub -> do! hub.sendAsPromise (Sync.Request.Set (alias, atomPath |> AtomPath.Value, null))
                | _ -> Logger.logDebug (fun () -> $"{nameof FsStore} | Engine.delete. invalid hub. skipping") getLocals
            | _ -> failwith $"{nameof FsStore} | Engine.delete. invalid alias"
        }
