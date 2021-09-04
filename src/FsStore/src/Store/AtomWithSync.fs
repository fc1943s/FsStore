namespace FsStore.Store

open FsStore
open FsJs

#nowarn "40"


module AtomWithSync =
    module Store =
        //        let inline atomWithReducer<'TKey, 'TValue> atomkey (defaultValue: 'TValue) = ()

        let inline atomWithSync<'TKey, 'TValue  when 'TValue: equality> storeAtomPath (defaultValue: 'TValue) =

            let referenceAtom = Atom.Primitives.atom defaultValue

            let atomWithSubscription =
                AtomWithSubscription.atomWithSubscription
                    storeAtomPath
                    defaultValue
                    (fun () ->
                        promise {
                        Profiling.addTimestamp (fun () -> $"@ atomWithSync subscribe {storeAtomPath}")
                        }
                        )
                    (fun () ->
                        Profiling.addTimestamp (fun () -> $"@ atomWithSync unsubscribe {storeAtomPath}")
                        )
                    referenceAtom

            atomWithSubscription
        //


//            let mutable lastUserAtomId = None
//
//            let syncEngine = Store.SyncEngine (defaultValue, None)
//            let syncState = SyncState<'TValue> ()
//            let atomPath = atomKey |> AtomKey.AtomPath
//
//            let getLocals () =
//                $"""
//    | atomWithSync debugInfo:
//    syncState={Json.encodeWithNull syncState}
//    syncEngine={Json.encodeWithNull syncEngine}
//    atomKey={Json.encodeWithNull atomKey}
//    lastUserAtomId={lastUserAtomId}
//    defaultValue={defaultValue}
//    atomPath={atomPath} """
//
//            let adapterValueMapAtom =
//                Store.atomFamily
//                    (fun (_alias: Gun.Alias option) ->
//                        [
//                            Guid.newTicksGuid (), AdapterValue.Internal defaultValue
//                        ]
//                        |> Map.ofList)
//
//            let rec lastSyncValueByTypeAtom =
//                Store.readSelectorFamily
//                    FsStore.root
//                    (nameof lastSyncValueByTypeAtom)
//                    (fun (alias: Gun.Alias option) getter ->
//                        let adapterValueMap = Atom.value getter (adapterValueMapAtom alias)
//
//                        groupAdapterValueMapByType adapterValueMap)
//
//            //        let rec lastSyncValueAtom =
//            //            Store.readSelectorFamily
//            //                FsStore.root
//            //                (nameof lastSyncValueAtom)
//            //                (fun (alias: Alias option) getter ->
//            //                    let lastSyncValueByTypeAtom = Atom.value getter (lastSyncValueByTypeAtom alias)
//            //
//            //                    lastSyncValueByTypeAtom
//            //                    |> Map.values
//            //                    |> Seq.sortByDescending fst
//            //                    |> Seq.head
//            //                    |> snd)
//
//
//            let syncTrigger (ticks, newValue) =
//                let _getter, setter = syncEngine.GetStore ()
//
//                Store.change
//                    setter
//                    (adapterValueMapAtom (syncEngine.GetAlias ()))
//                    (fun oldAdapterValueMap ->
//                        Logger.logDebug
//                            (fun () ->
//                                $"Store.atomWithSync. syncTrigger. setter. oldAdapterValueMap={oldAdapterValueMap} newValue={newValue}. {getLocals ()}")
//
//                        oldAdapterValueMap
//                        |> (match newValue with
//                            | Some value -> Map.add ticks value
//                            | None -> Map.remove ticks))
//
//            let subscribe (setAtom: 'TValue -> unit) (subscriptionId: SubscriptionId) =
//                promise {
//                    let trigger (ticks, adapterValue: AdapterValue<'TValue> option) =
//                        match syncState.AdapterValueMapByType with
//                        | Some adapterValueMapByType ->
//                            let trigger (ticks, newValue) =
//                                syncTrigger (ticks, newValue)
//                                newHashedDisposable ticks
//
//                            let lastValue =
//                                match adapterValue with
//                                | Some (AdapterValue.Internal _) ->
//                                    adapterValueMapByType
//                                    |> Map.tryFind AdapterType.Internal
//                                    |> Option.bind id
//                                | Some (AdapterValue.Gun _) ->
//                                    adapterValueMapByType
//                                    |> Map.tryFind AdapterType.Gun
//                                    |> Option.bind id
//                                | Some (AdapterValue.Hub _) ->
//                                    adapterValueMapByType
//                                    |> Map.tryFind AdapterType.Hub
//                                    |> Option.bind id
//                                | None -> None
//
//                            let onError () =
//                                match adapterValue with
//                                | Some (AdapterValue.Gun _) -> syncState.GunSubscription <- None
//                                | Some (AdapterValue.Hub _) -> syncState.HubSubscription <- None
//                                | _ -> ()
//
//                            //                            callback (
////                                lastValue
////                                |> Option.map snd
////                                |> Option.defaultValue defaultValue
////                            )
////                            callback (
////                                lastValue
////                                |> Option.map snd
////                                |> Option.defaultValue defaultValue
////                            )
//
//                            Logger.logDebug
//                                (fun () ->
//                                    $"Store.atomWithSync. subscribe on trigger. @@@ should call setAtom? {getLocals ()}")
//
//                            Store.setInternalFromSync
//                                getLocals
//                                trigger
//                                syncState.SyncPaused
//                                lastValue
//                                onError
//                                (ticks, adapterValue)
//                        | None -> ()
//
//                    let rec onError () =
//                        syncState.HubSubscription <- None
//
//                        JS.setTimeout
//                            (fun () ->
//                                Store.syncSubscribe getLocals syncEngine syncState trigger onError atomPath
//                                |> Promise.start)
//                            1000
//                        |> ignore
//
//                    do! Store.syncSubscribe getLocals syncEngine syncState trigger onError atomPath
//
//                    return
//                        Object.newDisposable
//                            (fun () ->
//                                Logger.logDebug
//                                    (fun () ->
//                                        $"AtomWithSync.subscribe. newDisposable. subscriptionId={subscriptionId} {getLocals ()} "))
//                        |> Some
//                }
//
//            let batchPutFromUi (ticks, newValue) =
//                Batcher.batch (
//                    Batcher.BatchType.Set (
//                        ticks,
//                        (fun ticks -> Store.putFromUi getLocals syncEngine syncTrigger (ticks, newValue))
//                    )
//                )
//
//            let debouncedPutFromUi = Js.debounce batchPutFromUi 100
//
//            let rec wrapper =
//                Primitives.selector
//                    atomKey
//                    (fun getter ->
//                        syncEngine.SetProviders getter wrapper
//
//                        //                    let userAtom = lastSyncValueAtom (syncEngine.GetAlias ())
//                        let userAtom = lastSyncValueByTypeAtom (syncEngine.GetAlias ())
//                        let lastSyncValueByType = Atom.value getter userAtom
//
//                        syncState.AdapterValueMapByType <- Some lastSyncValueByType
//
//                        let result =
//                            lastSyncValueByType
//                            |> Map.values
//                            |> Seq.choose id
//                            |> Seq.sortByDescending fst
//                            |> Seq.head
//                            |> snd
//
//                        Profiling.addCount (fun () -> $"{atomPath} get"
//
//                        Logger.logTrace
//                            (fun () ->
//                                if (string result
//                                    |> Option.ofObjUnbox
//                                    |> Option.defaultValue "")
//                                    .StartsWith "Ping " then
//                                    null
//                                else
//                                    $"Store.atomWithSync. atomFamily.wrapper.get() wrapper={wrapper} userAtom={userAtom} result={result} {getLocals ()}               ")
//
//                        Logger.logTrace
//                            (fun () ->
//                                if (string result
//                                    |> Option.ofObjUnbox
//                                    |> Option.defaultValue "")
//                                    .StartsWith "Ping " then
//                                    null
//                                else
//                                    $"Store.atomWithSync. atomFamily.wrapper.get() wrapper={wrapper} userAtom={userAtom} result={result} {getLocals ()}               ")
//
//                        let userAtomId = Some (userAtom.toString ())
//
//                        if userAtomId <> lastUserAtomId then
//                            lastUserAtomId <- userAtomId
//
//                            match syncState.GunSubscription with
//                            | None ->
//                                Logger.logTrace
//                                    (fun () ->
//                                        $"Store.atomWithSync. atomFamily.wrapper.get() subscribing wrapper={wrapper} userAtom={userAtom} {getLocals ()}                       ")
//
//                            //                                debouncedSubscribe ()
//                            | _ ->
//                                Logger.logTrace
//                                    (fun () ->
//                                        $"Store.atomWithSync. atomFamily.wrapper.get() skipping subscribe wrapper={wrapper} userAtom={userAtom} {getLocals ()}                           ")
//
//                        result)
//                    (fun getter setter newValueFn ->
//                        syncEngine.SetProviders getter wrapper
//                        Profiling.addCount (fun () -> $"{atomPath} set"
//
//                        Store.change
//                            setter
//                            (adapterValueMapAtom (syncEngine.GetAlias ()))
//                            (fun oldAdapterValueMap ->
//                                let newValue = newValueFn |> Object.invokeOrReturn
//
//                                let newAdapterValueMap =
//                                    oldAdapterValueMap
//                                    |> Map.add (Guid.newTicksGuid ()) (AdapterValue.Internal newValue)
//
//                                let adapterValueMapByType = groupAdapterValueMapByType oldAdapterValueMap
//
//                                syncState.AdapterValueMapByType <- Some adapterValueMapByType
//
//
//                                //                                if true
//                                //                                   || oldValue |> Object.compare newValue |> not
//                                //                                   || (lastValue.IsNone
//                                //                                       && newValue |> Object.compare defaultValue) then
//                                let newValueOption = newValue |> Option.ofObjUnbox
//
//                                let gunValue =
//                                    adapterValueMapByType
//                                    |> Map.tryFind AdapterType.Gun
//                                    |> Option.bind id
//                                    |> Option.map snd
//
//                                let hubValue =
//                                    adapterValueMapByType
//                                    |> Map.tryFind AdapterType.Hub
//                                    |> Option.bind id
//                                    |> Option.map snd
//
//                                Logger.logTrace
//                                    (fun () ->
//                                        $"<filter> Store.atomWithSync.  atomFamily.wrapper.set()
//    wrapper={wrapper} oldAdapterValueMap={oldAdapterValueMap} newValue={newValue} jsTypeof-newValue={jsTypeof newValue}
//    adapterValueMapByType={adapterValueMapByType} __x={(newValueOption, gunValue, hubValue)}
//    y={unbox newValueOption = unbox gunValue
//       && unbox gunValue = unbox hubValue}
//    z={box newValueOption = box gunValue
//       && box gunValue = box hubValue}
//    {getLocals ()}                                           ")
//
//
//
//                                if box newValueOption = box gunValue
//                                   && box gunValue = box hubValue then
//                                    Logger.logTrace
//                                        (fun () ->
//                                            $"<filter> Store.atomWithSync. atomFamily.wrapper.set(). skipped debouncedPut
//    wrapper={wrapper} oldAdapterValueMap={oldAdapterValueMap} newValue={newValue} jsTypeof-newValue={jsTypeof newValue} {getLocals ()} ")
//                                else
//
//                                    syncState.SyncPaused <- true
//                                    debouncedPutFromUi (Guid.newTicksGuid (), newValue)
//
//                                if Js.jestWorkerId then
//                                    match splitAtomPath atomPath with
//                                    | Some (root, guid) ->
//                                        let newSet =
//                                            match testKeysCache.TryGetValue root with
//                                            | true, guids -> guids |> Set.add guid
//                                            | _ -> Set.singleton guid
//
//                                        testKeysCache.[root] <- newSet
//                                    | None -> ()
//
//                                newAdapterValueMap))
//
//            Logger.logTrace
//                (fun () ->
//                    $"Store.atomWithSync constructor
//    adapterValueMapAtom[alias]={(adapterValueMapAtom (syncEngine.GetAlias ()))}
//    lastSyncValueByTypeAtom[alias]={(lastSyncValueByTypeAtom (syncEngine.GetAlias ()))} wrapper={wrapper} {getLocals ()}")
//
//            //            if atomKey.Keys
////               <> (string Guid.Empty |> List.singleton) then
////                wrapper?onMount <- fun (_setAtom: 'TValue option -> unit) ->
////                                       debouncedSubscribe ()
////
////                                       fun () ->
////                                           Store.syncUnsubscribe
////                                               getLocals
////                                               (syncEngine.GetGunAtomNode ())
////                                               syncState.GunSubscription
////                                               (fun () -> syncState.GunSubscription <- None)
//
//            let unsubscribe _subscriptionId =
//                match syncEngine.GetGunAtomNode () with
//                | Some _gunAtomNode ->
//
//                    Logger.logTrace
//                        (fun () ->
//                            $"Store.atomWithSync. gunAtomNode found. calling off(). (actually skipped) subscriptionId={_subscriptionId} {getLocals ()} ")
//
//                //                    gunAtomNode.off () |> ignore
//                //                    lastSubscription <- None
//                | None ->
//                    Logger.logTrace
//                        (fun () ->
//                            $"Store.atomWithSync. skipping unsubscribe, no gun atom node. subscriptionId={_subscriptionId} {getLocals ()} ")
//
//
//
//
////            if atomKey.Keys
////               <> (string Guid.Empty |> List.singleton) then
////                wrapper?onMount <- fun (setAtom: 'TValue -> unit) ->
////                                       syncEngine.Subscribe (subscribe, setAtom)
////                                       fun _ -> syncEngine.Unsubscribe unsubscribe
////
//
//            wrapper?init <- defaultValue
//
//            Internal.registerAtom Internal.AtomType.AtomWithSync atomPath wrapper
//
//            wrapper

        let inline atomFamilyWithSync<'TKey, 'TValue  when 'TValue: equality> storeAtomPath (defaultValueFn: 'TKey -> 'TValue) =
            Atom.Primitives.atomFamily (fun param -> atomWithSync<'TKey, 'TValue> storeAtomPath (defaultValueFn param))
