namespace FsStore

open FsCore
open System.Collections.Generic
open Fable.Core
open FsStore.Bindings
open FsStore.Bindings.Jotai
open FsStore.Model
open Microsoft.FSharp.Core.Operators
open FsJs
open Fable.Core.JsInterop

#nowarn "40"


module Atom =
    [<RequireQualifiedAccess>]
    type AdapterType =
        | Memory
        | Gun
        | Hub

    [<RequireQualifiedAccess>]
    type AdapterOptions =
        | Memory
        | Gun of gunPeers: Gun.GunPeer [] * alias: Gun.Alias
        | Hub of hubUrl: string * alias: Gun.Alias

    type AtomInternalKey = AtomInternalKey of key: string

    let private atomPathMap = Dictionary<StoreAtomPath, AtomConfig<obj>> ()
    let private atomIdMap = Dictionary<AtomInternalKey, StoreAtomPath> ()

    let rec globalAtomPathMap = Dom.Global.register (nameof globalAtomPathMap) atomPathMap
    let rec globalAtomIdMap = Dom.Global.register (nameof globalAtomIdMap) atomIdMap


    let inline getLocals () = ""

    let inline get<'A> (getter: Getter<obj>) (atom: AtomConfig<'A>) : 'A = getter (unbox atom) |> unbox<'A>
    let inline set<'A> (setter: Setter<obj>) (atom: AtomConfig<'A>) (value: 'A) = setter (unbox atom) value

    let inline change<'A> (setter: Setter<obj>) (atom: AtomConfig<'A>) (value: 'A -> 'A) =
        setter (unbox atom) (unbox value)


    let inline addSubscription<'A> (debounce: bool) mount unmount (atom: AtomConfig<'A>) =
        let mutable mounted = false

        let getLocals () =
            $"debounce={debounce} atom={atom} mounted={mounted}"

        let addTimestamp fn getLocals =
            Profiling.addTimestamp (fun () -> $"{nameof FsStore} | Atom.addSubscription {fn ()}") getLocals

        addTimestamp (fun () -> "[ constructor ]") getLocals

        let inline internalMount (setAtom: 'A -> unit) =
            promise {
                mounted <- true
                addTimestamp (fun () -> "[ internalMount ] (maybe debounced)") getLocals
                do! mount setAtom
            }

        let internalMount =
            if not debounce then
                internalMount >> Promise.start
            else
                Js.debounce (internalMount >> Promise.start) 0

        let inline internalUnmount () =
            if mounted then
                addTimestamp (fun () -> "[ internalUnmount ]") getLocals
                unmount ()

            mounted <- false

        let inline onMount (setAtom: _ -> unit) =
            internalMount setAtom
            fun _ -> internalUnmount ()

        let newOnMount =
            match jsTypeof atom?onMount with
            | "function" ->
                let oldOnMount = atom?onMount

                fun setAtom ->
                    let atomUnsubscribe = oldOnMount setAtom
                    let newUnsubscribe = onMount setAtom

                    fun () ->
                        newUnsubscribe ()
                        atomUnsubscribe ()

            | _ -> onMount

        atom?onMount <- newOnMount

        atom

    let register<'A> storeAtomPath (atom: AtomConfig<'A>) =
        let internalKey = AtomInternalKey (atom.ToString ())

        atomPathMap.[storeAtomPath] <- atom |> unbox<AtomConfig<obj>>
        atomIdMap.[internalKey] <- storeAtomPath

        let getLocals () =
            $"atom={atom} storeAtomPath={storeAtomPath |> StoreAtomPath.AtomPath} "

        let addTimestamp fn getLocals =
            Profiling.addTimestamp (fun () -> $"{nameof FsStore} | Atom.register {fn ()}") getLocals

        addTimestamp (fun () -> "[ body ]") getLocals

        atom

    let isRegistered atomReference =
        match atomReference with
        | AtomReference.Atom atom -> atomIdMap.ContainsKey (AtomInternalKey (atom.ToString ()))
        | AtomReference.Path path -> atomPathMap.ContainsKey path

    let rec query atomReference =
        let result =
            match atomReference with
            | AtomReference.Atom atom ->
                let internalKey = AtomInternalKey (atom.ToString ())
                atomIdMap |> Map.tryFindDictionary internalKey
            | AtomReference.Path path ->
                atomPathMap
                |> Map.tryFindDictionary path
                |> Option.map (fun atom -> query (AtomReference.Atom (atom |> unbox<AtomConfig<'A>>)))

        let getLocals () =
            $"atomReference={atomReference} result={result}"

        let addTimestamp fn getLocals =
            Profiling.addTimestamp (fun () -> $"{nameof FsStore} | Atom.query {fn ()}") getLocals

        addTimestamp (fun () -> "[ body ]") getLocals

        match result with
        | Some result -> result
        | None -> failwith $"{nameof FsStore} | Atom.query / result empty {getLocals ()}"


    module Primitives =
        let inline atom value = jotai.atom value

        let inline selector<'A> (read: Read<'A>) (write: Write<'A>) =
            let rec atom =
                jotai.atom (
                    read,
                    Some
                        (fun getter setter value ->
                            let newValue =
                                match jsTypeof value with
                                | "function" ->
                                    let oldValue = get getter atom
                                    (unbox value) oldValue |> unbox
                                | _ -> value

                            write getter setter newValue)
                )

            atom

        let inline readSelector (read: Read<'A>) =
            selector
                read
                (fun _ _ _ ->
                    failwith $"{nameof FsStore} | Atom.Primitives.readSelector / set() / read only atom {getLocals ()}")

        let inline setSelector (write: Write<'A>) = selector (fun _ -> JS.undefined) write

        let inline atomFamily (defaultValueFn: 'TKey -> AtomConfig<'A>) =
            jotaiUtils.atomFamily
                (fun key ->
                    let getLocals () = $"key={key} {getLocals ()}"
                    Profiling.addTimestamp (fun () -> $"{nameof FsStore} | Atom.Primitives.atomFamily") getLocals
                    defaultValueFn key)
                (if false then JS.undefined else Object.compare)

        let inline selectAtom atom selector =
            jotaiUtils.selectAtom
                atom
                (fun getter ->
                    let getLocals () = $"atom={atom} {getLocals ()}"
                    Profiling.addTimestamp (fun () -> $"{nameof FsStore} | Atom.Primitives.selectAtom") getLocals
                    selector getter)
                (if true then JS.undefined else Object.compare)

        let inline selectAtomFamily atom selector =
            atomFamily (fun param -> selectAtom atom (selector param))

        let inline selectorFamily<'TKey, 'A> (read: 'TKey -> Read<'A>) (write: 'TKey -> Write<'A>) =
            atomFamily (fun param -> selector (read param) (write param))

        let inline readSelectorFamily<'TKey, 'A> (read: 'TKey -> Read<'A>) : ('TKey -> AtomConfig<'A>) =
            selectorFamily
                read
                (fun _ _ _ ->
                    failwith
                        $"{nameof FsStore} | Atom.Primitives.readSelectorFamily / set() / read only atom {getLocals ()}")

        let inline asyncSelector<'A> (read: AsyncRead<'A>) (write: AsyncWrite<'A>) =
            jotai.atom (
                (fun getter -> promise { return! read getter }),
                Some (fun getter setter newValue -> promise { do! write getter setter newValue })
            )

        let inline asyncSetSelector (write: AsyncWrite<'A>) =
            asyncSelector (fun _ -> JS.undefined) write

        let inline asyncReadSelector<'A> (read: AsyncRead<'A>) =
            asyncSelector
                read
                (fun _ _ _newValue ->
                    promise {

                        failwith
                            $"{nameof FsStore} | Atom.Primitives.asyncReadSelector / set() / read only atom {getLocals ()}"
                    })

        let inline asyncSelectorFamily<'TKey, 'A> (read: 'TKey -> AsyncRead<'A>) (write: 'TKey -> AsyncWrite<'A>) =
            atomFamily
                (fun param ->
                    asyncSelector
                        (read param)
                        (fun getter setter newValue -> promise { do! write param getter setter newValue }))

        let inline asyncReadSelectorFamily<'TKey, 'A> (read: 'TKey -> AsyncRead<'A>) =
            asyncSelectorFamily
                read
                (fun _key _ _ _newValue ->
                    promise {
                        failwith
                            $"{nameof FsStore} | Atom.Primitives.asyncReadSelectorFamily / set() / read only atom {getLocals ()}"
                    })

        let inline create atomType =
            match atomType with
            | AtomType.Atom value -> atom value
            | AtomType.ReadSelector read -> readSelector read
            | AtomType.Selector (read, write) -> selector read write
            | AtomType.WriteOnlyAtom write -> setSelector write

    let empty = Primitives.atom ()

    let inline selector storeAtomPath (read: Read<'A>) (write: Write<'A>) =
        let getLocals () =
            $"atomPath={storeAtomPath |> StoreAtomPath.AtomPath} {getLocals ()}"

        let addTimestamp fn getLocals =
            Profiling.addTimestamp (fun () -> $"{nameof FsStore} | Atom.selector {fn ()}") getLocals

        let wrapper =
            Primitives.selector
                (fun getter ->
                    let result = read getter
                    let getLocals () = $"result={result} {getLocals ()}"
                    addTimestamp (fun () -> "[ read() ]") getLocals
                    result)
                (fun getter setter newValue ->
                    let getLocals () = $"newValue={newValue} {getLocals ()}"
                    addTimestamp (fun () -> "[ write() ]") getLocals
                    write getter setter newValue)

        let getLocals () = $"wrapper={wrapper} {getLocals ()}"
        addTimestamp (fun () -> "[ constructor ]") getLocals
        wrapper |> register storeAtomPath

    let inline readSelector storeAtomPath read =
        selector
            storeAtomPath
            read
            (fun _ _ _ -> failwith $"{nameof FsStore} | Atom.readSelector / set() / read only atom {getLocals ()}")

    let inline selectorFamily storeAtomPathFn read write =
        Primitives.atomFamily (fun param -> selector (storeAtomPathFn param) (read param) (write param))

    let inline readSelectorFamily storeAtomPathFn read =
        selectorFamily
            storeAtomPathFn
            read
            (fun _ _ _ ->
                failwith $"{nameof FsStore} | Atom.readSelectorFamily / set() / read only atom {getLocals ()}")

    let inline asyncSelector<'A> storeAtomPath (read: AsyncRead<'A>) (write: AsyncWrite<'A>) =
        let getLocals () =
            $"atomPath={storeAtomPath |> StoreAtomPath.AtomPath} {getLocals ()}"

        let addTimestamp fn getLocals =
            Profiling.addTimestamp (fun () -> $"{nameof FsStore} | Atom.asyncSelector {fn ()}") getLocals

        Primitives.asyncSelector
            (fun getter ->
                promise {
                    let! result = read getter
                    let getLocals () = $"result={result} {getLocals ()}"
                    addTimestamp (fun () -> "[ read() ]") getLocals
                    return result
                })
            (fun getter setter newValue ->
                promise {
                    do! write getter setter newValue

                    let getLocals () = $"newValue={newValue} {getLocals ()}"

                    addTimestamp (fun () -> "[ write() ]") getLocals
                })

    let inline asyncReadSelector<'A> storeAtomPath (read: AsyncRead<'A>) =
        asyncSelector
            storeAtomPath
            read
            (fun _ _ _newValue ->
                promise {
                    failwith $"{nameof FsStore} | Atom.asyncReadSelector / set() / read only atom {getLocals ()}" })

    let inline asyncSelectorFamily<'TKey, 'A>
        storeAtomPathFn
        (read: 'TKey -> AsyncRead<'A>)
        (write: 'TKey -> AsyncWrite<'A>)
        =
        Primitives.atomFamily
            (fun param ->
                asyncSelector
                    (storeAtomPathFn param)
                    (read param)
                    (fun getter setter newValue -> promise { do! write param getter setter newValue }))

    let inline asyncReadSelectorFamily<'TKey, 'A> storeAtomPathFn (read: 'TKey -> AsyncRead<'A>) =
        asyncSelectorFamily
            storeAtomPathFn
            read
            (fun _key _ _ _newValue ->
                promise {
                    failwith $"{nameof FsStore} | Atom.asyncReadSelectorFamily / set() / read only atom {getLocals ()}"
                })

    let inline atomFamilyAtom defaultValueFn =
        Primitives.atomFamily (fun param -> Primitives.atom (defaultValueFn param))

    let inline create storeAtomPath atomType =
        Primitives.create atomType
        |> register storeAtomPath

    let inline wrap read write atom =
        let storeAtomPath =
            if isRegistered (AtomReference.Atom atom) then
                Some (query (AtomReference.Atom atom))
            else
                None

        let getLocals () =
            $"atom={atom} atomPath={storeAtomPath |> Option.map StoreAtomPath.AtomPath} {getLocals ()}"

        let addTimestamp fn getLocals =
            Profiling.addTimestamp (fun () -> $"{nameof FsStore} | Atom.wrap {fn ()}") getLocals

        let rec wrapper = Primitives.selector read write
        let getLocals () = $"wrapper={wrapper} {getLocals ()}"
        addTimestamp (fun () -> "[ constructor ]") getLocals
        wrapper?init <- atom.init

        match storeAtomPath with
        | Some storeAtomPath -> wrapper |> register storeAtomPath
        | None -> wrapper

    let inline createWithStorage<'A> storeAtomPath (defaultValue: 'A) =
        let defaultValueFormatted = defaultValue |> Enum.formatIfEnum

        let internalAtom =
            jotaiUtils.atomWithStorage
                (storeAtomPath
                 |> StoreAtomPath.AtomPath
                 |> AtomPath.Value)
                defaultValueFormatted

        internalAtom
        |> wrap
            (fun getter -> get getter internalAtom)
            (fun _ setter argFn ->
                let newValue =
                    argFn
                    |> Object.invokeOrReturn
                    |> Enum.formatIfEnum

                set setter internalAtom newValue)
        |> register storeAtomPath


    let inline map<'A, 'B> readFn writeFn atom =
        let getLocals () = $"atom={atom} {getLocals ()}"

        let addTimestamp fn getLocals =
            Profiling.addTimestamp (fun () -> $"{nameof FsStore} | Atom.map {fn ()}") getLocals

        atom
        |> wrap
            (fun getter ->
                let value = get getter atom
                let newValue: 'B = readFn value

                let getLocals () =
                    $"value={value} newValue={newValue} {getLocals ()}"

                addTimestamp (fun () -> "[ read() ]") getLocals
                newValue)
            (fun _getter setter newValue ->
                let newValue: 'A = writeFn newValue

                change
                    setter
                    atom
                    (fun oldValue ->
                        let getLocals () =
                            $"oldValue={oldValue} newValue={newValue} {getLocals ()}"

                        addTimestamp (fun () -> "[ write() ]") getLocals
                        newValue))


    let emptyArrayAtom = Primitives.atom ([||]: obj [])

    let inline waitForAll<'T> (atoms: AtomConfig<'T> []) =
        match atoms with
        | [||] -> unbox emptyArrayAtom
        | _ -> jotaiUtils.waitForAll atoms

    let inline split atom = jotaiUtils.splitAtom atom

    module Atom =
        let mutable private keyCount = 0

        let create atomType =
            keyCount <- keyCount + 1
            let key = $"atom{keyCount}"

            let rec config =
                {
                    ToString = fun () -> key
                    init =
                        match atomType with
                        | AtomType.Atom value -> Some value
                        | _ -> None
                    Read =
                        match atomType with
                        | AtomType.Atom _ -> fun getter -> get getter config
                        | AtomType.ReadSelector read -> read
                        | AtomType.Selector (read, _) -> read
                        | AtomType.WriteOnlyAtom _ -> JS.undefined
                    Write =
                        match atomType with
                        | AtomType.Atom _ -> fun _ setter -> set setter config
                        | AtomType.ReadSelector _ -> JS.undefined
                        | AtomType.Selector (_, write) -> write
                        | AtomType.WriteOnlyAtom write -> write
                }

            config
