namespace FsStore.State

open FsCore
open FsStore.Model
open FsStore
open Microsoft.FSharp.Core.Operators
open FsJs

#nowarn "40"


module Sync =
    [<RequireQualifiedAccess>]
    type Request =
        | Connect of alias: string
        | Set of alias: string * atomPath: string * value: string
        | Get of alias: string * atomPath: string
        | Filter of alias: string * atomPath: string

    [<RequireQualifiedAccess>]
    type Response =
        | ConnectResult
        | SetResult of ok: bool
        | GetResult of value: string option
        | GetStream of alias: string * atomPath: string * value: string option
        | FilterResult of keys: string []
        | FilterStream of alias: string * atomPath: string * keys: string []


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

            Profiling.addTimestamp (fun () -> $"{nameof FsStore} | Selectors.store [ constructor ]") getLocals

            let rec valueWrapper =
                Atom.Primitives.selector
                    (fun getter ->
                        let result = Atom.get getter valueAtom

                        let getLocals () = $"result={result} {getLocals ()}"

                        Profiling.addTimestamp
                            (fun () -> $"{nameof FsStore} | Selectors.store [ valueWrapper.read(getter) ]")
                            getLocals

                        result)
                    (fun getter setter newValue ->
                        let getLocals () = $"newValue={newValue} {getLocals ()}"

                        Profiling.addTimestamp
                            (fun () ->
                                $"{nameof FsStore} | Selectors.store [ valueWrapper.set(getter,setter,newValue) ]")
                            getLocals

                        Atom.set setter accessorsAtom (Some (getter, setter))
                        Atom.set setter valueAtom newValue)
                |> Atom.addSubscription
                    false
                    (fun setAtom ->
                        promise {
                            let getLocals () = $"lastValue={lastValue} {getLocals ()}"

                            Profiling.addTimestamp
                                (fun () -> $"{nameof FsStore} | Selectors.store [ valueWrapper.onMount() ]")
                                getLocals

                            lastValue <- lastValue + 1
                            setAtom lastValue
                        })
                    (fun () ->
                        let getLocals () = $"lastValue={lastValue} {getLocals ()}"

                        Profiling.addTimestamp
                            (fun () -> $"{nameof FsStore} | Selectors.store [ valueWrapper.onUnmount() ] ")
                            getLocals)

            Atom.readSelector
                (RootAtomPath (FsStore.storeRoot, AtomName (nameof store)))
                (fun getter ->
                    let value = Atom.get getter valueWrapper
                    let accessors = Atom.get getter accessorsAtom

                    let getLocals () =
                        $"value={value} accessors={accessors.IsSome} {getLocals ()}"

                    Profiling.addTimestamp
                        (fun () -> $"{nameof FsStore} | Selectors.store [ wrapper.read(getter) ]")
                        getLocals

                    accessors)
