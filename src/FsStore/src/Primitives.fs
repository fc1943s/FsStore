namespace FsStore

open FsStore.Bindings.Jotai
open Microsoft.FSharp.Core.Operators
open FsJs

#nowarn "40"


module PrimitivesMagic =
    module OldStore =
        let inline writeOnlyAtom internalAtom =
            Atom.Primitives.setSelector
                (fun _getter setter newValue ->
                    Logger.logTrace (fun () -> $"writeOnlyAtom internalAtom={internalAtom} newValue={newValue}")
                    Atom.set setter internalAtom newValue)

