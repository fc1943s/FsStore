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

        let inline selectAtomFamily atom selector =
            Atom.Primitives.atomFamily (fun param -> Atom.Primitives.selectAtom atom (selector param))

        let inline asyncReadSelector<'A> (read: AsyncRead<'A>) =
            Atom.Primitives.asyncSelector
                read
                (fun _ _ _newValue -> promise { failwith "Primitives.asyncReadSelector is read only." })

        let inline asyncSelectorFamily<'TKey, 'A> (read: 'TKey -> AsyncRead<'A>) (write: 'TKey -> AsyncWrite<'A>) =
            Atom.Primitives.atomFamily
                (fun param ->
                    Atom.Primitives.asyncSelector
                        (read param)
                        (fun getter setter newValue -> promise { do! write param getter setter newValue }))

        let inline asyncReadSelectorFamily<'TKey, 'A> (read: 'TKey -> AsyncRead<'A>) =
            asyncSelectorFamily
                read
                (fun _key _ _ _newValue -> promise { failwith $"Primitives.asyncReadSelectorFamily is read only." })
