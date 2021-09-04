namespace FsStore.State

open FsStore.Bindings
open FsStore.Model
open FsStore
open Microsoft.FSharp.Core.Operators
open FsJs


[<AutoOpen>]
module AtomsMagic =
    module Atoms =
        let rec logLevel =
            Atom.createWithStorage
                (RootAtomPath (FsStore.storeRoot, AtomName (nameof logLevel)))
                Logger.DEFAULT_LOG_LEVEL

        let rec showDebug =
            Atom.createWithStorage
                (RootAtomPath (FsStore.storeRoot, AtomName (nameof showDebug)))
                Dom.deviceInfo.IsTesting

        let rec gunOptions =
            Atom.createWithStorage
                (RootAtomPath (FsStore.storeRoot, AtomName (nameof gunOptions)))
                (GunOptions.Sync [||])

        let rec hubUrl =
            Atom.createWithStorage (RootAtomPath (FsStore.storeRoot, AtomName (nameof hubUrl))) (None: string option)

        let rec sessionRestored =
            Atom.create (RootAtomPath (FsStore.storeRoot, AtomName (nameof sessionRestored))) (AtomType.Atom false)

        let rec internalAlias =
            Atom.create
                (RootAtomPath (FsStore.storeRoot, AtomName (nameof internalAlias)))
                (AtomType.Atom (None: Gun.Alias option))

        let rec gunTrigger =
            Atom.create (RootAtomPath (FsStore.storeRoot, AtomName (nameof gunTrigger))) (AtomType.Atom 0)

        let rec hubTrigger =
            Atom.create (RootAtomPath (FsStore.storeRoot, AtomName (nameof hubTrigger))) (AtomType.Atom 0)

        let rec routeTrigger =
            Atom.create (RootAtomPath (FsStore.storeRoot, AtomName (nameof routeTrigger))) (AtomType.Atom 0)
