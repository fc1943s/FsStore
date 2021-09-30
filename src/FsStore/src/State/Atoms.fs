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

        let rec gunPeers =
            Atom.createWithStorage (RootAtomPath (FsStore.storeRoot, AtomName (nameof gunPeers))) ([||]: Gun.GunPeer [])

        let rec gunSync = Atom.createWithStorage (RootAtomPath (FsStore.storeRoot, AtomName (nameof gunSync))) true

        let rec hubUrls =
            Atom.createWithStorage (RootAtomPath (FsStore.storeRoot, AtomName (nameof hubUrls))) ([||]: string [])

        let rec hubSync = Atom.createWithStorage (RootAtomPath (FsStore.storeRoot, AtomName (nameof hubSync))) false

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
