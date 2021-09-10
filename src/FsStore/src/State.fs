namespace FsStore.State

open FsStore
open FsStore.Model
open FsStore.State

module State =
    let messageIdAtoms =
        Engine.subscribeCollection FsStore.storeRoot Atoms.Message.collection (Engine.parseGuidKey MessageId)
