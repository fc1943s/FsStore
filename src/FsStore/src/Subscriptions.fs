namespace FsStore

open FsStore
open FsStore.Model
open FsStore.State

module Subscriptions =
    let messageIdAtoms = Engine.subscribeCollection storeRoot Atoms.Message.collection (Engine.parseGuidKey MessageId)
