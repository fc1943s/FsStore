namespace FsStore

open FsCore
open FsCore.BaseModel
open FsJs
open FsStore
open System
open FsStore.Model
open FsStore.State


module Hydrate =
    let fileChunkSize = 12800

    let inline hydrateAppMessage setter message =
        let messageId = MessageId.NewId ()
        Atom.set setter (State.Atoms.Message.appMessage messageId) message
        Atom.set setter (State.Atoms.Message.ack messageId) (Some false)
        messageId


    let inline hydrateFile setter (hexString: string) =
        let chunkCount = int (Math.Ceiling (float hexString.Length / float fileChunkSize))

        match hexString, chunkCount with
        | String.Invalid, _
        | _, 0 ->
            let getLocals () =
                $"hexString.Length={hexString.Length} chunkCount={chunkCount} {getLocals ()}"

            Logger.logError (fun () -> $"{nameof FsStore} | Hydrate.hydrateFile. invalid") getLocals

            None
        | _ ->
            let chunks =
                Js.chunkString
                    hexString
                    {|
                        size = fileChunkSize
                        unicodeAware = false
                    |}

            let getLocals () =
                $"
hexString.Length={hexString.Length}
chunkCount={chunkCount}
chunks.Length={chunks.Length}
chunks.[0].Length={if chunks.Length = 0 then unbox null else chunks.[0].Length} {getLocals ()}"

            Logger.logDebug (fun () -> $"{nameof FsStore} | Hydrate.hydrateFile") getLocals

            if chunks.Length = chunkCount then
                let fileId = FileId.NewId ()
                Atom.set setter (Atoms.File.chunkCount fileId) chunkCount

                chunks
                |> Array.iteri (fun i -> Atom.set setter (Atoms.File.chunk (fileId, i)))

                Some fileId
            else
                None
