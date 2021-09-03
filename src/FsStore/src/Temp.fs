namespace FsStore

open FsJs
open FsStore.Model

#nowarn "40"


module Temp =


    let run () =
        Profiling.measureTime
            "atom1"
            (fun () ->
                let _atom1 = Atom.Primitives.atom 0
                ())

        Profiling.measureTime
            "atom2"
            (fun () ->
                let _atom2 = Atom.Primitives.create (AtomType.Atom 0)
                ())

        Profiling.measureTime
            "atom3"
            (fun () ->
                let _atom2 = Atom.Atom.create (AtomType.Atom 0)
                ())

    run ()



    ()
