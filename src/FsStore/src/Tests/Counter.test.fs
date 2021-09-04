namespace FsStore.Tests

open FsStore.Hooks
open Feliz
open Fable.React
open Fable.Jester
open FsJs.Bindings
open Fable.ReactTestingLibrary
open Microsoft.FSharp.Core.Operators
open FsStore


module Counter =
    let counter = Atom.Primitives.atom 0

    [<ReactComponent>]
    let Counter () =
        let counter, setCounter = Store.useState counter

        Html.div [
            str $"[counter={counter}]"
            Html.button [
                prop.onClick (fun _ -> setCounter (counter + 1))
                prop.children [ str "add" ]
            ]
        ]

    Jest.test (
        "counter",
        promise {
            let context = RTL.render (Counter ())
            let! addButton = context.findByText "add"

            let! _ = context.findByText "[counter=0]"
            RTL.act (fun () -> addButton.click ())
            RTL.act (fun () -> addButton.click ())
            let! _ = context.findByText "[counter=2]"
            ()
        },
        Jest.maxTimeout
    )
