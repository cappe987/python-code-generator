
open System
open System.IO
open Utils
open Types
open Generator

let printCode state = 
  List.rev state.lines
  |> List.iter (fun x -> printfn "%A" x) 

let writeToFile filename state = 
  state.lines
  |> List.rev
  |> fun arr -> File.WriteAllLines(filename, arr)


[<EntryPoint>]
let main argv =
  printfn "Starting..."
  let state = initState

  Blocks.makeBlocks 5 state

  |> writeToFile "output.py"

  0 



(*
  Handle backslashes and other special chars in strings/chars
  Monads?
  Combinator?


  Pass in what the if-statement should contain


*)
