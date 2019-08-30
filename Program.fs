
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

  Make.makeCode 5 state


  |> writeToFile "output.py"

  0 



(*
  Make a settings.json that reads how many of each statements 
  can be picked.

  Use type code to make one array of both statements and blocks.

*)
