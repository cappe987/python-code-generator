
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

  initState
  |> declareVariable
  |> declareVariable
  |> declareVariable
  |> declareVariable
  |> declareIf 
  |> declareVariable
  |> declareVariable
  // |> printCode
  |> writeToFile "output.py"
  // |> fun state -> {state with indent=state.indent+2}
  // |> getIndent
  // |> printfn "%A"
  // |> fun state -> printfn "%A" state.lines



  0 // return an integer exit code



(*
  Handle backslashes in strings/chars
*)