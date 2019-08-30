module Utils

open System
open System.IO
open Types
open Table



let initState = 
  {lines=[]; table=Map.empty; indent=0; rand=System.Random()}

let printCode state = 
  List.rev state.lines
  |> List.iter (fun x -> printfn "%A" x) 

let writeToFile filename state = 
  state.lines
  |> List.rev
  |> fun arr -> File.WriteAllLines(filename, arr)

let addNewline state = 
  match state.lines with
  | x::_ when x = "" -> 
    // {state with lines=x::xs}
    state
  | xs -> 
    {state with lines=""::xs}

let getIndent state = 
  let mutable res = ""
  for _ in 1..state.indent do
    res <- res + " "
  res

let insert line state = {state with lines=line::state.lines}

let indent state = {state with indent=state.indent+2}

let outdent state = {state with indent=state.indent-2}




