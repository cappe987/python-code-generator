
open System
open System.IO
open Utils
open Types
open Generator
open Code

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
  // state
  // |> Statements.makeVariable
  // |> Statements.makeVariable
  // |> Statements.makeVariable
  // |> Statements.makeVariable
  // |> Statements.makeVariable
  // |> Statements.makeVariable
  // |> Statements.makeVariable
  // |> Statements.makeVariable
  // |> Statements.makeAssignment
  // |> Statements.makeAssignment
  // |> Statements.makeAssignment
  // |> Statements.makeAssignment
  // |> Statements.makeAssignment


  |> writeToFile "output.py"

  0 



(*
  Make a settings.json that reads how many of each statements 
  can be picked.

  Use type code to make one array of both statements and blocks.


  Issues:
    Empty assignments - because no other variables of the same type exists?
      Add support for random variables.

    - in string expression

  Replace genBoolExpression with genExpression

  Make use of the depth setting

  

*)
