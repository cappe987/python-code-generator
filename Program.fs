
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
  // let a = 
  let state = initState
    // let b = decIf state (Recursive decIf ) 
  // let c = declareVariable >>= decIf
  // let b = followedBy declareVariable declareVariable
  // let d = b >>= c
  // let e = followedBy declareVariable declareVariable
  // // b state 
  // let f = fx
  // let a = declareVariable >>= f
  // let a = declareVariable >.> identity
  // state
  // |> a

  makeBlocks 5 state


  // |> returnS 
  // declareVariable
  // returnS {state with lines="\n\n"::state.lines}
  // |> fun s -> s state
  

  // |> ifWith2Vars
  // |> ((e >>= d) >>= decIf)



    // decIf state b
    // |> fun state -> decIf state declareVariable
  // a
  // |> printCode

  // initState
  // |> declareVariable
  // |> declareVariable
  // |> declareVariable
  // |> declareVariable
  // |> declareIf 
  // |> declareVariable
  // |> declareVariable
  |> writeToFile "output.py"
  // |> fun state -> {state with indent=state.indent+2}
  // |> getIndent
  // |> printfn "%A"
  // |> fun state -> printfn "%A" state.lines



  0 



(*
  Handle backslashes and other special chars in strings/chars
  Monads?
  Combinator?


  Pass in what the if-statement should contain


*)
