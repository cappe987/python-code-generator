module Generator

open Types
open Code
open Code.Statements
open Code.Blocks


let rand = System.Random()

module Make = 

  let code = 
    Settings.settings
    |> Settings.getDistribution

  let makeRandomStatement(depth : int) : Statement = 
    let x = rand.Next(0, Array.length code)
    match code.[x] with
    | Statement s -> s
    | Block     s -> s (depth - 1)
    // if x = 0 then
    //   Blocks.randomBlock() |> fun block -> block (depth - 1)
    // else
    //   Statements.randomStatement()


  let makeCode depth state = 
    if depth <= 0 then
      state
    else 
      // block() |> fun b -> b state
      let total = Settings.maxStatements Settings.settings
      let x = state.rand.Next(total/2, total)
      List.init x (fun _ -> makeRandomStatement(depth))
      |> Monoid.concat
      |> fun block -> block state

    // let block() = 
    //   let x = rand.Next(1, 6)
    //   List.init x (fun _ -> makeRandomStatement(depth))
    //   |> Monoid.concat

  Blocks.blockRef := makeCode



let run() = 
  Make.makeCode Settings.depth Utils.initState
  |> Utils.writeToFile "output.py"




    




