module Generator

open Types
open Code
open Code.Statements
open Code.Blocks


module Make = 

  let code = 
    Settings.settings
    |> Settings.getDistribution

  let makeRandomStatement(depth : int) : Statement = 
    let x = Utils.rand.Next(0, Array.length code)
    match code.[x] with
    | Statement s -> s
    | Block     s -> s (depth - 1)


  let makeCode depth state = 
    if depth <= 0 then
      state
    else 
      let x = state.rand.Next(Settings.blocklengthMin, Settings.blocklengthMax)
      List.init x (fun _ -> makeRandomStatement(depth))
      |> Monoid.concat
      |> fun block -> block state


  Blocks.blockRef := makeCode



let run() = 
  Make.makeCode Settings.depth Utils.initState
  |> Utils.writeToFile "output/output.py"




    




