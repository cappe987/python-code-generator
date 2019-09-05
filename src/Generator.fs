module Generator

open Types
open Code
open Code.Statements
open Code.Blocks
open Settings


module Make = 


  let codeStatements = 
    [|
      ("variable", Statement makeVariable);
      ("print", Statement makePrint);
      ("assignment", Statement makeAssignment);

      ("if", Block makeIf);
      ("ifelse", Block makeIfelse);
    |]


  let multiply n f =
    Array.init n (fun _ -> f)

  let getDistribution js = 
    Array.map (fun (s, f) -> (f, parseIntSetting js s)) codeStatements
    |> Array.collect (fun (f, i) -> multiply i f)

  let code = 
    getDistribution settings

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
  // |> Utils.writeLineTimer "output/output.py"




    




