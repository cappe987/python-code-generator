module Generator

open Types
open Code
open Code.Statements
open Code.Blocks
open Settings
open Monoid


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


  let funcParts state = 
    let paramcount = 
      Utils.rand.Next(Settings.parametercountMin, Settings.parametercountMax)
      
    let vars = List.init paramcount (fun _ -> (
                                              Table.randomArr(Variables.typeArr),
                                              Variables.genString(state))
                                              )
    let varnames = Variables.makeFuncVarnames vars

    let declareFuncVars (instate : State) = 
      {instate with
        table= List.fold (fun m (t, s) -> Map.add s t m) instate.table vars
      }

    let declareFunction (instate : State) = 
      let name = Variables.genString(instate)
      let line = "def " + name + "(" + varnames + ")"+ ":"

      {instate with
        lines=line::instate.lines;
      }
    
    (declareFuncVars, declareFunction)


  let makeFunction depth = 

    let inner state = 
      let (declareFuncVars, declareFunction) = funcParts state
      (declareFuncVars >.> (makeCode depth)) =>> declareFunction
      |> fun statement -> statement state 
      |> Utils.addNewline
      |> fun state -> {state with lines=""::""::state.lines}

    inner 




  let makeFuncs depth state = 
    List.init functioncount (fun _ -> makeFunction depth)
    |> Monoid.concat
    |> fun block -> block state



let run() = 
  // Make.makeCode Settings.depth Utils.initState
  Make.makeFuncs Settings.depth Utils.initState
  // makeIf Settings.depth Utils.initState
  |> Utils.writeToFile "output/output.py"
  // |> Utils.writeLineTimer "output/output.py"




    




