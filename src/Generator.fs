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
    let x = Table.rand.Next(0, Array.length code)
    match code.[x] with
    | Statement s -> s
    | Block     s -> s (depth - 1)




  let makeCode depth state = 
    if depth <= 0 then
      state
    else 
      let x = Table.rand.Next(Settings.blocklengthMin, Settings.blocklengthMax)
      List.init x (fun _ -> makeRandomStatement(depth))
      |> Monoid.concat
      |> fun block -> block state



  Blocks.blockRef := makeCode



  let declareFunction name vars (instate : State) = 
    let varnames = Variables.makeFuncVarnames vars
    let line = "def " + name + "(" + varnames + ")"+ ":"

    {instate with
      lines=line::instate.lines;
    }

  let declareFuncVars vars (instate : State) = 
    {instate with
      table= List.fold (fun m (t, s) -> Map.add s t m) instate.table vars
    }

  let funcParts state = 
    let paramcount = 
      Table.rand.Next(Settings.parametercountMin, Settings.parametercountMax)
      
    let vars = List.init paramcount (fun _ -> (Variables.makeVar state))
    let name = Variables.genString(state)
    let signature = (name, List.map (fun (t, _) -> t) vars)

    (declareFuncVars vars, signature, declareFunction name vars)

    

  let makeFunction depth = 

    let inner state = 
      let (declareFuncVars, (name, parameters), declareFunction) = funcParts state
      (declareFuncVars >.> (makeCode depth)) =>> declareFunction
      |> fun statement -> statement state 
      |> Utils.addNewline
      |> fun state -> 
        { state with 
            lines=""::""::state.lines
            table=Map.add name (Function parameters) state.table
        }
      // Add the function name/parameters to the table.

    inner 




  let makeFuncs depth state = 
    List.init functioncount (fun _ -> makeFunction depth)
    |> Monoid.concat
    |> fun block -> block state



let run() = 
  // Make.makeCode Settings.depth Utils.initState
  Make.makeFuncs Settings.depth Utils.initState
  |> Make.makeCode Settings.depth
  // makeIf Settings.depth Utils.initState
  |> Utils.writeToFile "output/output.py"
  // |> Utils.writeLineTimer "output/output.py"




    




