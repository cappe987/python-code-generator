module Code
// open System
// open System.IO
open Utils
open Types
// open Table

// let rand = System.Random()

module Monoid = 

  let followedBy (st1 : Statement) (st2 : Statement) : Statement = 
    let inner (instate : State) = 
      st1 instate
      |> st2

    inner

  let insertInto (st1 : Statement) (st2 : Statement) : Statement= 
    let inner (instate : State) = 
      st1 instate
      |> indent 
      |> st2
      // |> addNewline
      |> fun state -> {instate with lines=state.lines}

    inner

  let ( >.> ) st1 st2 = followedBy st1 st2

  let ( =>> ) st2 st1 = insertInto st1 st2

  let identity : Statement = 
    let inner (instate : State) = instate
    inner

  let identityB : BlockStatement = 
    let inner i instate =  instate
    inner

  let concat l = 
    List.fold (>.>) identity l

  let map (f : Statement) : Statement =
    let inner (instate : State) = 
      f instate
    inner

  let createDummy() : (int -> State -> State) * (int -> State -> State) ref = 
    let dummy (depth : int) (_ : State) : State = failwith "Dummy not replaced"
    let blockRef = ref dummy
    
    let inner (depth : int) (instate : State) = 
      // printfn "In Ref"
      !blockRef depth instate
    
    (inner, blockRef)


  // let fx (state : State) : Statement = 
  //   let inner (instate : State) = 
  //     {state with lines="\n\n"::instate.lines}
  //   inner


  // let returnS (state : State) : Statement = 
  //   let inner (_ : State) = state
  //   inner


  // let bind (f : State -> Statement) (st1 : Statement) : Statement = 
  //   let inner (instate : State) = 
  //     st1 instate
  //     |> f instate
    
  //   inner

  // let ( >>= ) st1 f = bind f st1




module Statements = 

  open Monoid

  module OfType = 

    let makePrintOfStr str state = 
      let indentation = getIndent state

      let line = indentation + "print(" + str + ")"
      { state with
          lines=line::state.lines
      }

    let makeVariable ofType state = 
      let name = Variables.genRandomName state
      // let (tablevalue, value) = Variables.getRandomType state
      let (tablevalue, value) = Variables.genTypeValue ofType state
      let indentation = getIndent state
      let line = indentation + name + " = " + value 

      { state with
          lines=line::state.lines
          table=Map.add name tablevalue (state.table)
      }

    let makeAssignment ofType state = 
      let name = Table.getRandomVar state
      match name with
      | None -> state
      | Some (id, t) -> 
        let depth = state.rand.Next(1, 3)
        let indent = getIndent state
        // let ofType = Table.getType t
        // let id = string id
        let line = indent + id + " = " + (Variables.genExpression state ofType depth)
        {state with 
          lines=line::state.lines;
        }



  let makePrint state = 
    let str = 
      match Table.getRandomVarId state with
      | None   -> "\"" + Variables.genString state + "\""
      | Some s -> s
    OfType.makePrintOfStr str state


  let makeVariable state = 
    let ofType : VarTypes = Table.randomArr(state, Variables.varTypes)
    OfType.makeVariable ofType state


  let makeAssignment state = 
    let ofType : VarTypes = Table.randomArr(state, Variables.varTypes)
    OfType.makeAssignment ofType state






  let declareIf : Statement = 
    let inner (instate : State) = 
      let indentation = getIndent instate
      // let line = "\n" + indentation + "if " + Variables.genBoolExpression instate + ":"
      let line = indentation + "if " + Variables.genBoolExpression instate + ":"

      { instate with
          lines=line::instate.lines
      //     // indent=instate.indent+2
      }
    addNewline 
    >.> 
    inner


  let declareElse : Statement = 
    let inner (instate : State) = 
      let indentation = getIndent instate
      // let line = "\n" + indentation + "else" + ":"
      let line = indentation + "else" + ":"

      { instate with
          lines=line::instate.lines
      //     // indent=instate.indent+2
      }

    addNewline 
    >.> 
    inner

  // let statementarr : Statement [] = 
  //   [|
  //     makePrint;
  //     makeVariable;
  //     makeVariable;
  //     makeVariable;
  //     makeVariable;
  //     makeAssignment;
  //   |]

  // let randomStatement() = 
  //   let x = rand.Next(0, Array.length statementarr)
  //   statementarr.[x]





module Blocks = 
  // open System
  open Statements
  open Monoid



  // let ifWith2Vars = (makeVariable >.> makeVariable) =>> declareIf

  let (blockDummy, blockRef) = createDummy()



  let makeIf : BlockStatement = 
    let inner depth = 
      if depth <= 0 then
        identity
        // OfType.makePrintOfStr("------ Placeholder --------")
        // makeVariable
        // randomStatement()
      else
        (blockDummy depth =>> declareIf)
        >.> addNewline
    
    inner


  let makeIfelse : BlockStatement = 
    let inner depth = 
      if depth <= 0 then
        identity
        // OfType.makePrintOfStr("------ Placeholder --------")
        // makeVariable
        // randomStatement()
      else
        (blockDummy depth =>> declareIf)
        >.> 
        (blockDummy depth  =>> declareElse)
        >.> addNewline
    
    inner



  // let blockarr : BlockStatement [] = 
  //   [|
  //     makeIf; 
  //     makeIfelse; 
  //   |]


  // let randomBlock() = 
  //   let x = rand.Next(0, Array.length blockarr)
  //   blockarr.[x]