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
      let (tablevalue, value) = Variables.genTypeValue (ofType  , state)
      let indentation = getIndent state
      let line = indentation + name + " = " + value 

      { state with
          lines=line::state.lines
          table=Map.add name tablevalue (state.table)
      }

    let makeAssignment id ofType state = 
      let depth = Table.rand.Next(1, 3)
      let indent = getIndent state
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
    let ofType = Table.randomArr(Variables.typeArr)
    OfType.makeVariable ofType state


  let makeAssignment state = 
    match Table.getRandomVar state with
    | None -> OfType.makePrintOfStr "\"--- Assignment Placeholder ---\"" state
    | Some (id, ofType) -> 
      OfType.makeAssignment id ofType state






  let declareIf : Statement = 
    let inner (instate : State) = 
      let indent = getIndent instate
      let depth = 
        Table.rand.Next(Settings.conditionlengthMin,Settings.conditionlengthMax)
      let line = 
        indent + "if " + (Variables.genExpression instate Bool depth) + ":"

      { instate with
          lines=line::instate.lines
      }
    addNewline 
    >.> 
    inner


  let declareElse : Statement = 
    let inner (instate : State) = 
      let indentation = getIndent instate
      let line = indentation + "else" + ":"

      { instate with
          lines=line::instate.lines
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




  let (blockDummy, blockRef) = createDummy()



  let makeIf : BlockStatement = 
    let inner depth = 
      if depth <= 0 then
        makeAssignment
        // makePrint 
        // identity
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
        makeAssignment
        // makePrint 
        // identity
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