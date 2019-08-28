module Generator

// open System
// open System.IO
open Utils
open Types
// open Table

let rand = System.Random()

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

  let makePrint state = 
    let indentation = getIndent state
    let value = Table.getRandomVariable state
    let line = indentation + "print(" + value + ")"
    { state with
        lines=line::state.lines
    }


  let makeVariable state = 
    let name = Variables.genRandomName state
    let (tablevalue, value) = Variables.getRandomType state
    let indentation = getIndent state
    let line = indentation + name + " = " + value 

    { state with
        lines=line::state.lines
        table=Map.add name tablevalue (state.table)
    }

  let declareIf : Statement = 
    let inner (instate : State) = 
      let indentation = getIndent instate
      let line = "\n" + indentation + "if " + Variables.genBoolExpression instate + ":"

      { instate with
          lines=line::instate.lines
      //     // indent=instate.indent+2
      }

    inner


  let declareElse : Statement = 
    let inner (instate : State) = 
      let indentation = getIndent instate
      let line = "\n" + indentation + "else" + ":"

      { instate with
          lines=line::instate.lines
      //     // indent=instate.indent+2
      }

    inner

  let statementarr : Statement [] = 
    [|
      makePrint;
      makeVariable;
    |]

  let randomStatement() = 
    let x = rand.Next(0, Array.length statementarr)
    statementarr.[x]





module Blocks = 
  // open System
  open Statements
  open Monoid



  // let ifWith2Vars = (makeVariable >.> makeVariable) =>> declareIf

  let (blockDummy, blockRef) = createDummy()



  let makeIf : BlockStatement = 
    let inner depth = 
      if depth <= 1 then
        makeVariable
      else
        (blockDummy depth =>> declareIf)
        >.> addNewline
    
    inner


  let makeIfelse : BlockStatement = 
    let inner depth = 
      if depth <= 1 then
        makeVariable
      else
        (blockDummy depth =>> declareIf)
        >.> 
        (blockDummy depth  =>> declareElse)
        >.> addNewline
    
    inner



  let blockarr = 
    [|
      makeIf; 
      makeIfelse; 
    |]


  let randomBlock() = 
    let x = rand.Next(0, Array.length blockarr)
    blockarr.[x]




  // returns bool for if it's a recursive type
  // Replace with StatementType
  let getRandomStatement state = 
    let x = state.rand.Next(0, 10)
    match x with
    | x when x < 6 -> (declareIf, true)
    // | x when x < 10 -> (makeVariable, false)
    | x -> (makeVariable, false)



  let makeBlocks depth state = 

    let block = //(instate : State) = 
      let x = state.rand.Next(0,20)

      let (stmnt, isrec) = getRandomStatement state
      if isrec then
        makeIfelse (depth - 1)
        // >.> addNewline
        >.> blockDummy (depth - 1)

        // let st1 = (blockDummy (depth - 1)) =>> stmnt
        // if x < 10 then
        //   (blockDummy depth) >.> (makeVariable =>> st1)
        // else
        //   makeVariable =>> st1

      else
        // stmnt 
       stmnt >.> blockDummy depth
      //  >.> addNewline

    if depth <= 0 then
      state
    else 
      block state

  blockRef := makeBlocks



    




