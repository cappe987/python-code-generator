module Generator

// open System
// open System.IO
open Utils
open Types
// open Table


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


  let declareVariable state = 
    let name = Variables.genRandomName state
    let (tablevalue, value) = Variables.getRandomType state
    let indentation = getIndent state
    let line = indentation + name + " = " + value 

    { state with
        lines=line::state.lines
        table=Map.add name tablevalue (state.table)
    }

  let decIf : Statement = 
    let inner (instate : State) = 
      let indentation = getIndent instate
      let line = "\n" + indentation + "if " + Variables.genBoolExpression instate + ":"

      { instate with
          lines=line::instate.lines
      //     // indent=instate.indent+2
      }

    inner


  let decElse : Statement = 
    let inner (instate : State) = 
      let indentation = getIndent instate
      let line = "\n" + indentation + "else " + ":"

      { instate with
          lines=line::instate.lines
      //     // indent=instate.indent+2
      }

    inner







module Blocks = 
  open Statements
  open Monoid


  // let ifWith2Vars = (declareVariable >.> declareVariable) =>> decIf

  let (blockDummy, blockRef) = createDummy()





  let ifelse = 
    let inner depth = 
      if depth <= 1 then
        declareVariable
      else
        (blockDummy depth =>> decIf)
        >.> 
        (blockDummy depth  =>> decElse)
    
    inner




  // returns bool for if it's a recursive type
  // Replace with StatementType
  let getRandomStatement state = 
    let x = state.rand.Next(0, 10)
    match x with
    | x when x < 3 -> (decIf, true)
    // | x when x < 10 -> (declareVariable, false)
    | x -> (declareVariable, false)



  let makeBlocks depth state = 

    let block = //(instate : State) = 
      let x = state.rand.Next(0,20)

      let (stmnt, isrec) = getRandomStatement state
      if isrec then
        ifelse (depth - 1)

        // let st1 = (blockDummy (depth - 1)) =>> stmnt
        // if x < 10 then
        //   (blockDummy depth) >.> (declareVariable =>> st1)
        // else
        //   declareVariable =>> st1

      else
        // stmnt 
       stmnt >.> blockDummy depth

    if depth <= 0 then
      state
    else 
      block state

  blockRef := makeBlocks



    




