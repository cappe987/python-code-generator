module Utils

open System
open System.IO
open Types
open Table

module Variables = 

  let varTypes : VarTypes [] = 
    [|VarTypes.Bool; VarTypes.Int; VarTypes.String; VarTypes.Char |]


  let intOperators = [|"+"; "-"; "*"; "%"; "/";|]
 
  let escapechar c = 
    match c with
    | '\\' -> ' ' 
    | '\'' -> ' '
    | '\"' -> ' '
    | c    -> c

  let alphabet = ['A'..'Z'] @ ['a'..'z']

  let firstUpper (s : string) = 
    string (Char.ToUpper s.[0]) + s.[1..]

  let nameCondition c = (List.contains c alphabet)

  let genRandomName state = 
    let words = File.ReadAllLines("words.txt")
    let rec go() = 
      let x = state.rand.Next(0, 3000)
      let y = state.rand.Next(0, 3000)
      let word = words.[x].ToLower() + (firstUpper words.[y])
      if Map.containsKey word state.table then
        go()
      else
        String.filter nameCondition word
    go()


  let genBool state = state.rand.Next(0,2) = 1

  let genInt state = state.rand.Next(-2147483647/2, 2147483647/2)

  let genChar state = char (state.rand.Next(32, 127)) |> escapechar

  let genString state = 
    genRandomName state

  let genConnective state = if state.rand.Next(0,2) = 1 then " and " else " or "

  let rec genBoolExpression state = 
    let x = state.rand.Next(0,10)
    match x with
    | x when x >= 0  && x < 4-> 
      genBoolExpression state + (genConnective state) + genBoolExpression state

    | x when x >= 4 && x <= 6 -> 
      string (genBool state)

    | x when x > 6 && x < 10 -> 
      match getVarOfType (state, VarTypes.Bool) with
      | Some id -> id
      | None   -> string (genBool state)

    | x -> failwithf "Invalid number generated @ genBoolExpression |%d|" x

  let genTypeValue ofType state = 
    match ofType with
    | VarTypes.Bool -> 
      let value = genBool state
      (Bool value, string value)
    | VarTypes.String -> 
      let value = genString state
      (String value, "\"" + value + "\"")
    | VarTypes.Char -> 
      let value = genChar state
      (Char value, "\'" + string value + "\'")
    | VarTypes.Int -> 
      let value = genInt state
      (Int value, string value)
    | x -> failwithf "Invalid type @ genTypeValue |%A|" x


  let genRandomType state =
    let ofType : VarTypes = Table.randomArr(state, varTypes)
    genTypeValue ofType state

  // let genAssignment state = 
  //   match getRandomVarWithValue state with
  //   | None     -> None
  //   | Some (var,t) -> 

  let connective state t = 
    match t with
    | VarTypes.Bool   -> genConnective state
    | VarTypes.Int    -> randomArr(state, intOperators)
    | VarTypes.String -> "+"
    | VarTypes.Char   -> "+"
    | VarTypes.List   -> "+"
    | VarTypes.Function   -> failwith "No connective for Function"

  let genExpression (state : State) (ofType : VarTypes) (depth : int) = 
    // let ofType : VarTypes = randomArr(state, varTypes)
    let rec go (depth) = 
      if depth = 0 then
        match getVarOfType (state, ofType) with
        | None -> ""
        | Some id -> id
      else
        match getVarOfType (state, ofType) with
        | None -> ""
        | Some id -> 
          id + " " + connective state ofType + " " + go (depth - 1)

    go (depth)







let initState = 
  {lines=[]; table=Map.empty; indent=0; rand=Random()}

let addNewline state = 
  match state.lines with
  | x::_ when x = "" -> 
    // {state with lines=x::xs}
    state
  | xs -> 
    {state with lines=""::xs}

let getIndent state = 
  let mutable res = ""
  for _ in 1..state.indent do
    res <- res + " "
  res

let indent state = {state with indent=state.indent+2}

let outdent state = {state with indent=state.indent-2}




