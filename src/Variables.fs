
module Variables 
open System
open System.IO
open Types
open Table

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
  let words = File.ReadAllLines("data/words.txt")
  let rec go() = 
    let len = Array.length words
    let x = state.rand.Next(0, len)
    let y = state.rand.Next(0, len)
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
      | None -> 
        let (_, value) = genTypeValue ofType state
        value
      | Some id -> id
    else
      match getVarOfType (state, ofType) with
      | None -> 
        let (_, value) = genTypeValue ofType state
        value
      | Some id -> 
        if state.rand.Next(0, 3) = 0 then
          id + " " + connective state ofType + " " + go (depth - 1)
        else
          let (_, value) = genTypeValue ofType state
          value + " " + connective state ofType + " " + go (depth - 1)

  go (depth)