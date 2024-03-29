
module Variables 
open System
open System.IO
open Types
open Table

let typeArr : Types [] =
  [|Bool; Int; String; Char |]


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
    let x = Table.rand.Next(0, len)
    let y = Table.rand.Next(0, len)
    let word = words.[x].ToLower() + (firstUpper words.[y])
    if Map.containsKey word state.table then
      go()
    else
      String.filter nameCondition word
  go()


let genBool () = Table.rand.Next(0,2) = 1

let genInt () = Utils.biasedRandom(-2147483647/2, 2147483647/2, 20.0)
  //state.rand.Next(-2147483647/2, 2147483647/2)

let genChar () = char (Table.rand.Next(32, 127)) |> escapechar

let genString (state) = 
  genRandomName state

let genConnective() = if Table.rand.Next(0,2) = 1 then " and " else " or "


let genTypeValue (ofType, state) = 
  match ofType with
  | Bool -> 
    let value = genBool ()
    (Bool , string value)

  | String -> 
    let value = genString (state)
    (String , "\"" + value + "\"")

  | Char -> 
    let value = genChar ()
    (Char , "\'" + string value + "\'")

  | Int -> 
    let value = genInt ()
    (Int , string value)

  | Function _ -> failwithf "Can't generate function @ genTypeValue"


let genRandomType (state) =
  let ofType = Table.randomArr(typeArr)
  genTypeValue (ofType, state)



let connective(t) = 
  match t with
  | Bool   -> genConnective()
  | Int    -> randomArr(intOperators)
  | String -> "+"
  | Char   -> "+"
  // | List   -> "+"
  | Function _ -> failwith "No connective for Function"


let comparative () = 
  match Table.rand.Next(0,5) with
  | i when i = 0 -> "=="
  | i when i = 1 -> ">"
  | i when i = 2 -> "<"
  | i when i = 3 -> "<="
  | _ ->  ">="




// Returns a string in the form of a value or a variablename of ofType
let genValue(state, ofType) = 
  match getVarOfType (state, ofType) with
  | None -> 
    genTypeValue (ofType, state)
    |> fun (_,v) -> v
  | Some id -> 
    if Table.rand.Next(0, 3) = 0 then
      id 
    else
      genTypeValue (ofType, state)
      |> fun (_, v) -> v
      

let genBoolExpr (state) = 
  genValue(state, Int) + " " + comparative() + " " +  genValue(state, Int)



let genExpression (state : State) (ofType : Types) (depth : int) = 
  // let ofType : VarTypes = randomArr(state, varTypes)
  let rec go (depth) = 
    if depth = 0 then
      genValue(state, ofType)
    else
      if ofType = Bool then
        genBoolExpr state + " " + connective(ofType) + " " + go (depth - 1)
      else
      genValue(state, ofType) + " " + connective(ofType) + " " + go (depth - 1)

  go (depth)


let makeFuncVarnames vars = 
  let len = List.length vars - 1
  List.mapi (fun i (_, s) -> 
    if i = len then s
    else s + ", ") vars
  |> List.fold (+) ""


let makeVar (state) = 
  (
    randomArr(typeArr),
    genString(state)
  )
