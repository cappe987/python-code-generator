module Utils

open System
open System.IO
open Types
open Table

let alphabet = ['A'..'Z'] @ ['a'..'z']

let firstUpper (s : string) = 
  string (Char.ToUpper s.[0]) + s.[1..]

let nameCondition c = (List.contains c alphabet)

let genRandomName state = 
  let words = File.ReadAllLines("words.txt")
  let rec go() = 
    let x = state.rand.Next(0, 3000)
    let y = state.rand.Next(0, 3000)
    let word = words.[x] + (firstUpper words.[y])
    if Map.containsKey word state.table then
      go()
    else
      String.filter nameCondition word
  go()


let getIndent state = 
  let mutable res = ""
  for _ in 1..state.indent do
    res <- res + " "
  res

let initState = 
  {lines=[]; table=Map.empty; indent=0; rand=Random()}

let genBool state = state.rand.Next(0,2) = 1

let genInt state = state.rand.Next(-2147483647, 2147483647)

let genChar state = char (state.rand.Next(32, 127))

let genString state = 
  genRandomName state
  // let length = state.rand.Next(1, 20)
  // let rec go i = 
  //   if i <= 0 then
  //     ""
  //   else
  //     let c = string (genChar state)
  //     c + go (i - 1)
  
  // go length

let genConnective state = if state.rand.Next(0,2) = 1 then " and " else " or "

let rec genBoolExpression state = 
  let x = state.rand.Next(0,10)
  match x with
  | x when x >= 0  && x < 4-> 
    genBoolExpression state + (genConnective state) + genBoolExpression state

  | x when x >= 4 && x <= 6 -> 
    string (genBool state)

  | x when x > 6 && x < 10 -> 
    match tryFindBool state with
    | Some id -> id
    | None   -> string (genBool state)

  | x -> failwithf "Invalid number generated @ genBoolExpression %d" x




let getRandomType state =
  let x = state.rand.Next(0, 4)
  match x with
  | x when x = 0 -> 
    let value = genBool state
    (Bool value, string value)
  | x when x = 1 -> 
    let value = genString state
    (String value, "\"" + value + "\"")
  | x when x = 2 -> 
    let value = genChar state
    (Char value, "\'" + string value + "\'")
  | x when x = 3 -> 
    let value = genInt state
    (Int value, string value)
  | _ -> failwith "Invalid number generated @ getRandomType"