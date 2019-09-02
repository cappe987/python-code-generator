module Types

open System

// type VarTypes =
//   | Bool 
//   | String
//   | Char
//   | Int
//   | List
//   | Function

type Types = 
  | Bool   //of bool
  | String //of string
  | Char   //of char
  | Int    //of int
  | List   //of Types list
  | Function //of Types * Types list

type Identifier = string


type State = {
  lines  : string list //Reverse the lines at the end
  indent : int
  rand   : Random
  table  : Map<Identifier, Types>
}

type Statement = (State -> State)

type BlockStatement = (int -> State -> State)

type Code = 
  | Statement of Statement
  | Block     of BlockStatement



// let getType = 
//   function
//   | Bool   _ -> VarTypes.Bool
//   | Char   _ -> VarTypes.Char
//   | String _ -> VarTypes.String
//   | Int    _ -> VarTypes.Int
//   | List   _ -> VarTypes.List
//   | Function _ -> VarTypes.Function

// let matchType t1 t2 = getType t1 = getType t2 

// let isType t1 t2 = getType t1 = t2