module Types

open System

type Types = 
  | Bool   of bool
  | String of string
  | Char   of char
  | Int    of int
  | List   of Types list

type Identifier = string


// type Tokens = 
//   | Types of Types
//   | Id    of Identifier

// type Statements = 
//   | Function of Identifier * Identifier list
//   | If       of string



type State = {
  lines  : string list //Reverse the lines at the end
  indent : int
  rand   : Random
  table  : Map<Identifier, Types>
}

type Statement = (State -> State)

type StatementType = 
  | Rec    of Statement
  | Nonrec of Statement


