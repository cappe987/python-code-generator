module Types

open System

type VarTypes =
  | Bool 
  | String
  | Char
  | Int
  | List
  | Function

type Types = 
  | Bool   of bool
  | String of string
  | Char   of char
  | Int    of int
  | List   of Types list
  | Function of Types * Types list

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

