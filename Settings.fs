module Settings

open FSharp.Data
// open FSharp.Data.JsonExtensions
open System
open System.IO
open Types
open Code.Statements
open Code.Blocks

let (|Int|_|) (s : string) =
    match Int32.TryParse s with
    | true, x -> Some x
    | _       -> None

let settings = 
  File.ReadAllText("settings.json")
  |> JsonValue.Parse

let codeStatements = 
  [|
    ("variable", Statement makeVariable);
    ("print", Statement makePrint);
    ("assignment", Statement makeAssignment);

    ("if", Block makeIf);
    ("ifelse", Block makeIfelse);
  |]

let makeN n f =
  Array.init n (fun _ -> f)


let parseSetting js str = 
  match JsonExtensions.TryGetProperty (js, str) with
  | None -> failwithf "Invalid setting |%s|" str
  | Some i -> i.AsInteger()

let getDistribution js = 
  Array.map (fun (s, f) -> (f, parseSetting js s)) codeStatements
  |> Array.collect (fun (f, i) -> makeN i f)
  

  
let depth js = parseSetting js "depth"

let maxStatements js = 
  Array.sumBy (fun (s, _) -> parseSetting js s) codeStatements
