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

let multiply n f =
  Array.init n (fun _ -> f)


let parseIntSetting js str = 
  match JsonExtensions.TryGetProperty (js, str) with
  | None -> failwithf "Setting \"%s\" not found" str
  | Some i -> i.AsInteger()

let getDistribution js = 
  Array.map (fun (s, f) -> (f, parseIntSetting js s)) codeStatements
  |> Array.collect (fun (f, i) -> multiply i f)
  

  

let maxStatements js = 
  Array.sumBy (fun (s, _) -> parseIntSetting js s) codeStatements


let depth = parseIntSetting settings "depth"

let blocklengthMin = parseIntSetting settings "blocklength_min"
let blocklengthMax = parseIntSetting settings "blocklength_max" + 1