module Settings

open FSharp.Data
open System
open System.IO

let settings = 
  File.ReadAllText("settings.json")
  |> JsonValue.Parse



let parseIntSetting js str = 
  match JsonExtensions.TryGetProperty (js, str) with
  | None -> failwithf "Setting \"%s\" not found" str
  | Some i -> i.AsInteger()

  
let depth = parseIntSetting settings "depth"

let blocklengthMin = parseIntSetting settings "blocklength_min"
let blocklengthMax = parseIntSetting settings "blocklength_max" + 1

let conditionlengthMin = parseIntSetting settings "conditionlength_min"
let conditionlengthMax = parseIntSetting settings "conditionlength_max"