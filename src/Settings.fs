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


// + 1 since random is exclusive upper
let blocklengthMin = parseIntSetting settings "blocklength_min"
let blocklengthMax = parseIntSetting settings "blocklength_max" + 1

let conditionlengthMin = parseIntSetting settings "conditionlength_min"
let conditionlengthMax = parseIntSetting settings "conditionlength_max" + 1

let parametercountMin = parseIntSetting settings "parametercount_min"
let parametercountMax = parseIntSetting settings "parametercount_max" + 1

let functioncount = parseIntSetting settings "functions" 