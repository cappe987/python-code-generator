module Utils

open System
open System.IO
open Types
open Table



let initState = 
  {lines=[]; table=Map.empty; indent=0; rand=System.Random()}

let printCode state = 
  List.rev state.lines
  |> List.iter (fun x -> printfn "%A" x) 

let writeToFile filename state = 
  state.lines
  |> List.rev
  |> fun arr -> File.WriteAllLines(filename, arr)


let writeLineTimer filename state = 
  let rec go(xs) =
    match xs with
    | [] -> ()
    | x::xs ->
      File.AppendAllText(filename, x + "\n") 
      Threading.Thread.Sleep(50)
      go (xs)
  File.WriteAllText(filename, "")
  Console.ReadLine() |> ignore
  go (List.rev state.lines)


let addNewline state = 
  match state.lines with
  | x::_ when x = "" -> 
    state
  | xs -> 
    {state with lines=""::xs}

let getIndent state = 
  let mutable res = ""
  for _ in 1..state.indent do
    res <- res + " "
  res

let insert line state = {state with lines=line::state.lines}

let indent state = {state with indent=state.indent+2}

let outdent state = {state with indent=state.indent-2}




let rand = Random()

let biasedRandom(min, max, bias) = 
  // Bias level 
  // 1..  = bias towards min
  // 0..1 = bias towards max
  // let exp = float 5

  float min + (float (max - min)) * Math.Pow(rand.NextDouble(), bias)
  |> int
  |> fun d -> 
    if min < 0 && d < 0 then
      max + d
    else
      d