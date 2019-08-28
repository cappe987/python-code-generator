module Table

open Types

let tryFindBool state = 
  Map.filter (fun k t -> 
    match t with
    | Bool _ -> true
    | _      -> false
    ) state.table
  |> Map.toArray
  |> fun arr -> 
    match arr with
    | arr when Array.isEmpty arr -> None 
    | arr -> 
      let x = state.rand.Next(0, Array.length arr - 1)
      let (id, _) = arr.[x]
      Some id

let getRandomVariable state = 
  if Map.count state.table = 0 then
    None
  else
    let x = state.rand.Next(0, Map.count state.table - 1)
    Map.toArray state.table
    |> fun arr -> arr.[x]
    |> fun (id, _) -> Some id


    