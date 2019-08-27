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
    