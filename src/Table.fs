module Table

open Types


let rand = System.Random()

let randomFromArr (state) = 
  function
  | arr when Array.isEmpty arr -> None 
  | arr -> 
    let x = state.rand.Next(0, Array.length arr)
    let (id, _) = arr.[x]
    Some id

let randomArr(arr) = 
  let x = rand.Next(0, Array.length arr)
  arr.[x]


let getVarOfType(state, t1) = 
  Map.filter (fun k t -> t = t1) state.table
  |> Map.toArray
  |> randomFromArr (state)



let getRandomVar state = 
  if Map.count state.table = 0 then
    None
  else
    let x = state.rand.Next(0, Map.count state.table - 1)
    Map.toArray state.table
    |> fun arr -> arr.[x]
    |> fun (id, t) -> Some (id, t)

let getRandomVarId state = 
  getRandomVar state
  |> function
    | None -> None
    | Some (id, _) -> Some id
