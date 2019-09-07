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


let getRandomVarOfType state ofType = 
  if Map.count state.table = 0 then
    None
  else
    Map.toArray state.table
    |> Array.filter (fun (s,t) -> isType t ofType)
    |> fun arr -> 
      let x = state.rand.Next(0, Array.length arr - 1)
      Some arr.[x]

let getRandomVar state = 
  if Map.count state.table = 0 then
    None
  else
    Map.toArray state.table
    |> Array.filter (fun (s,t) -> match t with Function _ -> false | _ -> true)
    |> fun arr -> 
      let x = state.rand.Next(0, Array.length arr - 1)
      Some arr.[x]

let getRandomVarId state = 
  getRandomVar state
  |> function
    | None -> None
    | Some (id, _) -> Some id



let getRandomFunction state = 
  state.table
  |> Map.toList
  // |> Array.map (fun (k,t) -> match t with Function ts -> Some (k, ts) | _ -> None)
  |> List.fold (fun acc (id, t) -> 
                                match t with 
                                | Function ts -> (id,ts)::acc 
                                | _ -> acc
                                ) []
  |> List.toArray
  |> fun arr -> 
    let x = state.rand.Next(0, Array.length arr)
    arr.[x]