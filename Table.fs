module Table

open Types

let getType = 
  function
  | Bool   _ -> VarTypes.Bool
  | Char   _ -> VarTypes.Char
  | String _ -> VarTypes.String
  | Int    _ -> VarTypes.Int
  | List   _ -> VarTypes.List
  | Function _ -> VarTypes.Function

let matchType t1 t2 = getType t1 = getType t2 

let isType t1 t2 = getType t1 = t2

let randomFromArr (state) = 
  function
  | arr when Array.isEmpty arr -> None 
  | arr -> 
    let x = state.rand.Next(0, Array.length arr - 1)
    let (id, _) = arr.[x]
    Some id

let randomArr(state, arr) = 
  let x = state.rand.Next(0, Array.length arr - 1)
  arr.[x]
  // let (id, _) = arr.[x]
  // id

let getVarOfType(state, t1) = 
  Map.filter (fun k t -> isType t t1) state.table
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

    



let updateTable state k v = 
  Map.remove k state.table
  |> Map.add k v