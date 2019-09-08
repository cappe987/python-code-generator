module Table

open Types


let rand = System.Random()

let randomFromArr() = 
  function
  | arr when Array.isEmpty arr -> None 
  | arr -> 
    let x = rand.Next(0, Array.length arr)
    let (id, _) = arr.[x]
    Some id

let randomArr(arr) = 
  let x = rand.Next(0, Array.length arr)
  arr.[x]


let getVarOfType(state, t1) = 
  Map.filter (fun k t -> t = t1) state.table
  |> Map.toArray
  |> randomFromArr()


let getRandomVarOfType state ofType = 
  if Map.count state.table = 0 then
    None
  else
    Map.toArray state.table
    |> Array.filter (fun (s,t) -> isType t ofType)
    |> fun arr -> 
      let x = rand.Next(0, Array.length arr - 1)
      Some arr.[x]

let getRandomVar state = 
  Map.toArray state.table
  |> Array.filter (fun (_,t) -> match t with Function _ -> false | _ -> true)
  |> fun arr -> 
    if Array.isEmpty arr then None
    else
      let x = rand.Next(0, Array.length arr - 1)
      Some arr.[x]

let getRandomVarId state = 
  getRandomVar state
  |> Option.bind (fun (id,_) -> Some id)



// let getRandomFunction state = 
//   state.table
//   |> Map.toList
//   |> List.fold (fun acc (id, t) -> 
//                                 match t with 
//                                 | Function ts -> (id,ts)::acc 
//                                 | _ -> acc
//                                 ) []
//   |> List.toArray
//   |> fun arr -> 
//     let x = rand.Next(0, Array.length arr)
//     arr.[x]