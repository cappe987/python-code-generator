
open System


[<EntryPoint>]
let main argv =
  printfn "Starting..."
  Generator.run()


  0 



(*
  Make better folder structure
  Biased number generator for lower numbers.

  Issues:
    makeAssignment when no variables of type are available.
    Handle division by zero?


*)
