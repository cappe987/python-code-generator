
// open System


[<EntryPoint>]
let main argv =
  printfn "Starting..."
  Generator.run()


  0 



(*
  Make use of the biased number generator


  Issues:
    makeAssignment when no variables of type are available.
    Replace makeAssignment placeholder

    Handle division by zero?

    Funtions not returning anything
    Functions using non-existant variables.
    The `vars` generated are different from `varnames` because 
      of different states.
  
  


*)
