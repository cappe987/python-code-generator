module Generator

open System
open System.IO
open Utils
open Types


let declareVariable state = 
  let name = genRandomName state
  let (tablevalue, value) = getRandomType state
  let indentation = getIndent state
  let line = indentation + name + " = " + value 
  { state with
      lines=line::state.lines
      table=Map.add name tablevalue (state.table)
  }


let declareIf state = 
  let line = "\nif " + genBoolExpression state + ":"
  { state with
      lines=line::state.lines
      indent=state.indent+2
  }




// let makeFunction state = 
//   let name = getRandomName state
  





