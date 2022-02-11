module Solution

open System

type State = char list list
type Cost = int
type Decision = int * State

let compact (input:string) =
    let ls = input.Replace("_", "").Split "\r\n"
    let p i = sprintf "%c%c" ls.[2].[i] ls.[3].[i]
    sprintf "||%s|%s|%s|%s||" (p 3) (p 5) (p 7) (p 9)

let expand (line:string) =
    ""
