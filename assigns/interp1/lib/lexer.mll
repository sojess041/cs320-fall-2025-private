{
open Parser
}

let ws     = [' ' '\t' '\r' '\n']+
let digit  = ['0'-'9']
let int    = digit+
let letter = ['A'-'Z' 'a'-'z'