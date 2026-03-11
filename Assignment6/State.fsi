module Interpreter.State

    type state

    val mkState : int -> state
    val declare : string -> state -> option<state>
    val getVar : string -> state -> option<int>
    val setVar : string -> int -> state -> option<state>
    val alloc : string -> int -> state -> option<state>
    val free : int -> int -> state -> option<state>
    val getMem : int -> state -> option<int>
    val setMem : int -> int -> state -> option<state> 