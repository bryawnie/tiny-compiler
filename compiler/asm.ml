(* registers *)
type reg = 
(*            USAGE       | SAVING REGIME 
        ------------------+-------------- *)
| RAX   (* scratch/return |   R *)
| RBX   (*                |   E *)
| RCX   (* 4th Param      |   R *)
| RDX   (* 3rd Param      |   R *)
| RSI   (* 2nd Param      |   R *)
| RDI   (* 1st Param      |   R *)
| RSP   (* Stack Pointer  |   R *)
| RBP   (* Base Pointer   |   E *)
| R8    (* 5th Param      |   R *)
| R9    (* 6th Param      |   R *)
| R10   (*                |   R *)
| R11   (* Temp Register  |   R *)
| R15   (* HEAP Register  |   E *)
(* R = caller-save ; E = callee-save *)


(* arguments for instructions *)
type arg =
| Const of int64          (* Constant of 64 bits *)
| Reg of reg              (* Register *)
| Label of string         (* Label *)
| RegOffset of reg * int  (* Reg and its offset *)


(* asm instructions *)
type instruction =
| IMov of arg * arg   (* Move *)
| IAdd of arg * arg   (* Adition *)
| ISub of arg * arg   (* Substraction *)
| IOr  of arg * arg   (* Bitwise logical or *)
| IAnd of arg * arg   (* Bitwise logical and *)
| IXor of arg * arg   (* Bitwise exclusive or *)
| IMul of arg * arg   (* Arithmetic Product*)
| IDiv of arg         (* Divides RDX RAX / arg *)
| ISar of arg * arg   (* Arithmetic Right Shift *)
| IShr of arg * arg   (* Logical right shift (doesn't preserve sign)*)
| ISal of arg * arg   (* Arithmetic Left Shift *)
| IShl of arg * arg   (* Logical left shift *)
| ICmp of arg * arg   (* Comparer (-) *)
| ITest of arg * arg  (* Comparer (&) *)
| IJe  of arg         (* Jumps if equal *)
| IJne of arg         (* Jump if not equal*)
| IJnz of arg         (* Jumps if not zero *)
| IJz  of arg         (* Jumps if zero *)
| IJl  of arg         (* Jumps if less than *)
| IJg  of arg         (* Jumps if greater than *)
| IJge of arg         (* Jumps if greater or equal than *)
| IJmp of arg         (* Makes a jump *)
| ICall of arg        (* Calls a function *)
| ILabel of string    (* Simple Label *)
| ICqo                (* Extends RAX into RDX *)
| IPush of arg        (* Pushes an argument into the stack *)
| IPop of arg         (* Pop a value from stack *)
| IRet                (* Return *)
| IExtern of string   (* Declaration of an external label*)
| IEmpty              (* An empty line, for formatting purposes*)
| INop                (* No operation *)
| IComment of string  (* A comment for asm understanding*)


(* A pretty printing for registers *)
let pp_reg : reg Fmt.t =
  fun fmt r ->
    match r with
    | RAX -> Fmt.string fmt "RAX"
    | RBX -> Fmt.string fmt "RBX"
    | RCX -> Fmt.string fmt "RCX"
    | RDX -> Fmt.string fmt "RDX"
    | RSI -> Fmt.string fmt "RSI"
    | RDI -> Fmt.string fmt "RDI"
    | RSP -> Fmt.string fmt "RSP"
    | RBP -> Fmt.string fmt "RBP"
    | R8  -> Fmt.string fmt "R8"
    | R9  -> Fmt.string fmt "R9"
    | R10 -> Fmt.string fmt "R10"
    | R11 -> Fmt.string fmt "R11"
    | R15 -> Fmt.string fmt "R15"


(* A pretty printing for args *)
let pp_arg : arg Fmt.t =
  fun fmt arg ->
    match arg with
    | Const n         -> Fmt.pf fmt "%#Lx" n
    | Reg r           -> pp_reg fmt r
    | Label lbl       -> Fmt.string fmt lbl
    | RegOffset (r,i) -> 
      if i > 0 then
        Fmt.pf fmt "qword[%a + %a]" pp_reg r Fmt.int (8*i)
      else if i <0 then
        Fmt.pf fmt "qword[%a - %a]" pp_reg r Fmt.int (8*(-i))
      else  
        Fmt.pf fmt "qword[%a]" pp_reg r

(* A pretty printing for instruction *)
let pp_instr : instruction Fmt.t =
  fun fmt instr ->
  match instr with
  | IMov (a1, a2) -> Fmt.pf fmt "  mov  %a, %a" pp_arg a1 pp_arg a2
  | IAdd (a1, a2) -> Fmt.pf fmt "  add  %a, %a" pp_arg a1 pp_arg a2
  | ISub (a1, a2) -> Fmt.pf fmt "  sub  %a, %a" pp_arg a1 pp_arg a2
  | IMul (a1, a2) -> Fmt.pf fmt "  imul %a, %a" pp_arg a1 pp_arg a2
  | IDiv a        -> Fmt.pf fmt "  idiv %a" pp_arg a
  | ISar (a1, a2) -> Fmt.pf fmt "  sar  %a, %a" pp_arg a1 pp_arg a2
  | IShr (a1, a2) -> Fmt.pf fmt "  shr  %a, %a" pp_arg a1 pp_arg a2
  | ISal (a1, a2) -> Fmt.pf fmt "  sal  %a, %a" pp_arg a1 pp_arg a2
  | IShl (a1, a2) -> Fmt.pf fmt "  shl  %a, %a" pp_arg a1 pp_arg a2
  | ICmp (a1, a2) -> Fmt.pf fmt "  cmp  %a, %a" pp_arg a1 pp_arg a2
  | ITest (a1, a2)-> Fmt.pf fmt "  test %a, %a" pp_arg a1 pp_arg a2
  | IJe  lbl      -> Fmt.pf fmt "  je   %a" pp_arg lbl
  | IJne lbl      -> Fmt.pf fmt "  jne  %a" pp_arg lbl
  | IJz  lbl      -> Fmt.pf fmt "  jz   %a" pp_arg lbl
  | IJnz lbl      -> Fmt.pf fmt "  jnz  %a" pp_arg lbl
  | IJl  lbl      -> Fmt.pf fmt "  jl   %a" pp_arg lbl
  | IJge lbl      -> Fmt.pf fmt "  jge  %a" pp_arg lbl
  | IJg  lbl      -> Fmt.pf fmt "  jg   %a" pp_arg lbl
  | IJmp lbl      -> Fmt.pf fmt "  jmp  %a" pp_arg lbl
  | ILabel lbl    -> Fmt.pf fmt "%a:" Fmt.string lbl
  | ICqo          -> Fmt.pf fmt "  cqo" 
  | IRet          -> Fmt.pf fmt "  ret" 
  | ICall f       -> Fmt.pf fmt "  call %a" pp_arg f
  | IOr (a1, a2)  -> Fmt.pf fmt "  or   %a, %a" pp_arg a1 pp_arg a2
  | IAnd (a1, a2) -> Fmt.pf fmt "  and  %a, %a" pp_arg a1 pp_arg a2
  | IXor (a1, a2) -> Fmt.pf fmt "  xor  %a, %a" pp_arg a1 pp_arg a2
  | IPush x       -> Fmt.pf fmt "  push %a" pp_arg x
  | IPop x        -> Fmt.pf fmt "  pop  %a" pp_arg x
  | IExtern lbl   -> Fmt.pf fmt "extern %s" lbl
  | IEmpty        -> Fmt.pf fmt ""
  | INop          -> Fmt.pf fmt "  nop"
  | IComment comm -> Fmt.pf fmt ";; %s" comm


(* A pretty printing for instruction list *)
let pp_instrs : (instruction list) Fmt.t =
  Fmt.list ~sep:Format.pp_force_newline pp_instr
