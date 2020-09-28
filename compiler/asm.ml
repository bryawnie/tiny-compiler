(* registers *)
type reg = 
| RAX
| RBX
| RDX
| RSP
| R11


(* arguments for instructions *)
type arg =
| Const of int64          (* Constant of 64 bits *)
| Reg of reg              (* Register *)
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
| ISal of arg * arg   (* Arithmetic Left Shift *)
| ICmp of arg * arg   (* Comparer *)
| IJe  of string      (* Jumps if equal *)
| IJl  of string      (* Jumps if less than *)
| IJmp of string      (* Makes a jump *)
| ILabel of string    (* Simple Label *)
| IRet                (* Return *)


(* A pretty printing for registers *)
let pp_reg : reg Fmt.t =
  fun fmt r ->
    match r with
    | RAX -> Fmt.string fmt "RAX"
    | RBX -> Fmt.string fmt "RBX"
    | RDX -> Fmt.string fmt "RDX"
    | RSP -> Fmt.string fmt "RSP"
    | R11 -> Fmt.string fmt "R11"


(* A pretty printing for args *)
let pp_arg : arg Fmt.t =
  fun fmt arg ->
    match arg with
    | Const n         -> Fmt.pf fmt "%#Lx" n
    | Reg r           -> pp_reg fmt r
    | RegOffset (r,i) -> Fmt.pf fmt "[%a - %a]" pp_reg r Fmt.int (8*i)


(* A pretty printing for instruction *)
let pp_instr : instruction Fmt.t =
  fun fmt instr ->
  match instr with
  | IMov (a1, a2) -> Fmt.pf fmt "   mov  %a, %a" pp_arg a1 pp_arg a2
  | IAdd (a1, a2) -> Fmt.pf fmt "   add  %a, %a" pp_arg a1 pp_arg a2
  | ISub (a1, a2) -> Fmt.pf fmt "   sub  %a, %a" pp_arg a1 pp_arg a2
  | IMul (a1, a2) -> Fmt.pf fmt "   imul %a, %a" pp_arg a1 pp_arg a2
  | IDiv a        -> Fmt.pf fmt "   idiv %a" pp_arg a
  | ISar (a1, a2) -> Fmt.pf fmt "   sar  %a, %a" pp_arg a1 pp_arg a2
  | ISal (a1, a2) -> Fmt.pf fmt "   sal  %a, %a" pp_arg a1 pp_arg a2
  | ICmp (a1, a2) -> Fmt.pf fmt "   cmp  %a, %a" pp_arg a1 pp_arg a2
  | IJe  lbl      -> Fmt.pf fmt "   je   %a" Fmt.string lbl
  | IJl  lbl      -> Fmt.pf fmt "   jl   %a" Fmt.string lbl
  | IJmp lbl      -> Fmt.pf fmt "   jmp  %a" Fmt.string lbl
  | ILabel lbl    -> Fmt.pf fmt "%a:" Fmt.string lbl
  | IRet          -> Fmt.pf fmt "   ret" 
  | IOr (a1, a2)  -> Fmt.pf fmt "   or   %a, %a" pp_arg a1 pp_arg a2
  | IAnd (a1, a2) -> Fmt.pf fmt "   and  %a, %a" pp_arg a1 pp_arg a2
  | IXor (a1, a2) -> Fmt.pf fmt "   xor  %a, %a" pp_arg a1 pp_arg a2


(* A pretty printing for instruction list *)
let pp_instrs : (instruction list) Fmt.t =
  Fmt.list ~sep:Format.pp_force_newline pp_instr
