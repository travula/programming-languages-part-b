
datatype int_or_string = I of int | S of string | T of int*string

(* int_or_string list -> int *)
fun funny_sum xs =
    case xs of
        [] => 0
      | (I i)::xs' => i + funny_sum xs'
      | (S s)::xs' => String.size s + funny_sum xs'
      | (T (i,s))::xs' => String.size s + i + funny_sum xs';

funny_sum [(S "hello"), (I 10), (T(2,"hi"))]; (* 5 + 10 + 2 + 2 = 19 *)

datatype exp = Constant of int
             | Negate of exp
             | Add of exp * exp
             | Multiply of exp * exp;

val exp1 = Negate (Constant 19);
val exp2 = Add ( Negate (Constant 10), Constant 10);
val exp3 = (Multiply ( Add ( (Negate (Constant 10)), Constant 10), Constant 10));
val exp4 = Add (Constant 10, Multiply(Add(Constant 1, Constant 3), Constant 4));
val exp5 = Add (Negate(Constant 10), Multiply(Add(Negate(Constant 1), Constant 3), Constant 4));

fun eval_exp_old e =
    case e of
        Constant i => i
      | Negate e1 => ~ (eval_exp_old e1)
      | Add (e1, e2) => (eval_exp_old e1) + (eval_exp_old e2)
      | Multiply (e1, e2) => (eval_exp_old e1) * (eval_exp_old e2);

eval_exp_old exp3;
eval_exp_old exp2;
eval_exp_old exp1;

exception Error of string;
fun eval_exp_new e =
    let
      fun get_int e =
          case e of
              Constant i => i
            | _ => raise (Error "expected Const result")
    in
      case e of
          Constant _ => e
        | Negate e1 => Constant (~ (get_int(eval_exp_new e1)))
        | Add (e1, e2) => Constant ((get_int (eval_exp_new e1)) + (get_int (eval_exp_new e2)))
        | Multiply (e1, e2) => Constant ((get_int (eval_exp_new e1)) * (get_int (eval_exp_new e2)))
    end;

eval_exp_new exp5;
eval_exp_new exp4;;
