
open Ast

(*let rec imprimir_re = function
| Vazio -> "Vazio"
| Caracter c -> "Carácter " ^ String.make 1 c
| Concatenacao(a, b) -> "Concatenação("^imprimir_re a^", "^imprimir_re b^")"
| Escolha(a, b) -> "Escolha("^imprimir_re a^", "^imprimir_re b^")"
| Estrela(a) -> "Estrela("^imprimir_re a^")"*)

(* deve efetuar as definições das funções ou submódulos aqui ... *)

(* definição do tipo automato *)

type estado = int
type simbolo = char option 
type transicao = ((estado * simbolo) * estado)
type automato = (estado list * estado list * transicao list)

let count = ref 0

let novo_estado () =
  incr count; !count

let rec er_para_nfa : re -> automato = function
  | Vazio ->
    let q0 = novo_estado () in
    let t = [] in
    ([q0], [q0], t)

  | Caracter c ->
    let q0 = novo_estado () in
    let q1 = novo_estado () in
    let t = [((q0, Some c), q1)] in
    ([q0], [q1], t)

  | Concatenacao (a, b) ->
    let (ib, fb, tb) = er_para_nfa b in
    let (ia, fa, ta) = er_para_nfa a in
    let t = (List.concat (List.map (fun f -> List.map (fun i -> ((f, None), i)) ib) (List.rev fa))) @ ta @ tb in
    (ia, fb, t)

  | Escolha (a, b) ->
    let (ib, fb, tb) = er_para_nfa b in
    let (ia, fa, ta) = er_para_nfa a in
    let q0 = novo_estado () in
    let t = List.map (fun i -> ((q0, None), i)) ia @ List.map (fun i -> ((q0, None), i)) ib @ ta  @ tb in
    ([q0], fa @ fb, t)

  | Estrela a ->
    let (ia, fa, ta) = er_para_nfa a in
    let q0 = novo_estado () in
    let ti = List.map (fun f -> ((q0, None), f)) ia in
    let tf = List.concat (List.map (fun f -> List.map(fun q -> ((f, None), q)) ia) (List.rev fa)) in
    let t = ti @ tf @ ta in
    let qf = [q0] @ fa in
    ([q0], qf, t)

(* função para imprimir o automato *)
let imprimir_automato (inicial, final, transicoes) =
  begin
    (* Numero de estados totais *)
    Printf.printf "%d\n" !count;

    (* Numero de estados iniciais *)
    Printf.printf "%d\n" (List.length inicial);

    (* Estados iniciais *)
    List.iteri (fun i e -> 
      Printf.printf "%d" e;
      if i < List.length inicial - 1 then Printf.printf " "
    ) inicial;
    Printf.printf "\n";

    (* Numero de estados finais *)
    Printf.printf "%d\n" (List.length final);

    (* Estados finais *)
    List.iteri (fun i e -> 
      Printf.printf "%d" e;
      if i < List.length final - 1 then Printf.printf " "
    ) final;
    Printf.printf "\n";

    (* Numero de transições *)
    Printf.printf "%d\n" (List.length transicoes);

    (* Transições *)
    List.iter (fun ((e1, s), e2) ->
      if Option.is_none s then
        Printf.printf "%d _ %d\n" e1 e2
      else
        Printf.printf "%d %c %d\n" e1 (Option.get s) e2;
  ) transicoes
  
  end

(* função Main *)
let () =
  let s = read_line () in
  let r = Parser.main Lexer.token (Lexing.from_string s) in
  (*print_endline (imprimir_re r);*)

  let nfa = er_para_nfa r in
  imprimir_automato nfa