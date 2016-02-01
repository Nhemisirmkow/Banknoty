let banknoty k l =
let t_1 = Array.make (k + 1) (-1) in
let t_2 = Array.make (k + 1) (-1) in
let rec wypelnij l t_a t_b =
  match l with
  | [] -> t_a.(k)
  | g::o ->
     (
       let pomoc = Array.make g (-1) in
         for i = 0 to k do
	   if pomoc.(i mod g) = -1 then (t_b.(i) <- t_a.(i);)
	   else
		if t_a.(i) = -1 then (t_b.(i) <- (pomoc.(i mod g) + 1);)
		else (t_b.(i) <- (min (pomoc.(i mod g) + 1) t_a.(i)););
	   if t_a.(i) >= 0 then
	     (
               if pomoc.(i mod g) = -1 then (pomoc.(i mod g) <- t_a.(i);)
	       else (pomoc.(i mod g) <- (min (pomoc.(i mod g)) t_a.(i));)
	     );
	 done;
       wypelnij o t_b t_a;
     )
  in
    (
      t_1.(0) <- 0;
      wypelnij l t_1 t_2;
    );;


(* dopisalem jedynie średnik po done, bo go nie było, przez co się nie kompilował*)
(* wiem, że na końcach linii straszny bajzel zrobiłem z tych nawiasów na pracy   *)
(* nie mniej jednak, przepisałem co do znaku tak jak było w pracy i działa       *)
(* ten nadmiar średników (który nie wpływa na kompilacje ani rozwiązanie) jest   *)
(* wynikiem stresu.                                                              *) 
	 
