
let mem f =
  let t = Hashtbl.create 100 in
  fun x ->
    try Hashtbl.find t x with
    | Not_found ->
        let r = f x in ( Hashtbl.add t x r ; r )


(* An [admittedly primitive] implementation of Pollards p-1 factoring method. *)

module Pollard = struct

  let primes_to n =
    let rec scan = function
      | p when p > n -> []
      | p            -> p :: scan Z.(nextprime p) in
    scan (Z.of_int 2)

  let max_pow limit x =
    let rec expand lower upper =
      if Z.(pow x upper) > limit then (lower, upper)
      else expand upper (upper * 2)
    and narrow lower upper =
      if upper - lower = 1 then lower else
        let mid = (lower + upper) / 2 in
        if Z.(pow x mid) > limit then
          narrow lower mid
        else narrow mid upper
    in
    let (l, u) = expand 1 2 in
    narrow l u

  let ppowers_to n =
    let rec scan = function
      | p when p > n -> []
      | p ->
          let pp = Z.pow p (max_pow n p) in
          pp :: scan Z.(nextprime p) in
    scan (Z.of_int 2)

  let note ~msg f =
    Printf.printf "[%s] ->\n%!" msg ;
    let r = f () in
    Printf.printf "[%s] <-\n%!" msg ;
    r

  let prime_pows_to_prod = mem @@ fun n ->
    let rec scan acc = function
      | p when p > n -> acc
      | p -> scan Z.(acc * (pow p (max_pow n p)))
                  Z.(nextprime p) in
    note ~msg:"powers" @@ fun () ->
      scan Z.one Z.(of_int 2)

  let split ~limit n =
    let a = Nocrypto.Rng.Z.gen n in
    match Z.gcd n a with
    | d when d > Z.one -> a
    | _ ->
        let rec scan a m =
          let x = Z.(powm a (m * n) n) in
          if Z.(x = one) then
            if Z.(m mod of_int 2 = zero) then
              scan a Z.(m / of_int 2)
            else raise Not_found
          else
            let d = Z.(gcd (x - one) n) in
            if Z.(d > one) then d else raise Not_found
        in
        scan a (prime_pows_to_prod limit)

end

module RSA_misc = struct

  let slack = 8

  (* Rivest's p-minus strong prime generator. *)

  let rec pm_strong_prime ?g ~bits =
    let a_lim = Z.(pow (of_int 2) slack - one)
    in
    let rec mul_seq p = function
      | a when a > a_lim ->
          Printf.printf "++ mul seq: falling off the cliff.\n%!";
          None
      | a ->
          let p' = Z.(a * p + one) in
          match Z.probab_prime p' 25 with
          | 0 ->
              Printf.printf "+ mul seq: climb.\n%!";
              mul_seq p Z.(a + (of_int 2))
          | _ ->
              Printf.printf "** mul seq: prime with %s\n%!" Z.(to_string a);
              Some p'
    in
    let pmm = Nocrypto.Rng.prime ?g bits in
    match mul_seq pmm (Z.of_int 2) with
    | None    -> pm_strong_prime ?g ~bits
    | Some pm ->
        match mul_seq pm (Z.of_int 2) with
        | None   -> pm_strong_prime ?g ~bits
        | Some p -> (pmm, pm, p)

  let slim = Z.(pow (of_int 2) 8)

  (* Williams/Schmid strong prime generator. *)

  let rec p_strong_prime1 ?g ~bits =
    let (bits1, bits2) = (bits / 2, bits - bits / 2)
    in
    let pmm = Nocrypto.Rng.prime ?g bits1
    and pp  = Nocrypto.Rng.prime ?g bits2 in
    let r   = Z.(pp - invert pmm pp)
    in
    let rec find_a = function
      | a when a >= slim ->
          Printf.printf "off the cliff...\n%!" ;
          p_strong_prime1 ?g ~bits
      | a ->
          let pm = Z.((of_int 2) * a * pmm * pp + (of_int 2) * r * pmm + one) in
          match Z.probab_prime pm 25 with
          | 0 -> find_a Z.(a + one)
          | _ ->
              let p = Z.((of_int 2) * pm + one) in
              match Z.probab_prime p 25 with
              | 0 -> find_a Z.(a + one)
              | _ ->
                  Printf.printf "found pm, p with %s\n%!" Z.(to_string a);
                  (pmm, pm, pp, p)
    in
    find_a Z.(of_int 2)

end
