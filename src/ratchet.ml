open State

let check_keys dh_keys send recv gy =
  match
    dh_keys.their_keyid = send,
    dh_keys.their_keyid = Int32.succ send,
    dh_keys.our_keyid = recv,
    dh_keys.our_keyid = Int32.succ recv
  with
  | false, false, _    , _     -> Some "wrong send keyid"
  | _    , _    , false, false -> Some "wrong receive keyid"
  | _    , _    , _    , _     ->
    match
      Crypto.check_gy gy,
      dh_keys.their_keyid = Int32.succ send,
      Cstruct.len dh_keys.previous_gy = 0
    with
    | true, _   , _    -> Some "invalid gy"
    | _   , true, true -> Some "invalid previous gy"
    | _   , _   , _    -> None

let rotate_our_keys dhs recv =
  if dhs.our_keyid = recv then
    { dhs with our_keyid = Int32.succ dhs.our_keyid ;
               previous_dh = dhs.dh ;
               dh = Crypto.gen_dh_secret () }
  else
    dhs

let rotate_their_keys dhs send dh_y =
  if dhs.their_keyid = send then
    { dhs with their_keyid = Int32.succ send ;
               previous_gy = dhs.gy ;
               gy = dh_y }
  else
    dhs

let rotate_keys dh_keys send recv dh_y =
  rotate_their_keys (rotate_our_keys dh_keys recv) send dh_y

let setup_keys (dh_secret, gx) gy =
  let high = Crypto.mpi_gt gx gy
  and shared = Crypto.dh_shared_exn dh_secret gy
  in
  let send_aes, send_mac, recv_aes, recv_mac = Crypto.data_keys shared high in
  { send_aes ; send_mac ; send_ctr = 0L ; recv_aes ; recv_mac ; recv_ctr = 0L }

let find_keys keylist send recv =
  let rec go = function
    | [] -> None
    | (s, r, ks)::_ when s = send && r = recv -> Some ks
    | _::xs -> go xs
  in
  go keylist

let keys dh_keys symm_keys send recv =
  match find_keys symm_keys send recv with
  | None ->
    let gy = if dh_keys.their_keyid = send then dh_keys.gy else dh_keys.previous_gy
    and dh = if dh_keys.our_keyid = recv then dh_keys.dh else dh_keys.previous_dh
    in
    let symm = setup_keys dh gy in
    ((send, recv, symm)::symm_keys, symm)
  | Some x ->
    (symm_keys, x)

let rec set_recv_counter send recv newctr = function
  | (s, r, k)::xs when s = send && r = recv -> (s, r, { k with recv_ctr = newctr })::xs
  | x::xs -> x :: (set_recv_counter send recv newctr xs)
  | [] -> []

let rec inc_send_counter send recv = function
  | (s, r, k)::xs when s = send && r = recv -> (s, r, { k with send_ctr = Int64.succ k.send_ctr })::xs
  | x::xs -> x :: (inc_send_counter send recv xs)
  | [] -> []

let rec erase_keys p = function
  | [] -> ([], [])
  | x::xs when p x ->
    let keep, destroy = erase_keys p xs in
    (keep, x :: destroy)
  | x::xs ->
    let keep, destroy = erase_keys p xs in
    (x :: keep, destroy)

let erase_recv_keys recv =
  erase_keys (fun (_, r, _) -> r = recv)

let erase_send_keys send =
  erase_keys (fun (s, _, _) -> s = send)

let reveal dh_keys symm =
  let recv = Int32.pred (Int32.pred dh_keys.our_keyid)
  and send = Int32.pred (Int32.pred dh_keys.their_keyid)
  in
  let symm, erased = erase_recv_keys recv symm in
  let symm, erased2 = erase_send_keys send symm in
  let third (_, _, x) = x in
  (symm, List.map third (erased @ erased2))