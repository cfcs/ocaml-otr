open State

let policy ctx p = List.mem p ctx.config.policies

(* Monadic control-flow core. *)
type error = string
include Control.Or_error_make (struct type err = error end)

let handle_cleartext ctx =
  match ctx.state.message_state with
  | MSGSTATE_PLAINTEXT when policy ctx `REQUIRE_ENCRYPTION -> [`Warning "received unencrypted data"]
  | MSGSTATE_PLAINTEXT -> []
  | MSGSTATE_ENCRYPTED _ | MSGSTATE_FINISHED -> [`Warning "received unencrypted data"]

let commit ctx their_versions =
  match Ake.dh_commit ctx their_versions with
  | Ake.Ok (ctx, out) -> return (ctx, Some out)
  | Ake.Error (Ake.Unknown e) -> fail e
  | Ake.Error Ake.VersionMismatch -> fail "couldn't agree on a version"
  | Ake.Error Ake.InstanceMismatch -> fail "wrong instances"
  | Ake.Error (Ake.Unexpected _) -> fail "unexpected message"

let handle_whitespace_tag ctx their_versions =
  let warn = handle_cleartext ctx in
  (if policy ctx `WHITESPACE_START_AKE then
     commit ctx their_versions
   else
     return (ctx, None) ) >|= fun (ctx, out) ->
  (ctx, out, warn)

let handle_error ctx =
  if policy ctx `ERROR_START_AKE then
    Some (Builder.query_message ctx.config.versions)
  else
    None

let handle_tlv state typ buf =
  let open Packet in
  match typ with
  | Some PADDING -> (state, None, [])
  | Some DISCONNECTED -> ({ state with message_state = MSGSTATE_FINISHED },
                          None,
                          [`Warning "OTR connection lost"])
  | Some EXTRA_SYMMETRIC_KEY -> (state, None, [`Warning "not handling extra symmetric key"])
  | Some (SMP_MESSAGE_1
         | SMP_MESSAGE_2
         | SMP_MESSAGE_3
         | SMP_MESSAGE_4
         | SMP_ABORT
         | SMP_MESSAGE_1Q as smp_type) ->
    ( match Smp.handle_smp state.smp_state smp_type buf with
      | Smp.Ok (smp_state, out, usr) -> ({ state with smp_state }, out, usr)
      | Smp.Error e ->
        let msg = Smp.error_to_string e in
        ({ state with smp_state = SMPSTATE_EXPECT1 }, None, [`Warning msg]) )
  | None -> (state, None, [`Warning "unknown tlv type"])

let rec filter_map ?(f = fun x -> x) = function
  | []    -> []
  | x::xs ->
      match f x with
      | None    ->       filter_map ~f xs
      | Some x' -> x' :: filter_map ~f xs

let handle_tlvs state = function
  | None -> return (state, None, [])
  | Some data ->
    let rec process_data state data out warn =
      match Cstruct.len data with
      | 0 -> (state, out, warn)
      | _ -> match Parser.parse_tlv data with
        | Parser.Ok (typ, buf, rest) ->
          let state, out', warn' = handle_tlv state typ buf in
          process_data state rest (out' :: out) (warn @ warn')
        | Parser.Error _ -> (state, out, [`Warning "ignoring malformed TLV"])
    in
    let state, out, warn = process_data state (Cstruct.of_string data) [] [] in
    let out = match filter_map out with
      | [] -> None
      | xs -> Some (Cstruct.to_string (Nocrypto.Uncommon.Cs.concat xs))
    in
    return (state, out, warn)

let decrypt dh_keys symm version instances bytes =
  match Parser.parse_data bytes with
  | Parser.Ok (version', instances', _flags, s_keyid, r_keyid, dh_y, ctr', encdata, mac, reveal) ->
    if version <> version' then
      return (dh_keys, symm, None, [`Warning "ignoring message with invalid version"])
    else if
      match version, instances, instances' with
      | `V3, Some (mya, myb), Some (youra, yourb) when (mya = youra) && (myb = yourb) -> false
      | `V2, _, _ -> false
      | _ -> true
    then
      return (dh_keys, symm, None, [`Warning "ignoring message with invalid instances"])
    else
      begin
        match Ratchet.check_keys dh_keys s_keyid r_keyid dh_y with
        | Some x -> return (dh_keys, symm, None, [`Warning x])
        | None ->
          let symm, keyblock = Ratchet.keys dh_keys symm s_keyid r_keyid in
          if ctr' <= keyblock.recv_ctr then
            return (dh_keys, symm, None, [`Warning "ignoring message with invalid counter"])
          else
            let stop = Cstruct.len bytes - Cstruct.len reveal - 4 - 20 in
            guard (stop >= 0) "invalid data" >>= fun () ->
            let mac' = Crypto.sha1mac ~key:keyblock.recv_mac (Cstruct.sub bytes 0 stop) in
            guard (Cstruct.equal mac mac') "invalid mac" >|= fun () ->
            let dec = Cstruct.to_string (Crypto.crypt ~key:keyblock.recv_aes ~ctr:ctr' encdata) in
            let txt, data =
              let len = String.length dec in
              let stop =
                try String.index dec '\000'
                with Not_found -> len
              in
              let txt = String.sub dec 0 stop in
              if stop = len || succ stop = len then
                (txt, "")
              else
                let stop' = succ stop in
                (txt, String.sub dec stop' (len - stop'))
            in
            let data = if data = "" then None else Some data in
            let ret = (if txt = "" then [] else [`Received_encrypted txt]) in
            let dh_keys = Ratchet.rotate_keys dh_keys s_keyid r_keyid dh_y
            and symm = Ratchet.set_recv_counter ctr' s_keyid r_keyid symm
            in
            (dh_keys, symm, data, ret)
      end
  | Parser.Error Parser.Underflow -> fail "Malformed OTR data message: parser reported underflow"
  | Parser.Error Parser.LeadingZero -> fail "Malformed OTR data message: parser reported leading zero"
  | Parser.Error (Parser.Unknown x) -> fail ("Malformed OTR data message: " ^ x)

let encrypt dh_keys symm reveal_macs version instances flags data =
  let symm, reveal = Ratchet.reveal dh_keys symm in
  let our_id = Int32.pred dh_keys.our_keyid in
  let symm, keyblock = Ratchet.keys dh_keys symm dh_keys.their_keyid our_id in
  let our_ctr = Int64.succ keyblock.send_ctr in
  let enc = Crypto.crypt ~key:keyblock.send_aes ~ctr:our_ctr (Cstruct.of_string data) in
  let data = Builder.data version instances flags our_id dh_keys.their_keyid (snd dh_keys.dh) our_ctr enc in
  let mac = Crypto.sha1mac ~key:keyblock.send_mac data in
  let reveal =
    let macs = if reveal_macs then
        Nocrypto.Uncommon.Cs.concat (List.map (fun x -> x.recv_mac) reveal)
      else
        Cstruct.create 0
    in
    Builder.encode_data macs
  in
  let out = Nocrypto.Uncommon.Cs.concat [ data ; mac ; reveal] in
  let symm = Ratchet.inc_send_counter dh_keys.their_keyid our_id symm in
  (symm, out)

let wrap_b64string = function
  | None -> None
  | Some m ->
    let encoded = Nocrypto.Base64.encode m in
    Some ("?OTR:" ^ Cstruct.to_string encoded ^ ".")

let handle_data ctx bytes =
  match ctx.state.message_state with
  | MSGSTATE_PLAINTEXT ->
    ( match Ake.handle_auth ctx bytes with
      | Ake.Ok (ctx, out, warn) -> return (ctx, wrap_b64string out, warn)
      | Ake.Error (Ake.Unexpected ignore) ->
        if ignore then
          return (ctx, None, [])
        else
          return (ctx,
                  Some "?OTR Error: ignoring unreadable message",
                  [`Warning "received encrypted data while in plaintext mode, ignoring unreadable message"])
      | Ake.Error (Ake.Unknown x) ->  fail ("AKE error encountered: " ^ x)
      | Ake.Error Ake.VersionMismatch ->
        return (ctx, None, [`Warning "wrong version in message"])
      | Ake.Error Ake.InstanceMismatch ->
        return (ctx, None, [`Warning "wrong instances in message"]) )
  | MSGSTATE_ENCRYPTED enc_data ->
    decrypt enc_data.dh_keys enc_data.symms ctx.version ctx.instances bytes >>= fun (dh_keys, symms, data, ret) ->
    let state = { ctx.state with message_state = MSGSTATE_ENCRYPTED { enc_data with dh_keys ; symms } } in
    handle_tlvs state data >>= fun (state, out, warn) ->
    let state, out = match out with
      | None -> (state, None)
      | Some x ->
        match state.message_state with
        | MSGSTATE_ENCRYPTED enc_data ->
          let symms, out = encrypt enc_data.dh_keys enc_data.symms (reveal_macs ctx) ctx.version ctx.instances false ("\000" ^ x) in
          ({ state with message_state = MSGSTATE_ENCRYPTED { enc_data with symms } },
           wrap_b64string (Some out))
        | _ -> (state, out)
    in
    let ctx = { ctx with state } in
    return (ctx, out, ret @ warn)
  | MSGSTATE_FINISHED ->
    return (ctx, None, [`Warning "received data while in finished state, ignoring"])

(* operations triggered by a user *)
let start_otr ctx =
  (reset_session ctx, Builder.query_message ctx.config.versions)

let send_otr ctx data =
  match ctx.state.message_state with
  | MSGSTATE_PLAINTEXT when policy ctx `REQUIRE_ENCRYPTION ->
    (ctx,
     Some (Builder.query_message ctx.config.versions),
     `Warning ("didn't sent message, there was no encrypted connection: " ^ data))
  | MSGSTATE_PLAINTEXT when policy ctx `SEND_WHITESPACE_TAG ->
    (* XXX: and you have not received a plaintext message from this correspondent since last entering MSGSTATE_PLAINTEXT *)
    (ctx, Some (data ^ (Builder.tag ctx.config.versions)), `Sent data)
  | MSGSTATE_PLAINTEXT -> (ctx, Some data, `Sent data)
  | MSGSTATE_ENCRYPTED enc_data ->
    let symms, out = encrypt enc_data.dh_keys enc_data.symms (reveal_macs ctx) ctx.version ctx.instances false data in
    let state = { ctx.state with message_state = MSGSTATE_ENCRYPTED { enc_data with symms } } in
    let out = wrap_b64string (Some out) in
    ({ ctx with state }, out, `Sent_encrypted data)
  | MSGSTATE_FINISHED ->
     (ctx, None, `Warning ("didn't sent message, OTR session is finished: " ^ data))

let end_otr ctx =
  match ctx.state.message_state with
  | MSGSTATE_PLAINTEXT -> (ctx, None)
  | MSGSTATE_ENCRYPTED enc_data ->
    let data = Cstruct.to_string (Builder.tlv Packet.DISCONNECTED) in
    let _, out = encrypt enc_data.dh_keys enc_data.symms (reveal_macs ctx) ctx.version ctx.instances true ("\000" ^ data) in
    (reset_session ctx, wrap_b64string (Some out))
  | MSGSTATE_FINISHED ->
    (reset_session ctx, None)

let handle_fragment ctx (k, n) frag =
  match k, n, fst ctx.fragments with
  | 1, _, _ -> ({ ctx with fragments = ((k, n), frag) }, None)
  | k, n, (stored_k, stored_n) when n = stored_n && k = succ stored_k && n = k ->
    (* last fragment *)
    let full = (snd ctx.fragments) ^ frag in
    (rst_frag ctx, Some full)
  | k, n, (stored_k, stored_n) when n = stored_n && k = succ stored_k ->
    ({ ctx with fragments = ((k, n), (snd ctx.fragments) ^ frag) }, None)
  | _ -> (rst_frag ctx, None)

let handle_fragment_v3 ctx instances kn frag =
  match ctx.instances, instances with
  | Some (a, b), (a', b') when (a = a' && b = b') || b' = 0l ->
      handle_fragment ctx kn frag
  | _ -> (ctx, None)

let recv text = match text with None -> [] | Some x -> [ `Received x ]

let handle_input ctx = function
  | `PlainTag (versions, text) ->
    ( match handle_whitespace_tag ctx versions with
      | Ok (ctx, out, warn) ->
        (ctx, wrap_b64string out, warn @ recv text)
      | Error e ->
        (reset_session ctx,
         Some ("?OTR Error: " ^ e),
         [`Warning ("OTR Error: " ^ e)] @ recv text) )
  | `Query versions ->
    ( match commit ctx versions with
      | Ok (ctx, out) -> (ctx, wrap_b64string out, [])
      | Error e -> (reset_session ctx,
                    Some ("?OTR Error: " ^ e),
                    [`Warning ("OTR Error: " ^ e)] ) )
  | `Error message ->
    let out = handle_error ctx in
    (reset_session ctx, out,
     [`Received_error ("Received OTR Error: " ^ message)])
  | `Data bytes ->
    ( match handle_data ctx bytes with
      | Ok (ctx, out, warn) ->
        (ctx, out, warn)
      | Error e ->
        (reset_session ctx,
         Some ("?OTR Error: " ^ e),
         [ `Warning ("OTR error " ^ e)]) )
  | `String message ->
    let user = handle_cleartext ctx in
    (ctx, None, user @ recv (Some message))
  | `ParseError err ->
    (reset_session ctx,
     Some ("?OTR Error: " ^ err),
     [`Warning (err ^ " while processing OTR message")])
  | `Fragment_v2 _ | `Fragment_v3 _ ->
    (reset_session ctx,
     Some ("?OTR Error: unexpected recursive fragment"),
     [`Warning "ignoring unexpected recursive fragment"])

let handle_fragments ctx = function
  | `Fragment_v2 (kn, piece) ->
    if ctx.version = `V2 then
      return (handle_fragment ctx kn piece)
    else
      fail ("?OTR Error: wrong version in fragment")
  | `Fragment_v3 (instances, kn, piece) ->
    if ctx.version = `V3 then
      return (handle_fragment_v3 ctx instances kn piece)
    else
      fail ("?OTR Error: wrong version in fragment")

(* session -> string -> (session * to_send * ret) *)
let handle ctx bytes =
  match Parser.classify_input bytes with
  | `Fragment_v2 _ | `Fragment_v3 _ as f ->
    ( match handle_fragments ctx f with
      | Ok (ctx, None)   -> (ctx, None, [])
      | Ok (ctx, Some x) -> handle_input ctx (Parser.classify_input x)
      | Error txt -> (ctx,
                      Some ("?OTR Error: " ^ txt),
                      [`Warning ("Error: " ^ txt)]) )
    | x -> handle_input (rst_frag ctx) x

let handle_smp ctx call =
  let enc enc_data out smp_state =
    let data = "\000" ^ (Cstruct.to_string out) in
    let symms, out = encrypt enc_data.dh_keys enc_data.symms (reveal_macs ctx) ctx.version ctx.instances false data in
    let message_state = MSGSTATE_ENCRYPTED { enc_data with symms } in
    let state = { ctx.state with message_state ; smp_state } in
    ({ ctx with state }, wrap_b64string (Some out))
  in
  match ctx.state.message_state with
  | MSGSTATE_ENCRYPTED enc_data -> ( match call enc_data ctx.state.smp_state with
      | Smp.Ok (smp_state, Some out) ->
        let st, out = enc enc_data out smp_state in
        (st, out, [])
      | Smp.Ok (smp_state, None) ->
        let state = { ctx.state with smp_state } in
        ({ ctx with state }, None, [])
      | Smp.Error e ->
        let out = Builder.tlv Packet.SMP_ABORT in
        let st, out = enc enc_data out SMPSTATE_EXPECT1 in
        let err = Smp.error_to_string e in
        (st, out, [`Warning err]) )
  | _ -> (ctx, None, [`Warning "need an encrypted session for SMP"])

let start_smp ctx ?question secret =
  handle_smp ctx (fun enc smp -> Smp.start_smp ctx.dsa enc smp ?question secret)

let abort_smp ctx =
  handle_smp ctx (fun _ smp -> Smp.abort_smp smp)

let answer_smp ctx secret =
  handle_smp ctx (fun enc smp -> Smp.handle_secret ctx.dsa enc smp secret)
