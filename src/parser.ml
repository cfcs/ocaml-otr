
open Packet
open Cstruct
open State
open Result

type error =
  | Unknown of string
  | Underflow
  | LeadingZero

include Control.Or_error_make (struct type err = error end)
type 'a result = ('a, error) Result.result

exception Parser_error of error

let raise_unknown msg        = raise (Parser_error (Unknown msg))

let catch f x =
  try return (f x) with
  | Parser_error err   -> fail err
  | Invalid_argument _ -> fail Underflow

let maybe a =
  if a = "" then None else Some a

(* parse query string *)
let parse_query_exn str =
  let parse_v acc = function
    | '2' -> `V2 :: acc
    | '3' -> `V3 :: acc
    | _ -> acc
  in
  let parse idx =
    let _, left = Astring.String.span ~max:idx str in
    match Astring.String.cut ~sep:"?" left with
    | None -> ([], None)
    | Some (vs, post) ->
      let versions = Astring.String.fold_left parse_v [] vs in
      (List.rev versions, maybe post)
  in
  match String.get str 0, String.get str 1 with
  | '?', 'v' -> parse 2
  | 'v', _ -> parse 1
  | _ -> raise_unknown "no usable version found"

let parse_query = catch parse_query_exn

let mark_match sep data =
  match Astring.String.cut ~sep data with
  | Some (pre, post) -> Ok (maybe pre, post)
  | None -> Error (Unknown "parse failed")

open Sexplib.Conv

type ret = [
  | `Data of Cstruct.t
  | `ParseError of string
  | `Error of string
  | `PlainTag of State.version list * string option
  | `Query of State.version list
  | `String of string
  | `Fragment_v2 of (int * int) * string
  | `Fragment_v3 of (int32 * int32) * (int * int) * string
] [@@deriving sexp]

let parse_data_exn data =
  match Astring.String.cut ~sep:"." data with
  | None -> raise_unknown "empty OTR message"
  | Some (data, rest) ->
    let b64data = Cstruct.of_string data in
    match Nocrypto.Base64.decode b64data with
    | None -> raise_unknown "bad base64 data"
    | Some x -> (x, maybe rest)

let parse_data = catch parse_data_exn

let parse_plain_tag_exn data =
  let rec find_mark str acc =
    if String.length str < 8 then
      (List.rev acc, maybe str)
    else
      let tag, rest = Astring.String.span ~max:8 str in
      if tag = tag_v2 then
        find_mark rest (`V2 :: acc)
      else if tag = tag_v3 then
        find_mark rest (`V3 :: acc)
      else
        find_mark rest acc
  in
  find_mark data []

let parse_plain_tag = catch parse_plain_tag_exn


let parse_fragment_exn data =
  match Astring.String.cuts ~sep:"," data with
  | k :: n :: piece :: rest ->
    let k = int_of_string k in
    let n = int_of_string n in
    assert (k > 0 && k <= 65535 ) ;
    assert (n > 0 && n <= 65535 && k <= n) ;
    assert (String.length piece > 0) ;
    assert (String.length (String.concat "" rest) = 0) ;
    ((k, n), piece)
  | _ -> raise_unknown "invalid fragment"

let parse_fragment = catch parse_fragment_exn

let parse_fragment_v3_exn data =
  match Astring.String.cut ~sep:"|" data with
  | Some (sender_instance, data) ->
    ( match Astring.String.cut ~sep:"," data with
      | Some (receiver_instance, data) ->
        let sender_instance = Scanf.sscanf sender_instance "%lx" (fun x -> x) in
        let receiver_instance = Scanf.sscanf receiver_instance "%lx" (fun x -> x) in
        let kn, piece = parse_fragment_exn data in
        ((sender_instance, receiver_instance), kn, piece)
      | None -> raise_unknown "invalid fragment (receiver_instance)" )
  | None -> raise_unknown "invalid fragment (sender_instance)"

let parse_fragment_v3 = catch parse_fragment_v3_exn

let classify_input bytes =
  match mark_match otr_v2_frag bytes with
  | Ok (pre, data) ->
    begin match parse_fragment data with
      | Ok data when pre = None -> `Fragment_v2 data
      | Ok _ -> `ParseError "Malformed v2 fragment (predata)"
      | Error _ -> `ParseError "Malformed v2 fragment"
    end
  | Error _ -> match mark_match otr_v3_frag bytes with
    | Ok (pre, data) ->
      begin match parse_fragment_v3 data with
        | Ok data when pre = None -> `Fragment_v3 data
        | Ok _ -> `ParseError "Malformed v3 fragment (predata)"
        | Error _ -> `ParseError "Malformed v3 fragment"
      end
    | Error _ -> match mark_match otr_mark bytes with
      | Ok (pre, data) ->
        begin match parse_data data with
          | Ok (data, post) when pre = None && post = None -> `Data data
          | Ok _ -> `ParseError "Malformed OTR data (pre/postdata)"
          | Error _ -> `ParseError "Malformed OTR data message"
        end
      | Error _ -> match mark_match otr_err_mark bytes with
        | Ok (pre, data) when pre = None -> `Error data
        | Ok _ -> `ParseError "Malformed Error received (predata)"
        | Error _ ->  match mark_match otr_prefix bytes with
          | Ok (pre, data) ->
            begin match parse_query data with
              | Ok (versions, _) when pre = None -> `Query versions
              | Ok _ -> `ParseError "Malformed OTR query (pre/postdata)"
              | Error _ -> `ParseError "Malformed OTR query"
            end
          | Error _ -> match mark_match tag_prefix bytes with
            | Ok (pre, data) ->
              begin match parse_plain_tag data with
                | Ok (versions, None) -> `PlainTag (versions, pre)
                | Ok _ -> `ParseError "Malformed Tag (postdata)"
                | Error _ -> `ParseError "Malformed tag"
              end
            | Error _ -> `String bytes

(* real OTR data parsing *)
let decode_data_exn buf =
  let size = BE.get_uint32 buf 0 in
  let intsize = Int32.to_int size in
  (sub buf 4 intsize, shift buf (4 + intsize))

let decode_data = catch decode_data_exn

let parse_gy data =
  decode_data data >>= fun (gy, rst) ->
  guard (len rst = 0) Underflow >>= fun () ->
  guard (get_uint8 gy 0 <> 0) LeadingZero >|= fun () ->
  gy

let parse_header bytes =
  catch (BE.get_uint16 bytes) 0 >>= fun ver ->
  ( match version_of_int ver with
    | None -> fail (Unknown "version")
    | Some v -> return v ) >>= fun version ->
  catch (get_uint8 bytes) 2 >>= fun typ ->
  ( match int_to_message_type typ with
    | Some x -> return x
    | None -> fail (Unknown "message type") ) >>= fun typ ->
  ( match version with
    | `V2 -> return (None, shift bytes 3)
    | `V3 ->
      catch (BE.get_uint32 bytes) 3 >>= fun mine ->
      catch (BE.get_uint32 bytes) 7 >|= fun their ->
      (Some (mine, their), shift bytes 11) ) >|= fun (instances, buf) ->
  (version, typ, instances, buf)

let parse_signature_data buf =
  catch (split buf) 2 >>= fun (tag, buf) ->
  guard (BE.get_uint16 tag 0 = 0) (Unknown "key tag != 0") >>= fun () ->
  decode_data buf >>= fun (p, buf) ->
  guard (get_uint8 p 0 <> 0) LeadingZero >>= fun () ->
  decode_data buf >>= fun (q, buf) ->
  guard (get_uint8 q 0 <> 0) LeadingZero >>= fun () ->
  decode_data buf >>= fun (gg, buf) ->
  guard (get_uint8 gg 0 <> 0) LeadingZero >>= fun () ->
  decode_data buf >>= fun (y, buf) ->
  guard (get_uint8 y 0 <> 0) LeadingZero >>= fun () ->
  let key = Crypto.OtrDsa.pub ~p ~q ~gg ~y in
  catch (BE.get_uint32 buf) 0 >>= fun keyida ->
  let buf = shift buf 4 in
  guard (len buf = 40) (Unknown "signature length") >|= fun () ->
  let siga = split buf 20 in
  (key, keyida, siga)

let parse_reveal buf =
  decode_data buf >>= fun (r, buf) ->
  decode_data buf >>= fun (enc_data, mac) ->
  guard (len mac = 20) (Unknown "wrong mac length") >|= fun () ->
  (r, enc_data, mac)

let parse_dh_commit buf =
  decode_data buf >>= fun (gxenc, buf) ->
  decode_data buf >>= fun (hgx, buf) ->
  guard ((len buf = 0) && (len hgx = 32)) (Unknown "bad dh_commit") >|= fun () ->
  (gxenc, hgx)

let parse_data_body buf =
  catch (get_uint8 buf) 0 >>= fun flags ->
  catch (BE.get_uint32 buf) 1 >>= fun s_keyid ->
  catch (BE.get_uint32 buf) 5 >>= fun r_keyid ->
  decode_data (shift buf 9) >>= fun (dh_y, buf) ->
  guard (get_uint8 dh_y 0 <> 0) LeadingZero >>= fun () ->
  catch (BE.get_uint64 buf) 0 >>= fun ctr ->
  decode_data (shift buf 8) >>= fun (encdata, buf) ->
  catch (sub buf 0) 20 >>= fun mac ->
  decode_data (shift buf 20) >>= fun (reveal, buf) ->
  guard (len buf = 0) Underflow >|= fun () ->
  let flags = if flags = 1 then true else false in
  (flags, s_keyid, r_keyid, dh_y, ctr, encdata, mac, reveal)

let parse_data buf =
  parse_header buf >>= fun (version, typ, instances, buf) ->
  guard (typ = DATA) (Unknown "type") >>= fun () ->
  parse_data_body buf >|= fun (flags, s_keyid, r_keyid, dh_y, ctr, encdata, mac, reveal) ->
  (version, instances, flags, s_keyid, r_keyid, dh_y, ctr, encdata, mac, reveal)

let parse_tlv_exn buf =
  let typ = BE.get_uint16 buf 0 in
  let l = BE.get_uint16 buf 2 in
  (int_to_tlv_type typ, sub buf 4 l, shift buf (4 + l))

let parse_tlv = catch parse_tlv_exn

let parse_datas buf n =
  let rec p_data buf acc = function
    | 0 when len buf = 0 -> return (List.rev acc)
    | 0 -> fail Underflow
    | n ->
      decode_data buf >>= fun (x, buf) ->
      guard (get_uint8 x 0 <> 0) LeadingZero >>= fun () ->
      p_data buf (x :: acc) (pred n)
  in
  catch (BE.get_uint32 buf) 0 >>= fun cnt ->
  if cnt = Int32.of_int n then
    p_data (shift buf 4) [] n
  else
    fail Underflow
