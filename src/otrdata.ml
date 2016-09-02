(* Implementation of the "Simple File Transfer" protocol from
https://dev.guardianproject.info/projects/gibberbot/wiki/OTRDATA_Specifications
 *)

open Result
open Astring

type offer_request = { offer_request_id : string
                     ; offer_path : string (*naming is stupid because disambiguation syntax is impossible to remember *)
                     ; file_length: int64
                     ; sha1: string }

type get_request = { get_request_id : string
                   ; range_start : int64
                   ; range_end : int64
                   ; path : string }

type request =
  | OFFER_request of offer_request
  | GET_request of get_request

type response = { status_line: string
                ; request_id: string
                ; body: string }

type header =
  | Request_Id
  | File_Length
  | File_Hash_SHA1
  | Range

type otrdata_request_warning =
  | Request_cant_split_header_and_body
  | Invalid_file_length
  | Invalid_range_fields
  | Message_too_long
  | Missing_header
  | Missing_range
  | Missing_request_id
  | Offer_invalid_storage
  | Unknown_verb

type otrdata_response_warning =
  | Response_cant_split_header_and_body
  | Invalid_response
  | Response_message_too_long

let string_of_otrdata_request_warning = function
  | Request_cant_split_header_and_body -> "Request_cant_split_header_and_body"
  | Invalid_file_length -> "Invalid_file_length"
  | Invalid_range_fields -> "Invalid_range_fields"
  | Message_too_long -> "Message_too_long"
  | Missing_header -> "Missing_header"
  | Missing_range -> "Missing_range"
  | Missing_request_id -> "Missing_request_id"
  | Offer_invalid_storage -> "Offer_invalid_storage"
  | Unknown_verb -> "Unknown_verb"

let string_of_otrdata_response_warning = function
  | Response_cant_split_header_and_body -> "Response_cant_split_header_and_body"
  | Invalid_response -> "Invalid_response"
  | Response_message_too_long -> "Response_message_too_long"

let get_header needle lst =
  let rec handle_lines =
      function
      | [] -> None
      | hd::tl ->
         begin match String.cut ~rev:false ~sep:" " hd with
         (* note: this doesn't allow spaces in values (no messages in the spec requires spaces) *)
         | Some ( key , value ) ->
           begin match needle, key with
           | Request_Id , "Request-Id:"
             | File_Length , "File-Length:"
             | File_Hash_SHA1 , "File-Hash-SHA1:"
             | Range , "Range:"
             -> Some value
           | _ -> handle_lines tl
           end
         | None -> None
         end
    in handle_lines lst

let split_header_and_body msg =
  if 65535 < String.length msg then
    Error `Message_too_long
  else
    let body_pos = begin match String.find_sub ~rev:false ~start:0 ~sub:"\n\n" msg with
                   | None -> String.length msg
                   | Some body_pos -> body_pos end in
    let header , body = (String.span ~max:body_pos msg) in
    let headers = String.fields ~is_sep:(fun c -> c = '\n') header in
    begin match headers with
    | method_or_status :: headers ->
       Ok (method_or_status , headers , body)
    | _ -> Error `Cant_split_header_and_body
    end

let strip_otr_in_band_magic_prefix uri =
    let magic_prefix = "otr-in-band:/storage/" in
    let their_prefix , offered_path =
      String.(span ~max:(length magic_prefix) uri) in
    if their_prefix <> magic_prefix
    then Error Offer_invalid_storage
    else Ok offered_path

let handle_otrdata_request msg =
(* handle a request TLV tag *)
(* handled verbs: OFFER; GET *)
  let handle_offer uri headers =
    (* note: extraneous headers currently ignored *)
    let path = strip_otr_in_band_magic_prefix uri in
    let request_id = get_header Request_Id headers in
    let sha1 = get_header File_Hash_SHA1 headers in
    let file_length = get_header File_Length headers in
    begin match path , request_id , sha1, file_length with
    | Error _ as e , _ , _ , _ -> e
    | Ok _, None , _ , _ ->
       ignore @@ failwith "request_id er fucked"; Error Missing_header
    | Ok _, _ , None , _ ->
       ignore @@ failwith "sha1 er fucked"; Error Missing_header
    | Ok _, _ , _ , None ->
       ignore @@ failwith "file_length er fucked"; Error Missing_header
    | Ok path , Some request_id , Some sha1 , Some file_length ->
       begin match Int64.of_string file_length with
       | file_length when file_length > 0L ->
          Ok (OFFER_request { offer_request_id = request_id ; offer_path = path; file_length; sha1})
       | _ -> Error Invalid_file_length
       | exception Failure _ -> Error Invalid_file_length
       end
    end
  and handle_get uri headers =
    (* note: extraneous headers currently ignored *)
    let path = strip_otr_in_band_magic_prefix uri in
    let request_id = get_header Request_Id headers in
    let range = get_header Range headers in
    begin match path, request_id , range with
    | Error _ as e , _ , _ -> e
    | _ , None , _ -> Error Missing_request_id
    | _ ,    _ , None -> Error Missing_range
    | Ok path , Some request_id , Some range ->
       (* Range: bytes=123-456 *)
       let range_fields = String.cut ~rev:false ~sep:"-" range in
       begin match range_fields with
       | Some (range_start , range_end ) ->
             begin match Int64.of_string range_start
                       , Int64.of_string range_end
             with
               range_start , range_end when range_start >= 0L
                                            && range_end >= range_start ->
               Ok (GET_request {get_request_id = request_id; path ; range_start ; range_end })
             | rs, re ->
                Ok (GET_request {get_request_id = request_id ; path ; range_start =rs ; range_end = re})
             | exception Failure _ ->
                ignore @@ failwith ("hvad fanden er det for noget lort" ^ range_start ^ " ::: " ^range_end);
                Error Invalid_range_fields
             end
       | None ->
          ignore @@ failwith ("ODOADASDJxx" ^ range ^ "xx") ;
          Error Invalid_range_fields
       end
    end
  in
      begin match split_header_and_body msg with
      | Ok (request_line , headers , _) ->
       begin match String.cut ~rev:false ~sep:" " request_line with
       | Some ("OFFER" , uri) ->
           begin match handle_offer uri headers with
           | Error e -> `Warning (string_of_otrdata_request_warning e)
           | Ok x -> `Otrdata_request x
           end
       | Some ("GET"   , uri) ->
           begin match handle_get uri headers with
           | Error e -> `Warning (string_of_otrdata_request_warning e)
           | Ok x -> `Otrdata_request x
           end
       | Some (abc , def) -> `Warning ("alt er lort" ^ abc ^ " - " ^ def)
       | None -> `Warning ("req:"^request_line ^ string_of_otrdata_request_warning Unknown_verb)
       end
      | Error e ->
         `Warning
          (string_of_otrdata_response_warning
             (begin match e with
              | `Cant_split_header_and_body -> Response_cant_split_header_and_body
              | `Message_too_long -> Response_message_too_long end))
      end

let handle_otrdata_response msg =
  (* handle a response TLV tag *)
  (* split by \n\n, return status + request_id + body*)
  begin match split_header_and_body msg with
  | Ok (status_line , headers , body) ->
     let request_id = get_header Request_Id headers in
     begin match status_line, request_id with
     | "200 OK" , Some request_id ->
        `Otrdata_response {status_line ; request_id ; body}
     | _ , _ -> `Warning (string_of_otrdata_response_warning Invalid_response)
     end
  | Error e -> `Warning (string_of_otrdata_response_warning (begin match e with | `Cant_split_header_and_body -> Response_cant_split_header_and_body | `Message_too_long -> Response_message_too_long end))
  end

let make_otrdata_get request_id file_path byte_range =
  String.concat ~sep:"\n"
                [ "GET otr-in-band:/storage/" ^ file_path ;
                  "Request-Id: " ^ request_id;
                  "Range: " ^ (fst byte_range |> Int64.to_string)
                  ^ "-" ^ (snd byte_range |> Int64.to_string)
                            ; "" ; ""
                ]

let make_otrdata_offer request_id file_path hex_sha1 file_length =
  String.concat ~sep:"\n"
                [ "OFFER otr-in-band:/storage/" ^ file_path ;
                  "Request-Id: " ^ request_id;
                  "File-Hash-SHA1: " ^ hex_sha1;
                  "File-Length: " ^ (Int64.to_string file_length)
                                      ; "" ; ""
                ]
