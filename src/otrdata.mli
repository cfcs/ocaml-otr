type offer_request = { offer_request_id : string
                     ; offer_path : string
(* naming is stupid because disambiguation syntax is impossible to remember *)
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

val handle_otrdata_request : string ->
           [> `Otrdata_request of request
            | `Warning of string
           ]

val handle_otrdata_response : string ->
           [> `Otrdata_response of response
            | `Warning of string
           ]

val make_otrdata_get : string -> string -> (Int64.t * Int64.t) -> string

val make_otrdata_offer : string -> string -> string -> Int64.t -> string


