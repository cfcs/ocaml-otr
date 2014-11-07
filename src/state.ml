
open Sexplib.Conv

type keyblock = {
  ssid : Cstruct.t ;
  c    : Cstruct.t ;
  c'   : Cstruct.t ;
  m1   : Cstruct.t ;
  m2   : Cstruct.t ;
  m1'  : Cstruct.t ;
  m2'  : Cstruct.t ;
} with sexp

type dh_params = (Nocrypto.Dh.secret * Cstruct.t) with sexp

type encryption_keys = {
  dh : dh_params ;
  previous_dh : dh_params ;
  our_keyid : int32 ;
  our_ctr : int64 ;
  gy : Cstruct.t ;
  previous_gy : Cstruct.t ;
  their_keyid : int32 ;
  their_ctr : int64 ;
} with sexp

type message_state =
  | MSGSTATE_PLAINTEXT
  | MSGSTATE_ENCRYPTED of encryption_keys
  | MSGSTATE_FINISHED
with sexp

type auth_state =
  | AUTHSTATE_NONE
  | AUTHSTATE_AWAITING_DHKEY of Cstruct.t * Cstruct.t * dh_params * Cstruct.t
  | AUTHSTATE_AWAITING_REVEALSIG of dh_params * Cstruct.t
  | AUTHSTATE_AWAITING_SIG of Cstruct.t * keyblock * dh_params * Cstruct.t
with sexp

type policy = [
  | `REQUIRE_ENCRYPTION
  | `SEND_WHITESPACE_TAG
  | `WHITESPACE_START_AKE
  | `ERROR_START_AKE
] with sexp

type version = [ `V2 | `V3 ] with sexp

type config = {
  policies : policy list ;
  versions : version list ;
  dsa      : Nocrypto.Dsa.priv ;
} with sexp

type state = {
  message_state : message_state ;
  auth_state    : auth_state ;
} with sexp

type session = {
  instances : (int32 * int32) option ;
  version : version ;
  state : state ;
  config : config ;
  their_dsa : Nocrypto.Dsa.pub option ;
  ssid : Cstruct.t ;
  high : bool ;
} with sexp

let (<?>) ma b = match ma with None -> b | Some a -> a

let empty_session ?policies ?versions ~dsa () =
  let policies = policies <?> [] in
  let versions = versions <?> [`V3 ; `V2] in
  let config = { policies ; versions ; dsa } in
  let state = { message_state = MSGSTATE_PLAINTEXT ; auth_state = AUTHSTATE_NONE } in
  { instances = None ; version = `V3 ; state ; config ; their_dsa = None ; ssid = Cstruct.create 0 ; high = false }
