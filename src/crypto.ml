
open State

open Nocrypto

let encode_mpi n =
  Builder.encode_data (Numeric.Z.to_cstruct_be n)

let encode_message data =
  let d = Base64.encode data in
  "OTR?:" ^ Cstruct.to_string d ^ "."

let (<+>) = Uncommon.Cs.append

let hash_gt h1 h2 =
  Numeric.Z.(of_cstruct_be h1 > of_cstruct_be h2)

module OtrDsa = struct
  open Nocrypto
  open Nocrypto.Dsa
  open Nocrypto.Uncommon

  let (<+>) = Cs.append

  let to_wire ?notag { p ; q ; gg ; y } =
    let tag =
      match notag with
      | None -> Cs.create_with 2 0
      | Some _ -> Cstruct.create 0
    in
    tag <+> encode_mpi p <+> encode_mpi q <+> encode_mpi gg <+> encode_mpi y

  let pub = Dsa.pub

  let priv_to_wire k =
    to_wire (pub_of_priv k)

  let fingerprint k =
    (* only handling key type 0000, DSA *)
    Hash.digest `SHA1 (to_wire ~notag:() k)

  let signature ~key data =
    let r, s = sign ~key (massage ~key:(pub_of_priv key) data) in
    r <+> s

  let verify ~key rs data =
    Dsa.verify ~key rs (massage ~key data)
end

let derive_keys data =
  let secbytes = Builder.encode_data data in
  let h2 b = Hash.digest `SHA256 ((Cstruct.of_string b) <+> secbytes) in
  let ssid = Cstruct.sub (h2 "\000") 0 8 in
  let c, c' = Cstruct.split (h2 "\001") 16 in
  let m1 = h2 "\002" in
  let m2 = h2 "\003" in
  let m1' = h2 "\004" in
  let m2' = h2 "\005" in
  { ssid ; c ; c' ; m1 ; m2 ; m1' ; m2' }


let data_keys data high =
  let secbytes = Builder.encode_data data in
  let send, recv = if high then ("\001", "\002") else ("\002", "\001") in
  let h1 b = Hash.digest `SHA1 ((Cstruct.of_string b) <+> secbytes) in
  let sendaes = Cstruct.sub (h1 send) 0 16 in
  let sendmac = Hash.digest `SHA1 sendaes in
  let recvaes = Cstruct.sub (h1 recv) 0 16 in
  let recvmac = Hash.digest `SHA1 recvaes in
  (sendaes, sendmac, recvaes, recvmac)

module Counter = Cipher_block.Counters.Inc_BE
module AES_CTR = Cipher_block.AES.CTR (Counter)

let ctr0 _ = Uncommon.Cs.create_with 16 0

let crypt ~key ~ctr msg =
  AES_CTR.encrypt ~key:(AES_CTR.of_secret key) ~ctr msg

let gen_symmetric_key () =
  Rng.generate 16

let hash data =
  Hash.digest `SHA256 data

let mac ~key data =
  let data= Uncommon.Cs.concat data in
  Hash.mac `SHA256 ~key data

let mac160 ~key data =
  let buf = mac ~key [ Builder.encode_data data ] in
  Cstruct.sub buf 0 20

let sha1mac = Hash.mac `SHA1

let group = Dh.Group.oakley_5

let gen_dh_secret () =
  let sec, shared = Dh.gen_secret group in
  (sec, Builder.encode_data shared)

let dh_shared dh_secret message =
  let msg, empty = Parser.decode_data message in
  assert (Cstruct.len empty = 0);
  Dh.shared group dh_secret msg

let mpi_g gx gy =
  let x, _ = Parser.decode_data gx
  and y, _ = Parser.decode_data gy
  in
  hash_gt x y
