(* CakeML Runtime Library for OCaml transpilation *)

(* === Exceptions === *)
exception Chr_exn
exception Subscript_exn

(* === Integer operations (using Zarith for arbitrary precision) === *)
let int_div a b =
  if Z.equal b Z.zero then raise Division_by_zero
  else
    (* CakeML uses truncation toward zero *)
    let q = Z.div a b in
    (* Adjust for CakeML semantics: truncate toward zero *)
    if Z.sign (Z.mul (Z.sub a (Z.mul q b)) b) < 0 then
      Z.add q (if Z.sign b > 0 then Z.one else Z.minus_one)
    else q

let int_mod a b =
  if Z.equal b Z.zero then raise Division_by_zero
  else Z.sub a (Z.mul (int_div a b) b)

(* === Polymorphic equality === *)
let poly_equal (a : 'a) (b : 'a) = (a = b)

(* === Comparison operations === *)
(* Integer *)
let test_equal_int (a : Z.t) (b : Z.t) = Z.equal a b
let test_lt_int (a : Z.t) (b : Z.t) = Z.lt a b
let test_gt_int (a : Z.t) (b : Z.t) = Z.gt a b
let test_leq_int (a : Z.t) (b : Z.t) = Z.leq a b
let test_geq_int (a : Z.t) (b : Z.t) = Z.geq a b

(* Bool *)
let test_equal_bool (a : bool) (b : bool) = (a = b)
let test_lt_bool (_ : bool) (_ : bool) = false  (* CakeML: False < True *)
let test_gt_bool (_ : bool) (_ : bool) = false
let test_leq_bool (a : bool) (b : bool) = (a = b) || (not a && b)
let test_geq_bool (a : bool) (b : bool) = (a = b) || (a && not b)

(* String *)
let test_equal_str (a : string) (b : string) = String.equal a b
let test_lt_str (a : string) (b : string) = String.compare a b < 0
let test_gt_str (a : string) (b : string) = String.compare a b > 0
let test_leq_str (a : string) (b : string) = String.compare a b <= 0
let test_geq_str (a : string) (b : string) = String.compare a b >= 0

(* Char *)
let test_equal_char (a : char) (b : char) = Char.equal a b
let test_lt_char (a : char) (b : char) = Char.code a < Char.code b
let test_gt_char (a : char) (b : char) = Char.code a > Char.code b
let test_leq_char (a : char) (b : char) = Char.code a <= Char.code b
let test_geq_char (a : char) (b : char) = Char.code a >= Char.code b

(* Word8 (represented as int, 0-255) *)
let test_equal_w8 (a : int) (b : int) = a = b
let test_lt_w8 (a : int) (b : int) = a < b
let test_gt_w8 (a : int) (b : int) = a > b
let test_leq_w8 (a : int) (b : int) = a <= b
let test_geq_w8 (a : int) (b : int) = a >= b

(* Word64 (represented as int64) *)
(* Note: CakeML word comparisons are unsigned *)
let w64_unsigned_lt a b =
  (* Compare as unsigned by flipping sign bit *)
  let flip = Int64.min_int in
  Int64.compare (Int64.add a flip) (Int64.add b flip) < 0

let test_equal_w64 (a : int64) (b : int64) = Int64.equal a b
let test_lt_w64 (a : int64) (b : int64) = w64_unsigned_lt a b
let test_gt_w64 (a : int64) (b : int64) = w64_unsigned_lt b a
let test_leq_w64 (a : int64) (b : int64) = Int64.equal a b || w64_unsigned_lt a b
let test_geq_w64 (a : int64) (b : int64) = Int64.equal a b || w64_unsigned_lt b a

(* Float64 *)
let test_equal_float (a : float) (b : float) = Float.equal a b
let test_lt_float (a : float) (b : float) = a < b
let test_gt_float (a : float) (b : float) = a > b
let test_leq_float (a : float) (b : float) = a <= b
let test_geq_float (a : float) (b : float) = a >= b

(* Alt comparisons — identical semantics to regular comparisons *)
let test_altlt_int = test_lt_int
let test_altleq_int = test_leq_int
let test_altgt_int = test_gt_int
let test_altgeq_int = test_geq_int
let test_altlt_bool = test_lt_bool
let test_altleq_bool = test_leq_bool
let test_altgt_bool = test_gt_bool
let test_altgeq_bool = test_geq_bool
let test_altlt_str = test_lt_str
let test_altleq_str = test_leq_str
let test_altgt_str = test_gt_str
let test_altgeq_str = test_geq_str
let test_altlt_char = test_lt_char
let test_altleq_char = test_leq_char
let test_altgt_char = test_gt_char
let test_altgeq_char = test_geq_char
let test_altlt_w8 = test_lt_w8
let test_altleq_w8 = test_leq_w8
let test_altgt_w8 = test_gt_w8
let test_altgeq_w8 = test_geq_w8
let test_altlt_w64 = test_lt_w64
let test_altleq_w64 = test_leq_w64
let test_altgt_w64 = test_gt_w64
let test_altgeq_w64 = test_geq_w64
let test_altlt_float = test_lt_float
let test_altleq_float = test_leq_float
let test_altgt_float = test_gt_float
let test_altgeq_float = test_geq_float

(* === Word8 operations === *)
let w8_mask = 0xFF

let w8_from_int (n : Z.t) = Z.to_int n land w8_mask
let w8_to_int (w : int) = Z.of_int w
let w8_add (a : int) (b : int) = (a + b) land w8_mask
let w8_sub (a : int) (b : int) = (a - b) land w8_mask
let w8_and (a : int) (b : int) = a land b
let w8_or (a : int) (b : int) = a lor b
let w8_xor (a : int) (b : int) = (a lxor b) land w8_mask

let w8_lsl (a : int) (n : int) = (a lsl n) land w8_mask
let w8_lsr (a : int) (n : int) = (a lsr n) land w8_mask
let w8_asr (a : int) (n : int) =
  (* Sign-extend from 8 bits, then arithmetic shift *)
  let signed = if a land 0x80 <> 0 then a lor (lnot w8_mask) else a in
  (signed asr n) land w8_mask

(* === Word64 operations === *)
let w64_from_int (n : Z.t) =
  (* Truncate to 64 bits (modular conversion) *)
  let masked = Z.logand n (Z.sub (Z.shift_left Z.one 64) Z.one) in
  if Z.compare masked (Z.shift_left Z.one 63) >= 0 then
    (* Value >= 2^63: convert to signed int64 *)
    Z.to_int64 (Z.sub masked (Z.shift_left Z.one 64))
  else
    Z.to_int64 masked
let w64_to_int (w : int64) =
  (* Interpret as unsigned *)
  if Int64.compare w 0L >= 0 then Z.of_int64 w
  else Z.add (Z.of_int64 (Int64.add w Int64.max_int)) (Z.add (Z.of_int64 Int64.max_int) (Z.of_int 2))

let w64_add (a : int64) (b : int64) = Int64.add a b
let w64_sub (a : int64) (b : int64) = Int64.sub a b
let w64_and (a : int64) (b : int64) = Int64.logand a b
let w64_or (a : int64) (b : int64) = Int64.logor a b
let w64_xor (a : int64) (b : int64) = Int64.logxor a b

let w64_lsl (a : int64) (n : int) = Int64.shift_left a n
let w64_lsr (a : int64) (n : int) = Int64.shift_right_logical a n
let w64_asr (a : int64) (n : int) = Int64.shift_right a n
let w64_ror (a : int64) (n : int) =
  Int64.logor (Int64.shift_right_logical a n) (Int64.shift_left a (64 - n))

(* === String operations === *)
let strlen (s : string) = Z.of_int (String.length s)
let strsub (s : string) (i : Z.t) =
  let idx = Z.to_int i in
  if idx < 0 || idx >= String.length s then raise Subscript_exn
  else String.get s idx
let strcat (ss : string list) = String.concat "" ss
let explode (s : string) = List.of_seq (Seq.map (fun c -> c) (String.to_seq s))
let implode (cs : char list) =
  let buf = Buffer.create (List.length cs) in
  List.iter (Buffer.add_char buf) cs;
  Buffer.contents buf
let copy_str_str (s : string) (srcoff : Z.t) (len : Z.t) =
  String.sub s (Z.to_int srcoff) (Z.to_int len)
let copy_str_aw8 (s : string) (srcoff : Z.t) (len : Z.t) (dst : bytes) (dstoff : Z.t) =
  Bytes.blit_string s (Z.to_int srcoff) dst (Z.to_int dstoff) (Z.to_int len)
let copy_aw8_str (src : bytes) (srcoff : Z.t) (len : Z.t) =
  Bytes.sub_string src (Z.to_int srcoff) (Z.to_int len)
let copy_aw8_aw8 (src : bytes) (srcoff : Z.t) (len : Z.t) (dst : bytes) (dstoff : Z.t) =
  Bytes.blit src (Z.to_int srcoff) dst (Z.to_int dstoff) (Z.to_int len)

(* === Ord / Chr === *)
let ord (c : char) = Z.of_int (Char.code c)
let chr (n : Z.t) =
  let i = Z.to_int n in
  if i < 0 || i > 255 then raise Chr_exn
  else Char.chr i

(* === Char/Word8 conversions === *)
let char_to_w8 (c : char) = Char.code c
let w8_to_char (n : int) = Char.chr (n land 0xFF)

(* === Array operations === *)
let aalloc (n : Z.t) (v : 'a) = Array.make (Z.to_int n) v
let aalloc_empty () : 'a array = [||]
let asub (a : 'a array) (i : Z.t) =
  let idx = Z.to_int i in
  if idx < 0 || idx >= Array.length a then raise Subscript_exn
  else a.(idx)
let alength (a : 'a array) = Z.of_int (Array.length a)
let aupdate (a : 'a array) (i : Z.t) (v : 'a) =
  let idx = Z.to_int i in
  if idx < 0 || idx >= Array.length a then raise Subscript_exn
  else a.(idx) <- v

(* Unsafe array ops (no bounds checking) *)
let asub_unsafe (a : 'a array) (i : Z.t) = Array.unsafe_get a (Z.to_int i)
let aupdate_unsafe (a : 'a array) (i : Z.t) (v : 'a) = Array.unsafe_set a (Z.to_int i) v

(* === Byte array (Word8Array) operations === *)
let aw8alloc (n : Z.t) (v : int) = Bytes.make (Z.to_int n) (Char.chr (v land 0xFF))
let aw8sub (a : bytes) (i : Z.t) =
  let idx = Z.to_int i in
  if idx < 0 || idx >= Bytes.length a then raise Subscript_exn
  else Char.code (Bytes.get a idx)
let aw8length (a : bytes) = Z.of_int (Bytes.length a)
let aw8update (a : bytes) (i : Z.t) (v : int) =
  let idx = Z.to_int i in
  if idx < 0 || idx >= Bytes.length a then raise Subscript_exn
  else Bytes.set a idx (Char.chr (v land 0xFF))

(* Unsafe byte array ops (no bounds checking) *)
let aw8sub_unsafe (a : bytes) (i : Z.t) = Char.code (Bytes.unsafe_get a (Z.to_int i))
let aw8update_unsafe (a : bytes) (i : Z.t) (v : int) = Bytes.unsafe_set a (Z.to_int i) (Char.chr (v land 0xFF))

(* === Vector operations (immutable arrays) === *)
type 'a vector = 'a array
let vfrom_list (l : 'a list) : 'a vector = Array.of_list l
let vsub (v : 'a vector) (i : Z.t) =
  let idx = Z.to_int i in
  if idx < 0 || idx >= Array.length v then raise Subscript_exn
  else v.(idx)
let vlength (v : 'a vector) = Z.of_int (Array.length v)

(* Unsafe vector op *)
let vsub_unsafe (v : 'a vector) (i : Z.t) = Array.unsafe_get v (Z.to_int i)

(* === XorAw8Str (unsafe) === *)
let xor_aw8_str_unsafe (buf : bytes) (s : string) (off : Z.t) (len : Z.t) =
  let off_i = Z.to_int off in
  let len_i = Z.to_int len in
  for i = 0 to len_i - 1 do
    let j = off_i + i in
    let b = Char.code (Bytes.unsafe_get buf j) in
    let c = Char.code (String.unsafe_get s i) in
    Bytes.unsafe_set buf j (Char.chr (b lxor c))
  done

(* === Float operations === *)
let fp_add (a : float) (b : float) = a +. b
let fp_sub (a : float) (b : float) = a -. b
let fp_mul (a : float) (b : float) = a *. b
let fp_div (a : float) (b : float) = a /. b
let fp_abs (a : float) = Float.abs a
let fp_neg (a : float) = Float.neg a
let fp_sqrt (a : float) = Float.sqrt a
let fp_fma (a : float) (b : float) (c : float) = Float.fma a b c
let fp_from_word (w : int64) = Int64.float_of_bits w
let fp_to_word (f : float) = Int64.bits_of_float f

(* === ConfigGC: no-op === *)
let config_gc (_ : Z.t) (_ : Z.t) = ()

(* === Eval: stub === *)
let eval _ = failwith "CakeML Eval not supported in OCaml translation"

(* === FFI operations === *)

(* File descriptor table: maps CakeML fd numbers to Unix file descriptors *)
let fd_table : (int, Unix.file_descr) Hashtbl.t = Hashtbl.create 16

let () =
  Hashtbl.add fd_table 0 Unix.stdin;
  Hashtbl.add fd_table 1 Unix.stdout;
  Hashtbl.add fd_table 2 Unix.stderr

let get_fd n =
  try Hashtbl.find fd_table n
  with Not_found -> failwith (Printf.sprintf "Bad file descriptor: %d" n)

(* Read 8-byte big-endian integer from a string (fd encoding) *)
let read_be64_string s =
  let n = ref 0 in
  for i = 0 to min 7 (String.length s - 1) do
    n := (!n lsl 8) lor (Char.code s.[i])
  done;
  !n

(* Write 8-byte big-endian integer to bytes *)
let write_be64 buf off v =
  for i = 0 to 7 do
    Bytes.set buf (off + i) (Char.chr ((v lsr ((7-i)*8)) land 0xFF))
  done

(* Read 2 bytes as big-endian uint16 from bytes *)
let read_be16 buf off =
  (Char.code (Bytes.get buf off) lsl 8) lor Char.code (Bytes.get buf (off+1))

(* Write 2 bytes as big-endian uint16 to bytes *)
let write_be16 buf off v =
  Bytes.set buf off (Char.chr ((v lsr 8) land 0xFF));
  Bytes.set buf (off+1) (Char.chr (v land 0xFF))

let ffi_empty_name_ (s : string) (buf : bytes) =
  (* Debug/no-op FFI: just print to stderr if non-empty message *)
  if s = "nonzero_exit" then
    exit 1
  else begin
    if String.length s > 0 then
      Printf.eprintf "%s\n%!" s;
    ignore buf
  end

(* c = fd (8 bytes BE string), a = iobuff
   a layout: n(2 bytes BE) | off(2 bytes BE) | data...
   After: a[0] = err flag, a[1:3] = nwritten (2 bytes BE) *)
let ffi_write (c : string) (a : bytes) =
  let fd_n = read_be64_string c in
  let n = read_be16 a 0 in
  let off = read_be16 a 2 in
  let fd = get_fd fd_n in
  (try
    let n_written = Unix.write fd a (4 + off) n in
    Bytes.set a 0 (Char.chr 0);
    write_be16 a 1 n_written
  with _ ->
    Bytes.set a 0 (Char.chr 1))

(* c = fd (8 bytes BE string), a = iobuff
   a layout: n(2 bytes BE) | ...
   After: a[0] = err flag, a[1:3] = nread (2 bytes BE), a[4:] = data *)
let ffi_read (c : string) (a : bytes) =
  let fd_n = read_be64_string c in
  let n = read_be16 a 0 in
  let fd = get_fd fd_n in
  (try
    let n_read = Unix.read fd a 4 n in
    Bytes.set a 0 (Char.chr 0);
    write_be16 a 1 n_read
  with _ ->
    Bytes.set a 0 (Char.chr 1))

(* c = filename (null-terminated string), a = 9-byte result buffer
   After: a[0] = err flag, a[1:9] = fd (8 bytes BE) *)
let ffi_open_in (c : string) (a : bytes) =
  (* Strip trailing null byte from filename *)
  let fname = if String.length c > 0 && c.[String.length c - 1] = '\x00'
    then String.sub c 0 (String.length c - 1) else c in
  (try
    let fd = Unix.openfile fname [Unix.O_RDONLY] 0 in
    let fd_n : int = Obj.magic fd in
    Hashtbl.replace fd_table fd_n fd;
    Bytes.set a 0 (Char.chr 0);
    write_be64 a 1 fd_n
  with _ ->
    Bytes.set a 0 (Char.chr 1))

(* c = filename (null-terminated string), a = 9-byte result buffer *)
let ffi_open_out (c : string) (a : bytes) =
  let fname = if String.length c > 0 && c.[String.length c - 1] = '\x00'
    then String.sub c 0 (String.length c - 1) else c in
  (try
    let fd = Unix.openfile fname [Unix.O_RDWR; Unix.O_CREAT; Unix.O_TRUNC] 0o644 in
    let fd_n : int = Obj.magic fd in
    Hashtbl.replace fd_table fd_n fd;
    Bytes.set a 0 (Char.chr 0);
    write_be64 a 1 fd_n
  with _ ->
    Bytes.set a 0 (Char.chr 1))

(* c = fd (8 bytes BE string), a = result buffer, a[0] = err flag *)
let ffi_close (c : string) (a : bytes) =
  let fd_n = read_be64_string c in
  (try
    let fd = get_fd fd_n in
    Unix.close fd;
    Hashtbl.remove fd_table fd_n;
    Bytes.set a 0 (Char.chr 0)
  with _ ->
    Bytes.set a 0 (Char.chr 1))

let ffi_exit (_ : string) (a : bytes) =
  let code = Char.code (Bytes.get a 0) in
  exit code

(* Command-line arg FFI uses LITTLE-ENDIAN 2-byte encoding
   (unlike file I/O which uses big-endian) *)
let read_le16 buf off =
  Char.code (Bytes.get buf off) lor (Char.code (Bytes.get buf (off+1)) lsl 8)

let write_le16 buf off v =
  Bytes.set buf off (Char.chr (v land 0xFF));
  Bytes.set buf (off+1) (Char.chr ((v lsr 8) land 0xFF))

let ffi_get_arg_count (_ : string) (a : bytes) =
  let argc = Array.length Sys.argv in
  write_le16 a 0 argc

let ffi_get_arg_length (_ : string) (a : bytes) =
  let n = read_le16 a 0 in
  if n < Array.length Sys.argv then
    write_le16 a 0 (String.length Sys.argv.(n))
  else
    write_le16 a 0 0

let ffi_get_arg (_ : string) (a : bytes) =
  let n = read_le16 a 0 in
  if n < Array.length Sys.argv then begin
    let arg = Sys.argv.(n) in
    Bytes.blit_string arg 0 a 0 (min (String.length arg) (Bytes.length a))
  end

let ffi_poll_sigint (_ : string) (_ : bytes) = ()

let ffi_double_fromString (c : string) (a : bytes) =
  (* c = the number string (null-terminated), a = result buffer *)
  let s = if String.length c > 0 && c.[String.length c - 1] = '\x00'
    then String.sub c 0 (String.length c - 1) else c in
  (try
    let f = float_of_string s in
    let bits = Int64.bits_of_float f in
    Bytes.set a 0 (Char.chr 0); (* success *)
    for i = 0 to 7 do
      Bytes.set a (1+i) (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical bits (i*8)) 0xFFL)))
    done
  with _ ->
    Bytes.set a 0 (Char.chr 1))

let ffi_double_toString (_ : string) (buf : bytes) =
  let bits = ref 0L in
  for i = 0 to 7 do
    bits := Int64.logor !bits (Int64.shift_left (Int64.of_int (Char.code (Bytes.get buf i))) (i*8))
  done;
  let f = Int64.float_of_bits !bits in
  let s = Printf.sprintf "%.17g" f in
  let len = min (String.length s) (Bytes.length buf) in
  Bytes.blit_string s 0 buf 0 len

let ffi_double_fromInt (_ : string) (buf : bytes) =
  let bits = ref 0L in
  for i = 0 to 7 do
    bits := Int64.logor !bits (Int64.shift_left (Int64.of_int (Char.code (Bytes.get buf i))) (i*8))
  done;
  let n = !bits in
  let f = Int64.to_float n in
  let result_bits = Int64.bits_of_float f in
  for i = 0 to 7 do
    Bytes.set buf i (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical result_bits (i*8)) 0xFFL)))
  done

let ffi_double_toInt (_ : string) (buf : bytes) =
  let bits = ref 0L in
  for i = 0 to 7 do
    bits := Int64.logor !bits (Int64.shift_left (Int64.of_int (Char.code (Bytes.get buf i))) (i*8))
  done;
  let f = Int64.float_of_bits !bits in
  let n = Int64.of_float f in
  for i = 0 to 7 do
    Bytes.set buf i (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical n (i*8)) 0xFFL)))
  done

let ffi_double_pow (_ : string) (buf : bytes) =
  let read_double off =
    let bits = ref 0L in
    for i = 0 to 7 do
      bits := Int64.logor !bits (Int64.shift_left (Int64.of_int (Char.code (Bytes.get buf (off+i)))) (i*8))
    done;
    Int64.float_of_bits !bits
  in
  let write_double off f =
    let bits = Int64.bits_of_float f in
    for i = 0 to 7 do
      Bytes.set buf (off+i) (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical bits (i*8)) 0xFFL)))
    done
  in
  let a = read_double 0 in
  let b = read_double 8 in
  write_double 0 (a ** b)

let ffi_double_ln (_ : string) (buf : bytes) =
  let bits = ref 0L in
  for i = 0 to 7 do
    bits := Int64.logor !bits (Int64.shift_left (Int64.of_int (Char.code (Bytes.get buf i))) (i*8))
  done;
  let f = Int64.float_of_bits !bits in
  let result = log f in
  let result_bits = Int64.bits_of_float result in
  for i = 0 to 7 do
    Bytes.set buf i (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical result_bits (i*8)) 0xFFL)))
  done

let ffi_double_exp (_ : string) (buf : bytes) =
  let bits = ref 0L in
  for i = 0 to 7 do
    bits := Int64.logor !bits (Int64.shift_left (Int64.of_int (Char.code (Bytes.get buf i))) (i*8))
  done;
  let f = Int64.float_of_bits !bits in
  let result = exp f in
  let result_bits = Int64.bits_of_float result in
  for i = 0 to 7 do
    Bytes.set buf i (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical result_bits (i*8)) 0xFFL)))
  done

let ffi_double_floor (_ : string) (buf : bytes) =
  let bits = ref 0L in
  for i = 0 to 7 do
    bits := Int64.logor !bits (Int64.shift_left (Int64.of_int (Char.code (Bytes.get buf i))) (i*8))
  done;
  let f = Int64.float_of_bits !bits in
  let result = floor f in
  let result_bits = Int64.bits_of_float result in
  for i = 0 to 7 do
    Bytes.set buf i (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical result_bits (i*8)) 0xFFL)))
  done

let ffi_kernel_ffi (_ : string) (_ : bytes) =
  failwith "kernel_ffi not supported"
