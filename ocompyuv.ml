(* convert a sequence of seperate files containing y,u,v date, into
 * a concatentated file *)
open Printf

let prefix = ref ""
let start = ref 0
let end_ = ref max_int
let out = ref "out.yuv"

let () = Arg.(parse
  [
    "-i", Set_string prefix, "filename prefix";
    "-o", Set_string out, "output filename [out.yuv]";
    "-start", Set_int start, "start frame [0]";
    "-end", Set_int end_, "end frame [max-int]";
  ]
  (fun _ -> failwith "invalid arg")
  "ocompyuv")

exception Read_error of string

let rec input_until f b pos len =
  if len=0 then true
  else 
    let len' = input f b pos len in
    if len' = 0 then false
    else if len' = len then true
    else input_until f b (pos+len') (len-len')

let read_file name =
  let f = open_in_bin name in
  let len = in_channel_length f in
  let b = Bytes.create len in
  if input_until f b 0 len then begin
    close_in f;
    b
  end else
    raise (Read_error name)

let write_file f b = 
  let len = Bytes.length b in
  output f b 0 len 

let main () = 
  let f = open_out_bin !out in
  (try
    for i=(!start) to (!end_) do
      write_file f (read_file (sprintf "%s%i.Y" !prefix i));
      write_file f (read_file (sprintf "%s%i.U" !prefix i));
      write_file f (read_file (sprintf "%s%i.V" !prefix i))
    done
  with 
  | Read_error name -> eprintf "read_error: %s\n" name
  | e -> eprintf "uknown error: %s\n" (Printexc.to_string e));
  close_out f

let () = main ()

