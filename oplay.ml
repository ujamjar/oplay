open Tsdl

module YuvFormat = struct

  let packed = [
    "YUY2", Sdl.Pixel.format_yuy2; 
    "UYVY", Sdl.Pixel.format_uyvy;
    "YVYU", Sdl.Pixel.format_yvyu;
    "422", Sdl.Pixel.format_yuy2; (* a bodge *)
  ]

  let planar = [
    "YV12", Sdl.Pixel.format_yv12;
    "IYUV", Sdl.Pixel.format_iyuv; 
    "YUV", Sdl.Pixel.format_iyuv;
    "420", Sdl.Pixel.format_iyuv; 
  ]

  let formats = packed @ planar

  let find formats f = List.find (fun (n,f') -> Sdl.Pixel.eq f f') formats

  let is_planar f = try (ignore (find planar f); true) with _ -> false
  let is_packed f = try (ignore (find packed f); true) with _ -> false

  let string_of_format f = 
    try fst (find formats f)
    with _ -> failwith "invalid pixel format"

end

(* command line parsing *)
module Arg = 
struct 

  module Cfg = 
  struct
    let in_file = ref "" 
    let diff_file = ref ""
    let width = ref 0
    let height = ref 0
    let widtho = ref 0
    let heighto = ref 0
    let format = ref Sdl.Pixel.format_iyuv
    let auto422 = ref false
    let framerate = ref 30
    let fullscreen = ref false
    let verbose = ref false 
  end

  (* command line parsing *)
  let parse_size width' height' s = 
    try
      let (_,w,h,_) = List.find (fun (s',w,h,_) -> s = s') Stdsizes.sizes in
      width' := w;
      height' := h
    with  
    | _ ->
      try
        let index = String.index s 'x' in
        let width = String.sub s 0 index in
        let height = String.sub s (index+1) (String.length s - index - 1) in
        width' := int_of_string width;
        height' := int_of_string height
      with _ -> failwith ("Unable to parse size: " ^ s)

  let parse_format s = 
    try begin
      let s = String.uppercase s in
      Cfg.auto422 := s = "422";
      Cfg.format := List.assoc s YuvFormat.formats
    end with _ -> failwith "Invalid pixel format"

  let parse_int i s = i := int_of_string s
  let parse_switch v () = v := not !v

  let args = 
    [
      "-s", Arg.String(parse_size Cfg.width Cfg.height), " input size";
      "-S", Arg.String(parse_size Cfg.widtho Cfg.heighto), " output size";
      "-f", Arg.String(parse_format), " YUV format";
      "-r", Arg.String(parse_int Cfg.framerate), " frame rate (FPS)";
      "-d", Arg.Unit(parse_switch Cfg.fullscreen), " start in fullscreen mode";
      "-v", Arg.Unit(parse_switch Cfg.verbose), " be verbose";
    ]

  let parse args = 
    let file_names s = 
      if !Cfg.in_file = "" then Cfg.in_file := s
      else if !Cfg.diff_file = "" then Cfg.diff_file := s
      else failwith "Too many files specified on command line"
    in
    let usage = Sys.argv.(0) ^ " [options] [in_file] [diff_file]\n" in
    Arg.parse (Arg.align args) file_names usage;
    if !Cfg.width = 0 || !Cfg.height = 0 then
      begin
        failwith "No image size was specified"
      end;
    if !Cfg.widtho = 0 || !Cfg.heighto = 0 then
      begin
        Cfg.widtho := !Cfg.width;
        Cfg.heighto := !Cfg.height;
      end
end

open Arg

module FileIO = 
struct

  open Unix.LargeFile

  type file = 
    | UnixFile of Unix.file_descr * 
                  (int,
                   Bigarray.int8_unsigned_elt,Bigarray.c_layout) Bigarray.Array2.t *
                  int
    | CamlFile of in_channel *
                  (int,
                   Bigarray.int8_unsigned_elt,Bigarray.c_layout) Bigarray.Array1.t 

  let frame_size w h fmt = 
    if YuvFormat.is_planar fmt then w * h * 3 / 2
    else w * h * 2

  let open_unix name = 
    (* get length of file, and thus number of frames *)
    let length = (Unix.LargeFile.stat name).st_size in
    let size = frame_size !Cfg.width !Cfg.height !Cfg.format in
    let num_frames = Int64.to_int (Int64.div length (Int64.of_int size)) in
    Printf.printf "%Li %i %i\n" length size num_frames;
    (* open file *)
    let f = open_in_bin name in
    let f = Unix.descr_of_in_channel f in
    (* map file *)
    let m = Bigarray.Array2.map_file f ~pos:0L Bigarray.int8_unsigned 
        Bigarray.c_layout false num_frames size 
    in
    UnixFile(f, m, max 1 num_frames)

  let open_caml name = 
    let size = frame_size !Cfg.width !Cfg.height !Cfg.format in
    let ba = Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout size in
    CamlFile(stdin, ba)

  (* open file, or redirect from stdin *)
  let open_file_or_stdin name =
    try
      if name = "" then open_caml name
      else open_unix name
    with 
    | _ -> failwith ("Failed to open file " ^ name)

  let open_file_or_none name = 
    try
      if name = "" then None
      else Some(open_unix name)
    with 
    | _ -> failwith ("Failed to open file " ^ name)

  let copy422_to_yuy2 f t = 
    let width, height = !Cfg.width, !Cfg.height in
    for y=0 to height-1 do
      let h = width * y in
      for x=0 to width - 1 do
        t.{(h*2) + (x*2)} <- f.{h + x}
      done
    done;
    let c = width * height in
    for y=0 to height-1 do
      let h = width / 2 * y in
      for x=0 to width/2 - 1 do
        t.{(h*4) + (x*4) + 1} <- f.{c + h + x}
      done
    done;
    let c = c + (width * height / 2) in
    for y=0 to height-1 do
      let h = width / 2 * y in
      for x=0 to width/2 - 1 do
        t.{(h*4) + (x*4) + 3} <- f.{c + h + x}
      done
    done

  let read_file file frame_no = 
    match file with
    | UnixFile(f,b,s) -> begin
        let frame_no = frame_no mod s in
        Bigarray.Array2.slice_left b frame_no 
      end
    | CamlFile(f,b) ->
      let size = Bigarray.Array1.dim b in
      let rec rd n = if n <> size then (b.{n} <- input_byte f; rd (n+1)) in 
      rd 0;
      b

end

open Printf

let print_opts () = 
  if !Cfg.verbose = true then
    begin
      fprintf stderr "in-file: %s\n" !Cfg.in_file;
      fprintf stderr "diff-file: %s\n" !Cfg.diff_file;
      fprintf stderr "size-in: %dx%d\n" !Cfg.width !Cfg.height;
      fprintf stderr "size-out: %dx%d\n" !Cfg.widtho !Cfg.heighto;
      fprintf stderr "format: %s\n" (YuvFormat.string_of_format !Cfg.format);
      fprintf stderr "framerate: %d\n" !Cfg.framerate;
      fprintf stderr "fullscreen: %b\n" !Cfg.fullscreen;
      flush stderr
    end

(* app main loop *)
let main () = 
  (* simple resource management - undo what was done *)
  let cleanup = ref [] in
  let do_cleanup () = List.iter (fun f -> f()) !cleanup in
  let create_resource f c = 
    match f with
    | `Ok a -> (cleanup := (fun () -> c a) :: !cleanup; a)
    | `Error e -> (do_cleanup (); failwith "resource allocation")
  in

  try

    (* parse commandline *)
    Arg.parse Arg.args;
    print_opts ();

    (* sdl setup *)
    let () = create_resource (Sdl.init Sdl.Init.video) Sdl.quit in

    let window_flags () = 
      Sdl.Window.(if !Cfg.fullscreen then fullscreen_desktop else windowed)
    in

    let window = create_resource 
        (Sdl.create_window ~w:!Cfg.widtho ~h:!Cfg.heighto "OCaml YUV Player" (window_flags()))
        Sdl.destroy_window
    in

    let renderer = create_resource (Sdl.create_renderer window) Sdl.destroy_renderer in
    let _ = Sdl.render_set_logical_size renderer !Cfg.width !Cfg.height in
    let _ = Sdl.set_hint Sdl.Hint.render_scale_quality "linear" in
    let _ = Sdl.render_clear renderer in

    let display = create_resource 
        (Sdl.create_texture renderer !Cfg.format Sdl.Texture.(access_streaming) 
           ~w:!Cfg.width ~h:!Cfg.height)
        Sdl.destroy_texture
    in

    (* open file(s) *)
    let fin = FileIO.open_file_or_stdin !Cfg.in_file in
    (*let fref = FileIO.open_file_or_none !Cfg.diff_file in*)

    let total_frames = 
      match fin with
      | FileIO.UnixFile(_,_,n) -> n
      | _ -> 1
    in
    let set_frame_no, get_frame_no = 
      let frameno = ref 0 in
      let rec set n = 
        if n < 0 then set (n+total_frames)
        else frameno := (n mod total_frames)
      in
      set, fun () -> !frameno
    in

    (* key presses *)
    let finish = ref false in
    let redraw = ref true in
    let play = ref false in

    let key_presses key = 
      let open Sdl.K in
      if key = escape then finish := true
      else if key = tab then begin
        Cfg.fullscreen := not !Cfg.fullscreen;
        ignore (Sdl.set_window_fullscreen window (window_flags()));
        redraw := true;
        ignore (Sdl.render_clear renderer)
      end
      else if key = return || key = space then (play := not !play)
      else if key = right then (play := false; set_frame_no (get_frame_no()+1); redraw := true)
      else if key = left then (play := false; set_frame_no (get_frame_no()-1); redraw := true)
      else if key = home || key = k0 then (play := false; set_frame_no 0; redraw := true)
      else if key = kend then (play := false; set_frame_no (total_frames-1); redraw := true)
      else begin
        let num = 
          if key = k1 then 1
          else if key = k2 then 2
          else if key = k3 then 3
          else if key = k4 then 4
          else if key = k5 then 5
          else if key = k6 then 6
          else if key = k7 then 7
          else if key = k8 then 8
          else if key = k9 then 9
          else (-1)
        in
        if num <> (-1) then
          begin
            let num = float_of_int num in
            let num = int_of_float ((float_of_int total_frames) *. (num /.  10.0) +. 0.5) in
            play := false; set_frame_no num; redraw := true
          end
      end
    in

    let is_planar = YuvFormat.is_planar !Cfg.format in

    let display_frame frame_no = 
      if !redraw then begin
        let b = FileIO.read_file fin frame_no in
        ignore (Sdl.update_texture display None b 
                  (if is_planar then !Cfg.width else !Cfg.width*2));
        ignore (Sdl.render_copy renderer display);
        ignore (Sdl.render_present renderer);
        redraw := false;
        Sdl.set_window_title window (sprintf "[%i] oplay" frame_no)
      end
    in

    (* playout at given rate *)
    let ftimems () = Int32.to_float (Sdl.get_ticks()) in
    let lasttime = ref (ftimems ()) in
    let playout = 
      let lapse = 1000.0 /. (float_of_int !Cfg.framerate) in
      fun () ->
        if !play = true then
          begin
            let newtime = ftimems () in
            if newtime -. !lasttime >= lapse then
              begin
                lasttime := newtime;
                redraw := true;
                set_frame_no (get_frame_no()+1)
              end
          end
    in

    (* polling loop *)
    let event = Sdl.Event.create () in
    let rec poll' () = 
      Sdl.delay 1l; (* note; this stops the CPU going @ 100% *)
      (* playback timer *)
      playout ();
      (* draw frame *)
      display_frame (get_frame_no());
      (* poll events *)
      if !finish = false then
        begin
          (while Sdl.poll_event (Some event) do
             let open Sdl.Event in
             let typ = get event typ in
             if typ = quit then 
               (* quit *)
               finish := true
             else if typ = key_down then
               (* key press *)
               key_presses (get event keyboard_keycode)
             else  if typ = window_event then
               (* window event *)
               let wev = get event window_event_id in
               if wev = window_event_exposed then redraw := true
               else ()
             else
               ()
           done);
          poll' ()
        end
    in
    poll' ();
  with
  | Failure x -> 
    do_cleanup ();
    printf "FAILURE: %s\n" x;
    exit (-1)

(* run main *)
let _ = main ()


