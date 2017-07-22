open Base
open Float.O_dot
module Scanf = Caml.Scanf
module Sys = Caml.Sys

module Config = struct
  let min_length = 18.0
  let max_length = 100.0

  let entrance_proximity = 10.0
end

let read_entrances ~pos_file ~extremities:(x_min, x_max, y_min, y_max) =
  let chan = Caml.open_in pos_file in
  begin match Caml.input_line chan with
  | exception End_of_file -> failwith "Malformed .pos file: no header"
  | _header -> ()
  end;
  let rec read ents =
    match Caml.input_line chan with
    | exception End_of_file ->
      Caml.close_in chan;
      ents
    | line ->
      match String.split line ~on:')' with
      | [coords; station_name] ->
         begin match String.chop_prefix line ~prefix:"(" with
        | None -> Printf.failwithf "Malformed line in .pos file: %s" line ()
        | Some coords ->
          match String.split coords ~on:',' with
          | [x; y; _z] ->
            let x = Float.of_string x in
            let y = Float.of_string y in
            if Float.(>=) x x_min
              && Float.(<=) x x_max
              && Float.(>=) y y_min
              && Float.(<=) y y_max
            then
              read ((station_name, x, y) :: ents)
            else
              read ents
          | _ -> Printf.failwithf "Malformed line in .pos file: %s" line ()
         end
      | _ -> Printf.failwithf "Malformed line in .pos file: %s" line ()
  in
  read []

let num_entrances_close_to ~entrances ~x ~y ~allow_extra =
  let close_to =
    List.filter entrances ~f:(fun (station_name, ent_x, ent_y) ->
      let dx = ent_x -. x in
      let dy = ent_y -. y in
      let dist = Float.sqrt (dx*.dx +. dy*.dy) in
      Float.(<=) dist (allow_extra +. Config.entrance_proximity))
  in
  let names =
    List.map close_to ~f:(fun (station_name, _, _) -> station_name)
  in
  List.length close_to, names

let find_contours ~input ~output =
  Sys.remove output;
  let cmd = Printf.sprintf "gdal_contour -3d -i 1 %s %s" input output in
  if Sys.command cmd <> 0 then failwith "gdal_contour failed"

let convert_to_well_known_text ~input ~output =
  Sys.remove output;
  let cmd =
    Printf.sprintf "ogr2ogr -f csv -lco GEOMETRY=AS_WKT %s %s" output input
  in
  if Sys.command cmd <> 0 then failwith "ogr2ogr failed"

let extract_coords ~line =
  match String.rindex line ',' with
  | None -> None
  | Some comma ->
    let prefix = "\"LINESTRING Z (" in
    let prefix_len = String.length prefix in
    match String.chop_prefix line ~prefix with
    | None -> None
    | Some line ->
      match String.sub line ~pos:0 ~len:(comma - prefix_len - 2) with
      | exception _ -> None
      | coords -> Some coords

let parse_coords coords =
  try
    Scanf.sscanf coords "%f %f %d" (fun x y z ->
      Some (x, y, Float.of_int z))
  with _ -> None

let create_contour ~coords ~extremities =
  let coords = String.split coords ~on:',' in
  let coords = List.map coords ~f:parse_coords in
  match Option.all coords with
  | None | Some [] | Some [_] -> None
  | Some coords ->
    let zs = List.map coords ~f:(fun (_, _, z) -> z) in
    match List.dedup zs ~compare:Float.compare with
    | [z] ->
      let vertices = List.map coords ~f:(fun (x, y, _) -> x, y) in
      begin match Contour.create ~vertices ~z with
      | None -> None
      | Some contour ->
        let extremities =
          List.fold vertices ~init:extremities
            ~f:(fun (x_min, x_max, y_min, y_max) (x, y) ->
              Float.min x_min x, Float.max x_max x,
                Float.min y_min y, Float.max y_max y)
        in
        Some (extremities, contour)
      end
    | _ -> None

let read_lines_and_create_contour_groups ~wkt_reader =
  let rec read contours extremities =
    match Caml.input_line wkt_reader with
    | exception End_of_file -> contours, extremities
    | line ->
      match extract_coords line with
      | None -> read contours extremities
      | Some coords ->
        let contours, extremities =
          match create_contour ~coords ~extremities with
          | None -> contours, extremities
          | Some (extremities, contour) ->
            let contours =
              if Float.(<=) (Contour.length contour) Config.max_length then
                (Contour_group.create contour) :: contours
              else
                contours
            in
            contours, extremities
        in
        read contours extremities
  in
  read []
    (Float.max_value, Float.min_value, Float.max_value, Float.min_value)

let try_to_fit ~groups ~into =
  let found_one, into, didn't_fit =
    List.fold groups
      ~init:(false, into, [])
      ~f:(fun (found_one, into, didn't_fit) group ->
        let found_one', into, didn't_fit =
          if Contour_group.approx_inside group ~of_:into then
            true, Contour_group.union into group, didn't_fit
          else
            false, into, group::didn't_fit
        in
        let found_one = found_one || found_one' in
        found_one, into, didn't_fit)
  in
  found_one, into, List.rev didn't_fit

let assemble_depressions_one_pass ~contour_groups =
  let rec assemble ~found_one ~groups_before ~groups_at_and_after =
    match groups_at_and_after with
    | [] -> found_one, groups_before
    | group::groups_after ->
      let found_one_before, group, groups_before =
        try_to_fit ~groups:groups_before ~into:group
      in
      let found_one_after, group, groups_after =
        try_to_fit ~groups:groups_after ~into:group
      in
      let found_one = found_one || found_one_before || found_one_after in
      let groups_before = groups_before @ [group] in
      assemble ~found_one ~groups_before ~groups_at_and_after:groups_after
  in
  assemble ~found_one:false ~groups_before:[]
    ~groups_at_and_after:contour_groups

let assemble_depressions ~contour_groups =
  let rec assemble ~pass ~contour_groups =
    Caml.Printf.printf "Assembling depressions, pass %d\n%!" pass;
    let found_one, contour_groups =
      assemble_depressions_one_pass ~contour_groups
    in
    if not found_one then contour_groups
    else assemble ~pass:(pass + 1) ~contour_groups
  in
  assemble ~pass:1 ~contour_groups

let write_one_contour ~svx_writer ~contour ~starting_station =
  let x_fix, y_fix = Contour.fixed_point contour in
  let z = Contour.altitude contour in
  Caml.Printf.fprintf svx_writer "*fix %d %f %f %f\n"
    starting_station x_fix y_fix z;
  let last_station =
    Contour.fold_edges contour
      ~init:starting_station
      ~f:(fun station ~dx ~dy ->
        let next_station = station + 1 in
        Caml.Printf.fprintf svx_writer "%d %d %f %f 0.0\n"
          station next_station dx dy;
        next_station)
  in
  last_station + 1

let write_one_contour_group ~svx_writer ~contour_group ~starting_station =
  Contour_group.fold contour_group
    ~init:starting_station
    ~f:(fun station contour ->
      write_one_contour ~svx_writer ~contour ~starting_station:station)

let write_contour_groups ~svx_writer ~contour_groups ~entrances =
  let total_ents = Float.of_int (List.length entrances) in
  let _station, num_written, num_close, num_missed, _entrances =
    List.fold contour_groups
      ~init:(0, 0, 0, 0, entrances)
      ~f:(fun (station, num_written, num_close, num_missed, entrances)
              contour_group ->
        let longest_length = Contour_group.longest_length contour_group in
        let longest_axis = Contour_group.longest_axis contour_group in
        let num_close', names_close' =
          let x, y = Contour_group.centroid contour_group in
          num_entrances_close_to ~entrances ~x ~y ~allow_extra:longest_axis
        in
        let entrances =
          List.filter entrances ~f:(fun (station_name, _x, _y) ->
            not (List.mem names_close' station_name ~equal:String.(=)))
        in
        let slope = Contour_group.slope contour_group in
        let depth = Contour_group.depth contour_group in
        if Float.(>=) slope 45.0
          || Float.(>=) depth 3.0
          || (Contour_group.size contour_group > 2
            && Float.(>) longest_length Config.min_length)
(*
        if Contour_group.size contour_group > 3
(*
          || Float.(>=) slope 60.0
          || (Float.(>=) slope 30.0 && Float.(>=) depth 3.0)
*)
          || (Contour_group.size contour_group > 2
            && Float.(>) longest_length Config.min_length)
*)
        then begin
(*
Caml.Printf.printf "** accepted, len %.2f slope %.2f depth %.2f\n%!"
  longest_length slope depth;
*)
          let station =
            write_one_contour_group ~svx_writer ~contour_group
              ~starting_station:station
          in
          if num_close' >= 1 then begin
            Caml.Printf.printf "Entrances well located:";
            List.iter names_close' ~f:(fun name ->
              Caml.Printf.printf " %s" (String.strip name));
            Caml.Printf.printf "\n%!"
          end;
          station, num_written + 1, num_close + num_close', num_missed,
            entrances
        end else begin
(*
if Float.(>) depth 1.0 then begin
Caml.Printf.printf "** rejected, len %.2f slope %.2f depth %.2f\n%!"
  longest_length slope depth
end;
*)
          station, num_written, num_close, num_close' + num_missed,
            entrances
        end)
  in
  Caml.Printf.printf "Number of entrances well located: %d\n" num_close;
  Caml.Printf.printf "Number of entrances missed: %d\n" num_missed;
  Caml.Printf.printf ">> Ratio of well located / all entrances: %.0f%%\n"
    (Float.of_int num_close /. total_ents *. 100.0);
  num_written

let run ~als ~svx ~entrances =
  let module Filename = Caml.Filename in
  let module Printf = Caml.Printf in
  Printf.printf "Finding contours\n%!";
  let contours = Filename.temp_file "convert_contour" "contours" in
  find_contours ~input:als ~output:contours;
  let wkt = Filename.temp_file "convert_contour" "wkt" in
  Printf.printf "Converting to WKT\n%!";
  convert_to_well_known_text ~input:contours ~output:wkt;
  let wkt_csv = wkt ^ "/contour.csv" in
  let wkt_reader = Caml.open_in wkt_csv in
  let svx_writer = Caml.open_out (svx ^ ".svx") in
  Printf.fprintf svx_writer "*begin %s\n" svx;
  Printf.fprintf svx_writer "*cs UTM33N\n";
  Printf.fprintf svx_writer "*cs out UTM33N\n";
  Printf.fprintf svx_writer "*flags surface\n";
  Printf.fprintf svx_writer
    "*data cartesian from to easting northing altitude\n";
  Printf.printf "Reading contours from WKT.\n%!";
  let contour_groups, extremities =
    read_lines_and_create_contour_groups ~wkt_reader
  in
  Printf.printf "Created %d contours.\n%!" (List.length contour_groups);
  Caml.Printf.printf "Reading entrances .pos file\n%!";
  let entrances = read_entrances ~pos_file:entrances ~extremities in
  Caml.Printf.printf "%d entrances within terrain area.\n%!"
    (List.length entrances);
  let depressions = assemble_depressions ~contour_groups in
  Printf.printf "Found %d depressions.\n%!" (List.length depressions);
  let num_written =
    write_contour_groups ~svx_writer ~contour_groups:depressions ~entrances
  in
  Printf.printf "Writing out %d sufficiently large depressions.\n%!"
    num_written;
  Printf.fprintf svx_writer "*end %s\n" svx;
  Caml.close_in wkt_reader;
  Caml.close_out svx_writer;
  Sys.remove wkt_csv;
  Unix.rmdir wkt;
  List.iter ["dbf"; "shp"; "shx"]
    ~f:(fun suffix -> Sys.remove (contours ^ "/contour." ^ suffix));
  Unix.rmdir contours;
  Printf.printf "Done.\n%!"

let syntax () =
  failwith "syntax: cave_finder ALS-INPUT ENTRANCES-POS-FILE SURVEY-NAME"

let () =
  match Sys.argv with
  | [| _; als; entrances; svx |] -> run ~als ~svx ~entrances
  | _ -> syntax ()
