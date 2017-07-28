open Core_bench.Bench
open Core

let display_config = Display_config.create ~don't_display_table:true ()

type test = Test of Test.t * (float -> unit)

let make_test test writer = Test (test, writer)

let get_binop opname n1 n2 =
  let to_test f = Test.create ~name:opname (fun () -> (f n1 n2)) in
  match opname with
  | "Z.add" -> to_test Z.add
  | "Z.sub" -> to_test Z.sub
  | "Z.mul" -> to_test Z.mul
  | "Z.ediv_rem" -> to_test Z.ediv_rem
  | "Z.logor" -> to_test Z.logor
  | "Z.logxor" -> to_test Z.logxor
  | "Z.logand" -> to_test Z.logand
  | _ -> failwith (Printf.sprintf "Unknown binop %s" opname)

let get_unop opname n =
  let to_test f = Test.create ~name:opname (fun () -> f n) in
  match opname with
  | "Z.abs" -> to_test Z.abs
  | "Z.neg" -> to_test Z.neg
  | _ -> failwith (Printf.sprintf "Unknown unop %s" opname)


let run_and_write_test ~run_config (Test (test, writer)) =
  let measurement = measure ~run_config [test] in
  match measurement with
  | [measured] -> measured
                  |> analyze
                  |> Or_error.ok_exn
                  |> Simplified_benchmark.get_run_ns
                  |> writer; Pervasives.flush_all ()
  | _ -> failwith "Impossible";;

Random.self_init ~allow_in_tests:true ();;

let generate_numbers (min_val : Z.t) (max_val : Z.t) step =
  let open Z in
  let step = Option.value
      step
      ~default:(fst (ediv_rem (add min_val max_val) (of_int 10))) in
  let rec make_list acc min_val =
    if min_val < max_val
    then make_list (min_val :: acc) (min_val + step)
    else acc
  in make_list [] min_val

let cartesian : 'a.'a List.t -> ('a * 'a) List.t =
  fun l -> List.bind l ~f:(fun ele -> List.map l ~f:(fun x -> (x, ele)))

let inject_randomness (randomness : int) l =
  List.map l ~f:(fun x -> Z.add x (Z.of_int (Random.int randomness)))

(* Generate a random number with n binary digits *)
let digits_generator n = 
  let rec loop acc = function
    | 0 -> acc
    | n -> loop (acc ^ (Int.to_string (Random.int 2))) (n - 1)
  in Z.of_string_base 2 (loop "1" (n - 1))


(* This is a function because the combinators are composable,
but not reusable (as far as I can tell) *)
let default_spec () = 
  Command.Spec.(
    empty
    +> flag "output" (required file) ~doc:"file The file in which to store the benchmark data"
    +> flag "binop" (optional string) ~doc:"operation The binary operation to benchmark"
    +> flag "unop" (optional string) ~doc:"operation The unary operation to benchmark"
    +> flag "duration" (optional_with_default 3 int) ~doc:"duration The duration each benchmark is run for"
  );;

let ranged_spec =
  Command.Spec.(
    empty
    +> flag "min" (required string) ~doc:"min The minimum number to benchmark"
    +> flag "max" (required string) ~doc:"max The maximum number to benchmark"
    +> flag "step" (optional string)
      ~doc:"step The step between the two numbers. If not provided, set to 10% of the difference between the numbers"
    +> flag "randomness" (optional_with_default 0 int)
      ~doc:"randomness How much randomness to inject into each operation. Must be > 0"
    ++ default_spec ())

let exp_spec =
  Command.Spec.(
    empty
    +> flag "min" (required int) ~doc:"min The minimum number of base 2 digits, inclusive"
    +> flag "max" (required int) ~doc:"max The maximum number of base 2 digits, exclusive"
    +> flag "step" (optional_with_default 1 int) ~doc:"step The number of digits to step by (default 1)"
    ++ default_spec ()
  );;

let print_num_benchmarks values =
  Printf.eprintf "Running %d benchmarks\n" (List.length values)

let to_digits n =
  if Z.equal Z.zero n
  then 1
  else Z.log2 n

let common_command_behavior (numbers : Z.t List.t) =
  (fun out_file binop unop duration () ->
     let open Pervasives in
     let file = open_out_gen [Open_text; Open_append; Open_creat] 0o640 out_file in
     let run_config = Run_config.create
         ~verbosity:`Low
         ~time_quota:(Time.Span.create ~sec:duration ())
         () in
     let tests = 
       match binop, unop with
       | None, None -> failwith "Must set either unop or binop"
       | Some binop, None ->
           let values = (cartesian numbers) in
           print_num_benchmarks values;
           List.map values
             ~f:(fun (n1, n2) ->
                 make_test
                   (get_binop binop n1 n2)
                   (fun ns -> Printf.fprintf file "%s\t%s\t%f\n" (Z.to_string n1) (Z.to_string n2) ns; ))
       | None, Some unop ->
           print_num_benchmarks numbers;
           List.map
             numbers
             ~f:(fun n ->
                 make_test
                   (get_unop unop n)
                   (fun ns -> Printf.fprintf file "%s\t%f\n" (Z.to_string n) ns))
       | Some _, Some _ -> failwith "Cannot set both unop and binop" in
     List.iter ~f:(run_and_write_test ~run_config) tests)


let ranged_command =
  Command.basic
    ~summary:"Benchmark the Zarith library by providing a range of numbers"
    ~readme:(fun () -> "More detailed information")
    ranged_spec
    (fun min_val max_val step randomness ->
       common_command_behavior
       @@ inject_randomness randomness
       @@ generate_numbers (Z.of_string min_val) (Z.of_string max_val) (Option.map ~f:Z.of_string step))


let exp_command =
  Command.basic
    ~summary:"Benchmark the Zarith library for a range of lengths"
    ~readme:(fun () -> "Unavailable")
    exp_spec
    (fun min max step ->
       common_command_behavior
         (List.map ~f:digits_generator (List.range ~stride:step min max)))

let command =
  Command.group ~summary:"Manipulate dates"
    [ "ranged", ranged_command; "exp", exp_command ]


let () =
  Command.run ~version:"0.1.0" ~build_info:"Zarith benchmark 0.1.0" command
