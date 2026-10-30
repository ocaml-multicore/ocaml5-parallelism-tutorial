module T = Domainslib.Task

let (num_domains,x,y,z) =
  try
    let num_domains = int_of_string Sys.argv.(1) in
    let x = int_of_string Sys.argv.(2) in
    let y = int_of_string Sys.argv.(3) in
    let z = int_of_string Sys.argv.(4) in
    (num_domains,x,y,z)
  with _ -> (1,18,12,6)

let rec tak x y z =
  if x > y then
    tak (tak (x-1) y z) (tak (y-1) z x) (tak (z-1) x y)
  else z

let rec tak_par p x y z =
  if x < 20 && y < 20
  then tak x y z
  else
    let a = T.async pool (fun _ -> tak_par p (x-1) y z) in
    let b = T.async pool (fun _ -> tak_par p (y-1) z x) in
    let c = T.async pool(fun _ -> tak_par p (z-1) x y) in
    tak (T.await p a) (T.await p b) (T.await p c)

let main () =
  let p = T.setup_pool ~num_domains:(num_domains - 1) () in
  let r = T.run p (fun _ -> tak_par p x y z) in
  T.teardown_pool p;
  Printf.printf "tak %d %d %d = %d\n" x y z r

let _ = main ()
