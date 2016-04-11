let in_channel = stdin in
try
  while true do
    let line = input_line in_channel in
    Printf.printf "line => %s " line
    (* do something with line *)
  done
with End_of_file ->
  close_in in_channel
