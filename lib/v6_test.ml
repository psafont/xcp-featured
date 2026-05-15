let features =
  Alcotest.testable Fmt.(Dump.list @@ Dump.pair string string) ( = )

let test_additional_features =
  let test () =
    let xapi_ones = Features.all_features |> Features.to_assoc_list in
    let xcpng_ones = Xcp_ng.V6.Additional.params in
    let in_both =
      List.filter (fun (k, _) -> List.mem_assoc k xapi_ones) xcpng_ones
    in
    Alcotest.(check features)
      "A base feature can't be present in the additional ones" [] in_both
  in
  ("Not present in xapi features", `Quick, test)

let feature_tests = ("Additional features", [test_additional_features])

let test_editions =
  let test () =
    let open V6_interface in
    let edition = "" in
    let expected =
      Xcp_ng.V6.get_editions "" |> List.hd |> function {title; _} -> title
    in
    let {edition_name; _} = Xcp_ng.V6.apply_edition_test "dbg" edition [] in
    let msg = Printf.sprintf "Edition must be '%s'" expected in
    Alcotest.(check string) msg expected edition_name
  in
  ("Always apply the edition 'xcp-ng'", `Quick, test)

let edition_tests = ("Editions", [test_editions])

let () = Alcotest.run "XCP-ng V6 library" [feature_tests; edition_tests]
