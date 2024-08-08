module F = Format

open Rays

let%expect_test _ = 
  let p = Pixel.create (0,0,0) in
  F.printf "%a" Pixel.pp_ppm p;
  [%expect {|0 0 0|}]

let%expect_test _ =
  let p = Pixel.create (10, 20, 30) in
  F.printf "%a" Pixel.pp_ppm p;
  [%expect {|10 20 30|}]

let%expect_test _ =
  let img =Image.create ~height:2~width:3 ~f:(fun ~row ~col -> Pixel.create(row,col,0)) in
  F.printf "%a" Image.pp_ppm img;
  [%expect{||}]