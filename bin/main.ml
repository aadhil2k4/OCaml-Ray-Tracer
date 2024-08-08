open Rays

let sample_image () =
  let image_width = 256 in
  let image_height = 256 in
  let render ~row ~col = 
    let r = float_of_int row /. float_of_int (image_height - 1) in
    let g = float_of_int col /. float_of_int (image_width - 1) in
    let b = 0.0 in  
    let factor = 255.999 in
    let r = factor *. r |> int_of_float in
    let g = factor *. g |> int_of_float in
    let b = factor *. b |> int_of_float in
    Pixel.create (r, g, b)
  in
  Image.create ~height:image_height ~width:image_width ~f:render

let () =
  let oc = Out_channel.open_text "sample.ppm" in
  try
    let fmt = Format.formatter_of_out_channel oc in
    let img = sample_image () in 
    Format.fprintf fmt "%a" Image.pp_ppm img;
    Format.printf "Image generated\n";
    Out_channel.close oc
  with ex ->
    Format.eprintf "%s" (Printexc.to_string ex);
    Out_channel.close_noerr oc
