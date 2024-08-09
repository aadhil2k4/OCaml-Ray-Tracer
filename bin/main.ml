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

let raytraced_image () =
  let aspect_ratio = 16.0 /. 9.0 in
  let image_width = 400 in
  (*Image height Calculation*)
  let image_height = float_of_int image_width /. aspect_ratio |> int_of_float in
  (*Camera*)
  let focal_length = 1.0 in
  let viewport_height = 2.0 in
  let viewport_width = 
    viewport_height *. (float_of_int image_width /. float_of_int image_height)
  in
  let camera_center = (0.,0.,0.) in
  (*Calculate vectors left to right,top to bottom*)
  let viewport_lr = (viewport_width, 0., 0.) in
  let viewport_td = (0., -.viewport_height, 0.) in
  (*Delta Vectors Calculation*)
  let pixel_delta_lr = Vec3d.(viewport_lr /! image_width) in
  let pixel_delta_td = Vec3d.(viewport_td /! image_height) in
  (*Calc loc of upper left pixel*)
  let viewport_upper_left = Vec3d.(camera_center - (0., 0., focal_length) - viewport_lr / (float_of_int 2)
  - (viewport_td /! 2))
in
  (*Image Creation*)
    Image.create ~height: image_height ~width: image_width ~f:(fun ~row ~col -> Pixel.create (row, col, 0))

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
