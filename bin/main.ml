open Rays

let _sample_image () =
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

let ray_color _ray = (0., 0.,0.)

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
  let pixel00_loc = Vec3d.(viewport_upper_left + (0.5*(pixel_delta_lr + pixel_delta_td))) in
let render ~row ~col = 
  let pixel_center = Vec3d.(pixel00_loc + (col *! pixel_delta_lr) + (row *! pixel_delta_td)) in
  let ray_direction = Vec3d.(pixel_center - camera_center) in
  let ray = Ray.{origin= camera_center; dir= ray_direction} in
  let color = ray_color ray in
  Color.to_pixel color
in
  (*Image Creation*)
    Image.create ~height: image_height ~width: image_width ~f:render 

let () =
    let img = raytraced_image() in
    Out_channel.with_open_text "sample.ppm" (fun oc->
      let fmt = Format.formatter_of_out_channel oc in
      Format.fprintf fmt "%a" Image.pp_ppm img);
    Format.printf "Image generated successfully\n"