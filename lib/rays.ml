open Stdlib.StdLabels

module F = Format

module Pixel = struct
  type t = int

  let create (r, g, b) =
    let r = (r land 255) lsl 16 in
    let g = (g land 255) lsl 8 in
    let b = (b land 255) in
    r lor g lor b

  let r t = (t lsr 16)
  let g t = (t lsr 8) land 255
  let b t = t land 255

  let zero = create (0, 0, 0)

  let pp_ppm fmt t = 
    F.fprintf fmt "%d %d %d" (r t) (g t) (b t)
end

module Image = struct
  type t = { pixels : Pixel.t array array }

  let create ~height ~width ~f =
    let pixels = Array.make_matrix ~dimx:height ~dimy:width Pixel.zero in
    for row = 0 to height - 1 do
      for col = 0 to width - 1 do
        pixels.(row).(col) <- f ~row ~col
      done
    done;
    { pixels }

  let pp_ppm fmt { pixels } =
    let nums_rows = Array.length pixels in
    let nums_cols = Array.length pixels.(0) in
    F.fprintf fmt "P3\n%d %d\n255\n" nums_cols nums_rows;
    for row = 0 to nums_rows - 1 do
      for col = 0 to nums_cols - 1 do
        Pixel.pp_ppm fmt pixels.(row).(col);
        F.fprintf fmt " "
      done;
      F.fprintf fmt "\n"
    done
end

module Vec3d = struct
  type t = float * float * float

  let ( + ) (v11, v12, v13) (v21, v22, v23)  =
    (v11 +. v21, v12 +. v22, v13 +. v23)
  let ( - ) (v11, v12, v13) (v21, v22, v23)  =
    (v11 -. v21, v12 -. v22, v13 -. v23)
    let ( * ) a (v1, v2, v3) = (a *. v1, a *. v2, a *. v3)
    let ( / ) (v1 ,v2, v3) a = (v1 /. a, v2  /. a, v3 /. a)
  let length_squared (v1, v2, v3) = (v1 *. v1) +. (v2 *. v2) +. (v3 *. v3)
  let length v = sqrt (length_squared v)
end

module Point3d = Vec3d

module Color = struct
include Vec3d
  let to_pixel t =
    let factor = 255.99 in
    let r, g, b = factor * t in
    Pixel.create (int_of_float r, int_of_float g, int_of_float b)
end

module Ray = struct
  type t = {origin: Point3d.t; dir: Vec3d.t}
  let at scale {origin; dir} = Vec3d.(origin + (scale * dir))
end