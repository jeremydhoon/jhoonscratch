(* bitmap writing function *)
let write_bitmap fname w h clrs =
  let f = open_out_bin fname in
  let ob b = output_byte f b in
  let oi i =
    let rec helper rem =
      match rem with
      | 0 -> ()
      | _ -> (ob ((i lsr ((4 - rem)*8)) land 0xFF);
	helper (rem - 1))
    in
    helper 4
  in
  let rowpadding = (4 - ((3*w + 4)/4 - 1)) mod 4 in
  let rec pad n =
    match n with
    | 0 -> ()
    | _ -> (
      ob 0;
      pad (n-1))
  in
  let rec writerow rem clrs =
    match rem with
    | 0 -> (pad rowpadding; clrs)
    | _ -> (
      let curr = List.hd clrs in
      ob curr#b_byte; ob curr#g_byte; ob curr#r_byte;
      writerow (rem - 1) (List.tl clrs))
  in
  ob 0x42; ob 0x4D; (*magic number - 2 bytes*)
  oi (54 + (4 * (24*w + 31)/32)); (* BMP file size - 4 bytes*)
  oi 0x0; (* 4 empty bytes, for application specific purposes *)
  oi 54; (*location of start of bitmap data*)
  (*START V3 HEADER *)
  oi 40; (*header size*)
  oi w; (*bitmap width in pixels*)
  oi h; (*bitmap height in pixels *)
  ob 1; ob 0; (*little-endian way to indicate 1 color plane using 2 bytes*)
  ob 24; ob 0; (*24 bit color depth. little-endian*)
  oi 0; (*no compression*)
  oi 0; (*would be the image size field if we were using compression*)
  oi 2835; (*2835 pixels per meter horiz. res.*)
  oi 2835; (*2835 pixels per meter vert. res. *)
  oi 0; (*default number of colors in palette (2^(color depth)) *)
  oi 0; (* "import color" pretty much ignored *)

  (*DONE WITH THE BITMAP HEADER*)
  let rec helper rem clrs =
    match rem with
    | 0 -> ()
    | _ -> (
      helper (rem - 1) (writerow w clrs))
  in
  helper h clrs;
  close_out f
;;


class vector3 (x : float) (y : float) (z : float) =
  object (self : 'a)
    method x = x
    method y = y
    method z = z
    method dot (v3 : 'a) =
      (x *. v3#x) +. (y *. v3#y) +. (z *. v3#z)
    method cross (v3 : 'a) =
      new vector3 ((y *. v3#z) -. (z *. v3#y))
          ((x *. v3#z) -. (z *. v3#x)) ((x *. v3#y) -. (y *. v3#x))
    method sum (v3 : 'a) =
      new vector3 (x +. v3#x) (y +. v3#y) (z +. v3#z)
    method difference (v3 : 'a) =
      new vector3 (x -. v3#x) (y -. v3#y) (z -. v3#z)
    method inverse =
      new vector3 (-. x) (-. y) (-. z)
    method magnitude =
      sqrt ((x *. x) +. (y *. y) +. (z *. z))
    method scale (scale_factor : float) =
      new vector3 (x *. scale_factor) (y *. scale_factor) (z *. scale_factor)
    method norm =
      self#scale (1.0 /. self#magnitude)
    method tostring =
      Printf.sprintf "<%.2f, %.2f, %.2f>" x y z
  end
;;


let vector_tests a b =
  print_endline "Vector tests";
  print_endline (string_of_float (a#dot b));
  print_endline (a#cross b)#tostring;
  print_endline (a#sum b)#tostring;
  print_endline (a#inverse)#tostring;
;;
 
let test_v3_a = new vector3 4.0 1.0 0.5;;
let test_v3_b = new vector3 0.2 3.0 (-.1.0);;
 
let _COLOR_RANGE = 255.0
let _COLOR_RANGE_INT = 256
let channel_to_byte channel = (int_of_float (_COLOR_RANGE *. channel)) mod _COLOR_RANGE_INT;;

class color r g b =
  object (self)
    method r = r
    method g = g
    method b = b
    method scale factor =
      new color (r *. factor) (g *. factor) (b *. factor)
    method mix (clr : color) own_fraction =
      let other_fraction = 1.0 -. own_fraction in
      new color ((r *. own_fraction) +. (clr#r *. other_fraction))
          ((g *. own_fraction) +. (clr#g *. other_fraction))
          ((b *. own_fraction) +. (clr#b *. other_fraction))
    method r_byte = channel_to_byte r
    method g_byte = channel_to_byte g 
    method b_byte = channel_to_byte b
    method tointeger = (self#r_byte lsl 16) + (self#g_byte lsl 8) + self#b_byte
    method tostring = Printf.sprintf "(%d, %d, %d)" self#r_byte self#g_byte self#b_byte
    method tohex = 
      let i = self#tointeger in
      let red = (i asr 16) land 0xff in
      let green = (i asr 8) land 0xff in
      let blue = i land 0xff in
      Printf.sprintf "%02X%02X%02X" red green blue
    method tohtml = "#" ^ self#tohex
  end
;;
 
let color_tests a b =
  print_endline "Color tests";
  print_endline (string_of_int a#tointeger);
  print_endline (string_of_int b#tointeger);
  print_endline (a#tostring);
  print_endline (b#tostring);
  print_endline (a#tohtml);
  print_endline (b#tohtml);
;;
 
let test_color_a = new color 1.0 0.5 0.0;;
let test_color_b = new color 0.1 0.95 0.2;;
 
class intersection (pos : vector3) (norm : vector3) (clr : color) (dist : float) (reflectivity : float) =
  object (self)
    method pos = pos
    method norm = norm
    method color = clr
    method distance = dist
    method reflectivity = reflectivity
    method tostring =
      Printf.sprintf "Position:\t%s\nNormal Vector:\t%s\nShape Color:\t%s\nDistance:\t%.2f"
          pos#tostring norm#tostring clr#tostring dist
  end
;;

class virtual shape (pos : vector3) (clr : color) (reflectivity : float) =
  object (self)
    method pos = pos
    (* given two points (ray origin and ray direction),
       get the intersection with the shape, or None. Note
       that we expect the direction to be a unit vector. *)
    method virtual intersect : vector3 -> vector3 -> intersection option
  end
;; 

class sphere pos clr reflectivity (radius : float) =
  object (self)
    inherit shape pos clr reflectivity
    val r2 = radius *. radius
    method intersect (src : vector3) (dir : vector3) =    
      let orig = pos#difference src in
      let cdotd = orig#dot dir in
      let cmag2 = orig#dot orig in
      let discrim = ((cdotd*.cdotd) -. cmag2) +. r2 in
      if discrim < 0.0 then ((*Printf.printf "%s\n%s\n%f\n%f %f %f\n%f\n" orig#tostring dir#tostring ((dir#norm)#dot dir) cdotd cmag2 discrim dir#magnitude;*) None)
      else (
        (*Printf.printf "non-negative discrim for direction %s\n" dir#tostring; *)
        let sqrtdisc = sqrt discrim in
        (*let dist = min (cdotd -. sqrtdisc) (cdotd +. sqrtdisc) in*)
        let dist = cdotd -. sqrtdisc in
        if dist < 0.0 then None
        else (
          (*Printf.printf "Intersected at distance %f\n" dist;*)
          let isectpos = (dir#scale dist)#sum src in
          (*let norm = ((dir#scale dist)#difference orig)#norm in*)
          (*let norm = (new vector3 0.5 0.5 0.5)#norm#inverse in*)
          let norm = (isectpos#difference pos)#norm in
          (*print_endline norm#tostring;
          print_endline (string_of_float ((isectpos#difference pos)#magnitude));
          print_endline (string_of_float dist);
          print_endline isectpos#tostring;
          print_endline dir#tostring;
          print_endline pos#tostring;*)
          Some (new intersection (isectpos#sum (norm#scale (-.0.01))) norm clr (abs_float dist) reflectivity)))
  end
;;

let test_sphere_origin = new vector3 1.0 0.0 0.0;;
let test_sphere_color = new color 0.5 0.3 0.9;;
let test_sphere_radius = 2.23;;
let test_sphere_reflectivity = 1.0;;
let test_sphere = new sphere test_sphere_origin test_sphere_color test_sphere_reflectivity test_sphere_radius;;
let test_sphere_src = new vector3 (-.5.0) (-.5.0) (-.5.0);;
let test_sphere_dir = (new vector3 1.0 1.0 1.0)#norm;;

let sphere_tests s src dir =
  print_endline "Testing sphere";
  match s#intersect src dir with
  | None -> print_endline "No intersection"
  | Some isect -> print_endline isect#tostring
;;

class plane pos clr reflectivity normvector =
  object (self)
    inherit shape pos clr reflectivity
    val _d = pos#dot normvector
    method private offset = normvector#scale 0.01
    method intersect src dir =
      let perp_component = normvector#dot dir in
      if (abs_float perp_component) < 0.0001 then (None)
      else (
        let numerator = _d -. (normvector#dot src) in
        let dist = numerator /. perp_component in
        let isect_pos = src#sum (dir#scale dist) in
        if dist < 0.001 then None else (
        (*Printf.printf "dist: %f\tposition: %s\n" dist isect_pos#tostring;*)
        Some (new intersection isect_pos normvector clr (abs_float dist) reflectivity)))
  end
;;

let test_plane = new plane (new vector3 13.0 0.0 0.0) (new color 1.0 0.0 0.0) 0.2 (new vector3 1.0 0.0 0.0);;        

type ray_t = {src : vector3; dir : vector3; x : int; y : int; isect : intersection option};;

class camera (pos : vector3) (forward : vector3) (up : vector3) (view_angle_x : float) =
  object (self)
    val right = forward#cross up
    val true_up = (forward#cross up)#cross forward
    method pos = pos
    method project x y xcoor ycoor = 
      let dir = ((forward#scale 1.0)#sum (
        (right#scale (sin x))#sum (
          (true_up#scale (sin y)))))#norm in
      (*Printf.printf "ray dir: %s\tx: %f\ty: %f\n" dir#tostring x y;*)
      {src = pos; dir = dir; x = xcoor; y = ycoor; isect = None}
    method rays (xdim : int) (ydim : int) =
      let fx = float_of_int xdim in
      let fy = float_of_int ydim in
      let aspect_ratio = fx /. fy in 
      let view_angle_y = view_angle_x /. aspect_ratio in
      let dthetax = view_angle_x /. fx in
      let dthetay = view_angle_y /. fy in
      let start_theta_x = -. (view_angle_x /. 2.0) in
      let start_theta_y = -. (view_angle_y /. 2.0) in
      let rec list_rays x y xcoor ycoor ret =
        if xcoor < xdim && ycoor < ydim then
          list_rays (x +. dthetax) y (xcoor + 1) (ycoor) ((self#project x y xcoor ycoor) :: ret)
        else if xcoor == xdim && ycoor < ydim then
          list_rays start_theta_x (y +. dthetay) 0 (ycoor + 1) (ret)
        else ret
      in
      list_rays start_theta_x start_theta_y 0 0 []
  end
;;

let test_cam = new camera (new vector3 (-. 5.0) 0.0 0.0)
    (new vector3 1.0 0.0 0.0)#norm
    (new vector3 0.0 1.0 0.0)#norm
    (2.0 *. 3.14159265358979626 /. 3.0);;

let camera_tests cam x y =
  print_endline "Testing camera";
  print_endline (string_of_int (List.length (cam#rays x y)))
;;

class light (pos : vector3) (brightness : float) =
  object (self)
    method pos = pos
    method brightness = brightness
  end
;;

let test_light = new light (new vector3 ( -.3.0) 2.0 0.0) 0.99;;

class scene (cam : camera) (shapes : shape list) (lights : light list) (bgclr : color) (depth : int) (ambient : float) =
  object (self)
    method rays_to_color rays =
      let rec make_color rem_rays prev_refl =
        match rem_rays with
        | (ray :: []) -> (
          match ray.isect with
          | None -> bgclr#scale (self#check_lights ray.src ray.dir#norm)
          | Some isect -> (isect#color#scale (min 1.0 (ambient +. (self#check_lights isect#pos isect#norm)))))
        | (ray :: (tail : ray_t list)) -> (
            match ray.isect with
            | Some isect -> (
                let own_color = make_color tail isect#reflectivity in
                let clr_black = new color 0.0 0.0 0.0 in
                let reflect_color =
                    if prev_refl > 0.01 then
                        isect#color#scale (self#check_lights isect#pos isect#norm)
                    else 
                       clr_black
                in
                own_color#mix reflect_color 0.5)
            | None -> raise (invalid_arg "Got None for intersection before last ray") 
          )
        | _ -> raise (invalid_arg "make_color: Empty list of rays provided.")
      in
      make_color rays 1.0
    method check_lights src normvector =
      let rec check_rec rem_lights brightness =
        match rem_lights with
        | [] -> min 1.0 brightness
        | ((l : light) :: rest) -> (
            let dir = (src#difference l#pos)#norm in
            (*let dir = (src#difference l#pos)#norm in*)
            let light_dist = (l#pos#difference src)#magnitude in
            let rec check_shapes = function
              | [] -> (                
                let scale_factor = max (-.(dir#dot normvector)) 0.0 in
                check_rec rest (brightness +. (l#brightness *. scale_factor)))
              | ((s : shape) :: shape_rest) -> (
                match s#intersect src dir#inverse with
                | None -> check_shapes shape_rest
                | Some isect when light_dist < isect#distance -> (check_shapes shape_rest)
                | Some isect -> (check_rec rest brightness))
            in
            check_shapes shapes)
      in
      check_rec lights 0.0
    method cast_ray source_ray nbounces =
      let rec helper ray bounces previous_rays =
        match bounces with
        | 0 -> ((*print_endline (string_of_int (List.length previous_rays));*) self#rays_to_color previous_rays)
        | n -> (
            let rec examine_shapes (shapes_list : shape list) (best_isect : intersection option) =
              match shapes_list with
              | [] -> best_isect
              | s :: rest -> (
                let rdir = ray.dir in (*if n == nbounces then ray.dir else ray.dir in*)
                match s#intersect ray.src rdir with
                | None -> examine_shapes rest best_isect
                | Some isect -> (
                  match best_isect with
                  | None -> examine_shapes rest (Some isect)
                  | Some b_isect when b_isect#distance < isect#distance -> examine_shapes rest best_isect
                  | Some b_isect -> examine_shapes rest (Some isect)))
            in
            let isect = examine_shapes shapes None in
            match isect with
            | None -> new color 1.0 0.0 0.0 (*bgclr*)
            | Some (i : intersection) -> (
              let norm_component = i#norm#scale (i#norm#dot ray.dir) in
              let next_dir = (ray.dir#difference (norm_component))#inverse#sum norm_component in
              let next_ray = {src = i#pos; dir = next_dir#norm; x = ray.x; y = ray.y; isect = isect} in
              helper next_ray (n - 1) (next_ray :: previous_rays)))
      in
      helper source_ray nbounces []
    method render (xdim : int) (ydim : int) = 
      let rays = cam#rays xdim ydim in
      let rec cast_rays rem ret =
        match rem with
        | [] -> (List.rev ret)
        | (ray :: shape_rest) -> cast_rays shape_rest ((self#cast_ray ray depth) :: ret)
      in
      cast_rays rays []
    method render_to_bmp xdim ydim fname =
      write_bitmap fname xdim ydim (self#render xdim ydim)
  end


let light_2 = new light (new vector3 0.0 2.0 3.0) 0.99;;
let clr_green = new color 0.0 1.0 0.5;;
let clr_purple = new color 1.0 0.0 1.0;;
let plane_refl = 1.0;;
let small_sphere = new sphere (new vector3 1.0 0.5 (-. 3.5)) clr_purple 1.0 0.15;;
let bottom_plane = new plane (new vector3 0.0 (-. 4.0) 0.0) clr_green plane_refl (new vector3 0.0 1.0 0.0);;
let left_plane = new plane (new vector3 0.0 0.0 (-. 6.0)) clr_green plane_refl (new vector3 0.0 0.0 1.0);;
let right_plane = new plane (new vector3 0.0 0.0 6.0) clr_green plane_refl (new vector3 0.0 0.0 (-.1.0));;
let top_plane = new plane (new vector3 0.0 4.0 0.0) clr_green plane_refl (new vector3 0.0 (-.1.0) 0.0);;

let test_scene = new scene test_cam [top_plane; test_sphere; small_sphere; bottom_plane; left_plane; right_plane]
    [test_light] (new color 0.0 1.0 0.0) 2 0.25;;
test_scene#render_to_bmp 128 96 "raytracer2_test.bmp";;
  
(*
vector_tests test_v3_a test_v3_b;;
color_tests test_color_a test_color_b;;
sphere_tests test_sphere test_sphere_src test_sphere_dir;;
camera_tests test_cam 256 192;;
*)
