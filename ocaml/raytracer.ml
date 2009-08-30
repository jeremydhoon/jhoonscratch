let _COORS_LIST = [0;1;2];;
let _PI = 3.1415926535879626;;
let _DRAW_DISTANCE = 999999999999.0;;

let square x = x*.x;;
let l2distance source dest =
   List.fold_left (fun a b -> a +. b) 0.0 (List.map square (List.map (fun n -> (source#getcoor n) -. (dest#getcoor n)) _COORS_LIST))
;;
let norm_scalar iger minval maxval =
  (float_of_int iger) /. (float_of_int (maxval - minval))
;;

class color r g b =
  object(self)
    val mutable my_r = r land 0xFF
    val mutable my_g = g land 0xFF
    val mutable my_b = b land 0xFF
    method as_int =
      (my_r lsl 16) + (my_g lsl 8) + my_r
    method getr = my_r
    method getg = my_g
    method getb = my_b
  end;;

let blend c1 c2 =
  new color (c1#getr + c2#getr) (c1#getg + c2#getg) (c1#getb + c2#getb)
;;
(*let project lgt surface =
  let tot = float_of_int (surface#getr + surface#getg + surface#getb) in
  new color (int_of_float ((float_of_int lgt#getr) *. (float_of_int surface#getr)/.tot))
      (int_of_float ((float_of_int lgt#getg) *. (float_of_int surface#getg)/.tot))
      (int_of_float ((float_of_int lgt#getb) *. (float_of_int surface#getb)/.tot))
;;*)
let project lgt surface =
  lgt
;;
(*
let dim c factor =
  let recip = int_of_float (1.0/.factor) in
  new color (c#getr / recip) (c#getg / recip) (c#getb / recip)
;;
*)
let dim c factor =
  let convert channel =
    int_of_float ((float_of_int channel) *. factor)
  in
  new color (convert c#getr) (convert c#getg) (convert c#getb)
;;

class vector3 =
  object(self)
      val mutable my_x = 0.0
      val mutable my_y = 0.0
      val mutable my_z = 0.0
    method set_position xin yin zin =
      my_x <- xin;
      my_y <- yin;
      my_z <- zin;
      self
    method set_v3_position (v3 : vector3) =
      self#set_position (v3#getx) (v3#gety) (v3#getz)
    method getcoor n =
      match n with
    	| 0 -> my_x
	    | 1 -> my_y
	    | 2 -> my_z
	    | _ -> raise Not_found
    method setcoor n coor =
      match n with
	    | 0 -> my_x <- coor; ()
	    | 1 -> my_y <- coor; ()
	    | 2 -> my_z <- coor; ()
	    | _ -> raise Not_found	
    method list_coors =
      List.map (fun x -> self#getcoor x) _COORS_LIST
    method sum (v3 : vector3) =
      let ret = new vector3 self#getcolor in
      List.map (fun n -> ret#setcoor n ((self#getcoor n) +. (v3#getcoor n))) _COORS_LIST;
      ret
    method inverse =
      let ret = new vector3 self#getcolor in
      List.map (fun n -> ret#setcoor n (0.0 -. (self#getcoor n))) _COORS_LIST;
      ret
    method difference (v3: vector3) =
      self#sum (v3#inverse)
    method magnitude =
      sqrt (List.fold_left (fun acc x -> acc +. x*.x) 0.0 (List.map (fun n -> self#getcoor n) _COORS_LIST))
    method normalize =
      let ret = new vector3 self#getcolor in
      let mag = self#magnitude in
      List.map (fun n -> ret#setcoor n ((self#getcoor n) /. mag)) _COORS_LIST;
      ret
    method dot (v3: vector3) =
      List.fold_left2 (fun acc l r -> acc +. (l*.r)) 0.0 (v3#list_coors) (self#list_coors)
    method scale x =
      let ret = new vector3 self#getcolor in
      List.map2 (fun n m -> ret#setcoor n (x*.m)) _COORS_LIST self#list_coors;
      ret
    method cross (v3: vector3) =
      let ret = new vector3 self#getcolor in
      ret#set_position ((self#gety *. v3#getz) -. (self#getz *. v3#gety)) ((self#getz *. v3#getx) -. (self#getx *. 
          v3#getz)) ((self#getx *. v3#gety) -. (self#gety *. v3#getx))
    method getx = my_x
    method gety = my_y
    method getz = my_z
    method print =
      print_endline ((string_of_float my_x) ^ ", " ^ (string_of_float my_y) ^ ", " ^ (string_of_float my_z))
  end;;

let _ZERO_VECTOR = new vector3;;

let rec extractvalid ret lst =
  match lst with
    | [] -> ret
    | _ -> (
      let curr = List.hd lst in
      match curr with
	| None -> extractvalid ret (List.tl lst)
	| Some (v3) -> extractvalid (v3 :: ret) (List.tl lst))
;;

class light (clr : color) (v3 : vector3) intensity =
  object(self)
    val mutable my_clr = clr
    val mutable my_pos = v3
    val mutable my_inty = if intensity > 1.0 then 1.0 else if intensity < 0.0 then 0.0 else intensity
    method getpos = my_pos
    method getcolor = my_clr
    method getinty = my_inty
  end;;

class intersection (pos : v3) (clr : color) (norm : v3) =
  object(self)
    val mutable my_pos = pos
    val mutable my_clr = clr
    val mutable my_norm = norm
    method getpos = my_pos
    method getcolor = my_clr
    method getnorm = my_norm
  end;;

class virtual shape x y z =
  object(self)
    val mutable center = (new vector3)#set_position x y z
    val mutable my_color = new color 255 255 255
    method getcolor = my_color
    method setcolor c = my_color = c
    method virtual intersect: vector3 -> vector3 -> (vector3 option)
  end;;

class sphere x y z radius =
  object(self)
    val mutable my_radius = radius
    inherit shape x y z
    method norm location =
      (* we assume the location actually intersects the sphere *)
      (center#difference location)#normalize
    method intersect source direction =
      let cntr = (center#difference source) in
      let dir = direction#normalize in
      let b = cntr#dot dir in
      let discriminant = (square b) -. (cntr#dot cntr) +. (radius*.radius) in
      if discriminant < 0.0 then None
      else (
	      let addend = sqrt (discriminant) in
	      let one,two = ((-.b +. addend), (-.b -. addend)) in
	      let makeret scalar =
	        let retv3 = (new vector3)#set_v3_position ((dir#scale (-.scalar))#sum source) in
	        Some (new intersection retv3 self#getcolor (self#norm retv3)) 
	      in
	      let ret =
	        if one < 0.0 && two < 0.0 then None
	        else if one > two then makeret one
	        else makeret two
	      in
	      ret)
  end;;

class shapecollection x y z =
  object(self)
    inherit shape x y z
    val mutable my_shapes = ([] : shape list)
    method intersect source direction =
      let rec helper shapes ret =
	      match shapes with
	      | [] -> ret
	      | _ -> helper (List.tl shapes) (((List.hd shapes)#intersect source direction) :: ret)
      in	  
      let distpairs = List.map (fun x -> (l2distance center x, x)) (extractvalid [] (helper my_shapes [])) in
      match distpairs with
	    | [] -> None
	    | _ -> (
	      let retv3 = (new vector3)#set_v3_position (snd (List.fold_left (fun a b -> if (fst a) < (fst b) then a else b)
	        Some ( (0.0, _ZERO_VECTOR) distpairs)))
  end;;

class camera w h =
  object(self)
    val mutable my_width = w
    val mutable my_height = h
    val mutable my_pos = _ZERO_VECTOR
    val mutable my_front = (new vector3 None)#set_position 1.0 0.0 0.0
    val mutable my_up = (new vector3 None)#set_position 0.0 1.0 0.0
    method getpos = my_pos
    method set_v3_position v3 =
      my_pos <- v3;
      self
    method getrays =
      my_front <- my_front#normalize;
      my_up <- my_up#normalize;
      let viewing_angle_x = _PI /. 2.0 in
      let viewing_angle_y = viewing_angle_x *. float_of_int(h) /. float_of_int(w) in
      let my_right = (my_front#cross my_up)#normalize in
      (*print_endline "RIGHT:";
      my_right#print;*)
      let ray xin yin =
	let x = (norm_scalar xin 0 w) -. 0.5 in
	let y = (norm_scalar yin 0 h) -. 0.5 in
	((my_front#scale (-.1.0))#sum ((my_right#scale (sin (x*.viewing_angle_x)))#sum (my_up#scale (sin (y*.viewing_angle_y)))))#normalize
      in
      let rec helper ret xrem yrem =
	match yrem with
	  | -1 -> ret
	  | _ -> (
	  match xrem with
	    | -1 -> helper ret (w-1) (yrem-1)
	    | _ -> helper ((ray xrem yrem) :: ret) (xrem-1) yrem)
      in
      helper [] (w-1) (h-1)
  end;;

class scene (bgclr : color)  (cam : camera)=
  object(self)
    val mutable my_shapes = ([] : shape list)
    val mutable my_lights = ([] : light list)
    val mutable my_bgclr = bgclr
    val mutable my_camera = cam
    method add_shape s =
      my_shapes <- s :: my_shapes
    method add_light l =
      my_lights <- l :: my_lights
    method get_lighting v3 norm =
      let rec helper rem ret =
	      match rem with
	      | [] -> ret
	      | _ ->
	        let currlight = List.hd rem in
	        let dir = (currlight#getpos)#difference v3 in
	        let isect = self#intersect_ray ((v3#scale (1.0))#sum (dir#scale (0.1))) (dir#scale (-.1.0)) in
	        match isect with
	        | None -> (
	        (*print_endline ("magnitude:" ^ (string_of_float dir#magnitude));
	        print_endline "LGT:";
	        currlight#getpos#print;25
	        print_endline "V3:";
	        v3#print;*)
	          helper (List.tl rem) (blend ret (dim currlight#getcolor (4.0 /. (square dir#magnitude)))))
	        | Some(v3_2) -> (
	          (*print_endline "GOT INTERSECT:";
	          v3_2#print;
	          v3#print;*)
	        helper (List.tl rem) ret)
        in
      project (helper my_lights (new color 25 25 25)) v3#getcolor_or_err
    method intersect_ray source direction =
      let rec find_intersections ret rem =
        match rem with
	      | [] -> ret
	      | _ -> find_intersections (((List.hd rem)#intersect source direction) :: ret) (List.tl rem)
      in
      let cmp best isect =
	      let mag = (source#difference isect)#magnitude in
	      if mag < (snd best) then (isect,mag)
	      else best
      in
      let getcmpfirst valid_list = 
	      let first = List.hd valid_list in
	      (first, (source#difference first)#magnitude)
      in
      let valid = extractvalid [] (find_intersections [] my_shapes) in
      match valid with
	    | [] -> None
	    | _ -> (
	      (*(List.nth valid 0)#print;*)
	      Some (fst (List.fold_left cmp (getcmpfirst valid) valid)))
    method handle_ray source direction =
      match self#intersect_ray source direction with
	    | None -> my_bgclr
	    | Some(v3) -> self#get_lighting v3
    
    method render =
      let rays = my_camera#getrays in
      let rec getmax rem best =
      	match rem with
	      | [] -> best
	      | _ -> (
	        let curr = List.hd rem in
	        getmax (List.tl rem) (min (curr#gety) best))
      in
      let rec helper ret rem =
	      match rem with
	      | [] -> ret
	      | _ -> (
	        let curr = (List.hd rem) in
	        helper ((self#handle_ray my_camera#getpos curr) :: ret) (List.tl rem))
        in
      helper [] rays
  end;;

let swap i =
  ((i land 0xFF) lsl 24) + (((i lsr 8) land 0xFF) lsl 16) + (((i lsr 16) land 0xFF) lsl 8) + ((i lsr 24) land 0xFF)
;;
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
      ob curr#getb; ob curr#getg; ob curr#getr;
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
let cam = new camera 1024 640;;
let sph = new sphere 10.0 0.0 0.0 3.0;;
let sc = new scene (new color 0 255 0) cam;;
let lt = new light (new color 255 0 0) ((new vector3 None)#set_position 18.0 1.25 0.0) 1.0;;
let lt2 = new light (new color 0 0 255) ((new vector3 None)#set_position 12.0 (-.5.0) 2.0) 1.0;;
let lt3 = new light (new color 0 255 0) ((new vector3 None)#set_position 12.0 (0.0) 5.0) 1.0;;
let lt4 = new light (new color 255 255 255) ((new vector3 None)#set_position 12.0 0.0 0.0) 1.0;;
sph#setcolor (new color 255 255 255);;
sc#add_light lt;;
sc#add_light lt2;;
sc#add_light lt3;;
sc#add_light lt4;;
sc#add_shape sph;;
let img = sc#render;;
write_bitmap "./out.bmp" 1024 640 img;;
print_endline (string_of_int (List.length img))
