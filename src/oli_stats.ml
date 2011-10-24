let pi = 3.1415926535897932385

let draw_uniform a b = 
  let delta = b -. a in 
    a +. Random.float delta

let draw_iso_angle () = 
  let cos_theta = draw_uniform (-1.0) 1.0 in 
    acos cos_theta

let draw_from_dist ?(xmin = 0.0) ?(xmax = 1.0) ?(ymin = 0.0) ?(ymax = 1.0) f = 
  let rec loop x y = 
    if f x > y then 
      x
    else
      loop (draw_uniform xmin xmax) (draw_uniform ymin ymax) in 
    loop (draw_uniform xmin xmax) (draw_uniform ymin ymax) 

let draw_radius_by_volume () = 
  let v = Random.float 1.0 in 
    v**(1.0/.3.0)
    
  
