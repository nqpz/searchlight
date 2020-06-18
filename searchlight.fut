import "lib/github.com/diku-dk/cpprandom/random"
import "lib/github.com/athas/vector/vspace"
import "lib/github.com/diku-dk/lys/lys"

module rnge = xorshift128plus
module dist = uniform_real_distribution f32 rnge
module dist_norm = normal_distribution f32 rnge
module dist_int = uniform_int_distribution i32 rnge
type rng = rnge.rng

module vec2 = mk_vspace_2d f32

type maybe 'a = #just a | #nothing

let mix_colors (cs: [](maybe argb.colour)): argb.colour =
  match reduce_comm (\x y -> match (x, y)
                             case (#nothing, #nothing) -> #nothing
                             case (#just c, #nothing) -> #just c
                             case (#nothing, #just c) -> #just c
                             case (#just c1, #just c2) -> #just (argb.mix 0.5 c1 0.5 c2)) #nothing cs
  case #just c -> c
  case #nothing -> argb.black

type circle = {p: vec2.vector, r: f32}
type cone = {p: vec2.vector, angle: f32, speed: f32, color: argb.colour}

let translate t p = t vec2.+ p
let rotate a {x, y} = f32.({x=x * cos a - y * sin a,
                            y=y * cos a + x * sin a})

let base_adjust (cone: cone): (vec2.vector -> vec2.vector) =
  rotate (-cone.angle) <-< translate ({x=0, y=0} vec2.- cone.p)

let fov = 0.4f32 -- not really fov, but good enough

let random_circle (rng: rng): circle =
  let (rng, x) = dist.rand (-1, 1) rng
  let (rng, y) = dist.rand (-1, 1) rng
  let (_rng, r) = dist_norm.rand {mean=0, stddev=0.05} rng
  in {p={x, y}, r=f32.abs r}

let random_cone (rng: rng): cone =
  let (rng, x) = dist.rand (0.75, 1) rng
  let (rng, xf) = dist_int.rand (0, 1) rng
  let x = if xf == 0 then x else -x
  let (rng, y) = dist.rand (0.75, 1) rng
  let (rng, yf) = dist_int.rand (0, 1) rng
  let y = if yf == 0 then y else -y
  let (rng, angle) = dist.rand (0, 2 * f32.pi) rng
  let (rng, speed) = dist.rand (-1, 1) rng
  let (rng, cr) = dist.rand (0.25, 0.75) rng
  let (rng, cg) = dist.rand (0.25, 0.75) rng
  let (_rng, cb) = dist.rand (0.25, 0.75) rng
  let color = argb.from_rgba cr cg cb 1
  in {p={x, y}, angle, speed, color}

type text_content = (i32, i32, i32)
module lys: lys with text_content = text_content = {
  type state = {time: f32, h: i32, w: i32, rng: rng, paused: bool}

  let grab_mouse = false

  let new_rng (s: state): state =
    -- Hack: Spice up the rng with a poor source.
    let (_, seed) = rnge.rand s.rng
    let rng = rnge.rng_from_seed [t32 (3007 * s.time) ^ i32.u64 seed]
    in s with rng = rng

  let get_n_circles (rng: rng): (rng, i32) =
    dist_int.rand (10, 40) rng

  let get_circles (rng: rng): []circle =
    let (rng, n_circles) = get_n_circles rng
    let rngs = rnge.split_rng n_circles rng
    in map random_circle rngs

  let get_n_cones (rng: rng): (rng, i32) =
    dist_int.rand (3, 9) rng

  let get_cones (rng: rng): []cone =
    let (rng, n_cones) = get_n_cones rng
    let rngs = rnge.split_rng n_cones rng
    in map random_cone rngs

  let init (seed: u32) (h: i32) (w: i32): state =
    {time=0, w, h, rng=rnge.rng_from_seed [i32.u32 seed], paused=false}

  let resize (h: i32) (w: i32) (s: state): state =
    s with h = h with w = w

  let keydown (key: i32) (s: state): state =
    if key == SDLK_r
    then new_rng s
    else if key == SDLK_SPACE
    then s with paused = !s.paused
    else s

  let event (e: event) (s: state): state =
    match e
    case #step td -> s with time = if s.paused then s.time else s.time + td
    case #keydown {key} -> keydown key s
    case _ -> s

  let move_cone (time: f32) (cone: cone): cone =
    let p = rotate ((cone.speed * time * 0.1) % (2 * f32.pi)) cone.p
    in cone with p = p
            with angle = f32.atan2 p.y p.x + f32.pi / 2

  let render (s: state): [][]i32 =
    let size = i32.min s.h s.w

    let rng12 = rnge.split_rng 2 s.rng
    let circles = get_circles rng12[0]
    let cones = get_cones rng12[1]
    let cones = map (move_cone s.time) cones

    let render_pixel (yi: i32) (xi: i32): argb.colour =
      let p = {y=r32 (2 * (yi - (s.h - size) / 2 - size / 2)) / r32 size,
               x=r32 (2 * (xi - (s.w - size) / 2 - size / 2)) / r32 size}

      let in_cone ({x, y}: vec2.vector): bool =
        -y * fov < x && x < y * fov

      let is_visible (cone: cone) ({x, y}: vec2.vector) (circle: circle): bool =
        let circle = circle with p = base_adjust cone circle.p
        in vec2.(norm (circle.p - {x, y})) > circle.r &&
           let a = x / y
           let yp1 = 2 * (a * circle.p.x + circle.p.y)
           let yp2 = 2 * a**2 + 2
           let d = yp1**2 + 4 * (a**2 + 1) * (circle.r**2 - circle.p.y**2 - circle.p.x**2)
           in d < 0 ||
              let d' = f32.sqrt d
              let y1 = (yp1 + d') / yp2
              let y2 = (yp1 - d') / yp2
              in !((0 <= y1 && y1 < y) || (0 <= y2 && y2 < y))

      let ps_adjusted = map (flip base_adjust p) cones
      let cones_visible = map2 (\cone p' -> in_cone p' && reduce_comm (&&) true (map (is_visible cone p') circles))
                               cones ps_adjusted
      in mix_colors (map3 (\c v p' -> if v
                                      then let dist = vec2.norm p'
                                           in #just (argb.scale c.color ((1 / dist)**2))
                                      else #nothing) cones cones_visible ps_adjusted)

    in tabulate_2d s.h s.w render_pixel

  type text_content = text_content

  let text_format () = "Circles: %d\nCones: %d\nFPS: %d"

  let text_content (render_duration: f32) (s: state): text_content =
    ((get_n_circles s.rng).1, (get_n_cones s.rng).1, t32 render_duration)

  let text_colour = const argb.green
}
