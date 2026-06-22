(* sml-qoi demo: build a colourful 256x256 image, encode it to QOI, decode the
   bytes straight back, and verify the round trip is lossless -- then write the
   QOI stream and a PNG (rendered via the vendored sml-image) for inspection.

   Everything is integer arithmetic (a triangle-wave "pseudo sine" plasma), so
   the image -- and therefore the QOI bytes -- are byte-identical under MLton and
   Poly/ML.

   Writes:
     assets/demo.qoi   the QOI-encoded image
     assets/demo.png   the decoded image, as a PNG (for inline display) *)

val w = 256
val h = 256

fun isqrt n =
  let fun lp (x, y) = if y < x then lp ((x + n div x) div 2, x) else x
  in if n <= 0 then 0 else lp (n, n + 1) end

fun clamp v = if v < 0 then 0 else if v > 255 then 255 else v

(* Posterize into flat colour bands. A banded sunset whose sky/ground colour
   depends only on `y` makes whole scan-rows identical, which is exactly what
   QOI's RUN / INDEX / DIFF chunks were built for -- so the image looks clean and
   compresses to a fraction of the raw RGBA. *)
fun band v = (clamp v div 24) * 24

val horizon = 172
val sunX = 188 and sunY = 70 and sunR = 30

fun sky y =
  let val t = y * 255 div horizon
  in (band (30 + t * 190 div 255), band (50 + t * 150 div 255), band (150 + t * 80 div 255), 255) end

fun ground y =
  let val t = (y - horizon) * 255 div (h - horizon)
  in (band (45 + t * 55 div 255), band (115 - t * 65 div 255), band (45 + t * 15 div 255), 255) end

fun sun d = (255, band (235 - d * 4), band (130 - d * 5), 255)

fun pixel (x, y) =
  let val dx = x - sunX and dy = y - sunY
      val d = isqrt (dx * dx + dy * dy)
  in
    if d <= sunR andalso y < horizon then sun d
    else if y >= horizon then ground y
    else sky y
  end

val image : Image.image =
  let
    val data = Word8Array.array (4 * w * h, 0w0)
    fun lp p =
      if p >= w * h then ()
      else
        let
          val x = p mod w and y = p div w
          val (r, g, b, a) = pixel (x, y)
          val base = p * 4
        in
          Word8Array.update (data, base, Word8.fromInt r);
          Word8Array.update (data, base + 1, Word8.fromInt g);
          Word8Array.update (data, base + 2, Word8.fromInt b);
          Word8Array.update (data, base + 3, Word8.fromInt a);
          lp (p + 1)
        end
  in
    lp 0;
    { width = w, height = h, data = Word8Array.vector data }
  end

val qoiBytes = Qoi.encode image
val decoded = Qoi.decode qoiBytes
val lossless = #data decoded = #data image
            andalso #width decoded = w andalso #height decoded = h

val () =
  let val os = BinIO.openOut "assets/demo.qoi"
  in BinIO.output (os, qoiBytes); BinIO.closeOut os end

val () =
  let val os = BinIO.openOut "assets/demo.png"
  in BinIO.output (os, Image.encodePng decoded); BinIO.closeOut os end

val rawBytes = 4 * w * h
val qoiLen = Word8Vector.length qoiBytes
val permille = qoiLen * 1000 div rawBytes

val () =
  print
    ( "round-trip lossless: " ^ Bool.toString lossless ^ "\n"
    ^ "wrote assets/demo.qoi (" ^ Int.toString qoiLen ^ " bytes, "
    ^ Int.toString w ^ "x" ^ Int.toString h ^ ")\n"
    ^ "wrote assets/demo.png\n"
    ^ "raw RGBA: " ^ Int.toString rawBytes ^ " bytes -> QOI "
    ^ Int.toString qoiLen ^ " bytes ("
    ^ Int.toString (rawBytes div qoiLen) ^ "x smaller, "
    ^ Int.toString (permille div 10) ^ "." ^ Int.toString (permille mod 10)
    ^ "% of raw)\n" )
