(* support.sml -- shared helpers for the sml-qoi tests.

   Provides image construction from an (x,y) -> (r,g,b,a) function, pixel
   access, byte-stream dissection (header / body / end-marker), and the
   `referenceImage` whose pixels mirror tools/genqoi.c exactly, so the suite can
   cross-check our codec against the OFFICIAL QOI reference encoder's output
   committed at test/fixtures/reference.qoi. *)

structure Support =
struct
  structure Q = Qoi
  structure I = Image

  fun i2b i = Word8.fromInt (i mod 256)
  fun b2i b = Word8.toInt b

  (* ---- build an RGBA image from (x,y) -> (r,g,b,a) ---- *)
  fun mkImage (w, h, f) : I.image =
    let
      val data = Word8Array.array (4 * w * h, 0w0)
      fun lp p =
        if p >= w * h then ()
        else
          let
            val x = p mod w and y = p div w
            val (r, g, b, a) = f (x, y)
            val base = p * 4
          in
            Word8Array.update (data, base, i2b r);
            Word8Array.update (data, base + 1, i2b g);
            Word8Array.update (data, base + 2, i2b b);
            Word8Array.update (data, base + 3, i2b a);
            lp (p + 1)
          end
    in
      lp 0;
      { width = w, height = h, data = Word8Array.vector data }
    end

  fun pixel (img : I.image) (x, y) =
    let
      val base = (y * #width img + x) * 4
      val d = #data img
    in
      ( b2i (Word8Vector.sub (d, base))
      , b2i (Word8Vector.sub (d, base + 1))
      , b2i (Word8Vector.sub (d, base + 2))
      , b2i (Word8Vector.sub (d, base + 3)) )
    end

  (* exact image equality (dims + every RGBA byte) *)
  fun sameImage (a : I.image, b : I.image) =
    #width a = #width b andalso #height a = #height b
    andalso #data a = #data b

  (* ---- byte helpers ---- *)
  fun toList (v : Word8Vector.vector) =
    List.tabulate (Word8Vector.length v, fn i => b2i (Word8Vector.sub (v, i)))

  fun fromInts xs = Word8Vector.fromList (List.map i2b xs)

  (* QOI streams are: 14-byte header, body chunks, 8-byte end marker. *)
  fun header (v : Word8Vector.vector) = List.take (toList v, 14)
  fun endMarker (v : Word8Vector.vector) =
    let val n = Word8Vector.length v in List.drop (toList v, n - 8) end
  fun body (v : Word8Vector.vector) =
    let val n = Word8Vector.length v
    in List.take (List.drop (toList v, 14), n - 22) end

  val qoifMagic = [0x71, 0x6F, 0x69, 0x66]          (* "qoif" *)
  val theEndMarker = [0,0,0,0,0,0,0,1]

  fun expectedHeader (w, h) =
    qoifMagic @
    [ (w div 16777216) mod 256, (w div 65536) mod 256, (w div 256) mod 256, w mod 256
    , (h div 16777216) mod 256, (h div 65536) mod 256, (h div 256) mod 256, h mod 256
    , 4, 0 ]

  (* ---- read the committed reference fixture (relative to repo root) ---- *)
  fun readFile path =
    let
      val s = BinIO.openIn path
      val v = BinIO.inputAll s
    in
      BinIO.closeIn s; v
    end

  (* mirrors tools/genqoi.c pixel(): a 32x32 image exercising every QOI op *)
  fun referenceImage () : I.image =
    let
      fun pal k =
        case k of
            0 => (200, 10, 10, 255)
          | 1 => (10, 200, 10, 255)
          | 2 => (10, 10, 200, 255)
          | _ => (200, 200, 10, 255)
      fun f (x, y) =
        if y < 6 then (10, 20, 30, 255)
        else if y < 12 then (x, 64, 128, 255)
        else if y < 18 then (x * 3, x, x * 5, 255)
        else if y < 24 then pal ((x + y) mod 4)
        else (x * 8, y * 8, x * y, x * 8)
    in
      mkImage (32, 32, f)
    end
end
