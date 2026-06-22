(* qoi.sml

   Implementation of the QOI signature: a full encoder/decoder for the "Quite OK
   Image" format over the sml-image RGBA8 representation. Pure Basis integer
   arithmetic; total, deterministic, and byte-identical across MLton and Poly/ML.

   The chunk fields never overlap within a byte, so the bit packing is expressed
   as ordinary addition/multiplication (e.g. `(vr+2)*16` is `(vr+2) << 4`), which
   keeps the code free of any Word casts while producing the exact spec bytes. *)

structure Qoi :> QOI =
struct
  exception Qoi of string

  structure W8  = Word8
  structure W8V = Word8Vector
  structure W8A = Word8Array

  fun b2i (b : W8.word) = W8.toInt b
  fun i2b (i : int) = W8.fromInt i          (* truncates to the low 8 bits *)

  (* op tags *)
  val OP_INDEX = 0x00
  val OP_DIFF  = 0x40
  val OP_LUMA  = 0x80
  val OP_RUN   = 0xc0
  val OP_RGB   = 0xfe
  val OP_RGBA  = 0xff
  val MASK2    = 0xc0

  (* the 64-entry running pixel hash *)
  fun hashIndex (r, g, b, a) = (r * 3 + g * 5 + b * 7 + a * 11) mod 64

  (* signed 8-bit channel difference, wrapped into -128..127 *)
  fun sdiff (x, y) =
    let val d = (x - y) mod 256          (* SML mod w/ positive divisor: 0..255 *)
    in if d >= 128 then d - 256 else d end

  fun wrap8 x = x mod 256                  (* normalise into 0..255 *)

  (* ---------------------------------------------------------------- encode -- *)

  fun encode ({ width = w, height = h, data } : Image.image) : W8V.vector =
    let
      val () = if w < 0 orelse h < 0 then raise Qoi "negative dimension" else ()
      val () = if W8V.length data <> 4 * w * h
               then raise Qoi "data length does not match width*height*4" else ()
      val npx = w * h
      val maxSize = 14 + npx * 5 + 8       (* worst case: every pixel an RGBA chunk *)
      val out = W8A.array (maxSize, 0w0)
      val p = ref 0
      fun put byte = (W8A.update (out, !p, i2b byte); p := !p + 1)
      fun put32 v =
        ( put ((v div 16777216) mod 256); put ((v div 65536) mod 256)
        ; put ((v div 256) mod 256); put (v mod 256) )

      (* 14-byte header: "qoif", width, height, channels=4 (RGBA), colorspace=0 *)
      val () = (put 0x71; put 0x6f; put 0x69; put 0x66)
      val () = put32 w
      val () = put32 h
      val () = (put 4; put 0)

      val index = Array.array (64, (0, 0, 0, 0))
      val prevR = ref 0 and prevG = ref 0 and prevB = ref 0 and prevA = ref 255
      val run = ref 0

      fun getPx i =
        let val base = i * 4
        in ( b2i (W8V.sub (data, base)), b2i (W8V.sub (data, base + 1))
           , b2i (W8V.sub (data, base + 2)), b2i (W8V.sub (data, base + 3)) ) end

      fun emitRun () = (put (OP_RUN + (!run - 1)); run := 0)

      fun encPixel (i, (r, g, b, a)) =
        let
          val isLast = i = npx - 1
          val same = r = !prevR andalso g = !prevG andalso b = !prevB andalso a = !prevA
        in
          if same then
            ( run := !run + 1
            ; if !run = 62 orelse isLast then emitRun () else () )
          else
            ( if !run > 0 then emitRun () else ()
            ; let
                val ip = hashIndex (r, g, b, a)
                val (ir, ig, ib, ia) = Array.sub (index, ip)
              in
                if ir = r andalso ig = g andalso ib = b andalso ia = a then
                  put (OP_INDEX + ip)
                else
                  ( Array.update (index, ip, (r, g, b, a))
                  ; if a = !prevA then
                      let
                        val vr = sdiff (r, !prevR)
                        val vg = sdiff (g, !prevG)
                        val vb = sdiff (b, !prevB)
                        val vgr = vr - vg
                        val vgb = vb - vg
                      in
                        if vr >= ~2 andalso vr <= 1 andalso vg >= ~2 andalso vg <= 1
                           andalso vb >= ~2 andalso vb <= 1
                        then put (OP_DIFF + (vr + 2) * 16 + (vg + 2) * 4 + (vb + 2))
                        else if vgr >= ~8 andalso vgr <= 7 andalso vg >= ~32 andalso vg <= 31
                                andalso vgb >= ~8 andalso vgb <= 7
                        then (put (OP_LUMA + (vg + 32)); put ((vgr + 8) * 16 + (vgb + 8)))
                        else (put OP_RGB; put r; put g; put b)
                      end
                    else (put OP_RGBA; put r; put g; put b; put a) )
              end )
          ; prevR := r; prevG := g; prevB := b; prevA := a
        end

      fun loop i = if i >= npx then () else (encPixel (i, getPx i); loop (i + 1))
      val () = loop 0
      val () = (put 0; put 0; put 0; put 0; put 0; put 0; put 0; put 1)
    in
      Word8ArraySlice.vector (Word8ArraySlice.slice (out, 0, SOME (!p)))
    end

  (* ---------------------------------------------------------------- decode -- *)

  fun decode (bytes : W8V.vector) : Image.image =
    let
      val n = W8V.length bytes
      val () = if n < 14 + 8 then raise Qoi "stream too short" else ()
      fun at i = b2i (W8V.sub (bytes, i))
      val () = if at 0 = 0x71 andalso at 1 = 0x6f andalso at 2 = 0x69 andalso at 3 = 0x66
               then () else raise Qoi "bad magic"
      fun rd32 i = at i * 16777216 + at (i + 1) * 65536 + at (i + 2) * 256 + at (i + 3)
      val w = rd32 4
      val h = rd32 8
      val channels = at 12
      val () = if channels <> 3 andalso channels <> 4 then raise Qoi "bad channel count" else ()
      val () = if w < 0 orelse h < 0 then raise Qoi "bad dimensions" else ()
      val npx = w * h
      val out = W8A.array (4 * npx, 0w0)

      val index = Array.array (64, (0, 0, 0, 0))
      val r = ref 0 and g = ref 0 and b = ref 0 and a = ref 255
      val run = ref 0
      val p = ref 14
      val chunksEnd = n - 8                  (* the 8-byte end marker is not data *)
      fun rb () = let val v = at (!p) in p := !p + 1; v end
      fun storeIndex () = Array.update (index, hashIndex (!r, !g, !b, !a), (!r, !g, !b, !a))
      fun emit i =
        let val base = i * 4 in
          W8A.update (out, base, i2b (!r)); W8A.update (out, base + 1, i2b (!g));
          W8A.update (out, base + 2, i2b (!b)); W8A.update (out, base + 3, i2b (!a))
        end

      fun chunk () =
        let val b1 = rb () in
          if b1 = OP_RGB then (r := rb (); g := rb (); b := rb ())
          else if b1 = OP_RGBA then (r := rb (); g := rb (); b := rb (); a := rb ())
          else
            let val top = (b1 div 64) * 64 in   (* = b1 andb MASK2 *)
              if top = OP_INDEX then
                let val (ir, ig, ib, ia) = Array.sub (index, b1)
                in r := ir; g := ig; b := ib; a := ia end
              else if top = OP_DIFF then
                ( r := wrap8 (!r + ((b1 div 16) mod 4) - 2)
                ; g := wrap8 (!g + ((b1 div 4) mod 4) - 2)
                ; b := wrap8 (!b + (b1 mod 4) - 2) )
              else if top = OP_LUMA then
                let
                  val b2 = rb ()
                  val vg = (b1 mod 64) - 32
                in
                  r := wrap8 (!r + vg - 8 + ((b2 div 16) mod 16));
                  g := wrap8 (!g + vg);
                  b := wrap8 (!b + vg - 8 + (b2 mod 16))
                end
              else (* OP_RUN *) run := b1 mod 64
            end;
          storeIndex ()
        end

      fun px i =
        if i >= npx then ()
        else
          ( if !run > 0 then run := !run - 1
            else if !p < chunksEnd then chunk ()
            else ()                            (* exhausted: carry last pixel *)
          ; emit i
          ; px (i + 1) )
      val () = px 0
    in
      { width = w, height = h, data = W8A.vector out }
    end
end
