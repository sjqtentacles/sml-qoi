(* test_reference.sml -- cross-check against the OFFICIAL QOI reference encoder.

   test/fixtures/reference.qoi was produced by the canonical phoboslab/qoi
   `qoi_encode` (see tools/genqoi.c) from a 32x32 image whose pixels are mirrored
   by Support.referenceImage. We assert:

     (1) Qoi.decode of the committed fixture reproduces that image exactly
         (an independent third-party encoder -> our decoder), and
     (2) Qoi.encode of that image reproduces the fixture byte-for-byte
         (our encoder -> identical to the reference encoder).

   Together these pin our codec to the spec, not just to itself. *)

structure ReferenceTests =
struct
  open Support
  open Harness

  val fixturePath = "test/fixtures/reference.qoi"

  fun run () =
    let
      val () = section "QOI reference fixture (official qoi_encode)"
      val refBytes = readFile fixturePath
      val img = referenceImage ()

      val () = check "fixture header is valid qoif/32x32/RGBA"
                 (header refBytes = expectedHeader (32, 32))
      val () = check "fixture ends with the 8-byte end marker"
                 (endMarker refBytes = theEndMarker)

      (* (1) reference encoder -> our decoder reproduces the source pixels *)
      val decoded = Q.decode refBytes
      val () = checkInt "decoded width"  (32, #width decoded)
      val () = checkInt "decoded height" (32, #height decoded)
      val () = check "decode(reference.qoi) = source image" (sameImage (decoded, img))

      (* (2) our encoder -> byte-identical to the reference encoder *)
      val () = check "encode(image) = reference.qoi (byte-exact)"
                 (Q.encode img = refBytes)
      val () = checkInt "encoded length matches fixture"
                 (Word8Vector.length refBytes, Word8Vector.length (Q.encode img))

      val () = section "QOI malformed input"
      val () = checkRaises "empty input" (fn () => Q.decode (Word8Vector.fromList []))
      val () = checkRaises "bad magic"
                 (fn () => Q.decode (fromInts [0,1,2,3, 0,0,0,1, 0,0,0,1, 4,0, 0,0,0,0,0,0,0,1]))
      val () = checkRaises "truncated header"
                 (fn () => Q.decode (fromInts [0x71,0x6F,0x69,0x66, 0,0,0,1]))
    in
      ()
    end
end
