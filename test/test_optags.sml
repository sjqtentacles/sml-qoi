(* test_optags.sml -- assert the EXACT QOI chunk byte stream for known small
   pixel sequences, one per op tag, plus header/end-marker framing.

   Expected bytes are computed by hand from the QOI specification (the encoder
   starts from prev = {0,0,0,255}, a 64-entry running index of {0,0,0,0}, and
   emits RUN, INDEX, DIFF, LUMA, RGB/RGBA in that priority order). *)

structure OpTagTests =
struct
  open Support
  open Harness

  (* build a 1-row image from an explicit pixel list *)
  fun rowImage pixels =
    let val arr = Vector.fromList pixels
    in mkImage (Vector.length arr, 1, fn (x, _) => Vector.sub (arr, x)) end

  fun run () =
    let
      val () = section "QOI framing (header + end marker)"
      val img = rowImage [(1, 0, 0, 255)]
      val enc = Q.encode img
      val () = checkIntList "magic+dims+channels+colorspace" (expectedHeader (1, 1), header enc)
      val () = checkIntList "8-byte end marker" (theEndMarker, endMarker enc)

      val () = section "QOI op-tag chunk streams"

      (* QOP_OP_RUN: four black-opaque pixels collapse to one run of 4 (bias -1) *)
      val () = checkIntList "QOI_OP_RUN (run of 4)"
                 ([0xC3], body (Q.encode (rowImage (List.tabulate (4, fn _ => (0,0,0,255))))))

      (* QOI_OP_DIFF: (1,0,0) relative to (0,0,0): vr=1,vg=0,vb=0 *)
      val () = checkIntList "QOI_OP_DIFF (+1,0,0)"
                 ([0x7A], body (Q.encode (rowImage [(1,0,0,255)])))

      (* QOI_OP_LUMA: vg=10, vg_r=-2, vg_b=2 -> bytes 0xAA 0x6A *)
      val () = checkIntList "QOI_OP_LUMA (vg=10)"
                 ([0xAA, 0x6A], body (Q.encode (rowImage [(8,10,12,255)])))

      (* QOI_OP_RGB: large opaque diff falls through to a literal RGB chunk *)
      val () = checkIntList "QOI_OP_RGB literal"
                 ([0xFE,100,150,200], body (Q.encode (rowImage [(100,150,200,255)])))

      (* QOI_OP_RGBA: alpha differs from the running 255 -> literal RGBA chunk *)
      val () = checkIntList "QOI_OP_RGBA literal"
                 ([0xFF,0,0,0,128], body (Q.encode (rowImage [(0,0,0,128)])))

      (* QOI_OP_INDEX: pixel A (RGB), pixel B (RGB), then A again -> index hit (9) *)
      val () = checkIntList "QOI_OP_INDEX (hash hit at 9)"
                 ([0xFE,10,20,30, 0xFE,0,0,0, 0x09],
                  body (Q.encode (rowImage [(10,20,30,255),(0,0,0,255),(10,20,30,255)])))

      (* RUN flush then DIFF with signed wraparound: black,black,white.
         white-black = (-1,-1,-1) (mod 256) -> DIFF 0x55, after a run-of-2 flush *)
      val () = checkIntList "RUN flush + DIFF wraparound"
                 ([0xC1, 0x55],
                  body (Q.encode (rowImage [(0,0,0,255),(0,0,0,255),(255,255,255,255)])))
    in
      ()
    end
end
