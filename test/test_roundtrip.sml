(* test_roundtrip.sml -- encode -> decode reproduces the original image for a
   spread of synthetic rasters: solid (run-heavy), gradients (DIFF/LUMA), a
   checkerboard (INDEX), explicit per-pixel runs, and varying alpha (RGBA). *)

structure RoundtripTests =
struct
  open Support
  open Harness

  fun roundtrips name img =
    check name (sameImage (img, Q.decode (Q.encode img)))

  fun run () =
    let
      val () = section "QOI round-trip (encode then decode)"

      (* solid colour: maximal RUN compression *)
      val () = roundtrips "solid 16x16 opaque"
                 (mkImage (16, 16, fn _ => (37, 142, 213, 255)))
      val () = roundtrips "solid 1x1"
                 (mkImage (1, 1, fn _ => (0, 0, 0, 255)))
      val () = roundtrips "solid 13x7 (odd dims)"
                 (mkImage (13, 7, fn _ => (255, 0, 128, 255)))

      (* smooth gradient: small per-pixel diffs -> DIFF / LUMA chunks *)
      val () = roundtrips "horizontal gradient"
                 (mkImage (64, 8, fn (x, _) => (x mod 256, (x * 2) mod 256, (x * 3) mod 256, 255)))
      val () = roundtrips "diagonal luma gradient"
                 (mkImage (40, 40, fn (x, y) => ((x + y) * 3, x + y, (x + y) * 5, 255)))

      (* checkerboard: two colours alternate -> heavy INDEX reuse *)
      val () = roundtrips "checkerboard 32x32"
                 (mkImage (32, 32, fn (x, y) =>
                    if (x + y) mod 2 = 0 then (20, 20, 20, 255) else (230, 230, 230, 255)))

      (* explicit runs interrupted by literals *)
      val () = roundtrips "runs with breaks"
                 (mkImage (50, 4, fn (x, _) =>
                    if x mod 10 = 0 then (x * 5, 0, 0, 255) else (10, 20, 30, 255)))

      (* varying alpha -> RGBA chunks *)
      val () = roundtrips "alpha ramp"
                 (mkImage (16, 16, fn (x, y) => (x * 16, y * 16, 128, (x + y) * 8)))
      val () = roundtrips "fully transparent"
                 (mkImage (8, 8, fn _ => (50, 60, 70, 0)))

      (* a single row and a single column (edge shapes) *)
      val () = roundtrips "single row"
                 (mkImage (37, 1, fn (x, _) => (x * 7, x * 3, x * 11, 255)))
      val () = roundtrips "single column"
                 (mkImage (1, 29, fn (_, y) => (y * 9, 200, y * 4, 255 - y * 8)))

      (* the full reference image, end-to-end through our own codec *)
      val () = roundtrips "reference image self round-trip" (referenceImage ())
    in
      ()
    end
end
