(* qoi.sig

   Pure-Standard-ML QOI ("Quite OK Image") codec.

   QOI is a fast, simple, lossless image format (https://qoiformat.org). This
   library encodes and decodes the full specification on top of the `sml-image`
   RGBA8 in-memory representation:

       image = { width, height, data : Word8Vector.vector }   (* 4*w*h bytes *)

   A QOI byte stream is:

     - a 14-byte header: magic "qoif", 4-byte big-endian width, 4-byte big-endian
       height, 1-byte channels (3 = RGB, 4 = RGBA), 1-byte colorspace
       (0 = sRGB w/ linear alpha, 1 = all linear);
     - a sequence of chunks, each one of:
         QOI_OP_RGB   (0xFE)         3 following bytes  (r g b, alpha carried over)
         QOI_OP_RGBA  (0xFF)         4 following bytes  (r g b a)
         QOI_OP_INDEX (0b00xxxxxx)   reference into the 64-entry running array
         QOI_OP_DIFF  (0b01xxxxxx)   2-bit per-channel diff, bias 2 (-2..1)
         QOI_OP_LUMA  (0b10xxxxxx)   green diff (bias 32) + r/b relative (bias 8)
         QOI_OP_RUN   (0b11xxxxxx)   run length of the previous pixel, bias -1 (1..62)
     - an 8-byte end marker: seven 0x00 bytes then one 0x01 byte.

   The encoder follows the canonical reference order of operations (RUN, INDEX,
   DIFF, LUMA, RGB/RGBA), so its output is byte-identical to the reference
   encoder for the same RGBA pixels. Encoding always writes channels = 4 and
   colorspace = 0, since the `sml-image` representation is RGBA8/sRGB.

   Everything is integer arithmetic over the Basis library: total, deterministic,
   and byte-identical across MLton and Poly/ML. Malformed input (bad magic, bad
   header, truncated stream, dimension overflow) raises `Qoi`. *)

signature QOI =
sig
  exception Qoi of string

  (* Encode an RGBA8 image to a QOI byte stream (channels = 4, colorspace = 0). *)
  val encode : Image.image -> Word8Vector.vector

  (* Decode a QOI byte stream to an RGBA8 image. RGB (3-channel) input decodes
     with alpha forced to 255. Raises `Qoi` on malformed input. *)
  val decode : Word8Vector.vector -> Image.image
end
