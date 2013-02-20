MODULE AggColorConvRGB8;
IMPORT
  bas := AggBasics,
  acc := AggColorConv,
  bit := AggBit;


PROCEDURE color_conv_rgb24(VAR dst, src: ARRAY OF bas.int8u);
VAR
  p, width: bas.int32;
BEGIN
  width := bas.agg_min32(LEN(dst), LEN(src));
  p := 0;
  REPEAT
    dst[p + 0] := src[p + 2];
    dst[p + 1] := src[p + 1];
    dst[p + 2] := src[p + 0];

    INC(p, 3);
  UNTIL p = width;
END color_conv_rgb24;

PROCEDURE color_conv_rgba32(VAR dst, src: ARRAY OF bas.int8u; I1, I2, I3, I4: bas.int32);
VAR
  p, width: bas.int32;
BEGIN
  width := bas.agg_min32(LEN(dst), LEN(src));
  p := 0;
  REPEAT
    dst[p + 0] := src[p + I1];
    dst[p + 1] := src[p + I2];
    dst[p + 2] := src[p + I3];
    dst[p + 3] := src[p + I4];

    INC(p, 4);
  UNTIL p >= width;
END color_conv_rgba32;

PROCEDURE color_conv_rgb24_rgba32(VAR dst, src: ARRAY OF bas.int8u; I1, I2, I3, A: bas.int32);
VAR
  s, d, width: bas.int32;
BEGIN
  width := bas.agg_min32(LEN(dst), LEN(src));
  s := 0;
  d := 0;
  REPEAT
    dst[d + I1] := src[s + 0];
    dst[d + I2] := src[s + 1];
    dst[d + I3] := src[s + 2];
    dst[d +  A] := 255;

    INC(d, 4);
    INC(s, 3);
  UNTIL d >= width;
END color_conv_rgb24_rgba32;


PROCEDURE color_conv_rgba32_rgb24(VAR dst, src: ARRAY OF bas.int8u; I1, I2, I3: bas.int32);
VAR
  s, d, width: bas.int32;
BEGIN
  width := bas.agg_min32(LEN(dst), LEN(src));
  s := 0;
  d := 0;
  REPEAT
    dst[d + I1] := src[s + 0];
    dst[d + I2] := src[s + 1];
    dst[d + I3] := src[s + 2];

    INC(d, 4);
    INC(s, 3);
  UNTIL d >= width;
END color_conv_rgba32_rgb24;


(*
src

A        B
xxxxxxxx xxxxxxxx
z3333322 22211111

dst

(3) = A >> 2 & 1FH
(2) = ((A & 3) << 3) or (B & 0E0H) >> 5
(1) = B & 1FH

*)

PROCEDURE color_conv_rgb555_rgb24(VAR dst, src: ARRAY OF bas.int8u; R, B: bas.int32);
VAR
  s, d, width: bas.int32;
BEGIN
  width := bas.agg_min32(LEN(dst), LEN(src));
  s := 0;
  d := 0;
  REPEAT
    (* src[s + 0] === A *)
    (* src[s + 1] === B *)

    dst[d + R] := bit.and8u(bit.lsh8u(src[s + 0], -2), 1FH);
    dst[d + 1] := bit.or8u(bit.lsh8u(bit.and8u(src[s + 0], 3), 3), bit.lsh8u(bit.and8u(src[s + 1], 0E0H), -5));
    dst[d + B] := bit.and8u(src[s + 1], 1FH);

    INC(d, 3);
    INC(s, 2);
  UNTIL d >= width;
END color_conv_rgb555_rgb24;

(*
src
R        G        B
xxxrrrrr xxxggggg xxxbbbbb

dst

0        1
xrrrrrgg gggbbbbb

(0) ((R << 2) & 7CH) or ((G >> 3) & 3)
(1) ((G << 5) & E0H) or (B & 1FH)
*)

PROCEDURE color_conv_rgb24_rgb555(VAR dst, src: ARRAY OF bas.int8u; R, B: bas.int32);
VAR
  s, d, width: bas.int32;
BEGIN
  width := bas.agg_min32(LEN(dst), LEN(src));
  s := 0;
  d := 0;
  REPEAT

    dst[d + 0] := bit.or8u(bit.and8u(bit.lsh8u(src[s + R], 2), 7CH), bit.and8u(bit.lsh8u(src[s + 1], 3), 3));
    dst[d + 1] := bit.or8u(bit.and8u(bit.lsh8u(src[s + 1], 4), 0E0H), bit.and8u(src[s + B], 1FH));

    INC(d, 2);
    INC(s, 3);
  UNTIL d >= width;
END color_conv_rgb24_rgb555;


(*
src

A        B
xxxxxxxx xxxxxxxx
33333222 22211111

dst

(3) = A >> 3 & 1FH
(2) = ((A & 7) << 3) or (B & 0E0H) >> 5
(1) = B & 1FH

*)

PROCEDURE color_conv_rgb565_rgb24(VAR dst, src: ARRAY OF bas.int8u; R, B: bas.int32);
VAR
  s, d, width: bas.int32;
BEGIN
  width := bas.agg_min32(LEN(dst), LEN(src));
  s := 0;
  d := 0;
  REPEAT
    (* src[s + 0] === A *)
    (* src[s + 1] === B *)

    dst[d + R] := bit.and8u(bit.lsh8u(src[s + 0], -3), 1FH);
    dst[d + 1] := bit.or8u(bit.lsh8u(bit.and8u(src[s + 0], 3), 7), bit.lsh8u(bit.and8u(src[s + 1], 0E0H), -5));
    dst[d + B] := bit.and8u(src[s + 1], 1FH);

    INC(d, 3);
    INC(s, 2);
  UNTIL d >= width;
END color_conv_rgb565_rgb24;


(*
src
R        G        B
xxxrrrrr xxgggggg xxxbbbbb

dst

0        1
rrrrrggg gggbbbbb

(0) ((R << 3) & F8H) or ((G >> 3) & 7)
(1) ((G << 5) & E0H) or (B & 1FH)
*)

PROCEDURE color_conv_rgb24_rgb565(VAR dst, src: ARRAY OF bas.int8u; R, B: bas.int32);
VAR
  s, d, width: bas.int32;
BEGIN
  width := bas.agg_min32(LEN(dst), LEN(src));
  s := 0;
  d := 0;
  REPEAT

    dst[d + 0] := bit.or8u(bit.and8u(bit.lsh8u(src[s + R], 3), 0F8H), bit.and8u(bit.lsh8u(src[s + 1], 3), 7));
    dst[d + 1] := bit.or8u(bit.and8u(bit.lsh8u(src[s + 1], 4), 0E0H), bit.and8u(src[s + B], 1FH));

    INC(d, 2);
    INC(s, 3);
  UNTIL d >= width;
END color_conv_rgb24_rgb565;




(* color_conv_rgb24_to_bgr24 *)
PROCEDURE color_conv_rgb24_to_bgr24*(VAR dst, src: ARRAY OF bas.int8u);
BEGIN
  color_conv_rgb24(dst, src);
END color_conv_rgb24_to_bgr24;

(* color_conv_bgr24_to_rgb24 *)

PROCEDURE color_conv_bgr24_to_rgb24*(VAR dst, src: ARRAY OF bas.int8u);
BEGIN
  color_conv_rgb24(dst, src);
END color_conv_bgr24_to_rgb24;

(* color_conv_bgr24_to_bgr24 *)

PROCEDURE color_conv_bgr24_to_bgr24*(VAR dst, src: ARRAY OF bas.int8u);
BEGIN
  acc.color_conv_same(dst, src);
END color_conv_bgr24_to_bgr24;

(* color_conv_rgb24_to_rgb24 *)

PROCEDURE color_conv_rgb24_to_rgb24*(VAR dst, src: ARRAY OF bas.int8u);
BEGIN
  acc.color_conv_same(dst, src);
END color_conv_rgb24_to_rgb24;






PROCEDURE color_conv_argb32_to_abgr32*(VAR dst, src: ARRAY OF bas.int8u);
BEGIN
  color_conv_rgba32(dst, src, 0,3,2,1);
END color_conv_argb32_to_abgr32;

PROCEDURE color_conv_argb32_to_bgra32*(VAR dst, src: ARRAY OF bas.int8u);
BEGIN
  color_conv_rgba32(dst, src, 3,2,1,0);
END color_conv_argb32_to_bgra32;

PROCEDURE color_conv_argb32_to_rgba32*(VAR dst, src: ARRAY OF bas.int8u);
BEGIN
  color_conv_rgba32(dst, src, 1,2,3,0);
END color_conv_argb32_to_rgba32;

PROCEDURE color_conv_bgra32_to_abgr32*(VAR dst, src: ARRAY OF bas.int8u);
BEGIN
  color_conv_rgba32(dst, src, 3,0,1,2);
END color_conv_bgra32_to_abgr32;

PROCEDURE color_conv_bgra32_to_argb32*(VAR dst, src: ARRAY OF bas.int8u);
BEGIN
  color_conv_rgba32(dst, src, 3,2,1,0);
END color_conv_bgra32_to_argb32;

PROCEDURE color_conv_bgra32_to_rgba32*(VAR dst, src: ARRAY OF bas.int8u);
BEGIN
  color_conv_rgba32(dst, src, 2,1,0,3);
END color_conv_bgra32_to_rgba32;

PROCEDURE color_conv_rgba32_to_abgr32*(VAR dst, src: ARRAY OF bas.int8u);
BEGIN
  color_conv_rgba32(dst, src, 3,2,1,0);
END color_conv_rgba32_to_abgr32;

PROCEDURE color_conv_rgba32_to_argb32*(VAR dst, src: ARRAY OF bas.int8u);
BEGIN
  color_conv_rgba32(dst, src, 3,0,1,2);
END color_conv_rgba32_to_argb32;

PROCEDURE color_conv_rgba32_to_bgra32*(VAR dst, src: ARRAY OF bas.int8u);
BEGIN
  color_conv_rgba32(dst, src, 2,1,0,3);
END color_conv_rgba32_to_bgra32;

PROCEDURE color_conv_abgr32_to_argb32*(VAR dst, src: ARRAY OF bas.int8u);
BEGIN
  color_conv_rgba32(dst, src, 0,3,2,1);
END color_conv_abgr32_to_argb32;

PROCEDURE color_conv_abgr32_to_bgra32*(VAR dst, src: ARRAY OF bas.int8u);
BEGIN
  color_conv_rgba32(dst, src, 1,2,3,0);
END color_conv_abgr32_to_bgra32;

PROCEDURE color_conv_abgr32_to_rgba32*(VAR dst, src: ARRAY OF bas.int8u);
BEGIN
  color_conv_rgba32(dst, src, 3,2,1,0);
END color_conv_abgr32_to_rgba32;





PROCEDURE color_conv_rgba32_to_rgba32*(VAR dst, src: ARRAY OF bas.int8u);
BEGIN
  acc.color_conv_same(dst, src);
END color_conv_rgba32_to_rgba32;

PROCEDURE color_conv_argb32_to_argb32*(VAR dst, src: ARRAY OF bas.int8u);
BEGIN
  acc.color_conv_same(dst, src);
END color_conv_argb32_to_argb32;

PROCEDURE color_conv_bgra32_to_bgra32*(VAR dst, src: ARRAY OF bas.int8u);
BEGIN
  acc.color_conv_same(dst, src);
END color_conv_bgra32_to_bgra32;

PROCEDURE color_conv_abgr32_to_abgr32*(VAR dst, src: ARRAY OF bas.int8u);
BEGIN
  acc.color_conv_same(dst, src);
END color_conv_abgr32_to_abgr32;




PROCEDURE color_conv_rgb24_to_argb32*(VAR dst, src: ARRAY OF bas.int8u);
BEGIN
  color_conv_rgb24_rgba32(dst, src, 1,2,3,0);
END color_conv_rgb24_to_argb32;

PROCEDURE color_conv_rgb24_to_abgr32*(VAR dst, src: ARRAY OF bas.int8u);
BEGIN
  color_conv_rgb24_rgba32(dst, src, 3,2,1,0);
END color_conv_rgb24_to_abgr32;

PROCEDURE color_conv_rgb24_to_bgra32*(VAR dst, src: ARRAY OF bas.int8u);
BEGIN
  color_conv_rgb24_rgba32(dst, src, 2,1,0,3);
END color_conv_rgb24_to_bgra32;

PROCEDURE color_conv_rgb24_to_rgba32*(VAR dst, src: ARRAY OF bas.int8u);
BEGIN
  color_conv_rgb24_rgba32(dst, src, 0,1,2,3);
END color_conv_rgb24_to_rgba32;

PROCEDURE color_conv_bgr24_to_argb32*(VAR dst, src: ARRAY OF bas.int8u);
BEGIN
  color_conv_rgb24_rgba32(dst, src, 3,2,1,0);
END color_conv_bgr24_to_argb32;

PROCEDURE color_conv_bgr24_to_abgr32*(VAR dst, src: ARRAY OF bas.int8u);
BEGIN
  color_conv_rgb24_rgba32(dst, src, 1,2,3,0);
END color_conv_bgr24_to_abgr32;

PROCEDURE color_conv_bgr24_to_bgra32*(VAR dst, src: ARRAY OF bas.int8u);
BEGIN
  color_conv_rgb24_rgba32(dst, src, 0,1,2,3);
END color_conv_bgr24_to_bgra32;

PROCEDURE color_conv_bgr24_to_rgba32*(VAR dst, src: ARRAY OF bas.int8u);
BEGIN
  color_conv_rgb24_rgba32(dst, src, 2,1,0,3);
END color_conv_bgr24_to_rgba32;



PROCEDURE color_conv_argb32_to_rgb24*(VAR dst, src: ARRAY OF bas.int8u);
BEGIN
  color_conv_rgba32_rgb24(dst, src, 1,2,3);
END color_conv_argb32_to_rgb24;

PROCEDURE color_conv_abgr32_to_rgb24*(VAR dst, src: ARRAY OF bas.int8u);
BEGIN
  color_conv_rgba32_rgb24(dst, src, 3,2,1);
END color_conv_abgr32_to_rgb24;

PROCEDURE color_conv_bgra32_to_rgb24*(VAR dst, src: ARRAY OF bas.int8u);
BEGIN
  color_conv_rgba32_rgb24(dst, src, 2,1,0);
END color_conv_bgra32_to_rgb24;

PROCEDURE color_conv_rgba32_to_rgb24*(VAR dst, src: ARRAY OF bas.int8u);
BEGIN
  color_conv_rgba32_rgb24(dst, src, 0,1,2);
END color_conv_rgba32_to_rgb24;

PROCEDURE color_conv_argb32_to_bgr24*(VAR dst, src: ARRAY OF bas.int8u);
BEGIN
  color_conv_rgba32_rgb24(dst, src, 3,2,1);
END color_conv_argb32_to_bgr24;

PROCEDURE color_conv_abgr32_to_bgr24*(VAR dst, src: ARRAY OF bas.int8u);
BEGIN
  color_conv_rgba32_rgb24(dst, src, 1,2,3);
END color_conv_abgr32_to_bgr24;

PROCEDURE color_conv_bgra32_to_bgr24*(VAR dst, src: ARRAY OF bas.int8u);
BEGIN
  color_conv_rgba32_rgb24(dst, src, 0,1,2);
END color_conv_bgra32_to_bgr24;

PROCEDURE color_conv_rgba32_to_bgr24*(VAR dst, src: ARRAY OF bas.int8u);
BEGIN
  color_conv_rgba32_rgb24(dst, src, 2,1,0);
END color_conv_rgba32_to_bgr24;


PROCEDURE color_conv_rgb555_to_bgr24*(VAR dst, src: ARRAY OF bas.int8u);
BEGIN
  color_conv_rgb555_rgb24(dst, src, 2,0);
END color_conv_rgb555_to_bgr24;

PROCEDURE color_conv_rgb555_to_rgb24*(VAR dst, src: ARRAY OF bas.int8u);
BEGIN
  color_conv_rgb555_rgb24(dst, src, 0,2);
END color_conv_rgb555_to_rgb24;




PROCEDURE color_conv_bgr24_to_rgb555*(VAR dst, src: ARRAY OF bas.int8u);
BEGIN
  color_conv_rgb24_rgb555(dst, src, 2,0);
END color_conv_bgr24_to_rgb555;

PROCEDURE color_conv_rgb24_to_rgb555*(VAR dst, src: ARRAY OF bas.int8u);
BEGIN
  color_conv_rgb24_rgb555(dst, src, 0,2);
END color_conv_rgb24_to_rgb555;





PROCEDURE color_conv_rgb565_to_bgr24*(VAR dst, src: ARRAY OF bas.int8u);
BEGIN
  color_conv_rgb565_rgb24(dst, src, 2,0);
END color_conv_rgb565_to_bgr24;

PROCEDURE color_conv_rgb565_to_rgb24*(VAR dst, src: ARRAY OF bas.int8u);
BEGIN
  color_conv_rgb565_rgb24(dst, src, 0,2);
END color_conv_rgb565_to_rgb24;





PROCEDURE color_conv_bgr24_to_rgb565*(VAR dst, src: ARRAY OF bas.int8u);
BEGIN
  color_conv_rgb24_rgb565(dst, src, 2,0);
END color_conv_bgr24_to_rgb565;

PROCEDURE color_conv_rgb24_to_rgb565*(VAR dst, src: ARRAY OF bas.int8u);
BEGIN
  color_conv_rgb24_rgb565(dst, src, 0,2);
END color_conv_rgb24_to_rgb565;





END AggColorConvRGB8.