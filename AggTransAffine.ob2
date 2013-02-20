MODULE AggTransAffine;
(*
----------------------------------------------------------------------------

 Affine transformation are linear transformations in Cartesian coordinates
 (strictly speaking not only in Cartesian, but for the beginning we will
 think so). They are rotation, scaling, translation and skewing.
 After any affine transformation a line segment remains a line segment
 and it will never become a curve.

 There will be no math about matrix calculations, since it has been
 described many times. Ask yourself a very simple question:
 "why do we need to understand and use some matrix stuff instead of just
 rotating, scaling and so on". The answers are:

 1. Any combination of transformations can be done by only 4 multiplications
    and 4 additions in floating point.
 2. One matrix transformation is equivalent to the number of consecutive
    discrete transformations, i.e. the matrix "accumulates" all transformations
    in the order of their settings. Suppose we have 4 transformations:
       * rotate by 30 degrees,
       * scale X to 2.0,
       * scale Y to 1.5,
       * move to (100, 100).
    The result will depend on the order of these transformations,
    and the advantage of matrix is that the sequence of discret calls:
    rotate(30), scaleX(2.0), scaleY(1.5), move(100,100)
    will have exactly the same result as the following matrix transformations:

    affine_matrix m;
    m *= rotate_matrix(30);
    m *= scaleX_matrix(2.0);
    m *= scaleY_matrix(1.5);
    m *= move_matrix(100,100);

    m.transform_my_point_at_last(x, y);

 What is the good of it? In real life we will set-up the matrix only once
 and then transform many points, let alone the convenience to set any
 combination of transformations.

 So, how to use it? Very easy - literally as it's shown above. Not quite,
 let us write a correct example:

 agg::trans_affine m;
 m *= agg::trans_affine_rotation(30.0 * 3.1415926 / 180.0);
 m *= agg::trans_affine_scaling(2.0, 1.5);
 m *= agg::trans_affine_translation(100.0, 100.0);
 m.transform(&x, &y);

 The affine matrix is all you need to perform any linear transformation,
 but all transformations have origin point (0,0). It means that we need to
 use 2 translations if we want to rotate someting around (100,100):

 m *= agg::trans_affine_translation(-100.0, -100.0);         // move to (0,0)
 m *= agg::trans_affine_rotation(30.0 * 3.1415926 / 180.0);  // rotate
 m *= agg::trans_affine_translation(100.0, 100.0);           // move back to (100,100)
*)

IMPORT
  bas := AggBasics,
  mat := MathL;

CONST
  affine_epsilon = 1.0E-14; (* About of precision of doubles *)

TYPE
  parallelo_ptr* = POINTER TO parallelogram;
  parallelogram* = bas.double_6;

  trans_affine_ptr* = POINTER TO trans_affine;

  proc_transform* = PROCEDURE (this: trans_affine_ptr; VAR x, y: bas.double);

  trans_affine* = RECORD
    m0-, m1-, m2-, m3-, m4-, m5-: bas.double;
    transform*,
    transform_2x2*,
    inverse_transform* : proc_transform;
  END;
(*
  Rotation matrix. sin() and cos() are calculated twice for the same angle.
  There's no harm because the performance of sin()/cos() is very good on all
  modern processors. Besides, this operation is not going to be invoked too
  often.
*)
  trans_affine_rotation_ptr* = POINTER TO trans_affine_rotation;
  trans_affine_rotation* = RECORD(trans_affine)
  END;

(* Scaling matrix. sx, sy - scale coefficients by X and Y respectively *)
  trans_affine_scaling_ptr* = POINTER TO trans_affine_scaling;
  trans_affine_scaling* = RECORD(trans_affine)
  END;

(* Translation matrix *)
  trans_affine_translation_ptr* = POINTER TO trans_affine_translation;
  trans_affine_translation* = RECORD(trans_affine)
  END;

(* Sckewing (shear) matrix *)
  trans_affine_skewing_ptr* = POINTER TO trans_affine_skewing;
  trans_affine_skewing* = RECORD(trans_affine)
  END;

  trans_affine_line_segment_ptr* = POINTER TO trans_affine_line_segment;
  trans_affine_line_segment* = RECORD(trans_affine)
  END;

PROCEDURE (VAR ta: trans_affine) determinant*(): bas.double;
BEGIN
  IF bas.is_equal_eps(ta.m0 * ta.m3 - ta.m1 * ta.m2, 0, affine_epsilon) THEN
    RETURN 0
  ELSE
    RETURN 1 / (ta.m0 * ta.m3 - ta.m1 * ta.m2);
  END;
END determinant;

(* trans_affine_transform *)
PROCEDURE trans_affine_transform(this: trans_affine_ptr; VAR x, y: bas.double);
VAR
  tx: bas.double;
BEGIN
  tx := x;
  x  := tx * this.m0 + y * this.m2 + this.m4;
  y  := tx * this.m1 + y * this.m3 + this.m5;
END trans_affine_transform;

(* trans_affine_transform_2x2 *)
PROCEDURE trans_affine_transform_2x2(this: trans_affine_ptr; VAR x, y: bas.double);
VAR
  tx: bas.double;
BEGIN
  tx := x;
  x  := tx * this.m0 + y * this.m2;
  y  := tx * this.m1 + y * this.m3;
END trans_affine_transform_2x2;

(* trans_affine_inverse_transform *)
PROCEDURE trans_affine_inverse_transform(this: trans_affine_ptr; VAR x, y: bas.double);
VAR
  d, a, b: bas.double;
BEGIN
  d := this.determinant();
  a := (x - this.m4) * d;
  b := (y - this.m5) * d;

  x := a * this.m3 - b * this.m2;
  y := b * this.m0 - a * this.m1;
END trans_affine_inverse_transform;

(* Construct a matrix with different transform function *)
PROCEDURE (ta: trans_affine_ptr) ConstructTR*(tr: proc_transform);
BEGIN
  ASSERT(ta # NIL);

  ta.m0 := 1;
  ta.m1 := 0;
  ta.m2 := 0;
  ta.m3 := 1;
  ta.m4 := 0;
  ta.m5 := 0;

  ta.transform         := tr;
  ta.transform_2x2     := trans_affine_transform_2x2;
  ta.inverse_transform := trans_affine_inverse_transform;
END ConstructTR;

PROCEDURE (ta: trans_affine_ptr) Construct*();
BEGIN
  ASSERT(ta # NIL);
  ta.m0 := 1;
  ta.m1 := 0;
  ta.m2 := 0;
  ta.m3 := 1;
  ta.m4 := 0;
  ta.m5 := 0;

  ta.transform         := trans_affine_transform;
  ta.transform_2x2     := trans_affine_transform_2x2;
  ta.inverse_transform := trans_affine_inverse_transform;
END Construct;

PROCEDURE (ta: trans_affine_ptr) ConstructM*(v0, v1, v2, v3, v4, v5: bas.double);
BEGIN
  ASSERT(ta # NIL);
  ta.m0 := v0;
  ta.m1 := v1;
  ta.m2 := v2;
  ta.m3 := v3;
  ta.m4 := v4;
  ta.m5 := v5;

  ta.transform         := trans_affine_transform;
  ta.transform_2x2     := trans_affine_transform_2x2;
  ta.inverse_transform := trans_affine_inverse_transform;
END ConstructM;

PROCEDURE (tar: trans_affine_rotation_ptr) ConstructR*(a: bas.double);
BEGIN
  tar.ConstructM(mat.cos(a), mat.sin(a), -mat.sin(a), mat.cos(a), 0, 0);
END ConstructR;

PROCEDURE (tas: trans_affine_scaling_ptr) ConstructS*(sx, sy: bas.double);
BEGIN
  tas.ConstructM(sx, 0, 0, sy, 0, 0);
END ConstructS;

PROCEDURE (tas: trans_affine_scaling_ptr) ConstructS1*(s: bas.double);
BEGIN
  tas.ConstructM(s, 0, 0, s, 0, 0);
END ConstructS1;

PROCEDURE (tat: trans_affine_translation_ptr) ConstructT*(tx, ty: bas.double);
BEGIN
  tat.ConstructM(1, 0, 0, 1, tx, ty);
END ConstructT;

PROCEDURE (tat: trans_affine_skewing_ptr) ConstructS*(sx, sy: bas.double);
BEGIN
  tat.ConstructM(1, mat.tan(sy), mat.tan(sx), 1, 0, 0);
END ConstructS;

PROCEDURE (ta: trans_affine_ptr) assign*(from: trans_affine_ptr);
BEGIN
  ASSERT(ta # NIL);
  ta.m0 := from.m0;
  ta.m1 := from.m1;
  ta.m2 := from.m2;
  ta.m3 := from.m3;
  ta.m4 := from.m4;
  ta.m5 := from.m5;
END assign;

PROCEDURE (ta: trans_affine_ptr) assign_all*(from: trans_affine_ptr);
BEGIN
  ASSERT(ta # NIL);
  ta.m0 := from.m0;
  ta.m1 := from.m1;
  ta.m2 := from.m2;
  ta.m3 := from.m3;
  ta.m4 := from.m4;
  ta.m5 := from.m5;

  ta.transform         := from.transform;
  ta.transform_2x2     := from.transform_2x2;
  ta.inverse_transform := from.inverse_transform;
END assign_all;


(* Reset - actually load an identity matrix *)
PROCEDURE (ta: trans_affine_ptr) reset*();
BEGIN
  ASSERT(ta # NIL);
  ta.m0 := 1;
  ta.m1 := 0;
  ta.m2 := 0;
  ta.m3 := 1;
  ta.m4 := 0;
  ta.m5 := 0;
END reset;

(*
  Invert matrix. Do not try to invert degenerate matrices,
  there's no check for validity. If you set scale to 0 and
  then try to invert matrix, expect unpredictable result.
*)

PROCEDURE (ta: trans_affine_ptr) invert*();
VAR
  d, t0, t4: bas.double;
BEGIN
  ASSERT(ta # NIL);
  d := ta.determinant();

  t0    :=  ta.m3 * d;
  ta.m3 :=  ta.m0 * d;
  ta.m1 := -ta.m1 * d;
  ta.m2 := -ta.m2 * d;

  t4    := -ta.m4 * t0 - ta.m5 * ta.m2;
  ta.m5 := -ta.m4 * ta.m1 - ta.m5 * ta.m3;

  ta.m0 := t0;
  ta.m4 := t4;
END invert;


(* Mirroring around X *)

PROCEDURE (ta: trans_affine_ptr) flip_x*();
BEGIN
  ASSERT(ta # NIL);
  ta.m0 := -ta.m0;
  ta.m1 := -ta.m1;
  ta.m4 := -ta.m4;
END flip_x;

(* Mirroring around Y *)

PROCEDURE (ta: trans_affine_ptr) flip_y*();
BEGIN
  ASSERT(ta # NIL);
  ta.m2 := -ta.m2;
  ta.m3 := -ta.m3;
  ta.m5 := -ta.m5;
END flip_y;


(* Multiply matrix to another one *)
PROCEDURE (ta: trans_affine_ptr) multiply*(m: trans_affine_ptr);
VAR
  t0, t2, t4: bas.double;
BEGIN
  ASSERT(ta # NIL);
  t0 := ta.m0 * m.m0 + ta.m1 * m.m2;
  t2 := ta.m2 * m.m0 + ta.m3 * m.m2;
  t4 := ta.m4 * m.m0 + ta.m5 * m.m2 + m.m4;
  ta.m1 := ta.m0 * m.m1 + ta.m1 * m.m3;
  ta.m3 := ta.m2 * m.m1 + ta.m3 * m.m3;
  ta.m5 := ta.m4 * m.m1 + ta.m5 * m.m3 + m.m5;
  ta.m0 := t0;
  ta.m2 := t2;
  ta.m4 := t4;
END multiply;

(* Multiply "m" to "this" and assign the result to "this" *)
PROCEDURE (ta: trans_affine_ptr) premultiply*(m: trans_affine_ptr);
VAR
  t: trans_affine_ptr;
BEGIN
  ASSERT(ta # NIL);
  NEW(t);
  t.assign_all(m);
  t.multiply(ta);
  ta.assign(t);
END premultiply;

(* Multiply matrix to inverse of another one *)

PROCEDURE (ta: trans_affine_ptr) multiply_inv*(m: trans_affine_ptr);
VAR
  t: trans_affine_ptr;
BEGIN
  ASSERT(ta # NIL);
  NEW(t);
  t.assign_all(m);
  t.invert();
  ta.multiply(t);
END multiply_inv;

PROCEDURE (ta: trans_affine_ptr) premultiply_inv*(m: trans_affine_ptr);
VAR
  t: trans_affine_ptr;
BEGIN
  ASSERT(ta # NIL);
  NEW(t);
  t.assign_all(m);
  t.invert();
  t.multiply(ta);
  ta.assign(t);
END premultiply_inv;

(*
  Get the average scale (by X and Y).
  Basically used to calculate the approximation_scale when
  decomposinting curves into line segments.
*)
PROCEDURE (ta: trans_affine_ptr) scale*(): bas.double;
VAR
  x, y: bas.double;
BEGIN
  ASSERT(ta # NIL);
  x := 0.707106781 * ta.m0 + 0.707106781 * ta.m2;
  y := 0.707106781 * ta.m1 + 0.707106781 * ta.m3;
  RETURN mat.sqrt(x * x + y * y);
END scale;

(* Check to see if it's an identity matrix*)

PROCEDURE (ta: trans_affine_ptr) is_identity*(epsilon: bas.double): BOOLEAN;
BEGIN
  RETURN
    bas.is_equal_eps(ta.m0, 1, epsilon) &
    bas.is_equal_eps(ta.m1, 0, epsilon) &
    bas.is_equal_eps(ta.m2, 0, epsilon) &
    bas.is_equal_eps(ta.m3, 1, epsilon) &
    bas.is_equal_eps(ta.m4, 0, epsilon) &
    bas.is_equal_eps(ta.m5, 0, epsilon) ;
END is_identity;

(* Check to see if two matrices are equal *)

PROCEDURE (ta: trans_affine_ptr) is_equal*(m: trans_affine_ptr; epsilon: bas.double): BOOLEAN;
BEGIN
  RETURN
    bas.is_equal_eps(ta.m0, m.m0, epsilon) &
    bas.is_equal_eps(ta.m1, m.m1, epsilon) &
    bas.is_equal_eps(ta.m2, m.m2, epsilon) &
    bas.is_equal_eps(ta.m3, m.m3, epsilon) &
    bas.is_equal_eps(ta.m4, m.m4, epsilon) &
    bas.is_equal_eps(ta.m5, m.m5, epsilon) ;
END is_equal;

(* Determine the major parameters. Use carefully considering degenerate matrices *)

PROCEDURE (ta: trans_affine_ptr) rotation*(): bas.double;
VAR
  x1, y1, x2, y2: bas.double;
BEGIN
  x1 := 0;
  y1 := 0;
  x2 := 1;
  y2 := 0;

  ta.transform(ta, x1, y1);
  ta.transform(ta, x2, y2);

  RETURN mat.arctan2(y2 - y1, x2 - x1);
END rotation;

PROCEDURE (ta: trans_affine_ptr) translation*(VAR dx, dy: bas.double);
BEGIN
  ta.transform(ta, dx, dy);
END translation;

PROCEDURE (ta: trans_affine_ptr) scaling*(VAR sx, sy: bas.double);
VAR
  t: trans_affine_rotation_ptr;
  x1, y1, x2, y2: bas.double;

BEGIN
  NEW(t);

  x1 := 0;
  y1 := 0;
  x2 := 1;
  y2 := 1;

  t.assign_all(ta);

  t.ConstructR(-ta.rotation());

  t.transform(ta, x1, y1);
  t.transform(ta, x2, y2);

  sx := x2 - x1;
  sy := y2 - y1;
END scaling;

PROCEDURE (ta: trans_affine_ptr) scaling_abs*(VAR sx, sy: bas.double);
BEGIN
 sx := mat.sqrt(ta.m0 * ta.m0 + ta.m2 * ta.m2);
 sy := mat.sqrt(ta.m1 * ta.m1 + ta.m3 * ta.m3);
END scaling_abs;

PROCEDURE (ta: trans_affine_ptr) parl_to_parl*(src, dst: parallelogram);
VAR
  m: trans_affine_ptr;
BEGIN
  ta.m0 := src[2] - src[0];
  ta.m1 := src[3] - src[1];
  ta.m2 := src[4] - src[0];
  ta.m3 := src[5] - src[1];
  ta.m4 := src[0];
  ta.m5 := src[1];

  ta.invert();

  NEW(m);
  m.ConstructM(
   dst[2] - dst[0],
   dst[3] - dst[1],
   dst[4] - dst[0],
   dst[5] - dst[1],
   dst[0],
   dst[1]);

  ta.multiply(m);
END parl_to_parl;

PROCEDURE (ta: trans_affine_ptr) rect_to_parl*(x1, y1, x2, y2: bas.double; parl: parallelogram);
VAR
  src: parallelogram;
BEGIN
  src[0] := x1;
  src[1] := y1;
  src[2] := x2;
  src[3] := y1;
  src[4] := x2;
  src[5] := y2;

  ta.parl_to_parl(src, parl);
END rect_to_parl;

PROCEDURE (ta: trans_affine_ptr) parl_to_rect*(parl: parallelogram; x1, y1, x2, y2: bas.double);
VAR
  dst: parallelogram;
BEGIN
  dst[0] := x1;
  dst[1] := y1;
  dst[2] := x2;
  dst[3] := y1;
  dst[4] := x2;
  dst[5] := y2;

  ta.parl_to_parl(parl, dst);
END parl_to_rect;

PROCEDURE (ta: trans_affine_ptr) store_to*(m: parallelogram);
BEGIN
  m[0] := ta.m0;
  m[1] := ta.m1;
  m[2] := ta.m2;
  m[3] := ta.m3;
  m[4] := ta.m4;
  m[5] := ta.m5;
END store_to;

PROCEDURE (ta: trans_affine_ptr) load_from*(m: parallelogram);
BEGIN
  ta.m0 := m[0];
  ta.m1 := m[1];
  ta.m2 := m[2];
  ta.m3 := m[3];
  ta.m4 := m[4];
  ta.m5 := m[5];
END load_from;


PROCEDURE (tals: trans_affine_line_segment_ptr) ConstructL*(x1, y1, x2, y2, dist: bas.double);
VAR
  dx, dy: bas.double;

  s: trans_affine_scaling_ptr;
  r: trans_affine_rotation_ptr;
  t: trans_affine_translation_ptr;
BEGIN
  dx := x2 - x1;
  dy := y2 - y1;

  IF dist > 0 THEN
    NEW(s);
    s.ConstructS1(mat.sqrt(dx * dx + dy * dy ) / dist);
    tals.multiply(s);
  END;

  NEW(r);
  r.ConstructR(mat.arctan2(dy, dx));
  tals.multiply(r);

  NEW(t);
  t.ConstructT(x1, y1);
  tals.multiply(t);
END ConstructL;

END AggTransAffine.