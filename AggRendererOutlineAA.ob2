MODULE AggRendererOutlineAA;

IMPORT
  bas := AggBasics,
  lab := AggLineAABasics,
  adl := AggDDALine,
  arb := AggRendererBase,
  col := AggColor,
  bit := AggBit,
  avs := AggVertexSource,
  mth := AggMath,
  aeb := AggEllipseBresenham;

CONST
  max_half_width = 64;

  subpixel_shift = lab.line_subpixel_shift;
  subpixel_size  = ASH(1, subpixel_shift);
  subpixel_mask  = subpixel_size - 1;

  aa_shift = 8;
  aa_num   = ASH(1, aa_shift);
  aa_mask  = aa_num - 1;

TYPE
  distance_interpolator_ptr* = POINTER TO distance_interpolator;
  distance_interpolator* = RECORD

  END;

  distance_interpolator0* = RECORD(distance_interpolator)
    adx,
    ady,
    adist: bas.int32;
  END;

  distance_interpolator1* = RECORD(distance_interpolator)
    adx,
    ady,
    adist: bas.int32;
  END;

  distance_interpolator2* = RECORD(distance_interpolator)
    adx,
    ady,

    adx_start,
    ady_start,

    adist,
    adist_start: bas.int;
  END;

  distance_interpolator3* = RECORD(distance_interpolator)
    adx,
    ady,

    adx_start,
    ady_start,

    adx_end,
    ady_end,

    adist, adist_start, adist_end: bas.int32;
  END;

  renderer_outline_ptr* = POINTER TO renderer_outline;
  renderer_outline_aa_ptr* = POINTER TO renderer_outline_aa;

  line_interpolator_aa_base* = RECORD
    lp  : lab.line_parameters_ptr;
    li  : adl.dda2_line_interpolator;
    ren : renderer_outline_aa_ptr;

    len, x, y, old_x, old_y, count-, width-,
    max_extent, step      : bas.int32;

    dist   : ARRAY max_half_width + 1     OF bas.int32;
    covers : col.covers_array_ptr;
    (*ARRAY max_half_width * 2 + 4 OF bas.int8u;*)
  END;

  line_interpolator_aa0_ptr* = POINTER TO line_interpolator_aa0;
  line_interpolator_aa0* = RECORD (line_interpolator_aa_base)
    di: distance_interpolator1;
  END;

  line_interpolator_aa1_ptr* = POINTER TO line_interpolator_aa1;
  line_interpolator_aa1* = RECORD (line_interpolator_aa_base)
    di: distance_interpolator2;
  END;

  line_interpolator_aa2_ptr* = POINTER TO line_interpolator_aa2;
  line_interpolator_aa2* = RECORD (line_interpolator_aa_base)
    di: distance_interpolator2;
  END;

  line_interpolator_aa3_ptr* = POINTER TO line_interpolator_aa3;
  line_interpolator_aa3* = RECORD (line_interpolator_aa_base)
    di: distance_interpolator3;
  END;

  line_profile_aa_ptr* = POINTER TO line_profile_aa;
  line_profile_aa* = RECORD
    size-   : bas.int32;
    profile : POINTER TO ARRAY OF bas.int8u;
    gamma   : ARRAY aa_num OF bas.int8u;

    subpixel_width-: bas.int32;
    min_width*, smoother_width*: bas.double;
  END;

  renderer_outline* = RECORD
    ren     : arb.renderer_base_ptr;
    color*  : col.aggclr;
  END;

  renderer_outline_aa* = RECORD (renderer_outline)
    profile*: line_profile_aa_ptr;
  END;

  cmp_function* = PROCEDURE (d: bas.int32): BOOLEAN;


PROCEDURE (VAR di: distance_interpolator) inc_x_*();
BEGIN
END inc_x_;

PROCEDURE (VAR di: distance_interpolator) dec_x_*();
BEGIN
END dec_x_;

PROCEDURE (VAR di: distance_interpolator) inc_y_*();
BEGIN

END inc_y_;

PROCEDURE (VAR di: distance_interpolator) dec_y_*();
BEGIN

END dec_y_;

PROCEDURE (VAR di: distance_interpolator) inc_x*(dy: bas.int);
BEGIN

END inc_x;

PROCEDURE (VAR di: distance_interpolator) dec_x*(dy: bas.int);
BEGIN

END dec_x;

PROCEDURE (VAR di: distance_interpolator) inc_y*(dx: bas.int);
BEGIN

END inc_y;

PROCEDURE (VAR di: distance_interpolator) dec_y*(dx: bas.int);
BEGIN

END dec_y;

PROCEDURE (VAR di: distance_interpolator) dist*(): bas.int;
BEGIN
  RETURN 0;
END dist;

PROCEDURE (VAR di: distance_interpolator) dist_start*(): bas.int;
BEGIN
  RETURN 0;
END dist_start;

PROCEDURE (VAR di: distance_interpolator) dist_end*(): bas.int;
BEGIN
  RETURN 0;
END dist_end;

PROCEDURE (VAR di: distance_interpolator) dx*(): bas.int;
BEGIN
  RETURN 0;
END dx;

PROCEDURE (VAR di: distance_interpolator) dy*(): bas.int;
BEGIN
  RETURN 0;
END dy;

PROCEDURE (VAR di: distance_interpolator) dx_start*(): bas.int;
BEGIN
  RETURN 0;
END dx_start;

PROCEDURE (VAR di: distance_interpolator) dy_start*(): bas.int;
BEGIN
  RETURN 0;
END dy_start;

PROCEDURE (VAR di: distance_interpolator) dx_end*(): bas.int;
BEGIN
  RETURN 0;
END dx_end;

PROCEDURE (VAR di: distance_interpolator) dy_end*(): bas.int;
BEGIN
  RETURN 0;
END dy_end;

PROCEDURE (VAR di0: distance_interpolator0) Construct*(x1, y1, x2, y2, x, y: bas.int);
BEGIN
 di0.adx := lab.line_mr(x2) - lab.line_mr(x1);
 di0.ady := lab.line_mr(y2) - lab.line_mr(y1);

 di0.adist :=
  (lab.line_mr(x + lab.line_subpixel_size DIV 2) - lab.line_mr(x2)) * di0.ady -
  (lab.line_mr(y + lab.line_subpixel_size DIV 2) - lab.line_mr(y2)) * di0.adx;

 di0.adx := ASH(di0.adx, lab.line_mr_subpixel_shift);
 di0.ady := ASH(di0.ady, lab.line_mr_subpixel_shift);
END Construct;

PROCEDURE (VAR di0: distance_interpolator0) inc_x_*;
BEGIN
  INC(di0.adist, di0.ady);
END inc_x_;

PROCEDURE (VAR di0: distance_interpolator0) dec_x_*;
BEGIN
  DEC(di0.adist, di0.ady);
END dec_x_;

PROCEDURE (VAR di0: distance_interpolator0) inc_y_*;
BEGIN
  INC(di0.adist, di0.adx);
END inc_y_;

PROCEDURE (VAR di0: distance_interpolator0) dec_y_*;
BEGIN
  INC(di0.adist, di0.adx);
END dec_y_;

PROCEDURE (VAR di0: distance_interpolator0) inc_x*(dy: bas.int);
BEGIN
  INC(di0.adist, di0.ady);

  IF dy > 0 THEN
    DEC(di0.adist, di0.adx);
  END;

  IF dy < 0 THEN
    INC(di0.adist, di0.adx);
  END;
END inc_x;

PROCEDURE (VAR di0: distance_interpolator0) dec_x*(dy: bas.int);
BEGIN
  DEC(di0.adist, di0.ady);

  IF dy > 0 THEN
    DEC(di0.adist, di0.adx);
  END;

  IF dy < 0 THEN
    INC(di0.adist, di0.adx);
  END;
END dec_x;

PROCEDURE (VAR di0: distance_interpolator0) inc_y*(dx: bas.int);
BEGIN
  DEC(di0.adist, di0.adx);

  IF dx > 0 THEN
    INC(di0.adist, di0.ady);
  END;

  IF dx < 0 THEN
    DEC(di0.adist, di0.ady);
  END;
END inc_y;

PROCEDURE (VAR di0: distance_interpolator0) dec_y*(dx: bas.int);
BEGIN
  INC(di0.adist, di0.adx);

  IF dx > 0 THEN
    INC(di0.adist, di0.ady);
  END;

  IF dx < 0 THEN
    DEC(di0.adist, di0.ady);
  END;
END dec_y;

PROCEDURE (VAR di0: distance_interpolator0) dist*(): bas.int;
BEGIN
  RETURN di0.adist;
END dist;

PROCEDURE (VAR di0: distance_interpolator0) dx*(): bas.int;
BEGIN
  RETURN di0.adx;
END dx;

PROCEDURE (VAR di0: distance_interpolator0) dy*(): bas.int;
BEGIN
  RETURN di0.ady;
END dy;

PROCEDURE (VAR di1: distance_interpolator1) Construct*(x1, y1, x2, y2, x, y: bas.int32);
BEGIN
  di1.adx := x2 - x1;
  di1.ady := y2 - y1;

  di1.adist:=
   ENTIER(
    (x + lab.line_subpixel_size / 2 - x2) * di1.ady -
    (y + lab.line_subpixel_size / 2 - y2) * di1.adx );

  di1.adx := ASH(di1.adx, lab.line_subpixel_shift);
  di1.ady := ASH(di1.ady, lab.line_subpixel_shift);
END Construct;

PROCEDURE (VAR di1: distance_interpolator1) inc_x_*();
BEGIN
  INC(di1.adist, di1.ady);
END inc_x_;

PROCEDURE (VAR di1: distance_interpolator1) dec_x_*();
BEGIN
  DEC(di1.adist, di1.ady);
END dec_x_;

PROCEDURE (VAR di1: distance_interpolator1) inc_y_*();
BEGIN
  INC(di1.adist, di1.adx);
END inc_y_;

PROCEDURE (VAR di1: distance_interpolator1) dec_y_*();
BEGIN
  DEC(di1.adist, di1.adx);
END dec_y_;

PROCEDURE (VAR di1: distance_interpolator1) inc_x*(dy: bas.int32);
BEGIN
  INC(di1.adist, di1.ady);

  IF dy > 0 THEN
    DEC(di1.adist, di1.adx);
  END;

  IF dy < 0 THEN
    INC(di1.adist, di1.adx);
  END;
END inc_x;

PROCEDURE (VAR di1: distance_interpolator1) dec_x*(dy: bas.int32);
BEGIN
  DEC(di1.adist, di1.ady);

  IF dy > 0 THEN
    DEC(di1.adist, di1.adx );
  END;

  IF dy < 0 THEN
    INC(di1.adist, di1.adx);
  END;
END dec_x;

PROCEDURE (VAR di1: distance_interpolator1) inc_y*(dx: bas.int32);
BEGIN
  DEC(di1.adist, di1.adx);

  IF dx > 0 THEN
    INC(di1.adist, di1.ady);
  END;

  IF dx < 0 THEN
    DEC(di1.adist, di1.ady);
  END;
END inc_y;

PROCEDURE (VAR di1: distance_interpolator1) dec_y*(dx: bas.int32);
BEGIN
  INC(di1.adist, di1.adx);

  IF dx > 0 THEN
    INC(di1.adist, di1.ady);
  END;

  IF dx < 0 THEN
    DEC(di1.adist, di1.ady);
  END;
END dec_y;

PROCEDURE (VAR di1: distance_interpolator1) dist*(): bas.int32;
BEGIN
  RETURN di1.adist;
END dist;

PROCEDURE (VAR di1: distance_interpolator1) dx*(): bas.int32;
BEGIN
  RETURN di1.adx;
END dx;

PROCEDURE (VAR di1: distance_interpolator1) dy*(): bas.int32;
BEGIN
  RETURN di1.ady;
END dy;

PROCEDURE (VAR di2: distance_interpolator2) Construct*(x1, y1, x2, y2, sx, sy, x, y: bas.int);
BEGIN
  di2.adx := x2 - x1;
  di2.ady := y2 - y1;

  di2.adx_start := lab.line_mr(sx) - lab.line_mr(x1);
  di2.ady_start := lab.line_mr(sy) - lab.line_mr(y1);

  di2.adist :=
   ENTIER(
    (x + lab.line_subpixel_size / 2 - x2) * di2.ady -
    (y + lab.line_subpixel_size / 2 - y2) * di2.adx);

  di2.adist_start :=
   (lab.line_mr(x + lab.line_subpixel_size DIV 2) - lab.line_mr(sx)) * di2.ady_start -
   (lab.line_mr(y + lab.line_subpixel_size DIV 2) - lab.line_mr(sy)) * di2.adx_start;

  di2.adx := ASH(di2.adx, lab.line_subpixel_shift);
  di2.ady := ASH(di2.ady, lab.line_subpixel_shift);

  di2.adx_start := ASH(di2.adx_start, lab.line_mr_subpixel_shift);
  di2.ady_start := ASH(di2.ady_start, lab.line_mr_subpixel_shift);
END Construct;
<*+WOFF301*>
PROCEDURE (VAR di2: distance_interpolator2) Construct1*(x1, y1, x2, y2, ex, ey, x, y, z: bas.int);
BEGIN
  di2.adx := x2 - x1;
  di2.ady := y2 - y1;

  di2.adx_start := lab.line_mr(ex) - lab.line_mr(x2);
  di2.ady_start := lab.line_mr(ey) - lab.line_mr(y2);

  di2.adist :=
   ENTIER(
    (x + lab.line_subpixel_size / 2 - x2) * di2.ady -
    (y + lab.line_subpixel_size / 2 - y2) * di2.adx);

  di2.adist_start:=
   (lab.line_mr(x + lab.line_subpixel_size DIV 2) - lab.line_mr(ex)) * di2.ady_start -
   (lab.line_mr(y + lab.line_subpixel_size DIV 2) - lab.line_mr(ey)) * di2.adx_start;

  di2.adx := ASH(di2.adx, lab.line_subpixel_shift);
  di2.ady := ASH(di2.ady, lab.line_subpixel_shift);

  di2.adx_start := ASH(di2.adx_start, lab.line_mr_subpixel_shift);
  di2.ady_start := ASH(di2.ady_start, lab.line_mr_subpixel_shift);
END Construct1;
<*-WOFF301*>

PROCEDURE (VAR di2: distance_interpolator2) inc_x_*();
BEGIN
  INC(di2.adist, di2.ady);
  INC(di2.adist_start, di2.ady_start);
END inc_x_;

PROCEDURE (VAR di2: distance_interpolator2) dec_x_*();
BEGIN
  DEC(di2.adist, di2.ady);
  DEC(di2.adist_start, di2.ady_start);
END dec_x_;

PROCEDURE (VAR di2: distance_interpolator2) inc_y_*();
BEGIN
  DEC(di2.adist, di2.adx);
  DEC(di2.adist_start, di2.adx_start);
END inc_y_;

PROCEDURE (VAR di2: distance_interpolator2) dec_y_*();
BEGIN
  INC(di2.adist, di2.adx);
  INC(di2.adist_start, di2.adx_start);
END dec_y_;

PROCEDURE (VAR di2: distance_interpolator2) inc_x*(dy: bas.int32);
BEGIN
 INC(di2.adist, di2.ady);
 INC(di2.adist_start, di2.ady_start);

  IF dy > 0 THEN
    DEC(di2.adist, di2.adx);
    DEC(di2.adist_start, di2.adx_start);
  END;

  IF dy < 0 THEN
    INC(di2.adist, di2.adx);
    INC(di2.adist_start, di2.adx_start);
  END;
END inc_x;

PROCEDURE (VAR di2: distance_interpolator2) dec_x*(dy: bas.int32);
BEGIN
  DEC(di2.adist, di2.ady);
  DEC(di2.adist_start, di2.ady_start);

  IF dy > 0 THEN
    DEC(di2.adist, di2.adx);
    DEC(di2.adist_start, di2.adx_start);
  END;

  IF dy < 0 THEN
    INC(di2.adist, di2.adx);
    INC(di2.adist_start, di2.adx_start);
  END;
END dec_x;

PROCEDURE (VAR di2: distance_interpolator2) inc_y*(dx: bas.int32);
BEGIN
  DEC(di2.adist, di2.adx);
  DEC(di2.adist_start, di2.adx_start);

  IF dx > 0 THEN
    INC(di2.adist, di2.ady);
    INC(di2.adist_start, di2.ady_start);
  END;

  IF dx < 0 THEN
    DEC(di2.adist, di2.ady);
    DEC(di2.adist_start, di2.ady_start);
  END;
END inc_y;

PROCEDURE (VAR di2: distance_interpolator2) dec_y*(dx: bas.int32);
BEGIN
  INC(di2.adist, di2.adx);
  INC(di2.adist_start, di2.adx_start);

  IF dx > 0 THEN
    INC(di2.adist, di2.ady);
    INC(di2.adist_start, di2.ady_start);
  END;

  IF dx < 0 THEN
    DEC(di2.adist, di2.ady);
    DEC(di2.adist_start, di2.ady_start);
  END;
END dec_y;

PROCEDURE (VAR di2: distance_interpolator2) dist*(): bas.int32;
BEGIN
  RETURN di2.adist;
END dist;

PROCEDURE (VAR di2: distance_interpolator2) dist_start*(): bas.int32;
BEGIN
  RETURN di2.adist_start;
END dist_start;

PROCEDURE (VAR di2: distance_interpolator2) dist_end*(): bas.int32;
BEGIN
  RETURN di2.adist_start;
END dist_end;

PROCEDURE (VAR di2: distance_interpolator2) dx*(): bas.int32;
BEGIN
  RETURN di2.adx;
END dx;

PROCEDURE (VAR di2: distance_interpolator2) dy*(): bas.int32;
BEGIN
  RETURN di2.ady;
END dy;

PROCEDURE (VAR di2: distance_interpolator2) dx_start*(): bas.int32;
BEGIN
  RETURN di2.adx_start;
END dx_start;

PROCEDURE (VAR di2: distance_interpolator2) dy_start*(): bas.int32;
BEGIN
  RETURN di2.ady_start;
END dy_start;

PROCEDURE (VAR di2: distance_interpolator2) dx_end*(): bas.int32;
BEGIN
  RETURN di2.adx_start;
END dx_end;

PROCEDURE (VAR di2: distance_interpolator2) dy_end*(): bas.int32;
BEGIN
  RETURN di2.ady_start;
END dy_end;


PROCEDURE (VAR di3: distance_interpolator3) Construct*(x1, y1, x2, y2, sx, sy, ex, ey, x, y: bas.int32);
BEGIN
  di3.adx := x2 - x1;
  di3.ady := y2 - y1;

  di3.adx_start := lab.line_mr(sx) - lab.line_mr(x1);
  di3.ady_start := lab.line_mr(sy) - lab.line_mr(y1);

  di3.adx_end := lab.line_mr(ex) - lab.line_mr(x2);
  di3.ady_end := lab.line_mr(ey) - lab.line_mr(y2);

  di3.adist :=
   ENTIER(
    (x + lab.line_subpixel_size / 2 - x2) * di3.ady -
    (y + lab.line_subpixel_size / 2 - y2) * di3.adx);

  di3.adist_start :=
   (lab.line_mr(x + lab.line_subpixel_size DIV 2) - lab.line_mr(sx)) * di3.ady_start -
   (lab.line_mr(y + lab.line_subpixel_size DIV 2) - lab.line_mr(sy)) * di3.adx_start;

  di3.adist_end :=
   (lab.line_mr(x + lab.line_subpixel_size DIV 2) - lab.line_mr(ex)) * di3.ady_end -
   (lab.line_mr(y + lab.line_subpixel_size DIV 2) - lab.line_mr(ey)) * di3.adx_end;

  di3.adx := ASH(di3.adx, lab.line_subpixel_shift);
  di3.ady := ASH(di3.ady, lab.line_subpixel_shift);

  di3.adx_start := ASH(di3.adx_start, lab.line_mr_subpixel_shift);
  di3.ady_start := ASH(di3.ady_start, lab.line_mr_subpixel_shift);

  di3.adx_end := ASH(di3.adx_end, lab.line_mr_subpixel_shift);
  di3.ady_end := ASH(di3.ady_end, lab.line_mr_subpixel_shift);
END Construct;

PROCEDURE (VAR di3: distance_interpolator3) inc_x_*;
BEGIN
  INC(di3.adist, di3.ady);
  INC(di3.adist_start, di3.ady_start);
  INC(di3.adist_end, di3.ady_end);
END inc_x_;

PROCEDURE (VAR di3: distance_interpolator3) dec_x_*;
BEGIN
  DEC(di3.adist, di3.ady);
  DEC(di3.adist_start, di3.ady_start);
  DEC(di3.adist_end, di3.ady_end);
END dec_x_;

PROCEDURE (VAR di3: distance_interpolator3) inc_y_*;
BEGIN
  DEC(di3.adist, di3.adx);
  DEC(di3.adist_start, di3.adx_start);
  DEC(di3.adist_end, di3.adx_end);
END inc_y_;

PROCEDURE (VAR di3: distance_interpolator3) dec_y_*;
BEGIN
  INC(di3.adist, di3.adx);
  INC(di3.adist_start, di3.adx_start);
  INC(di3.adist_end, di3.adx_end);
END dec_y_;

PROCEDURE (VAR di3: distance_interpolator3) inc_x*(dy: bas.int32);
BEGIN
  INC(di3.adist, di3.ady);
  INC(di3.adist_start, di3.ady_start);
  INC(di3.adist_end, di3.ady_end);

  IF dy > 0 THEN
    DEC(di3.adist, di3.adx);
    DEC(di3.adist_start, di3.adx_start);
    DEC(di3.adist_end, di3.adx_end);
  END;

  IF dy < 0 THEN
    INC(di3.adist, di3.adx);
    INC(di3.adist_start, di3.adx_start);
    INC(di3.adist_end, di3.adx_end);
  END;
END inc_x;

PROCEDURE (VAR di3: distance_interpolator3) dec_x*(dy: bas.int32);
BEGIN
  DEC(di3.adist, di3.ady);
  DEC(di3.adist_start, di3.ady_start);
  DEC(di3.adist_end, di3.ady_end);

  IF dy > 0 THEN
    DEC(di3.adist, di3.adx);
    DEC(di3.adist_start, di3.adx_start);
    DEC(di3.adist_end, di3.adx_end);
  END;

  IF dy < 0 THEN
    INC(di3.adist, di3.adx);
    INC(di3.adist_start, di3.adx_start);
    INC(di3.adist_end, di3.adx_end);
  END;
END dec_x;

PROCEDURE (VAR di3: distance_interpolator3) inc_y*(dx: bas.int32);
BEGIN
  DEC(di3.adist, di3.adx);
  DEC(di3.adist_start, di3.adx_start);
  DEC(di3.adist_end, di3.adx_end);

  IF dx > 0 THEN
    INC(di3.adist, di3.ady);
    INC(di3.adist_start, di3.ady_start);
    INC(di3.adist_end, di3.ady_end);
  END;

  IF dx < 0 THEN
    DEC(di3.adist, di3.ady);
    DEC(di3.adist_start, di3.ady_start);
    DEC(di3.adist_end, di3.ady_end);
  END;
END inc_y;

PROCEDURE (VAR di3: distance_interpolator3) dec_y*(dx: bas.int32);
BEGIN
  INC(di3.adist, di3.adx);
  INC(di3.adist_start, di3.adx_start);
  INC(di3.adist_end, di3.adx_end);

  IF dx > 0 THEN
    INC(di3.adist, di3.ady);
    INC(di3.adist_start, di3.ady_start);
    INC(di3.adist_end, di3.ady_end);
  END;

  IF dx < 0 THEN
    DEC(di3.adist, di3.ady);
    DEC(di3.adist_start, di3.ady_start);
    DEC(di3.adist_end, di3.ady_end);
  END;
END dec_y;

PROCEDURE (VAR di3: distance_interpolator3) dist*(): bas.int32;
BEGIN
  RETURN di3.adist;
END dist;

PROCEDURE (VAR di3: distance_interpolator3) dist_start*(): bas.int32;
BEGIN
  RETURN di3.adist_start;
END dist_start;

PROCEDURE (VAR di3: distance_interpolator3) dist_end*(): bas.int32;
BEGIN
  RETURN di3.adist_end;
END dist_end;

PROCEDURE (VAR di3: distance_interpolator3) dx*(): bas.int32;
BEGIN
  RETURN di3.adx;
END dx;

PROCEDURE (VAR di3: distance_interpolator3) dy*(): bas.int32;
BEGIN
  RETURN di3.ady;
END dy;

PROCEDURE (VAR di3: distance_interpolator3) dx_start*(): bas.int32;
BEGIN
  RETURN di3.adx_start;
END dx_start;

PROCEDURE (VAR di3: distance_interpolator3) dy_start*(): bas.int32;
BEGIN
  RETURN di3.ady_start;
END dy_start;

PROCEDURE (VAR di3: distance_interpolator3) dx_end*(): bas.int32;
BEGIN
  RETURN di3.adx_end;
END dx_end;

PROCEDURE (VAR di3: distance_interpolator3) dy_end*(): bas.int32;
BEGIN
  RETURN di3.ady_end;
END dy_end;

(* ================================================================================================ *)
(* ================================================================================================ *)
(* ================================================================================================ *)
(* Вынесено, поскольку имеется явный перекос в порядке объявляений. *)
PROCEDURE (ren: renderer_outline_ptr) subpixel_width*(): bas.int32;
BEGIN
  RETURN 0;
END subpixel_width;

PROCEDURE (renaa: renderer_outline_aa_ptr) subpixel_width*(): bas.int32;
BEGIN
  RETURN renaa.profile.subpixel_width;
END subpixel_width;

PROCEDURE (VAR lpa: line_profile_aa) value*(dist: bas.int): bas.int8u;
BEGIN
  RETURN lpa.profile^[dist + subpixel_size * 2];
END value;

PROCEDURE (renaa: renderer_outline_aa_ptr) cover*(d: bas.int32): bas.int8u;
BEGIN
  RETURN renaa.profile.value(d);
END cover;

PROCEDURE (renaa: renderer_outline_aa_ptr) blend_solid_hspan*(x, y, len: bas.int32; VAR covers: col.covers_array_ptr; VAR covers_offset: bas.int32);
BEGIN
  renaa.ren.blend_solid_hspan(x, y, len, renaa.color, covers, covers_offset);
END blend_solid_hspan;

PROCEDURE (renaa: renderer_outline_aa_ptr) blend_solid_vspan*(x, y, len: bas.int32; VAR covers: col.covers_array_ptr; VAR covers_offset: bas.int32);
BEGIN
  renaa.ren.blend_solid_vspan(x, y, len, renaa.color, covers, covers_offset);
END blend_solid_vspan;
(* ================================================================================================ *)
(* ================================================================================================ *)
(* ================================================================================================ *)

PROCEDURE (VAR lia: line_interpolator_aa_base) Construct*(ren: renderer_outline_aa_ptr; lp: lab.line_parameters_ptr);
VAR
  li: adl.dda2_line_interpolator;
  i: bas.int32;

  stop: bas.int32;

BEGIN
  NEW(lia.covers, max_half_width * 2 + 4);

  lia.lp := lp;

  IF lp.vertical THEN
    lia.li.Construct2(lab.line_dbl_hr(lp.x2 - lp.x1), ABS(lp.y2 - lp.y1))
  ELSE
    lia.li.Construct2(lab.line_dbl_hr(lp.y2 - lp.y1), ABS(lp.x2 - lp.x1) + 1);
  END;

  lia.ren := ren;

  IF lp.vertical = (lp.inc > 0 ) THEN
    lia.len := -lp.len
  ELSE
    lia.len :=  lp.len;
  END;

  lia.x := bas.shr_int32(lp.x1, lab.line_subpixel_shift);
  lia.y := bas.shr_int32(lp.y1, lab.line_subpixel_shift);

  lia.old_x := lia.x;
  lia.old_y := lia.y;

  IF lp.vertical THEN
    lia.count := ABS(bas.shr_int32(lp.y2, lab.line_subpixel_shift) - lia.y)
  ELSE
    lia.count:=ABS(bas.shr_int32(lp.x2, lab.line_subpixel_shift) - lia.x );
  END;

  lia.width      := ren.subpixel_width();
  lia.max_extent := bas.shr_int32(lia.width, lab.line_subpixel_shift - 2);
  lia.step       := 0;

  IF lp.vertical THEN
    li.Construct(0, ASH(lp.dy, lab.line_subpixel_shift), lp.len)
  ELSE
    li.Construct(0, ASH(lp.dx, lab.line_subpixel_shift), lp.len);
  END;

  stop := lia.width + lab.line_subpixel_size * 2;

  i:=0;

  LOOP
    IF i >= max_half_width THEN EXIT; END;

    lia.dist[i] := li.y;

    IF lia.dist[i] >= stop THEN
      EXIT;
    END;

    li.plus_operator();

    INC(i);
  END;

  lia.dist[i] := 7FFF0000H;

END Construct;

PROCEDURE (VAR lia: line_interpolator_aa_base) step_hor*(): BOOLEAN;
BEGIN
  RETURN FALSE;
END step_hor;

PROCEDURE (VAR lia: line_interpolator_aa_base) step_ver*(): BOOLEAN;
BEGIN
  RETURN FALSE;
END step_ver;

PROCEDURE (VAR lia: line_interpolator_aa_base) step_hor_base*(VAR di: distance_interpolator): bas.int32;
BEGIN
  lia.li.plus_operator();

  INC(lia.x, lia.lp.inc);

  lia.y := bas.shr_int32(lia.lp.y1 + lia.li.y, lab.line_subpixel_shift);

  IF lia.lp.inc > 0 THEN
    di.inc_x(lia.y - lia.old_y)
  ELSE
    di.dec_x(lia.y - lia.old_y);
  END;

  lia.old_y := lia.y;

  (* DIV при делении на отрицательное число дает ошибку :( *)
  RETURN di.dist() DIV lia.len;
END step_hor_base;

PROCEDURE (VAR lia: line_interpolator_aa_base) step_ver_base*(VAR di: distance_interpolator): bas.int32;
BEGIN
  lia.li.plus_operator();

  INC(lia.y, lia.lp.inc);

  lia.x := bas.shr_int32(lia.lp.x1 + lia.li.y, lab.line_subpixel_shift);

  IF lia.lp.inc > 0 THEN
    di.inc_y(lia.x - lia.old_x)
  ELSE
    di.dec_y(lia.x - lia.old_x);
  END;

  lia.old_x := lia.x;

  RETURN di.dist() DIV lia.len;
END step_ver_base;

PROCEDURE (VAR lia: line_interpolator_aa_base) vertical*(): BOOLEAN;
BEGIN
  RETURN lia.lp.vertical;
END vertical;

PROCEDURE (VAR li0: line_interpolator_aa0) Construct*(ren: renderer_outline_aa_ptr; lp: lab.line_parameters_ptr);
BEGIN
  li0.Construct^(ren, lp);

  li0.di.Construct(
    lp.x1, lp.y1, lp.x2, lp.y2,
    bit.and32(lp.x1, bit.not32(lab.line_subpixel_mask)),
    bit.and32(lp.y1, bit.not32(lab.line_subpixel_mask)));

  li0.li.adjust_forward();

END Construct;

PROCEDURE (VAR li0: line_interpolator_aa0) step_hor*(): BOOLEAN;
VAR
  dist, dy, s1: bas.int32;
  p0, p1: bas.int32;
BEGIN
  s1 := li0.step_hor_base(li0.di);
  p0 := max_half_width + 2;
  p1 := p0;

  li0.covers[p1] := li0.ren.cover(s1);

  INC(p1);

  dy   := 1;
  dist := li0.dist[1] - s1;

  WHILE dist <= li0.width DO
    li0.covers[p1] := li0.ren.cover(dist);

    INC(p1);
    INC(dy);

    dist := li0.dist[dy] - s1;

  END;

  dy   := 1;
  dist := li0.dist[1] + s1;

  WHILE dist <= li0.width DO
    DEC(p0);
    li0.covers[p0] := li0.ren.cover(dist);
    INC(dy);

    dist := li0.dist[dy] + s1;
  END;
(* x, y, len, covers, covers_offset*)
  li0.ren.blend_solid_vspan(li0.x, li0.y - dy + 1, p1 - p0, li0.covers, p0);

  INC(li0.step);

  RETURN li0.step < li0.count;
END step_hor;

PROCEDURE (VAR li0: line_interpolator_aa0) step_ver*(): BOOLEAN;
VAR
 dist, dx, s1 : bas.int32;

 p0, p1: bas.int32;

BEGIN
  s1 := li0.step_ver_base(li0.di);
  p0 := max_half_width + 2;
  p1 := p0;

  li0.covers[p1] := li0.ren.cover(s1);

  INC(p1);

  dx   := 1;
  dist := li0.dist[1] - s1;

  WHILE dist <= li0.width DO
    li0.covers[p1] := li0.ren.cover(dist);

    INC(p1);
    INC(dx);

    dist:=li0.dist[dx] - s1;
  END;

  dx   := 1;
  dist := li0.dist[1] + s1;

  WHILE dist <= li0.width DO
    DEC(p0);

    li0.covers[p0] := li0.ren.cover(dist);

    INC(dx);

    dist := li0.dist[dx] + s1;
  END;

  li0.ren.blend_solid_hspan(li0.x - dx + 1, li0.y, p1 - p0, li0.covers, p0);

  INC(li0.step);

  RETURN li0.step < li0.count;
END step_ver;


PROCEDURE (VAR li1: line_interpolator_aa1) Construct1*(ren: renderer_outline_aa_ptr; lp: lab.line_parameters_ptr; sx, sy: bas.int32);
VAR
 dist1_start, dist2_start, npix, dx, dy: bas.int32;

BEGIN
  li1.Construct(ren, lp);

  li1.di.Construct(
   lp.x1, lp.y1, lp.x2, lp.y2, sx, sy,
   bit.and32(lp.x1, bit.not32(lab.line_subpixel_mask)),
   bit.and32(lp.y1, bit.not32(lab.line_subpixel_mask)));

  npix := 1;

  IF lp.vertical THEN
    LOOP
      li1.li.minus_operator();

      DEC(li1.y, lp.inc);

      li1.x := bas.shr_int32(li1.lp.x1 + li1.li.y, lab.line_subpixel_shift);

      IF lp.inc > 0 THEN
       li1.di.dec_y(li1.x - li1.old_x)
      ELSE
       li1.di.inc_y(li1.x - li1.old_x);
      END;

      li1.old_x := li1.x;

      dist1_start := li1.di.dist_start();
      dist2_start := dist1_start;

      dx := 0;

      IF dist1_start < 0 THEN
        INC(npix);
      END;

      REPEAT
        INC(dist1_start, li1.di.dy_start());
        DEC(dist2_start, li1.di.dy_start());

        IF dist1_start < 0 THEN
          INC(npix);
        END;

        IF dist2_start < 0 THEN
          INC(npix);
        END;

        INC(dx);
      UNTIL li1.dist[dx] > li1.width;

      DEC(li1.step);

      IF npix = 0 THEN EXIT; END;

      npix:=0;

      IF li1.step < -li1.max_extent THEN EXIT END;
    END;
  ELSE
    LOOP
      li1.li.minus_operator();

      DEC(li1.x, lp.inc);

      li1.y := bas.shr_int32(li1.lp.y1 + li1.li.y, lab.line_subpixel_shift);

      IF lp.inc > 0 THEN
       li1.di.dec_x(li1.y - li1.old_y)
      ELSE
       li1.di.inc_x(li1.y - li1.old_y);
      END;

      li1.old_y := li1.y;

      dist1_start := li1.di.dist_start();
      dist2_start := dist1_start;

      dy := 0;

      IF dist1_start < 0 THEN
        INC(npix);
      END;

      REPEAT
        DEC(dist1_start, li1.di.dx_start());
        INC(dist2_start, li1.di.dx_start());

        IF dist1_start < 0 THEN
          INC(npix);
        END;

        IF dist2_start < 0 THEN
          INC(npix);
        END;

        INC(dy);
      UNTIL li1.dist[dy] > li1.width;

      DEC(li1.step);

      IF npix = 0 THEN
        EXIT;
      END;

      npix:=0;

      IF li1.step < -li1.max_extent THEN EXIT; END;
    END;
  END;

 li1.li.adjust_forward();
END Construct1;

PROCEDURE (VAR li1: line_interpolator_aa1) step_hor*(): BOOLEAN;
VAR
  dist_start, dist, dy, s1: bas.int32;
  p0, p1: bas.int32;
BEGIN
  s1 := li1.step_hor_base(li1.di);

  dist_start := li1.di.dist_start();

  p0 := max_half_width + 2;
  p1 := p0;

  li1.covers[p1] := 0;

  IF dist_start <= 0 THEN
    li1.covers[p1] := li1.ren.cover(s1);
  END;

  INC(p1);

  dy   := 1;
  dist := li1.dist[1] - s1;

  WHILE dist <= li1.width DO
    DEC(dist_start, li1.di.dx_start());

    li1.covers[p1] := 0;

    IF dist_start <= 0 THEN
      li1.covers[p1] := li1.ren.cover(dist);
    END;

    INC(p1);
    INC(dy);

    dist := li1.dist[dy] - s1;

  END;

  dy         := 1;
  dist_start := li1.di.dist_start();
  dist       := li1.dist[1] + s1;

  WHILE dist <= li1.width DO
    INC(dist_start, li1.di.dx_start());
    DEC(p0);

    li1.covers[p0] := 0;

    IF dist_start <= 0 THEN
      li1.covers[p0] := li1.ren.cover(dist);
    END;

    INC(dy);

    dist := li1.dist[dy] + s1;
  END;

  li1.ren.blend_solid_vspan(li1.x, li1.y - dy + 1, p1 - p0, li1.covers, p0);

  INC(li1.step);

  RETURN li1.step < li1.count;

END step_hor;

PROCEDURE (VAR li1: line_interpolator_aa1) step_ver*(): BOOLEAN;
VAR
  dist_start, dist, dx, s1: bas.int32;
  p0, p1: bas.int32;
BEGIN
  s1 := li1.step_ver_base(li1.di);
  p0 := max_half_width + 2;
  p1 := p0;

  dist_start := li1.di.dist_start();

  li1.covers[p1] := 0;

  IF dist_start <= 0 THEN
    li1.covers[p1] := li1.ren.cover(s1);
  END;

  INC(p1);

  dx   := 1;
  dist := li1.dist[1] - s1;

  WHILE dist <= li1.width DO
    INC(dist_start, li1.di.dy_start());

    li1.covers[p1] := 0;

    IF dist_start <= 0 THEN
      li1.covers[p1] := li1.ren.cover(dist);
    END;

    INC(p1);
    INC(dx);

    dist := li1.dist[dx] - s1;
  END;

  dx         := 1;
  dist_start := li1.di.dist_start();
  dist       := li1.dist[1] + s1;

  WHILE dist <= li1.width DO
    DEC(dist_start, li1.di.dy_start());
    DEC(p0);

    li1.covers[p0] := 0;

    IF dist_start <= 0 THEN
      li1.covers[p0] := li1.ren.cover(dist);
    END;

    INC(dx);

    dist := li1.dist[dx] + s1;
  END;

  li1.ren.blend_solid_hspan(li1.x - dx + 1, li1.y, p1 - p0, li1.covers, p0);

  INC(li1.step);
  RETURN li1.step < li1.count;
END step_ver;

PROCEDURE (VAR li2: line_interpolator_aa2) Construct1*(ren: renderer_outline_aa_ptr; lp: lab.line_parameters_ptr; ex, ey: bas.int32);
BEGIN
  li2.Construct(ren, lp);

  li2.di.Construct1(
   lp.x1, lp.y1, lp.x2, lp.y2, ex, ey,
   bit.and32(lp.x1, bit.not32(lab.line_subpixel_mask)),
   bit.and32(lp.y1, bit.not32(lab.line_subpixel_mask)),
   0);

  li2.li.adjust_forward();

  DEC(li2.step, li2.max_extent);
END Construct1;

PROCEDURE (VAR li2: line_interpolator_aa2) step_hor*(): BOOLEAN;
VAR
  dist_end, dist, dy, s1, npix: bas.int32;
  p0, p1: bas.int32;

BEGIN
  s1 := li2.step_hor_base(li2.di);
  p0 := max_half_width + 2;
  p1 := p0;

  dist_end := li2.di.dist_end();

  npix := 0;
  li2.covers[p1] := 0;

  IF dist_end > 0 THEN
    li2.covers[p1] := li2.ren.cover(s1);
    npix := 1;
  END;

  INC(p1);

  dy   := 1;
  dist := li2.dist[1] - s1;

  WHILE dist <= li2.width DO
    DEC(dist_end, li2.di.dx_end());

    li2.covers[p1] := 0;

    IF dist_end > 0 THEN
      li2.covers[p1] := li2.ren.cover(dist);
      INC(npix);
    END;

    INC(p1);
    INC(dy);

    dist := li2.dist[dy] - s1;
  END;

  dy       := 1;
  dist_end := li2.di.dist_end();
  dist     := li2.dist[1] + s1;

  WHILE dist <= li2.width DO
    INC(dist_end, li2.di.dx_end());
    DEC(p0);

    li2.covers[p0] := 0;

    IF dist_end > 0 THEN
      li2.covers[p0] := li2.ren.cover(dist);
      INC(npix);
    END;

    INC(dy);
    dist := li2.dist[dy ] + s1;
  END;

  li2.ren.blend_solid_vspan(li2.x, li2.y - dy + 1, p1 - p0, li2.covers, p0);

  INC(li2.step);

  RETURN (npix # 0) & (li2.step < li2.count);
END step_hor;

PROCEDURE (VAR li2: line_interpolator_aa2) step_ver*(): BOOLEAN;
VAR
  dist_end, dist, dx, s1, npix: bas.int32;
  p0, p1: bas.int32;
BEGIN
  s1 := li2.step_ver_base(li2.di);
  p0 := max_half_width + 2;
  p1 := p0;

  dist_end := li2.di.dist_end();

  npix:=0;
  li2.covers[p1] := 0;

  IF dist_end > 0 THEN
    li2.covers[p1] := li2.ren.cover(s1);
    npix := 1;
  END;

  INC(p1);

  dx   := 1;
  dist := li2.dist[1] - s1;

  WHILE dist <= li2.width DO
    INC(dist_end, li2.di.dy_end());

    li2.covers[p1] := 0;

    IF dist_end > 0 THEN
      li2.covers[p1] := li2.ren.cover(dist);
      INC(npix);
    END;

    INC(p1);
    INC(dx);

    dist := li2.dist[dx] - s1;
  END;

  dx       := 1;
  dist_end := li2.di.dist_end();
  dist     := li2.dist[1] + s1;

  WHILE dist <= li2.width DO
    DEC(dist_end, li2.di.dy_end());
    DEC(p0);

    li2.covers[p0] := 0;

    IF dist_end > 0 THEN
      li2.covers[p0] := li2.ren.cover(dist);
      INC(npix);
    END;

    INC(dx);
    dist:=li2.dist[dx] + s1;
  END;

  li2.ren.blend_solid_hspan(li2.x - dx + 1, li2.y, p1 - p0, li2.covers, p0);

  INC(li2.step);

  RETURN (npix <> 0) & (li2.step < li2.count);
END step_ver;

PROCEDURE (VAR li3: line_interpolator_aa3) Construct1*(ren: renderer_outline_aa_ptr; lp: lab.line_parameters_ptr; sx, sy, ex, ey: bas.int32);
VAR
 dist1_start, dist2_start, npix, dx, dy: bas.int32;
BEGIN
  li3.Construct(ren, lp);

  li3.di.Construct(
    lp.x1, lp.y1, lp.x2, lp.y2, sx, sy, ex, ey,
    bit.and32(lp.x1, bit.not32(lab.line_subpixel_mask)),
    bit.and32(lp.y1, bit.not32(lab.line_subpixel_mask)));

  npix := 1;

  IF lp.vertical THEN
    LOOP
      li3.li.minus_operator();

      DEC(li3.y, lp.inc);

      li3.x := bas.shr_int32(li3.lp.x1 + li3.li.y, lab.line_subpixel_shift);

      IF lp.inc > 0 THEN
        li3.di.dec_y(li3.x - li3.old_x)
      ELSE
        li3.di.inc_y(li3.x - li3.old_x);
      END;

      li3.old_x := li3.x;

      dist1_start := li3.di.dist_start();
      dist2_start := dist1_start;

      dx := 0;

      IF dist1_start < 0 THEN
        INC(npix);
      END;

      REPEAT
        INC(dist1_start, li3.di.dy_start());
        DEC(dist2_start, li3.di.dy_start());

        IF dist1_start < 0 THEN
         INC(npix);
        END;

        IF dist2_start < 0 THEN
          INC(npix);
        END;

        INC(dx);
      UNTIL li3.dist[dx] > li3.width;

      IF npix = 0 THEN EXIT; END;

      npix := 0;
      DEC(li3.step);

      IF li3.step < -li3.max_extent THEN EXIT; END;
    END;
  ELSE
    LOOP
      li3.li.minus_operator();

      DEC(li3.x, lp.inc);

      li3.y := bas.shr_int32(li3.lp.y1 + li3.li.y, lab.line_subpixel_shift);

      IF lp.inc > 0 THEN
        li3.di.dec_x(li3.y - li3.old_y)
      ELSE
        li3.di.inc_x(li3.y - li3.old_y);
      END;

      li3.old_y := li3.y;

      dist1_start := li3.di.dist_start();
      dist2_start := dist1_start;

      dy := 0;

      IF dist1_start < 0 THEN
        INC(npix);
      END;

      REPEAT
       DEC(dist1_start, li3.di.dx_start());
       INC(dist2_start, li3.di.dx_start());

       IF dist1_start < 0 THEN
         INC(npix);
       END;

       IF dist2_start < 0 THEN
         INC(npix);
       END;

       INC(dy);
      UNTIL li3.dist[dy] > li3.width;

      IF npix = 0 THEN EXIT; END;

      npix := 0;
      DEC(li3.step);

      IF li3.step < -li3.max_extent THEN EXIT; END;
    END;
  END;

  li3.li.adjust_forward();
  DEC(li3.step, li3.max_extent);
END Construct1;

PROCEDURE (VAR li3: line_interpolator_aa3) step_hor*(): BOOLEAN;
VAR
  dist_start, dist_end, dist, dy, s1, npix: bas.int32;
  p0, p1: bas.int32;
BEGIN
  s1 := li3.step_hor_base(li3.di);
  p0 := max_half_width + 2;
  p1 := p0;

  dist_start := li3.di.dist_start();
  dist_end   := li3.di.dist_end();

  npix := 0;
  li3.covers[p1] := 0;

  IF dist_end > 0 THEN

    IF dist_start <= 0 THEN
      li3.covers[p1] := li3.ren.cover(s1);
    END;

    npix := 1;
  END;

  INC(p1);

  dy   := 1;
  dist := li3.dist[1] - s1;

  WHILE dist <= li3.width DO
    DEC(dist_start, li3.di.dx_start());
    DEC(dist_end, li3.di.dx_end());

    li3.covers[p1] := 0;

    IF (dist_end > 0) & (dist_start <= 0) THEN
      li3.covers[p1] := li3.ren.cover(dist);
      INC(npix);
    END;

    INC(p1);
    INC(dy);

    dist := li3.dist[dy] - s1;
  END;

  dy         := 1;
  dist_start := li3.di.dist_start();
  dist_end   := li3.di.dist_end();
  dist       := li3.dist[1] + s1;

  WHILE dist <= li3.width DO
    INC(dist_start, li3.di.dx_start());
    INC(dist_end, li3.di.dx_end());
    DEC(p0);

    li3.covers[p0] := 0;

    IF (dist_end > 0) & (dist_start <= 0) THEN
      li3.covers[p0] := li3.ren.cover(dist);
      INC(npix);
    END;

    INC(dy);
    dist := li3.dist[dy] + s1;
  END;

  li3.ren.blend_solid_vspan(li3.x, li3.y - dy + 1, p1 - p0, li3.covers, p0);
  INC(li3.step);

  RETURN (npix # 0) & (li3.step < li3.count);
END step_hor;

PROCEDURE (VAR li3: line_interpolator_aa3) step_ver*(): BOOLEAN;
VAR
  dist_start, dist_end, dist, dx, s1, npix: bas.int32;
  p0, p1: bas.int32;
BEGIN
  s1 := li3.step_ver_base(li3.di);
  p0 := max_half_width + 2;
  p1 := p0;

  dist_start := li3.di.dist_start();
  dist_end   := li3.di.dist_end();

  npix := 0;
  li3.covers[p1] := 0;

  IF dist_end > 0 THEN
    IF dist_start <= 0 THEN
      li3.covers[p1] := li3.ren.cover(s1);
    END;
    npix := 1;
  END;

  INC(p1);

  dx   := 1;
  dist := li3.dist[1] - s1;

  WHILE dist <= li3.width DO
    INC(dist_start, li3.di.dy_start());
    INC(dist_end, li3.di.dy_end());

    li3.covers[p1] := 0;

    IF (dist_end > 0) & (dist_start <= 0) THEN
      li3.covers[p1] := li3.ren.cover(dist);
      INC(npix);
    END;

    INC(p1);
    INC(dx);

    dist := li3.dist[dx] - s1;
  END;

  dx         := 1;
  dist_start := li3.di.dist_start();
  dist_end   := li3.di.dist_end();
  dist       := li3.dist[1] + s1;

  WHILE dist <= li3.width DO
    DEC(dist_start, li3.di.dy_start());
    DEC(dist_end, li3.di.dy_end());
    DEC(p0);

    li3.covers[p0] := 0;

    IF (dist_end > 0) & (dist_start <= 0) THEN
      li3.covers[p0] := li3.ren.cover(dist);
      INC(npix);
    END;

    INC(dx);
    dist:=li3.dist[dx ] + s1;
  END;

  li3.ren.blend_solid_hspan(li3.x - dx + 1, li3.y, p1 - p0, li3.covers,  p0);

  INC(li3.step);

  RETURN (npix # 0) & (li3.step < li3.count);
END step_ver;

PROCEDURE (lpa: line_profile_aa_ptr) apply_gamma*(gamma_function: avs.vertex_source_ptr);
VAR
  i: bas.int32;
BEGIN
  FOR i := 0 TO aa_num - 1 DO
    lpa.gamma[i] := bas.agg_getbyte(ENTIER(gamma_function.func_operator_gamma(i / aa_mask) * aa_mask));
  END;
END apply_gamma;

PROCEDURE (lpa: line_profile_aa_ptr) resize_profile*(w: bas.double);
VAR
  size: bas.int32;
BEGIN
  lpa.subpixel_width := ENTIER(w * subpixel_size);
  size := lpa.subpixel_width + subpixel_size * 6;

  IF size > lpa.size THEN
    NEW(lpa.profile, size);
    lpa.size := size;
  END;
END resize_profile;

PROCEDURE (lpa: line_profile_aa_ptr) set*(center_width, smoother_width: bas.double);
VAR
  base_val, width, k: bas.double;
  subpixel_center_width, subpixel_smoother_width, i, n_smoother: bas.int32;
  val: bas.int8u;
  ch, ch_center, ch_smoother: bas.int32;
BEGIN
  base_val := 1.0;

  IF center_width = 0.0 THEN
    center_width:=1.0 / subpixel_size;
  END;

  IF smoother_width = 0.0 THEN
    smoother_width:=1.0 / subpixel_size;
  END;

  width := center_width + smoother_width;

  IF width < lpa.min_width THEN
    k := width / lpa.min_width;

    (*base_val       := base_val * k; === 1.0 * k *)
    base_val       := k;
    center_width   := center_width / k;
    smoother_width := smoother_width / k;
  END;

  lpa.resize_profile(center_width + smoother_width);

  subpixel_center_width   := ENTIER(center_width * subpixel_size);
  subpixel_smoother_width := ENTIER(smoother_width * subpixel_size);

  ch_center   := subpixel_size * 2;
  ch_smoother := subpixel_size * 2 + subpixel_center_width;

  val := lpa.gamma[ENTIER(base_val * aa_mask)];

  ch := subpixel_size * 2;

  i:=0;

  WHILE i < subpixel_center_width DO
    lpa.profile^[ch] := val;
    INC(ch);
    INC(i);
  END;

  i:=0;

  WHILE i < subpixel_smoother_width DO
    lpa.profile^[ch_smoother] := lpa.gamma[ENTIER((base_val - base_val * (i / subpixel_smoother_width)) * aa_mask)];
    INC(ch_smoother);
    INC(i);
  END;

  n_smoother:= lpa.size - subpixel_smoother_width - subpixel_center_width - subpixel_size * 2;

  val := lpa.gamma[0];

  FOR i := 0 TO n_smoother - 1 DO
    lpa.profile^[ch_smoother] := val;
    INC(ch_smoother);
  END;

  ch := subpixel_size * 2;

  FOR i := 0 TO subpixel_size * 2 - 1 DO
    lpa.profile^[ch] := lpa.profile^[ch_center];
    DEC(ch);
    INC(ch_center);
  END;
END set;

PROCEDURE (lpa: line_profile_aa_ptr) set_width*(w: bas.double);
VAR
  s: bas.double;
BEGIN
  IF w < 0.0 THEN
    w := 0.0;
  END;

  IF w < lpa.smoother_width THEN
    w := w + w
  ELSE
    w := w + lpa.smoother_width;
  END;

  w := w * 0.5;
  w := w - lpa.smoother_width;
  s := lpa.smoother_width;

  IF w < 0.0 THEN
    s := s + w;
    w := 0.0;
  END;

  lpa.set(w, s);
END set_width;


PROCEDURE (lpa: line_profile_aa_ptr) Init*;
VAR
  i: bas.int32;
BEGIN
  lpa.size    := 0;
  lpa.profile := NIL;

  lpa.subpixel_width := 0;
  lpa.min_width      := 1.0;
  lpa.smoother_width := 1.0;

  FOR i := 0 TO aa_num - 1 DO
    lpa.gamma[i] := bas.agg_getbyte(i);
  END;
END Init;

PROCEDURE (lpa: line_profile_aa_ptr) Construct*(w: bas.double; gamma_function: avs.vertex_source_ptr);
BEGIN
  lpa.size    := 0;
  lpa.profile := NIL;

  lpa.subpixel_width := 0;
  lpa.min_width      := 1.0;
  lpa.smoother_width := 1.0;

  lpa.apply_gamma(gamma_function);
  lpa.set_width(w);
END Construct;

PROCEDURE (lpa: line_profile_aa_ptr) Destruct*;
BEGIN
  lpa.size    := 0;
  lpa.profile := NIL;
END Destruct;

PROCEDURE (ro: renderer_outline_ptr) accurate_join_only*(): BOOLEAN;
BEGIN
  (* abstract *)
  RETURN FALSE;
END accurate_join_only;

PROCEDURE (ro: renderer_outline_ptr) semidot*(cmp: cmp_function; xc1, yc1, xc2, yc2: bas.int32);
BEGIN
  (* abstract *)
END semidot;

PROCEDURE (ro: renderer_outline_ptr) line0*(lp: lab.line_parameters_ptr);
BEGIN
  (* abstract *)
END line0;

PROCEDURE (ro: renderer_outline_ptr) line1*(lp: lab.line_parameters_ptr; sx, sy: bas.int32);
BEGIN
  (* abstract *)
END line1;

PROCEDURE (ro: renderer_outline_ptr) line2*(lp: lab.line_parameters_ptr; ex, ey: bas.int32);
BEGIN
  (* abstract *)
END line2;

PROCEDURE (ro: renderer_outline_ptr) line3*(lp: lab.line_parameters_ptr; sx, sy, ex, ey: bas.int32);
BEGIN
  (* abstract *)
END line3;

PROCEDURE (roaa: renderer_outline_aa_ptr) semidot_hline*(cmp: cmp_function; xc1, yc1, xc2, yc2, x1, y1, x2: bas.int32);
VAR
  covers: col.covers_array_ptr; (*array[0..max_half_width * 2 + 4 - 1 ] of int8u;*)
  p0, p1: bas.int32;
  x, y, w, x0, dx, dy, d: bas.int32;
  di: distance_interpolator0;
BEGIN
  NEW(covers, max_half_width * 2 + 4);

  p0 := 0;
  p1 := 0;

  x := ASH(x1, lab.line_subpixel_shift);
  y := ASH(y1, lab.line_subpixel_shift);
  w := roaa.subpixel_width();

  di.Construct(xc1, yc1, xc2, yc2, x, y);

  INC(x, lab.line_subpixel_size DIV 2);
  INC(y, lab.line_subpixel_size DIV 2);

  x0 := x1;
  dx := x - xc1;
  dy := y - yc1;

  REPEAT
    d := mth.fast_sqrt(dx * dx + dy * dy);

    covers[p1] := 0;

    IF cmp(di.dist()) & (d <= w) THEN
      covers[p1] := roaa.cover(d);
    END;

    INC(p1);
    INC(dx, lab.line_subpixel_size);

    di.inc_x_();
    INC(x1);
  UNTIL x1 > x2;

  roaa.ren.blend_solid_hspan(x0, y1, p1 - p0, roaa.color, covers, p0);
END semidot_hline;

PROCEDURE (roaa: renderer_outline_aa_ptr) semidot*(cmp: cmp_function; xc1, yc1, xc2, yc2: bas.int32);
VAR
  r, dx, dy, dy0, dx0, x, y: bas.int32;
  ei: aeb.ellipse_bresenham_interpolator;
BEGIN
  r := bas.shr_int32(roaa.subpixel_width() + lab.line_subpixel_mask, lab.line_subpixel_shift);

  IF r < 1 THEN
    r := 1;
  END;

  ei.Construct(r, r);

  dx  :=  0;
  dy  := -r;
  dy0 := dy;
  dx0 :=  0;(*dy*)
  x   := bas.shr_int32(xc1, lab.line_subpixel_shift);
  y   := bas.shr_int32(yc1, lab.line_subpixel_shift);

  REPEAT
   INC(dx, ei.dx);
   INC(dy, ei.dy);

   IF dy <> dy0 THEN
     roaa.semidot_hline(cmp, xc1, yc1, xc2, yc2, x - dx0, y + dy0, x + dx0);
     roaa.semidot_hline(cmp, xc1, yc1, xc2, yc2, x - dx0, y - dy0, x + dx0);
   END;

   dx0 := dx;
   dy0 := dy;

   ei.inc_operator();

  UNTIL dy >= 0;

  roaa.semidot_hline(cmp, xc1, yc1, xc2, yc2, x - dx0, y + dy0, x + dx0);

END semidot;

PROCEDURE (roaa: renderer_outline_aa_ptr) line0*(lp: lab.line_parameters_ptr);
VAR
  li: line_interpolator_aa0;
BEGIN
  li.Construct(roaa, lp);

  IF li.count # 0 THEN
    IF li.vertical() THEN
      WHILE li.step_ver() DO END;
    ELSE
      WHILE li.step_hor() DO END;
    END;
  END;

END line0;

PROCEDURE (roaa: renderer_outline_aa_ptr) line1*(lp: lab.line_parameters_ptr; sx, sy: bas.int32);
VAR
  li: line_interpolator_aa1;
BEGIN
  lab.fix_degenerate_bisectrix_start(lp, sx, sy);
  li.Construct1(roaa, lp, sx, sy);

  IF li.vertical() THEN
    WHILE li.step_ver() DO END;
  ELSE
    WHILE li.step_hor() DO END;
  END;
END line1;

PROCEDURE (roaa: renderer_outline_aa_ptr) line2*(lp: lab.line_parameters_ptr; ex, ey: bas.int32);
VAR
  li: line_interpolator_aa2;
BEGIN
  lab.fix_degenerate_bisectrix_end(lp, ex, ey);

  li.Construct1(roaa, lp, ex, ey);

  IF li.vertical() THEN
    WHILE li.step_ver() DO END;
  ELSE
    WHILE li.step_hor() DO END;
  END;
END line2;

PROCEDURE (roaa: renderer_outline_aa_ptr) line3*(lp: lab.line_parameters_ptr; sx, sy, ex, ey: bas.int32);
VAR
  li : line_interpolator_aa3;
BEGIN
  lab.fix_degenerate_bisectrix_start(lp, sx, sy);
  lab.fix_degenerate_bisectrix_end  (lp, ex, ey);

  li.Construct1(roaa, lp, sx, sy, ex, ey);

  IF li.vertical() THEN
    WHILE li.step_ver() DO END;
  ELSE
    WHILE li.step_hor() DO END;
  END;
END line3;

PROCEDURE (roaa: renderer_outline_aa_ptr) Construct*(ren: arb.renderer_base_ptr; prof: line_profile_aa_ptr);
BEGIN
  roaa.ren     := ren;
  roaa.profile := prof;

  roaa.color.ConstructI();
END Construct;

END AggRendererOutlineAA.
PROCEDURE (roaa: renderer_outline_aa_ptr)