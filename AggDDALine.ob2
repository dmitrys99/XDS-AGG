MODULE AggDDALine;

IMPORT bas := AggBasics;

CONST
  subpixel_shift* = 8;
  subpixel_size*  = ASH(1, subpixel_shift);
  subpixel_mask*  = subpixel_size - 1;

TYPE
  (* dda_line_interpolator *)
  dda_line_interpolator* = RECORD
    y-, inc-, dy-,
    FractionShift ,YShift: bas.int32;
  END;

  (* line_bresenham_interpolator *)
  dda2_line_interpolator* = RECORD
    cnt, lft-, rem-, mod-, y-: bas.int32;
  END;

  line_bresenham_interpolator* = RECORD
    x1_lr,
    y1_lr,
    x2_lr,
    y2_lr: bas.int32;
    ver-: BOOLEAN;
    len-: bas.int32u;
    inc-: bas.int32;
    interpolator: dda2_line_interpolator;
  END;

(* Construct *)

PROCEDURE (VAR dli: dda_line_interpolator) Construct*(FS, YS: bas.int32);
BEGIN
  dli.FractionShift := FS;
  dli.YShift        := YS;
END Construct;

(* Construct2 *)
PROCEDURE (VAR dli: dda_line_interpolator) Construct2*(y1, y2, count: bas.int32; FS, YS: bas.int32);
BEGIN
  dli.Construct(FS, YS);

  dli.y   := y1;
  dli.inc := ASH(y2 - y1, dli.FractionShift) DIV count;
  dli.dy  := 0;
END Construct2;

(* plus_operator *)

PROCEDURE (VAR dli: dda_line_interpolator) plus_operator*();
BEGIN
  INC(dli.dy, dli.inc);
END plus_operator;

(* minus_operator *)

PROCEDURE (VAR dli: dda_line_interpolator) minus_operator*();
BEGIN
  DEC(dli.dy, dli.inc);
END minus_operator;

(* inc_operator *)

PROCEDURE (VAR dli: dda_line_interpolator) inc_operator*(n: bas.int32);
BEGIN
  INC(dli.dy, n * dli.inc);
END inc_operator;

(* dec_operator *)

PROCEDURE (VAR dli: dda_line_interpolator) dec_operator*(n: bas.int32);
BEGIN
  DEC(dli.dy, n * dli.inc);
END dec_operator;

(* Y *)

PROCEDURE (VAR dli: dda_line_interpolator) Y*(): bas.int32;
BEGIN
  (*RETURN dli.y + (ASH(dli.dy, dli.FractionShift - dli.YShift));*)
  RETURN dli.y + (bas.shr_int32(dli.dy, dli.FractionShift - dli.YShift));
END Y;

(* Construct *)

PROCEDURE (VAR dli2: dda2_line_interpolator) Construct*(y1, y2, count: bas.int32);
BEGIN
  IF count <= 0 THEN
    dli2.cnt := 1
  ELSE
    dli2.cnt := count;
  END;

  dli2.lft := (y2 - y1) DIV dli2.cnt;
  dli2.rem := (y2 - y1) MOD dli2.cnt;
  dli2.mod := dli2.rem;
  dli2.y   := y1;

  IF dli2.mod <= 0 THEN
    dli2.mod := dli2.mod + count;
    dli2.rem := dli2.rem + count;

    DEC(dli2.lft);
  END;

  dli2.mod := dli2.mod - count;
END Construct;

(* Construct2 *)

PROCEDURE (VAR dli2: dda2_line_interpolator) Construct2*(y, count: bas.int32);
BEGIN
  IF count <= 0 THEN
    dli2.cnt := 1
  ELSE
    dli2.cnt := count;
  END;

  dli2.lft := y DIV dli2.cnt;
  dli2.rem := y MOD dli2.cnt;
  dli2.mod := dli2.rem;
  dli2.y   := 0;

  IF dli2.mod <= 0 THEN
    INC(dli2.mod, count);
    INC(dli2.rem, count);
    DEC(dli2.lft);
  END;
END Construct2;

(* plus_operator *)

PROCEDURE (VAR dli2: dda2_line_interpolator) plus_operator*();
BEGIN
  INC(dli2.mod, dli2.rem);
  INC(dli2.y, dli2.lft);

  IF dli2.mod > 0 THEN
    DEC(dli2.mod ,dli2.cnt);
    INC(dli2.y);
  END;
END plus_operator;

(* minus_operator *)

PROCEDURE (VAR dli2: dda2_line_interpolator) minus_operator*();
BEGIN
  IF dli2.mod <= dli2.rem THEN
    INC(dli2.mod ,dli2.cnt);
    DEC(dli2.y);
  END;

  DEC(dli2.mod, dli2.rem);
  DEC(dli2.y, dli2.lft);
END minus_operator;

(* adjust_forward *)
PROCEDURE (VAR dli2: dda2_line_interpolator) adjust_forward*();
BEGIN
  DEC(dli2.mod, dli2.cnt);
END adjust_forward;

(* adjust_backward *)

PROCEDURE (VAR dli2: dda2_line_interpolator) adjust_backward*();
BEGIN
  INC(dli2.mod, dli2.cnt);
END adjust_backward;

(*  line_lr *)
PROCEDURE (VAR lbi: line_bresenham_interpolator) line_lr*(v: bas.int32): bas.int32;
BEGIN
  (*RETURN ASH(v, subpixel_shift);*)
  RETURN bas.shr_int32(v, subpixel_shift);
END line_lr;

(* Construct *)

PROCEDURE (VAR lbi: line_bresenham_interpolator) Construct*(x1, y1, x2, y2: bas.int32);
BEGIN
  lbi.x1_lr := lbi.line_lr(x1);
  lbi.y1_lr := lbi.line_lr(y1);
  lbi.x2_lr := lbi.line_lr(x2);
  lbi.y2_lr := lbi.line_lr(y2);

  lbi.ver := ABS(lbi.x2_lr - lbi.x1_lr) < ABS(lbi.y2_lr - lbi.y1_lr);

  IF lbi.ver THEN
    lbi.len := ABS(lbi.y2_lr - lbi.y1_lr)
  ELSE
    lbi.len := ABS(lbi.x2_lr - lbi.x1_lr);
  END;

  IF lbi.ver THEN

    IF y2 > y1 THEN
      lbi.inc := 1
    ELSE
      lbi.inc := -1
    END;

  ELSE

    IF x2 > x1 THEN
      lbi.inc := 1
    ELSE
      lbi.inc := -1
    END;

  END;

  IF lbi.ver THEN
    lbi.interpolator.Construct(x1, x2, lbi.len);
  ELSE
    lbi.interpolator.Construct(y1, y2, lbi.len);
  END;
END Construct;

PROCEDURE (VAR lbi: line_bresenham_interpolator) hstep*();
BEGIN
  lbi.interpolator.plus_operator();
  lbi.x1_lr := lbi.x1_lr + lbi.inc;
END hstep;

PROCEDURE (VAR lbi: line_bresenham_interpolator) vstep*();
BEGIN
  lbi.interpolator.plus_operator();
  lbi.y1_lr := lbi.y1_lr + lbi.inc;
END vstep;

PROCEDURE (VAR lbi: line_bresenham_interpolator) x1*(): bas.int32;
BEGIN
  RETURN lbi.x1_lr;
END x1;

PROCEDURE (VAR lbi: line_bresenham_interpolator) y1*(): bas.int32;
BEGIN
  RETURN lbi.y1_lr;
END y1;

PROCEDURE (VAR lbi: line_bresenham_interpolator) x2*(): bas.int32;
BEGIN
  RETURN lbi.line_lr(lbi.interpolator.y);
END x2;

PROCEDURE (VAR lbi: line_bresenham_interpolator) y2*(): bas.int32;
BEGIN
  RETURN lbi.line_lr(lbi.interpolator.y);
END y2;

PROCEDURE (VAR lbi: line_bresenham_interpolator) x2_hr*(): bas.int32;
BEGIN
  RETURN lbi.interpolator.y;
END x2_hr;

PROCEDURE (VAR lbi: line_bresenham_interpolator) y2_hr*(): bas.int32;
BEGIN
  RETURN lbi.interpolator.y;
END y2_hr;


END AggDDALine.