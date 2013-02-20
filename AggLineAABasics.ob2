MODULE AggLineAABasics;

IMPORT
  bas := AggBasics,
  math := MathL;

CONST
  line_subpixel_shift* = 8;                           (*----line_subpixel_shift*)
  line_subpixel_size*  = ASH(1, line_subpixel_shift); (*----line_subpixel_size *)
  line_subpixel_mask*  = line_subpixel_size - 1;      (*----line_subpixel_mask *)

  line_mr_subpixel_shift* = 4;                              (*----line_mr_subpixel_shift*)
  line_mr_subpixel_size*  = ASH(1, line_mr_subpixel_shift); (*----line_mr_subpixel_size *)
  line_mr_subpixel_mask*  = line_mr_subpixel_size - 1;      (*----line_mr_subpixel_mask *)

TYPE
  line_parameters_ptr* = POINTER TO line_parameters;
  line_parameters* = RECORD
    x1-, y1-, x2-, y2-, dx-, dy-, sx, sy: bas.int32;
    vertical-: BOOLEAN;
    inc-, len-, octant: bas.int32;
  END;

VAR
  s_orthogonal_quadrant - : ARRAY 8 OF bas.int8u;
  s_diagonal_quadrant   - : ARRAY 8 OF bas.int8u;

PROCEDURE (lp: line_parameters_ptr) Construct*(x1, y1, x2, y2, len: bas.int32);
BEGIN
  lp.x1 := x1;
  lp.y1 := y1;
  lp.x2 := x2;
  lp.y2 := y2;

  lp.dx := ABS(x2 - x1);
  lp.dy := ABS(y2 - y1);

  IF x2 > x1 THEN
    lp.sx := 1
  ELSE
    lp.sx := -1
  END;

  IF y2 > y1 THEN
    lp.sy := 1
  ELSE
    lp.sy := -1
  END;

  lp.vertical := lp.dy >= lp.dx;

  IF lp.vertical THEN
    lp.inc := lp.sy
  ELSE
    lp.inc := lp.sx
  END;

  lp.len := len;

  lp.octant := 0;

  IF lp.vertical THEN
    INC(lp.octant, 1);
  END;

  IF lp.sx = -1 THEN
    INC(lp.octant, 2);
  END;

  IF lp.sy = -1 THEN
    INC(lp.octant, 4);
  END;
END Construct;

PROCEDURE (lp: line_parameters_ptr) orthogonal_quadrant*(): bas.int8u;
BEGIN
  RETURN s_orthogonal_quadrant[lp.octant];
END orthogonal_quadrant;

PROCEDURE (lp: line_parameters_ptr) diagonal_quadrant*(): bas.int8u;
BEGIN
  RETURN s_diagonal_quadrant[lp.octant];
END diagonal_quadrant;

PROCEDURE (lp: line_parameters_ptr) same_orthogonal_quadrant*(octant: bas.int32): BOOLEAN;
BEGIN
  RETURN s_orthogonal_quadrant[octant] = s_orthogonal_quadrant[lp.octant];
END same_orthogonal_quadrant;

PROCEDURE (lp: line_parameters_ptr) same_diagonal_quadrant*(octant: bas.int32): BOOLEAN;
BEGIN
  RETURN s_diagonal_quadrant[octant] = s_diagonal_quadrant[lp.octant];
END same_diagonal_quadrant;


PROCEDURE line_mr*(x: bas.int32): bas.int32;
BEGIN
  RETURN bas.shr_int32(x, line_subpixel_shift - line_mr_subpixel_shift);
END line_mr;

PROCEDURE line_hr*(x: bas.int32): bas.int32;
BEGIN
  RETURN ASH(x, (line_subpixel_shift - line_mr_subpixel_shift));
END line_hr;

PROCEDURE line_dbl_hr*(x: bas.int32): bas.int32;
BEGIN
  RETURN ASH(x, line_subpixel_shift);
END line_dbl_hr;

PROCEDURE line_coord*(x: bas.double): bas.int32;
BEGIN
  RETURN ENTIER(x * line_subpixel_size);
END line_coord;


PROCEDURE bisectrix*(VAR l1, l2: line_parameters; VAR x, y: bas.int);
VAR
 k, tx, ty, dx, dy: bas.double;
BEGIN
  k  := l2.len / l1.len;
  tx := l2.x2 - (l2.x1 - l1.x1 ) * k;
  ty := l2.y2 - (l2.y1 - l1.y1 ) * k;

  (* All bisectrices must be on the right of the line
     If the next point is on the left (l1 => l2.2)
     then the bisectix should be rotated by 180 degrees. *)
  IF (l2.x2 - l2.x1) * (l2.y1 - l1.y1) <
     (l2.y2 - l2.y1) * (l2.x1 - l1.x1) + 100.0 THEN
    tx := tx - ((tx - l2.x1) * 2.0);
    ty := ty - ((ty - l2.y1) * 2.0);
  END;

  (* Check if the bisectrix is too short *)
  dx := tx - l2.x1;
  dy := ty - l2.y1;

  IF ENTIER(math.sqrt(dx * dx + dy * dy)) < line_subpixel_size THEN
    x := bas.shr_int32(l2.x1 + l2.x1 + (l2.y1 - l1.y1) + (l2.y2 - l2.y1), 1);
    y := bas.shr_int32(l2.y1 + l2.y1 - (l2.x1 - l1.x1) - (l2.x2 - l2.x1), 1);

    RETURN;
  END;

  x := ENTIER(tx);
  y := ENTIER(ty);
END bisectrix;

PROCEDURE fix_degenerate_bisectrix_start*(lp: line_parameters_ptr; VAR x, y: bas.int32);
VAR
  d: bas.int32;
BEGIN
  d := ENTIER(
    ((x - lp.x2) * (lp.y2 - lp.y1) -
     (y - lp.y2) * (lp.x2 - lp.x1)) / lp.len);

  IF d < line_subpixel_size THEN
    x := lp.x1 + (lp.y2 - lp.y1);
    y := lp.y1 - (lp.x2 - lp.x1);
  END;

END fix_degenerate_bisectrix_start;

PROCEDURE fix_degenerate_bisectrix_end  *(lp: line_parameters_ptr; VAR x, y: bas.int32);
VAR
  d: bas.int32;
BEGIN
  d := ENTIER(
    ((x - lp.x2) * (lp.y2 - lp.y1) -
     (y - lp.y2) * (lp.x2 - lp.x1)) / lp.len);

  IF d < line_subpixel_size THEN
    x := lp.x2 + (lp.y2 - lp.y1);
    y := lp.y2 - (lp.x2 - lp.x1);
  END;
END fix_degenerate_bisectrix_end;


BEGIN
  s_orthogonal_quadrant[0] := 0;
  s_orthogonal_quadrant[1] := 0;
  s_orthogonal_quadrant[2] := 1;
  s_orthogonal_quadrant[3] := 1;
  s_orthogonal_quadrant[4] := 3;
  s_orthogonal_quadrant[5] := 3;
  s_orthogonal_quadrant[6] := 2;
  s_orthogonal_quadrant[7] := 2;

  s_diagonal_quadrant[0]   := 0;
  s_diagonal_quadrant[1]   := 1;
  s_diagonal_quadrant[2]   := 2;
  s_diagonal_quadrant[3]   := 1;
  s_diagonal_quadrant[4]   := 0;
  s_diagonal_quadrant[5]   := 3;
  s_diagonal_quadrant[6]   := 2;
  s_diagonal_quadrant[7]   := 3;
END AggLineAABasics.
