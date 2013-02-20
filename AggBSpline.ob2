MODULE AggBSpline;

IMPORT
  bas := AggBasics;

(* --------------------------------------------------------------------bspline *)
(* A very simple class of Bi-cubic Spline interpolation.                       *)
(* First call init(num, x[], y[]) where num - number of source points,         *)
(* x, y - arrays of X and Y values respectively. Here Y must be a function     *)
(* of X. It means that all the X-coordinates must be arranged in the ascending *)
(* order.                                                                      *)
(* Then call get(x) that calculates a value Y for the respective X.            *)
(* The class supports extrapolation, i.e. you can call get(x) where x is       *)
(* outside the given with init() X-range. Extrapolation is a simple linear     *)
(* function.                                                                   *)
(* --------------------------------------------------------------------bspline *)
TYPE
  triple = RECORD
    am, x, y: bas.double;
  END;
  bspline_data = POINTER TO ARRAY OF triple;
  bspline_ptr* = POINTER TO bspline;
  bspline* = RECORD
    max, num: bas.int32;
    data: bspline_data;
    last_idx: bas.int32;
  END;

PROCEDURE (bs: bspline_ptr) Construct*();
BEGIN
  bs.max  := 0;
  bs.num  := 0;

  bs.data := NIL;

  bs.last_idx := -1;
END Construct;

PROCEDURE (bs: bspline_ptr) init1*(max: bas.int32);
BEGIN
  IF (max > 2) & (max > bs.max) THEN
    NEW(bs.data, max);

    bs.max := max;
  END;

  bs.num      := 0;
  bs.last_idx := -1;
END init1;

PROCEDURE (bs: bspline_ptr) add_point*(x, y: bas.double);
BEGIN
  IF bs.num < bs.max THEN
    bs.data[bs.num].x := x;
    bs.data[bs.num].x := y;

    INC(bs.num);
  END;
END add_point;

PROCEDURE (bs: bspline_ptr) prepare*();
VAR
  i, k, n1: bas.int32;
  al : POINTER TO ARRAY OF triple;
  h, p, d, f, e  : bas.double;
BEGIN
  IF bs.num > 2 THEN
    FOR k := 0 TO bs.num - 1 DO
      bs.data[k].am := 0.0;
    END;

    NEW(al, bs.num);


    FOR k := 0 TO bs.num - 1 DO
      al[k].am := 0;
      al[k].x  := 0;
      al[k].y  := 0;
    END;

    n1 :=  bs.num - 1;
    d  :=  bs.data[1].x - bs.data[0].x;
    e  := (bs.data[1].y - bs.data[0].y) / d;

    k := 1;

    WHILE k < n1 DO
      h := d;
      d := bs.data[k + 1].x - bs.data[k].x;
      f := e;
      e := (bs.data[k + 1].y - bs.data[k].y) / d;
      al[k].am := d / (d + h);
      al[k].x  := 1.0 - al[k].am;
      al[k].y  := 6.0 * (e - f) / (h + d);

      INC(k );
    END;

    k := 1;

    WHILE k < n1 DO
      p := 1.0 / (al[k].x * al[k - 1].am + 2.0);

      al[k].am := al[k].am * (-p);
      al[k].y  := (al[k].y - al[k].x * al[k - 1].y) * p;

      INC(k );
    END;

    bs.data[n1].am := 0.0;
    al[n1 - 1].am  := al[n1 - 1].y;
    bs.data[n1 - 1].am := al[n1 - 1].am;

    k := n1 - 2;
    i := 0;

    WHILE i < bs.num - 2 DO

     al[k].am := al[k].am * al[k + 1].am + al[k].y;
     bs.data[k].am := al[k].am;

     INC(i);
     DEC(k);
    END;

  END;

  bs.last_idx := -1;
END prepare;

PROCEDURE (bs: bspline_ptr) init2*(num: bas.int32; x, y: ARRAY OF bas.double);
VAR
  i: bas.int32;
BEGIN
  IF num > 2 THEN
    bs.init1(num);

    FOR i := 0 TO num - 1 DO
      bs.add_point(x[i], y[i]);
    END;
    bs.prepare();
  END;

  bs.last_idx := -1;
END init2;

PROCEDURE (bs: bspline_ptr) extrapolation_left*(x: bas.double): bas.double;
VAR
  d: bas.double;
BEGIN
  d := bs.data[1].x - bs.data[0].x;
  RETURN (-d * bs.data[1].am / 6 + (bs.data[1].y - bs.data[0].y) / d) * (x - bs.data[0].x) + bs.data[0].y;
END extrapolation_left;

PROCEDURE (bs: bspline_ptr) extrapolation_right*(x: bas.double): bas.double;
VAR
  d: bas.double;
BEGIN
  d := bs.data[bs.num - 1].x - bs.data[bs.num - 2].x;
  RETURN (d * bs.data[bs.num - 2].am / 6 + (bs.data[bs.num - 1].y - bs.data[bs.num - 2].y) / d) * (x - bs.data[bs.num - 1].x) + bs.data[bs.num - 1].y;
END extrapolation_right;

PROCEDURE (bs: bspline_ptr) interpolation*(x: bas.double; i: bas.int32): bas.double;
VAR
  j: bas.int32;
  d, h, r, p: bas.double;
BEGIN
  j := i + 1;
  d := bs.data[i].x - bs.data[j].x;
  h := x - bs.data[j].x;
  r := bs.data[i].x - x;
  p := d * d / 6.0;

  RETURN (bs.data[j].am * r * r * r + bs.data[i].am * h * h * h) / 6.0 / d +
         ((bs.data[j].y - bs.data[j].am * p) * r + (bs.data[i].y - bs.data[i].am * p) * h) / d;
END interpolation;

PROCEDURE (bs: bspline_ptr) bsearch(n: bas.int32; d: bspline_data; x0: bas.double; VAR i: bas.int32);
VAR
 j, k: bas.int32;
BEGIN
 j := n - 1;
 i := 0;

  WHILE j - i > 1 DO
    k:= ASH(i + j, -1);

    IF x0 < d[k].x THEN
      j := k;
    ELSE
      i := k;
    END;
  END;
END bsearch;

PROCEDURE (bs: bspline_ptr) get*(x: bas.double): bas.double;
VAR
  i: bas.int32;
BEGIN
  IF (bs.num > 2) THEN

    (* Extrapolation on the left *)
    IF (x < bs.data[0].x) THEN
      RETURN bs.extrapolation_left(x);
    END;

    (* Extrapolation on the right *)
    IF (x >= bs.data[bs.num - 1].x) THEN
      RETURN bs.extrapolation_right(x);
    END;

    (* Interpolation *)
    bs.bsearch(bs.num, bs.data, x, i);
    RETURN bs.interpolation(x, i);
  END;
  RETURN 0.0;
END get;

PROCEDURE (bs: bspline_ptr) get_stateful*(x: bas.double): bas.double;
BEGIN
  IF(bs.num > 2) THEN

    (* Extrapolation on the left *)
    IF (x < bs.data[0].x) THEN
      RETURN bs.extrapolation_left(x);
    END;

    (* Extrapolation on the right *)
    IF (x >= bs.data[bs.num - 1].x) THEN
      RETURN bs.extrapolation_right(x);
    END;

    IF (bs.last_idx >= 0) THEN

        (* Check if x is not in current range *)
        IF (x < bs.data[bs.last_idx].x) OR (x > bs.data[bs.last_idx + 1].x) THEN
            (* Check if x between next points (most probably) *)

            IF (bs.last_idx < bs.num - 2) & (x >= bs.data[bs.last_idx + 1].x) & (x <= bs.data[bs.last_idx + 2].x) THEN
                INC(bs.last_idx);
            ELSE
              IF (bs.last_idx > 0) & (x >= bs.data[bs.last_idx - 1].x) & (x <= bs.data[bs.last_idx].x) THEN
                (* x is between pevious points *)
                DEC(bs.last_idx);
              ELSE
                (* Else perform full search *)
                bs.bsearch(bs.num, bs.data, x, bs.last_idx);
              END;
            END;
        END;
        RETURN bs.interpolation(x, bs.last_idx);
    ELSE
        (* Interpolation *)
        bs.bsearch(bs.num, bs.data, x, bs.last_idx);
        RETURN bs.interpolation(x, bs.last_idx);
    END;
  END;
  RETURN 0.0;
END get_stateful;

END AggBSpline.

PROCEDURE (bs: bspline_ptr)