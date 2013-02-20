MODULE AggClipLiangBarsky;

IMPORT
  bas := AggBasics;

(*
  Determine the clipping code of the vertex according to the
  Cyrus-Beck line clipping algorithm

         |        |
   0110  |  0010  | 0011
         |        |
  -------+--------+-------- clip_box.y2
         |        |
   0100  |  0000  | 0001
         |        |
  -------+--------+-------- clip_box.y1
         |        |
   1100  |  1000  | 1001
         |        |
   clip_box.x1  clip_box.x2
*)

CONST
  nearzero = 1.0E-30;

PROCEDURE clipping_flags_int*(x, y: bas.int; clip_box: bas.rect): SET;
VAR
  r: SET;
BEGIN
  r := {};
  IF (x > clip_box.x2) THEN INCL(r, 0); END;
  IF (y > clip_box.y2) THEN INCL(r, 1); END;
  IF (x < clip_box.x1) THEN INCL(r, 2); END;
  IF (y < clip_box.y1) THEN INCL(r, 3); END;
  RETURN r;
END clipping_flags_int;

PROCEDURE clip_liang_barsky_int*(x1, y1, x2, y2: bas.int32; clip_box: bas.rect; VAR x, y: bas.int_4): bas.int32;
VAR
  deltax, deltay, xin, xout, yin, yout, tinx, tiny, toutx, touty, tin1, tin2, tout1: bas.double;
  np, i: bas.int32;

BEGIN
  deltax := x2 - x1;
  deltay := y2 - y1;

  (* bump off of the vertical *)
  IF deltax = 0.0 THEN
    IF x1 > clip_box.x1 THEN
      deltax := -nearzero
    ELSE
      deltax := nearzero;
    END
  END;

  (* bump off of the horizontal *)
  IF deltay = 0.0 THEN
    IF y1 > clip_box.y1 THEN
      deltay := -nearzero
    ELSE
      deltay := nearzero;
    END;
  END;

  IF deltax > 0.0 THEN
    (* points to right *)
    xin  := clip_box.x1;
    xout := clip_box.x2;
  ELSE
    xin  := clip_box.x2;
    xout := clip_box.x1;
  END;

  IF deltay > 0.0 THEN
    (* points up *)
    yin  := clip_box.y1;
    yout := clip_box.y2;
  ELSE
    yin  := clip_box.y2;
    yout := clip_box.y1;
  END;

  tinx := (xin - x1) / deltax;
  tiny := (yin - y1) / deltay;

  IF tinx < tiny THEN
    (* hits x first *)
    tin1 := tinx;
    tin2 := tiny;
  ELSE
    (* hits y first *)
    tin1 := tiny;
    tin2 := tinx;
  END;

  i := 0;
  np := 0;

  IF tin1 <= 1.0 THEN

    IF 0.0 < tin1 THEN
      x[0] := ENTIER(xin);
      y[0] := ENTIER(yin);

      i  := 1;
      np := 1;
    END;

    IF tin2 <= 1.0 THEN

      toutx := (xout - x1) / deltax;
      touty := (yout - y1) / deltay;

      IF toutx < touty THEN
        tout1 := toutx
      ELSE
        tout1 := touty;
      END;

      IF (tin2 > 0.0) OR (tout1 > 0.0) THEN
        IF tin2 <= tout1 THEN
          IF tin2 > 0.0 THEN

            IF tinx > tiny THEN
              x[i] := ENTIER(xin);
              y[i] := ENTIER(y1 + tinx * deltay);
            ELSE
              x[i] := ENTIER(x1 + tiny * deltax);
              y[i] := ENTIER(yin);
            END;

            INC(i);
            INC(np);
          END;

          IF tout1 < 1.0 THEN
            IF toutx < touty THEN
              x[i] := ENTIER(xout);
              y[i] := ENTIER(y1 + toutx * deltay);
            ELSE
              x[i] := ENTIER(x1 + touty * deltax);
              y[i] := ENTIER(yout);
            END
          ELSE
            x[i] := x2;
            y[i] := y2;
          END;

          INC(np);
        ELSE
          IF tinx > tiny THEN
            x[i] := ENTIER(xin);
            y[i] := ENTIER(yout);
          ELSE
            x[i] := ENTIER(xout);
            y[i] := ENTIER(yin);
          END;

          INC(np);
        END;
      END;
    END;
  END;

  RETURN np;

END clip_liang_barsky_int;

PROCEDURE clip_liang_barsky_d  *(x1, y1, x2, y2: bas.double; clip_box: bas.rectd; VAR x, y: bas.double_4): bas.int32;
VAR
 deltax, deltay, xin, xout, yin, yout, tinx, tiny, toutx, touty, tin1, tin2, tout1: bas.double;
 i, np: bas.int32;

BEGIN
  deltax := x2 - x1;
  deltay := y2 - y1;

  np := 0;
  i := 0;
  (* bump off of the vertical *)
  IF deltax = 0.0 THEN
    IF x1 > clip_box.x1 THEN
      deltax := -nearzero
    ELSE
      deltax := nearzero;
    END;
  END;

  (* bump off of the horizontal*)
  IF deltay = 0.0 THEN
    IF y1 > clip_box.y1 THEN
      deltay := -nearzero
    ELSE
      deltay := nearzero;
    END;
  END;

  IF deltax > 0.0 THEN
    (* points to right *)
    xin  := clip_box.x1;
    xout := clip_box.x2;
  ELSE
    xin  := clip_box.x2;
    xout := clip_box.x1;
  END;

  IF deltay > 0.0 THEN
    (* points up *)
    yin  := clip_box.y1;
    yout := clip_box.y2;
  ELSE
    yin  := clip_box.y2;
    yout := clip_box.y1;
  END;

  tinx := (xin - x1) / deltax;
  tiny := (yin - y1) / deltay;

  IF tinx < tiny THEN
    (* hits x first *)
    tin1:=tinx;
    tin2:=tiny;
  ELSE
    (* hits y first *)
    tin1:=tiny;
    tin2:=tinx;
  END;

  IF tin1 <= 1.0 THEN
    IF 0.0 < tin1 THEN
      x[0] := xin;
      y[0] := yin;

      i := 1;
      np := 1;
    END;

    IF tin2 <= 1.0 THEN
      toutx := (xout - x1) / deltax;
      touty := (yout - y1) / deltay;

      IF toutx < touty THEN
        tout1 := toutx
      ELSE
        tout1:=touty;
      END;

      IF (tin2 > 0.0) OR (tout1 > 0.0) THEN
        IF tin2 <= tout1 THEN
          IF tin2 > 0.0 THEN

            IF tinx > tiny THEN
              x[i] := xin;
              y[i] := y1 + tinx * deltay;
            ELSE
              x[i] := x1 + tiny * deltax;
              y[i] := yin;
            END;

            INC(i);
            INC(np);
          END;

          IF tout1 < 1.0 THEN
            IF toutx < touty THEN
              x[i] :=xout;
              y[i] :=y1 + toutx * deltay;
            ELSE
              x[i] := x1 + touty * deltax;
              y[i] := yout;
            END
          ELSE
            x[i] := x2;
            y[i] := y2;
          END;

          INC(np);
        ELSE

          IF tinx > tiny THEN
            x[i] := xin;
            y[i] := yout;
          ELSE
            x[i] := xout;
            y[i] := yin;
          END;

          INC(np);
        END;
      END;
    END;
  END;

  RETURN np;

END clip_liang_barsky_d;

END AggClipLiangBarsky.