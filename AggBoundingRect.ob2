MODULE AggBoundingRect;

IMPORT
  bas := AggBasics,
  avs := AggVertexSource;

PROCEDURE bounding_rect*(
          vs: avs.vertex_source_ptr;
          VAR gi: ARRAY OF bas.int32u;
          start, num: bas.int32;
          VAR x1, y1, x2, y2: bas.double): BOOLEAN;
VAR
  i: bas.int32;
  cmd: SET;
  x, y: bas.double;
  first: BOOLEAN;
BEGIN
  first := TRUE;

  x1 := 1;
  y1 := 1;
  x2 := 0;
  y2 := 0;

  i := 0;

  WHILE i < num DO
    vs.rewind(gi[start + i]);
    cmd := vs.vertex(x, y);

    WHILE  ~bas.is_stop(cmd) DO
      IF bas.is_vertex(cmd) THEN
        IF first THEN
          x1 := x;
          y1 := y;
          x2 := x;
          y2 := y;

          first := FALSE;
        ELSE
          IF x < x1 THEN x1 := x; END;
          IF y < y1 THEN y1 := y; END;
          IF x > x2 THEN x2 := x; END;
          IF y > y2 THEN y2 := y; END;
        END;
      END;
      cmd:=vs.vertex(x, y);
    END;

    INC(i);
  END;

  RETURN (x1 <= x2) & (y1 <= y2);
END bounding_rect;

(*

PROCEDURE bounding_rect_ul*(
          vs: vertex_source_ptr;
          ul: unsigned_list_ptr;
          start, num:  unsigned;
          x1, y1, x2, y2:  double_ptr ):  BOOLEAN;
BEGIN

END bounding_rect_ul;

 function bounding_rect_single(
           vs:  vertex_source_ptr;
           path_id:  unsigned;
           x1, y1, x2, y2:  double_ptr ):  boolean;
*)
END AggBoundingRect.