MODULE AggMathStroke;

IMPORT
  bas  := AggBasics,
  math := MathL,
  amth := AggMath,
  avs  := AggVertexSequence;

CONST
  butt_cap*   = 0;
  square_cap* = 1;
  round_cap*  = 2;

  miter_join*         = 0;
  miter_join_revert*  = 1;
  miter_join_round*   = 4;
  round_join*         = 2;
  bevel_join*         = 3;

  inner_bevel* = 0;
  inner_miter* = 1;
  inner_jag*   = 2;
  inner_round* = 3;

  (* Minimal angle to calculate round joins, less than 0.1 degree.*)
  stroke_theta* = 0.001;

PROCEDURE stroke_calc_arc*(
          out_vertices: avs.vertex_sequence_ptr;
          x, y, dx1, dy1, dx2, dy2, width, approximation_scale: bas.double);
VAR
  (*pt: bas.point_type_ptr;*)
  pt: avs.vertex_dist_ptr;

  a1, a2, da: bas.double;

  ccw: BOOLEAN;

BEGIN
  a1 := math.arctan2(dx1, dy1);(*ok*)
  a2 := math.arctan2(dx2, dy2);(*ok*)
  da := a1 - a2;

(* Possible optimization. Not important at all; consumes time but happens rarely
 if Abs(da ) < stroke_theta then
  begin
   pt.x:=(x + x + dx1 + dx2 ) * 0.5;
   pt.y:=(y + y + dy1 + dy2 ) * 0.5;

   out_vertices.add(@pt );
   exit;

  end;*)

  ccw := (da > 0.0) & (da < math.pi);

  IF width < 0 THEN
    width := -width;
  END;

  da := math.arccos(width / (width + 0.125 / approximation_scale)) * 2;

  NEW(pt);
  pt.x := x + dx1;
  pt.y := y + dy1;

  out_vertices.add(pt);

  IF ~ccw THEN
    IF a1 > a2 THEN
      a2 := a2 + (2 * math.pi);
    END;

    a2 := a2 - (da / 4);
    a1 := a1 + da;

    WHILE a1 < a2 DO
      NEW(pt);

      pt.x := x + math.cos(a1) * width;
      pt.y := y + math.sin(a1) * width;

      out_vertices.add(pt);

      a1:=a1 + da;

    END;
  ELSE
    IF a1 < a2 THEN
      a2 := a2 - (2 * math.pi);
    END;

    a2 := a2 + (da / 4);
    a1 := a1 - da;

    WHILE a1 > a2 DO
      NEW(pt);

      pt.x := x + math.cos(a1) * width;
      pt.y := y + math.sin(a1) * width;

      out_vertices.add(pt);

      a1 := a1 - da;
    END;

  END;

  NEW(pt);

  pt.x := x + dx2;
  pt.y := y + dy2;

  out_vertices.add(pt);

END stroke_calc_arc;

PROCEDURE stroke_calc_miter*(
          out_vertices: avs.vertex_sequence_ptr;
          v0, v1, v2: avs.vertex_dist_ptr;
          dx1, dy1, dx2, dy2, width: bas.double;
          line_join: bas.int32;
          miter_limit, approximation_scale: bas.double);
VAR
  (*pt: bas.point_type_ptr;*)
  pt: avs.vertex_dist_ptr;
  xi, yi, d1, lim, x2, y2: bas.double;
  miter_limit_exceeded: BOOLEAN;
BEGIN
  ASSERT(v0 # NIL);
  ASSERT(v1 # NIL);
  ASSERT(v2 # NIL);

  xi := v1.x;
  yi := v1.y;

  miter_limit_exceeded := TRUE; (* Assume the worst *)

  IF amth.calc_intersection(
       v0.x + dx1, v0.y - dy1,
       v1.x + dx1, v1.y - dy1,
       v1.x + dx2, v1.y - dy2,
       v2.x + dx2, v2.y - dy2,
       xi, yi) THEN

  (* Calculation of the intersection succeeded
     --------------------- *)
    d1  := amth.calc_distance(v1.x, v1.y, xi, yi);
    lim := width * miter_limit;

    IF d1 <= lim THEN
      (* Inside the miter limit
       --------------------- *)
      NEW(pt);

      pt.x := xi;
      pt.y := yi;

      out_vertices.add(pt);

      miter_limit_exceeded := FALSE;
    END;
  ELSE
    (* Calculation of the intersection failed, most probably
       the three points lie one straight line.
       First check if v0 and v2 lie on the opposite sides of vector:
       (v1.x, v1.y) -> (v1.x+dx1, v1.y-dy1), that is, the perpendicular
       to the line determined by vertices v0 and v1.
       This condition determines whether the next line segments continues
       the previous one or goes back.
       ---------------- *)
    x2 := v1.x + dx1;
    y2 := v1.y - dy1;

    IF (((x2 - v0.x) * dy1 - (v0.y - y2) * dx1 < 0.0)#
        ((x2 - v2.x) * dy1 - (v2.y - y2) * dx1 < 0.0)) THEN

      (* This case means that the next segment continues
         the previous one (straight line)
         -----------------*)
      NEW(pt);

      pt.x := v1.x + dx1;
      pt.y := v1.y - dy1;

      out_vertices.add(pt);

      miter_limit_exceeded := FALSE;

    END;
  END;

  IF miter_limit_exceeded THEN
    (* Miter limit exceeded
       ------------------------ *)
    CASE line_join OF
      miter_join_revert:
        (* For the compatibility with SVG, PDF, etc,
           we use a simple bevel join instead of
           "smart" bevel
           -------------------*)
        NEW(pt);

        pt.x := v1.x + dx1;
        pt.y := v1.y - dy1;

        out_vertices.add(pt);

        NEW(pt);

        pt.x:=v1.x + dx2;
        pt.y:=v1.y - dy2;

        out_vertices.add(pt);

    | miter_join_round :
        stroke_calc_arc(
         out_vertices,
         v1.x, v1.y, dx1, -dy1, dx2, -dy2,
         width, approximation_scale);

    ELSE
      (* If no miter-revert, calculate new dx1, dy1, dx2, dy2
         ---------------- *)
      NEW(pt);

      pt.x := v1.x + dx1 + dy1 * miter_limit;
      pt.y := v1.y - dy1 + dx1 * miter_limit;

      out_vertices.add(pt);

      NEW(pt);

      pt.x := v1.x + dx2 - dy2 * miter_limit;
      pt.y := v1.y - dy2 - dx2 * miter_limit;

      out_vertices.add(pt);
    END;
  END;
END stroke_calc_miter;

PROCEDURE stroke_calc_cap*(
          out_vertices: avs.vertex_sequence_ptr;
          v0, v1: avs.vertex_dist_ptr;
          len: bas.double;
          line_cap: bas.int32;
          width, approximation_scale: bas.double);
VAR
  (*pt: bas.point_type_ptr;*)
  pt: avs.vertex_dist_ptr;

  dx1, dy1,
  dx2, dy2,

  a1, a2, da: bas.double;

BEGIN
  out_vertices.remove_all();

  dx1 := (v1.y - v0.y) / len;
  dy1 := (v1.x - v0.x) / len;
  dx2 := 0;
  dy2 := 0;

  dx1 := dx1 * width;
  dy1 := dy1 * width;

  IF line_cap # round_cap THEN
    IF line_cap = square_cap THEN
      dx2 := dy1;
      dy2 := dx1;
    END;

    NEW(pt);

    pt.x := v0.x - dx1 - dx2;
    pt.y := v0.y + dy1 - dy2;

    out_vertices.add(pt);

    NEW(pt);

    pt.x:=v0.x + dx1 - dx2;
    pt.y:=v0.y - dy1 - dy2;

    out_vertices.add(pt);

  ELSE
    a1 := math.arctan2(-dx1, dy1);(*ok*)
    a2 := a1 + math.pi;
    da := math.arccos(width / (width + 0.125 / approximation_scale)) * 2;

    NEW(pt);

    pt.x := v0.x - dx1;
    pt.y := v0.y + dy1;

    out_vertices.add(pt);

    a1:=a1 + da;
    a2:=a2 - (da / 4);

    WHILE a1 < a2 DO
      NEW(pt);

      pt.x := v0.x + math.cos(a1) * width;
      pt.y := v0.y + math.sin(a1) * width;

      out_vertices.add(pt);

      a1 := a1 + da;
    END;

    NEW(pt);

    pt.x := v0.x + dx1;
    pt.y := v0.y - dy1;

    out_vertices.add(pt);
  END;
END stroke_calc_cap;

PROCEDURE stroke_calc_join*(out_vertices: avs.vertex_sequence_ptr;
            v0, v1, v2: avs.vertex_dist_ptr;
            len1, len2, width: bas.double;
            line_join, inner_join: bas.int32;
            miter_limit, inner_miter_limit, approximation_scale: bas.double);
VAR
  (*pt: bas.point_type_ptr;*)
  pt: avs.vertex_dist_ptr;

  d, dx1, dy1, dx2, dy2: bas.double;

BEGIN
  dx1 := width * (v1.y - v0.y) / len1;
  dy1 := width * (v1.x - v0.x) / len1;

  dx2 := width * (v2.y - v1.y) / len2;
  dy2 := width * (v2.x - v1.x) / len2;

  out_vertices.remove_all();

  IF amth.calc_point_location(v0.x, v0.y, v1.x, v1.y, v2.x, v2.y) > 0 THEN
  (* Inner join
     --------------- *)
    CASE inner_join OF
    inner_miter :
      stroke_calc_miter(out_vertices,
        v0, v1, v2, dx1, dy1, dx2, dy2,
        width,
        miter_join_revert,
        inner_miter_limit,
        1.0);

    | inner_jag, inner_round :
      d := (dx1 - dx2) * (dx1 - dx2) + (dy1 - dy2) * (dy1 - dy2);

      IF (d < len1 * len1) & (d < len2 * len2) THEN
        stroke_calc_miter(out_vertices,
          v0, v1, v2, dx1, dy1, dx2, dy2,
          width,
          miter_join_revert,
          inner_miter_limit,
          1.0)

      ELSE
        IF inner_join = inner_jag THEN

          NEW(pt);

          pt.x := v1.x + dx1;
          pt.y := v1.y - dy1;

          out_vertices.add(pt);

          NEW(pt);

          pt.x := v1.x;
          pt.y := v1.y;

          out_vertices.add(pt);

          NEW(pt);

          pt.x := v1.x + dx2;
          pt.y := v1.y - dy2;

          out_vertices.add(pt);
        ELSE
          NEW(pt);

          pt.x := v1.x + dx1;
          pt.y := v1.y - dy1;

          out_vertices.add(pt);

          NEW(pt);

          pt.x := v1.x;
          pt.y := v1.y;

          out_vertices.add(pt);

          stroke_calc_arc(out_vertices,
            v1.x, v1.y, dx2, -dy2, dx1, -dy1,
            width, approximation_scale);

          NEW(pt);

          pt.x := v1.x;
          pt.y := v1.y;

          out_vertices.add(pt);

          NEW(pt);

          pt.x := v1.x + dx2;
          pt.y := v1.y - dy2;

          out_vertices.add(pt);
        END;
      END;
    ELSE (* inner_bevel *)
      NEW(pt);

      pt.x := v1.x + dx1;
      pt.y := v1.y - dy1;

      out_vertices.add(pt);

      NEW(pt);

      pt.x := v1.x + dx2;
      pt.y := v1.y - dy2;

      out_vertices.add(pt);
    END;
  ELSE
  (* Outer join
     --------------- *)
    CASE line_join OF
    miter_join, miter_join_revert, miter_join_round:
      stroke_calc_miter(out_vertices,
       v0, v1, v2, dx1, dy1, dx2, dy2,
       width,
       line_join,
       miter_limit, approximation_scale);

    | round_join:
      stroke_calc_arc(out_vertices,
       v1.x, v1.y, dx1, -dy1, dx2, -dy2,
       width, approximation_scale);

    ELSE (* Bevel join *)
      NEW(pt);

      pt.x := v1.x + dx1;
      pt.y := v1.y - dy1;

      out_vertices.add(pt);

      NEW(pt);

      pt.x := v1.x + dx2;
      pt.y := v1.y - dy2;

      out_vertices.add(pt);
    END;
  END;
END stroke_calc_join;


END AggMathStroke.