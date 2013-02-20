MODULE AggShortenPath;

IMPORT
  bas := AggBasics,
  avs := AggVertexSequence;

PROCEDURE shorten_path*(vs: avs.vertex_sequence_ptr; s: bas.double; closed: BOOLEAN);
VAR
  n: bas.int32;
  d, x, y: bas.double;
  prev, last: avs.vertex_dist_ptr;
BEGIN
  IF (s > 0.0) & (vs.size > 1) THEN
    n := vs.size - 2;

    WHILE (n # 0) & (vs.array[n](avs.vertex_dist_ptr).dist <= s) DO
      d := vs.array[n](avs.vertex_dist_ptr).dist;
      vs.remove_last();
      s := s - d;
      DEC(n);
    END;

    IF vs.size < 2 THEN
      vs.remove_all()
    ELSE
      n := vs.size - 1;

      prev := vs.array[n - 1](avs.vertex_dist_ptr);
      last := vs.array[n](avs.vertex_dist_ptr);

      d := (prev^.dist - s) / prev^.dist;

      x := prev^.x + (last^.x - prev^.x) * d;
      y := prev^.y + (last^.y - prev^.y) * d;

      last^.x:=x;
      last^.y:=y;

      IF ~vs.func(prev, last) THEN
        vs.remove_last();
      END;
      vs.close(closed);
    END;
  END;
END shorten_path;

END AggShortenPath.