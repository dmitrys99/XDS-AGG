MODULE AggRenderScanlines;

IMPORT
  bas := AggBasics,
  rzs := AggRasterizerScanline,
  asl := AggScanline,
  res := AggRendererScanline,
  avs := AggVertexSource,
  col := AggColor;

PROCEDURE render_scanlines*(ras: rzs.rasterizer_scanline_ptr; sl: asl.scanline_ptr; ren: res.renderer_scanline_ptr);
BEGIN
  IF ras.rewind_scanlines() THEN
    sl.reset(ras.min_x(), ras.max_x());
    ren.prepare((ras.max_x() - ras.min_x() + 2));

    IF sl.is_embedded() THEN
      WHILE ras.sweep_scanline_em(sl) DO
        ren.render(sl);
      END;
    ELSE
      WHILE ras.sweep_scanline(sl) DO
        ren.render(sl);
      END;
    END;
  END;
END render_scanlines;

PROCEDURE render_all_paths*(ras       : rzs.rasterizer_scanline_ptr;
                            sl        : asl.scanline_ptr;
                            r         : res.renderer_scanline_ptr;
                            vs        : avs.vertex_source_ptr;
                            cs        : col.colors_array;
                            path_id   : ARRAY OF bas.int32u;
                            num_paths : bas.int32);
VAR
  i: bas.int32;
BEGIN
  i := 0;

  WHILE i < num_paths DO
    ras.reset();
    ras.add_path(vs, path_id[i]);
    r.color := cs[i];

    render_scanlines(ras, sl, r);

    INC(i);
  END;
END render_all_paths;

END AggRenderScanlines.