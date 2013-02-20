MODULE AggRendererScanline;
IMPORT
(* DEBUG
  STextIO,
  SWholeIO,
*)
  bas := AggBasics,
  col := AggColor,
  asl := AggScanline,
  arb := AggRendererBase,
  ars := AggRasterizerScanline,
  asg := AggSpanGenerator;

TYPE
  renderer_scanline_ptr* = POINTER TO renderer_scanline;
  renderer_scanline* = RECORD(ars.rasterizer_scanline)
    color*: col.aggclr;
    ren: arb.renderer_base_ptr;
  END;

  renderer_scanline_aa_ptr* = POINTER TO renderer_scanline_aa;
  renderer_scanline_aa* = RECORD(renderer_scanline)
    span_gen: asg.span_generator_ptr;
  END;

  renderer_scanline_aa_solid_ptr* = POINTER TO renderer_scanline_aa_solid;
  renderer_scanline_aa_solid* = RECORD(renderer_scanline)
  END;

PROCEDURE (rs: renderer_scanline_ptr) Construct*(ren: arb.renderer_base_ptr);
BEGIN
  rs.color.ConstructI();
  rs.ren := ren;
END Construct;

PROCEDURE (rs: renderer_scanline_ptr) ConstructI*();
BEGIN
  rs.color.ConstructI();
  rs.ren := NIL;
END ConstructI;

PROCEDURE (rs: renderer_scanline_ptr) prepare*(u: bas.int32);
BEGIN
END prepare;

PROCEDURE (rs: renderer_scanline_ptr) render*(sl: asl.scanline_ptr);
BEGIN
(*  STextIO.WriteString("renderer_scanline.render"); STextIO.WriteLn(); *)
END render;

PROCEDURE (rsaa: renderer_scanline_aa_ptr) ConstructI*();
BEGIN
  rsaa.ren := NIL;
  rsaa.span_gen := NIL;
END ConstructI;

PROCEDURE (rsaa: renderer_scanline_aa_ptr) ConstructSG*(ren: arb.renderer_base_ptr; span_gen: asg.span_generator_ptr);
BEGIN
  rsaa.ren := ren;
  rsaa.span_gen := span_gen;
END ConstructSG;

PROCEDURE (rsaa: renderer_scanline_aa_ptr) attach*(ren: arb.renderer_base_ptr; span_gen: asg.span_generator_ptr);
BEGIN
  rsaa.ren := ren;
  rsaa.span_gen := span_gen;
END attach;

PROCEDURE (rsaa: renderer_scanline_aa_ptr) prepare*(u: bas.int32);
BEGIN
  rsaa.span_gen.prepare(u);
END prepare;

PROCEDURE (rsaa: renderer_scanline_aa_ptr) render*(sl: asl.scanline_ptr);

VAR
  y, xmin, xmax, x, len: bas.int32;
  num_spans: bas.int32;
  span_ptr: bas.int32;
  solid  : BOOLEAN;
  covers_offset: bas.int32;
  colors: col.colors_array_ptr;
BEGIN
(*  STextIO.WriteString("renderer_scanline_aa.render"); STextIO.WriteLn(); *)
  rsaa.render^(sl);
  y := sl.y;
  rsaa.ren.first_clip_box();

  REPEAT
    xmin := rsaa.ren.xmin();
    xmax := rsaa.ren.xmax();

    IF (y >= rsaa.ren.ymin()) & (y <= rsaa.ren.ymax()) THEN

      num_spans := sl.num_spans();

      span_ptr := 0;

      LOOP
        x   := sl.spans^[span_ptr].x;
        len := sl.spans^[span_ptr].len;

        solid  := FALSE;
        covers_offset := sl.spans^[span_ptr].covers_offset;

        IF len < 0 THEN
          solid := TRUE;
          len   := -len;
        END;

        IF x < xmin THEN
          DEC(len, xmin - x);

          IF ~solid THEN
            INC(covers_offset, xmin - x);
          END;

          x := xmin;
        END;

        IF len > 0 THEN
          IF (x + len) > xmax THEN
            len := xmax - x + 1;
          END;

          IF len > 0 THEN
            IF ~solid THEN
              colors := rsaa.span_gen.generate(x, y, len);
              rsaa.ren.blend_color_hspan_no_clip_offset(x, y, len, colors, 0, sl.covers, covers_offset, sl.covers^[covers_offset]);
            ELSE
              colors := rsaa.span_gen.generate(x, y, len);
              rsaa.ren.blend_color_hspan_no_clip_offset(x, y, len, colors, 0, sl.covers, covers_offset, sl.covers^[covers_offset]);
            END;
          END;
        END;

        DEC(num_spans);

        IF num_spans = 0 THEN
          EXIT;
        END;
        INC(span_ptr);
      END;
    END;
 UNTIL ~rsaa.ren.next_clip_box();
END render;

PROCEDURE (rsas: renderer_scanline_aa_solid_ptr) render*(sl: asl.scanline_ptr);
VAR
  x, y, num_spans, span_pl: bas.int32;
BEGIN
(*  STextIO.WriteString("renderer_scanline_aa_solid.render"); STextIO.WriteLn(); *)
  rsas.render^(sl);
  y := sl.y;

  num_spans := sl.num_spans();

  span_pl := 1;

  LOOP
(* DEBUG
    STextIO.WriteString('num_spans = ');
    SWholeIO.WriteInt(num_spans, 4);
    STextIO.WriteLn();
    STextIO.WriteString('span_pl = ');
    SWholeIO.WriteInt(span_pl, 4);
    STextIO.WriteLn(); *)
    x := sl.spans[span_pl].x;

(* DEBUG
    STextIO.WriteString('x = ');
    SWholeIO.WriteInt(x, 4);
    STextIO.WriteString(';');

    STextIO.WriteString('y = ');
    SWholeIO.WriteInt(y, 4);
    STextIO.WriteString(';');

    STextIO.WriteString('sl.spans[span_pl].len = ');
    SWholeIO.WriteInt(sl.spans[span_pl].len, 4);
    STextIO.WriteString(';');

    STextIO.WriteString('span_pl = ');
    SWholeIO.WriteInt(span_pl, 4);

    STextIO.WriteLn();
*)
    IF sl.spans[span_pl].len > 0 THEN
      rsas.ren.blend_solid_hspan(x, y, sl.spans[span_pl].len, rsas.color, sl.covers, sl.spans[span_pl].covers_offset)
    ELSE
      rsas.ren.blend_hline(x, y, x - sl.spans[span_pl].len - 1, rsas.color, sl.covers[sl.spans[span_pl].covers_offset]);
    END;

    DEC(num_spans);

    IF num_spans = 0 THEN
      EXIT;
    END;

    INC(span_pl);
  END;
END render;

END AggRendererScanline.
