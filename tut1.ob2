<* +MAIN *>
MODULE tut1;

IMPORT
  AggBasics, rb := AggRenderingBuffer, utl := tut_utl;

CONST
  frame_width  = 320;
  frame_height = 200;
  frame_bpp    = 3;

VAR
  arb: rb.rendering_buffer;

(* Рисуем прямым доступом в массив *)

PROCEDURE DrawBlackFrame*(renb: rb.rendering_buffer);
VAR
  i, row, pos: AggBasics.int32;
BEGIN
  i := 0;
  WHILE i < renb.byte_height DO
    row := renb.row(i);

    renb.buf^[row, 0] := 0;
    renb.buf^[row, 1] := 0;
    renb.buf^[row, 2] := 0;

    pos := renb.byte_width -1;
    renb.buf^[row, pos] := 0; DEC(pos);
    renb.buf^[row, pos] := 0; DEC(pos);
    renb.buf^[row, pos] := 0;
    INC(i);
  END;

  AggBasics.agg_fill(AggBasics.agg_adr(renb.buf^[renb.row(0), 0]), renb.byte_width, 0);

  AggBasics.agg_fill(AggBasics.agg_adr(renb.buf^[renb.row(renb.byte_height - 1), 0]), renb.byte_width, 0);
END DrawBlackFrame;

BEGIN
  arb.Create(frame_height, frame_width, frame_bpp);
  utl.Fill(arb, 200);
  DrawBlackFrame(arb);
  utl.WritePPM("tut1.ppm", arb);
  arb.Destroy();
END tut1.