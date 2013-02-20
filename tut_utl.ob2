MODULE tut_utl;

IMPORT
  AggBasics,
  rb := AggRenderingBuffer,
  SeqFile,
  RawIO,
  FormStr,
  SYSTEM;

PROCEDURE WritePPM*(filename: ARRAY OF CHAR; renb: rb.rendering_buffer_ptr);
VAR
  f: SeqFile.ChanId;
  res: SeqFile.OpenResults;
  s: ARRAY 15 OF CHAR;
  i, j: AggBasics.int32;
BEGIN
  SeqFile.OpenWrite(f, filename, SeqFile.raw + SeqFile.old, res);
  IF res = SeqFile.fileExists THEN
    SeqFile.Rewrite(f);
  END;
  FormStr.print(s, "P6 %d %d 255 ", renb.byte_width DIV renb.bppabs(), renb.byte_height);
  s[14] := 20X;
  RawIO.Write(f, s);

  i := 0;

  WHILE i < LEN(renb.buf^, 0) DO
    j := 0;
    WHILE j < LEN(renb.buf^, 1) DO
      RawIO.Write(f, renb.buf^[i,j]);
      INC(j);
    END;
    INC(i);
  END;

  SeqFile.Close(f);
END WritePPM;

PROCEDURE Fill*(renb: rb.rendering_buffer_ptr; value: AggBasics.int8u);
BEGIN
  AggBasics.agg_fill(AggBasics.agg_adr(renb.buf^), LEN(renb.buf^, 0) * LEN(renb.buf^, 1), value);
END Fill;

END tut_utl.