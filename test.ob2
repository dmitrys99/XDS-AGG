<*+MAIN*>
<*+O2EXTENSIONS*>
MODULE test;
IMPORT
  Out, SYSTEM;

VAR
  h: LONGINT;
  i: INTEGER;
  p: POINTER TO ARRAY OF SYSTEM.CARD8;
BEGIN

  NEW(p, 100);
  FOR i := 0 TO 99 DO
    p[i] := 0AAH;
  END;

END test.