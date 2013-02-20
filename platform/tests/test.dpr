{$APPTYPE Console}
PROGRAM test;

USES
 Windows;

VAR
  start, stop, freq: TLARGEINTEGER;
  i: INTEGER;
CONST
  ticks = 1;

BEGIN
  QueryPerformanceFrequency(freq);
  QueryPerformanceCounter(start);
  FOR i := 0 TO ticks-1 DO
  BEGIN
    Sleep(1000);
    Write('.');
  END;
  QueryPerformanceCounter(stop);
  Write((stop - start) * 1000.0 / freq);
END.