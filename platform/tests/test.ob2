<*+MAIN*>
MODULE test;

IMPORT
  win := Windows,
  tio := STextIO,
  lio := SLongIO;

VAR
  start, stop, freq: win.LARGE_INTEGER;
  i: INTEGER;
CONST
  ticks = 1;

BEGIN
  win.QueryPerformanceFrequency(freq);
  win.QueryPerformanceCounter(start);
  FOR i := 0 TO ticks-1 DO
    win.Sleep(1000);
    tio.WriteString(".");
  END;
  win.QueryPerformanceCounter(stop);
  lio.WriteReal((stop.QuadPart - start.QuadPart) * 1000.0 / freq.QuadPart, 10);
END test.