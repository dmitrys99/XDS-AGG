<* +MAIN *>
<* ALIGNMENT="1"*>
MODULE bmp_test;

IMPORT
  awb := AggWin32Bmp,
  STextIO;

VAR
  pm: awb.pixel_map;
BEGIN
  pm.create(320, 200, awb.org_color24, 127);
  IF pm.save("test.bmp") THEN
    STextIO.WriteString("Saved!");
    STextIO.WriteLn();

    IF pm.load("test.bmp") THEN
      STextIO.WriteString("Loaded!");
      STextIO.WriteLn();

      IF pm.save("save.bmp") THEN
        STextIO.WriteString("Saved again!");
        STextIO.WriteLn();
      END;
    END;
  ELSE
    STextIO.WriteString("NOT SAVED");
  END;
  pm.destroy();
END bmp_test.
