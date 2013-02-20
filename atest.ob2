<*+MAIN*>
MODULE atest;

IMPORT
  bas := AggBasics,
  aps := AggPlatformSupport;

CONST
 flip_y = TRUE;

TYPE
  the_application_ptr = POINTER TO the_application;
  the_application = RECORD(aps.platform_support)
  END;

VAR
 app : the_application_ptr;

 r08 : bas.int8;
 r16 : bas.int16;
 r32 : bas.int32;


PROCEDURE (VAR ta: the_application) on_key(x, y: bas.int32; key: bas.int32; flags: SET);
BEGIN
  IF key = aps.key_f1 THEN
    ta.message(" How to play with: Note: F2 key saves current 'screenshot' file in this demo's directory.");
  END;
END on_key;

BEGIN
  NEW(app);

  r08 := bas.shr_int8 (-10, 1);
  r16 := bas.shr_int16(-10, 1);
  r32 := bas.shr_int32(-10, 1);

  app.Construct(aps.pix_format_bgr24, flip_y);
(*  app.setcaption('AGG Example. XXX (F1-Help)');*)

  IF app.init(400, 300, aps.window_resize) THEN
    IF app.run() = 0 THEN END;
  END;

  app.Destruct;

END atest.