MODULE AggPlatformSupport;

IMPORT
  bas := AggBasics,
  ctl := AggCtrl,
  win := Windows,
  awb := AggWin32Bmp,
  arb := AggRenderingBuffer,
  acc := AggColorConv,
  ac8 := AggColorConvRGB8,
  ata := AggTransAffine,
  atv := AggTransViewport,

  sys := SYSTEM,
  bit := AggBit,

  fls := FileSys,
  o2s := O2Strings;
(**
  It's not a part of the AGG library, it's just a helper class to create
  interactive demo examples. Since the examples should not be too complex
  this class is provided to support some very basic interactive graphical
  funtionality, such as putting the rendered image to the window, simple
  keyboard and mouse input, window resizing, setting the window title,
  and catching the "idle" events.

  The most popular platforms are:

  Windows-32 API
  X-Window API
  SDL library (see http://www.libsdl.org/)
  MacOS C/C++ API

  All the system dependent stuff sits in the platform_specific class.
  The platform_support class has just a pointer to it and it's
  the responsibility of the implementation to create/delete it.
  This class being defined in the implementation file can have
  any platform dependent stuff such as HWND, X11 Window and so on.
                                                                          *)
CONST
(**
  ----------------------------------------------------------window_flag_e
  These are flags used in method init(). Not all of them are
  applicable on different platforms, for example the win32_api
  cannot use a hardware buffer (window_hw_buffer).
  The implementation should simply ignore unsupported flags. *)

  window_resize            * = {0};
  window_hw_buffer         * = {1};
  window_keep_aspect_ratio * = {2};
  window_process_all_keys  * = {3};

CONST
(**
  -----------------------------------------------------------pix_format_e
  Possible formats of the rendering buffer. Initially I thought that it's
  reasonable to create the buffer and the rendering functions in
  accordance with the native pixel format of the system because it
  would have no overhead for pixel format conersion.
  But eventually I came to a conclusion that having a possibility to
  convert pixel formats on demand is a good idea. First, it was X11 where
  there lots of different formats and visuals and it would be great to
  render everything in, say, RGB-24 and display it automatically without
  any additional efforts. The second reason is to have a possibility to
  debug renderers for different pixel formats and colorspaces having only
  one computer and one system.

  This stuff is not included into the basic AGG functionality because the
  number of supported pixel formats (and/or colorspaces) can be great and
  if one needs to add new format it would be good only to add new
  rendering files without having to modify any existing ones (a general
  principle of incapsulation and isolation).

  Using a particular pixel format doesn't obligatory mean the necessity
  of software conversion. For example, win32 API can natively display
  gray8, 15-bit RGB, 24-bit BGR, and 32-bit BGRA formats.
  This list can be (and will be!) extended in future. *)

  pix_format_undefined * = 00; (* By default. No conversions are applied *)
  pix_format_bw        * = 01; (* 1 bit per color B/W *)
  pix_format_gray8     * = 02; (* Simple 256 level grayscale *)
  pix_format_gray16    * = 03; (* Simple 65535 level grayscale *)
  pix_format_rgb555    * = 04; (* 15 bit rgb. Depends on the byte ordering! *)
  pix_format_rgb565    * = 05; (* 16 bit rgb. Depends on the byte ordering! *)
  pix_format_rgbAAA    * = 06; (* 30 bit rgb. Depends on the byte ordering! *)
  pix_format_rgbBBA    * = 07; (* 32 bit rgb. Depends on the byte ordering! *)
  pix_format_bgrAAA    * = 08; (* 30 bit bgr. Depends on the byte ordering! *)
  pix_format_bgrABB    * = 09; (* 32 bit bgr. Depends on the byte ordering! *)
  pix_format_rgb24     * = 10; (* R-G-B, one byte per color component *)
  pix_format_bgr24     * = 11; (* B-G-R, native win32 BMP format. *)
  pix_format_rgba32    * = 12; (* R-G-B-A, one byte per color component *)
  pix_format_argb32    * = 13; (* A-R-G-B, native MAC format *)
  pix_format_abgr32    * = 14; (* A-B-G-R, one byte per color component *)
  pix_format_bgra32    * = 15; (* B-G-R-A, native win32 BMP format *)
  pix_format_rgb48     * = 16; (* R-G-B, 16 bits per color component *)
  pix_format_bgr48     * = 17; (* B-G-R, native win32 BMP format. *)
  pix_format_rgba64    * = 18; (* R-G-B-A, 16 bits byte per color component *)
  pix_format_argb64    * = 19; (* A-R-G-B, native MAC format *)
  pix_format_abgr64    * = 20; (* A-B-G-R, one byte per color component *)
  pix_format_bgra64    * = 21; (* B-G-R-A, native win32 BMP format *)

(**
  -------------------------------------------------------------input_flag_e
  Mouse and keyboard flags. They can be different on different platforms
  and the ways they are obtained are also different. But in any case
  the system dependent flags should be mapped into these ones. The meaning
  of that is as follows. For example, if kbd_ctrl is set it means that the
  ctrl key is pressed and being held at the moment. They are also used in
  the overridden methods such as on_mouse_move(), on_mouse_button_down(),
  on_mouse_button_dbl_click(), on_mouse_button_up(), on_key().
  In the method on_mouse_button_up() the mouse flags have different
  meaning. They mean that the respective button is being released, but
  the meaning of the keyboard flags remains the same.
  There's absolut minimal set of flags is used because they'll be most
  probably supported on different platforms. Even the mouse_right flag
  is restricted because Mac's mice have only one button, but AFAIK
  it can be simulated with holding a special key on the keydoard. *)

  mouse_left  * = {0};
  mouse_right * = {1};
  kbd_shift   * = {2};
  kbd_ctrl    * = {3};

(**
  --------------------------------------------------------------key_code_e
  Keyboard codes. There's also a restricted set of codes that are most
  probably supported on different platforms. Any platform dependent codes
  should be converted into these ones. There're only those codes are
  defined that cannot be represented as printable ASCII-characters.
  All printable ASCII-set can be used in a regilar C/C++ manner:
  ' ', 'A', '0' '+' and so on.
  Since the clasas is used for creating very simple demo-applications
  we don't need very rich possibilities here, just basic ones.
  Actually the numeric key codes are taken from the SDL library, so,
  the implementation of the SDL support does not require any mapping.
  ASCII set. Should be supported everywhere *)
 key_backspace      * = 8;
 key_tab            * = 9;
 key_clear          * = 12;
 key_return         * = 13;
 key_pause          * = 19;
 key_escape         * = 27;

(** Keypad *)
 key_delete         * = 127;
 key_kp0            * = 256;
 key_kp1            * = 257;
 key_kp2            * = 258;
 key_kp3            * = 259;
 key_kp4            * = 260;
 key_kp5            * = 261;
 key_kp6            * = 262;
 key_kp7            * = 263;
 key_kp8            * = 264;
 key_kp9            * = 265;
 key_kp_period      * = 266;
 key_kp_divide      * = 267;
 key_kp_multiply    * = 268;
 key_kp_minus       * = 269;
 key_kp_plus        * = 270;
 key_kp_enter       * = 271;
 key_kp_equals      * = 272;

(** Arrow-keys and stuff *)
 key_up             * = 273;
 key_down           * = 274;
 key_right          * = 275;
 key_left           * = 276;
 key_insert         * = 277;
 key_home           * = 278;
 key_end            * = 279;
 key_page_up        * = 280;
 key_page_down      * = 281;

(**
  Functional keys. You'd better avoid using
  f11...f15 in your applications if you want
  the applications to be portable *)

 key_f1             * = 282;
 key_f2             * = 283;
 key_f3             * = 284;
 key_f4             * = 285;
 key_f5             * = 286;
 key_f6             * = 287;
 key_f7             * = 288;
 key_f8             * = 289;
 key_f9             * = 290;
 key_f10            * = 291;
 key_f11            * = 292;
 key_f12            * = 293;
 key_f13            * = 294;
 key_f14            * = 295;
 key_f15            * = 296;

(**
  The possibility of using these keys is
  very restricted. Actually it's guaranteed
  only in win32_api and win32_sdl implementations *)

 key_numlock        * = 300;
 key_capslock       * = 301;
 key_scrollock      * = 302;

 max_ctrl * = 64;

TYPE
  crtl_container_ptr * = POINTER TO ctrl_container;
  ctrl_container     * = RECORD
    ctrl: ARRAY max_ctrl OF ctl.ctrl_ptr;
    num_ctrl: bas.int32;
    cur_ctrl: bas.int32;
  END;

(*
  ---------------------------------------------------------platform_support
  This class is a base one to the apllication classes. It can be used
  as follows:

   the_application = object(platform_support )

       constructor Construct(bpp : unsigned; flip_y : boolean );
       . . .

       //override stuff . . .
       procedure on_init; virtual;
       procedure on_draw; virtual;
       procedure on_resize(sx ,sy : int ); virtual;
       // . . . and so on, see virtual functions

       //any your own stuff . . .
   };

   VAR
    app : the_application;

   BEGIN
    app.Construct(pix_format_rgb24 ,TRUE );
    app.caption  ("AGG Example. Lion" );

    if app.init(500 ,400 ,window_resize ) then
     app.run;

    app.Destruct;

   END.
*)
CONST
  max_images * = 16;

  AGGAppClass = 'AGGAppClass';

TYPE
  platform_specific_ptr* = POINTER TO platform_specific;
  platform_specific* = RECORD
    format,
    sys_format: bas.int32;

    flip_y: BOOLEAN;

    bpp,
    sys_bpp: bas.int32;
    hwnd-: win.HWND;

    pmap_window: awb.pixel_map;
    pmap_img: ARRAY max_images OF awb.pixel_map;

    keymap: ARRAY 256 OF bas.int32;

    last_translated_key: bas.int32;

    cur_x,
    cur_y: bas.int32;

    input_flags: SET;
    redraw_flag*: BOOLEAN;
    current_dc: win.HDC;
(*    sw_freq: win.LARGE_INTEGER;
      sw_start: win.LARGE_INTEGER; *)
  END;


  platform_support_ptr* = POINTER TO platform_support;
  platform_support* = RECORD
    specific-     : platform_specific_ptr;
    ctrls         : ctrl_container;

    format-       : bas.int32;
    bpp-          : bas.int32;

    (**
    So, finally, how to draw anythig with AGG? Very simple.
    rbuf_window() returns a reference to the main rendering
    buffer which can be attached to any rendering class.
    rbuf_img() returns a reference to the previously created
    or loaded image buffer (see load_img()). The image buffers
    are not displayed directly, they should be copied to or
    combined somehow with the rbuf_window(). rbuf_window() is
    the only buffer that can be actually displayed.
    *)
    rbuf_window-  : arb.rendering_buffer_ptr;
    rbuf_img-     : ARRAY max_images OF arb.rendering_buffer_ptr;

    window_flags  : SET;
    (**
    The following provides a very simple mechanism of doing someting
    in background. It's not multitheading. When whait_mode is true
    the class waits for the events and it does not ever call on_idle().
    When it's FALSE it calls on_idle() when the event queue is empty.
    The mode can be changed anytime. This mechanism is satisfactory
    for creation very simple animations.
    *)
    wait_mode*    ,
    flip_y-       : BOOLEAN;        (* flip_y - true if you want to have the Y-axis flipped vertically *)
    caption-      : ARRAY 256 OF CHAR;
    resize_mtx    : ata.trans_affine_ptr;

    initial_width*,
    initial_height*: bas.int32;
  END;

VAR
(*
  Hmmm, I had to rip the fields below out of the platform_specific object,
  because being them the part of that object/class, the corresponding
  Windows API calls QueryPerformanceXXX are working NOT!.
  Anyway, since we use usually only one instance of platform_specific in
  our agg demos, it's okay to do that this way. See {hack}.
*)
  sw_freq,
  sw_start: win.LARGE_INTEGER;

PROCEDURE (VAR cc: ctrl_container) Construct*();
BEGIN
  cc.num_ctrl :=  0;
  cc.cur_ctrl := -1;
END Construct;

PROCEDURE (VAR cc: ctrl_container) Destruct*();
BEGIN
END Destruct;

PROCEDURE (VAR cc: ctrl_container) add*(c: ctl.ctrl_ptr);
BEGIN
  IF cc.num_ctrl < max_ctrl THEN
    cc.ctrl[cc.num_ctrl] := c;
    INC(cc.num_ctrl);
  END;
END add;

PROCEDURE (VAR cc: ctrl_container) in_rect*(x, y: bas.double): BOOLEAN;
VAR
  i: bas.int32;
BEGIN
  i := 0;
  WHILE i < cc.num_ctrl DO
    IF cc.ctrl[i].in_rect(x, y) THEN
      RETURN TRUE;
    END;
    INC(i);
  END;
  RETURN FALSE;
END in_rect;

PROCEDURE (VAR cc: ctrl_container) on_mouse_button_down*(x, y: bas.double): BOOLEAN;
VAR
  i: bas.int32;
BEGIN
  i := 0;
  WHILE i < cc.num_ctrl DO
    IF cc.ctrl[i].on_mouse_button_down(x, y) THEN
      RETURN TRUE;
    END;
    INC(i);
  END;
  RETURN FALSE;
END on_mouse_button_down;

PROCEDURE (VAR cc: ctrl_container) on_mouse_button_up*(x, y: bas.double): BOOLEAN;
VAR
  i: bas.int32;
BEGIN
  i := 0;
  WHILE i < cc.num_ctrl DO
    IF cc.ctrl[i].on_mouse_button_up(x, y) THEN
      RETURN TRUE;
    END;
    INC(i);
  END;
  RETURN FALSE;
END on_mouse_button_up;

PROCEDURE (VAR cc: ctrl_container) on_mouse_move*(x, y: bas.double; button_flag: BOOLEAN): BOOLEAN;
VAR
  i: bas.int32;
BEGIN
  i := 0;
  WHILE i < cc.num_ctrl DO
    IF cc.ctrl[i].on_mouse_move(x, y, button_flag) THEN
      RETURN TRUE;
    END;
    INC(i);
  END;
  RETURN FALSE;
END on_mouse_move;

PROCEDURE (VAR cc: ctrl_container) on_arrow_keys*(left, right, down, up: BOOLEAN): BOOLEAN;
BEGIN
  IF cc.cur_ctrl >= 0 THEN
    RETURN cc.ctrl[cc.cur_ctrl].on_arrow_keys(left, right, down, up);
  ELSE
    RETURN FALSE;
  END;
END on_arrow_keys;

PROCEDURE (VAR cc: ctrl_container) set_cur*(x, y: bas.double): BOOLEAN;
VAR
  i: bas.int32;

BEGIN
  i := 0;

  WHILE i < cc.num_ctrl DO
    IF cc.ctrl[i].in_rect(x, y) THEN
      IF cc.cur_ctrl # i THEN
        cc.cur_ctrl := i;
        RETURN TRUE;
      END;
    END;
    INC(i);
  END;

  IF cc.cur_ctrl # -1 THEN
    cc.cur_ctrl := -1;
    RETURN TRUE;
  END;

  RETURN FALSE;
END set_cur;

PROCEDURE (VAR ps: platform_specific) Construct*(format: bas.int32; flip_y: BOOLEAN);
VAR
  i: bas.int32;
BEGIN
  ps.pmap_window.Construct();

  FOR i := 0 TO max_images - 1 DO
    ps.pmap_img[i].Construct();
  END;

  ps.format     := format;
  ps.sys_format := pix_format_undefined;

  ps.flip_y  := flip_y;
  ps.bpp     := 0;
  ps.sys_bpp := 0;
  ps.hwnd    := NIL;

  ps.last_translated_key := 0;

  ps.cur_x := 0;
  ps.cur_y := 0;

  ps.input_flags := {};
  ps.redraw_flag := TRUE;
  ps.current_dc  := NIL;

  FOR i := 0 TO 255 DO
    ps.keymap[0] := 0;
  END;

  ps.keymap[win.VK_PAUSE] := key_pause;
  ps.keymap[win.VK_CLEAR] := key_clear;

  ps.keymap[win.VK_NUMPAD0]  := key_kp0;
  ps.keymap[win.VK_NUMPAD1]  := key_kp1;
  ps.keymap[win.VK_NUMPAD2]  := key_kp2;
  ps.keymap[win.VK_NUMPAD3]  := key_kp3;
  ps.keymap[win.VK_NUMPAD4]  := key_kp4;
  ps.keymap[win.VK_NUMPAD5]  := key_kp5;
  ps.keymap[win.VK_NUMPAD6]  := key_kp6;
  ps.keymap[win.VK_NUMPAD7]  := key_kp7;
  ps.keymap[win.VK_NUMPAD8]  := key_kp8;
  ps.keymap[win.VK_NUMPAD9]  := key_kp9;
  ps.keymap[win.VK_DECIMAL]  := key_kp_period;
  ps.keymap[win.VK_DIVIDE]   := key_kp_divide;
  ps.keymap[win.VK_MULTIPLY] := key_kp_multiply;
  ps.keymap[win.VK_SUBTRACT] := key_kp_minus;
  ps.keymap[win.VK_ADD]      := key_kp_plus;

  ps.keymap[win.VK_UP]     := key_up;
  ps.keymap[win.VK_DOWN]   := key_down;
  ps.keymap[win.VK_RIGHT]  := key_right;
  ps.keymap[win.VK_LEFT]   := key_left;
  ps.keymap[win.VK_INSERT] := key_insert;
  ps.keymap[win.VK_DELETE] := key_delete;
  ps.keymap[win.VK_HOME]   := key_home;
  ps.keymap[win.VK_END]    := key_end;
  ps.keymap[win.VK_PRIOR]  := key_page_up;
  ps.keymap[win.VK_NEXT]   := key_page_down;

  ps.keymap[win.VK_F1]  := key_f1;
  ps.keymap[win.VK_F2]  := key_f2;
  ps.keymap[win.VK_F3]  := key_f3;
  ps.keymap[win.VK_F4]  := key_f4;
  ps.keymap[win.VK_F5]  := key_f5;
  ps.keymap[win.VK_F6]  := key_f6;
  ps.keymap[win.VK_F7]  := key_f7;
  ps.keymap[win.VK_F8]  := key_f8;
  ps.keymap[win.VK_F9]  := key_f9;
  ps.keymap[win.VK_F10] := key_f10;
  ps.keymap[win.VK_F11] := key_f11;
  ps.keymap[win.VK_F12] := key_f12;
  ps.keymap[win.VK_F13] := key_f13;
  ps.keymap[win.VK_F14] := key_f14;
  ps.keymap[win.VK_F15] := key_f15;

  ps.keymap[win.VK_NUMLOCK] := key_numlock;
  ps.keymap[win.VK_CAPITAL] := key_capslock;
  ps.keymap[win.VK_SCROLL]  := key_scrollock;

  CASE ps.format OF
    pix_format_bw:
      ps.sys_format := pix_format_bw;
      ps.bpp        := 1;
      ps.sys_bpp    := 1;

  | pix_format_gray8:
      ps.sys_format := pix_format_bgr24; (*//pix_format_gray8;{hack} *)
      ps.bpp        := 8;
      ps.sys_bpp    := 24; (*8;*)

  | pix_format_gray16:
      ps.sys_format := pix_format_gray8;
      ps.bpp        := 16;
      ps.sys_bpp    := 8;

  | pix_format_rgb565, pix_format_rgb555:
      ps.sys_format := pix_format_rgb555;
      ps.bpp        := 16;
      ps.sys_bpp    := 16;

  | pix_format_rgbAAA,
    pix_format_bgrAAA,
    pix_format_rgbBBA,
    pix_format_bgrABB:
      ps.sys_format := pix_format_bgr24;
      ps.bpp        := 32;
      ps.sys_bpp    := 24;

  | pix_format_rgb24, pix_format_bgr24:
      ps.sys_format := pix_format_bgr24;
      ps.bpp        := 24;
      ps.sys_bpp    := 24;

  | pix_format_rgb48, pix_format_bgr48:
      ps.sys_format := pix_format_bgr24;
      ps.bpp        := 48;
      ps.sys_bpp    := 24;

  | pix_format_bgra32,
    pix_format_abgr32,
    pix_format_argb32,
    pix_format_rgba32:
      ps.sys_format := pix_format_bgra32;
      ps.bpp        := 32;
      ps.sys_bpp    := 32;

  | pix_format_bgra64,
    pix_format_abgr64,
    pix_format_argb64,
    pix_format_rgba64:
      ps.sys_format := pix_format_bgra32;
      ps.bpp        := 64;
      ps.sys_bpp    := 32;
  END;

  win.QueryPerformanceFrequency(sw_freq);(*{hack}*)
  win.QueryPerformanceCounter  (sw_start);
END Construct;

PROCEDURE (VAR ps: platform_specific) Destruct*();
VAR
  i: bas.int32;
BEGIN
  ps.pmap_window.Destruct();

  FOR i := 0 TO max_images - 1 DO
    ps.pmap_img[i].Destruct();
  END;
END Destruct;

PROCEDURE (VAR ps: platform_specific) create_pmap*(width, height: bas.int32; wnd: arb.rendering_buffer_ptr);
BEGIN
  ps.pmap_window.create(width, height, ps.bpp, 255);

  IF ps.flip_y THEN
    wnd.attach(ps.pmap_window.bmp.buf, ps.pmap_window.Bpp() DIV 8)
  ELSE
    wnd.attach(ps.pmap_window.bmp.buf, -ps.pmap_window.Bpp() DIV 8);
  END;
END create_pmap;

PROCEDURE convert_pmap(VAR dst, src: arb.rendering_buffer_ptr; format: bas.int32);
BEGIN
  CASE format OF
(*    pix_format_gray8  : acc.color_conv(dst, src, ac8.color_conv_gray8_to_bgr24);
    | pix_format_gray16 : acc.color_conv(dst, src, ac8.color_conv_gray16_to_gray8);
    | pix_format_rgb565 : acc.color_conv(dst, src, ac8.color_conv_rgb565_to_rgb555);
    | pix_format_rgbAAA : acc.color_conv(dst, src, ac8.color_conv_rgbAAA_to_bgr24);
    | pix_format_bgrAAA : acc.color_conv(dst, src, ac8.color_conv_bgrAAA_to_bgr24);
    | pix_format_rgbBBA : acc.color_conv(dst, src, ac8.color_conv_rgbBBA_to_bgr24);
    | pix_format_bgrABB : acc.color_conv(dst, src, ac8.color_conv_bgrABB_to_bgr24);*)
      pix_format_rgb24  : acc.color_conv(dst, src, ac8.color_conv_rgb24_to_bgr24);
(*  | pix_format_rgb48  : acc.color_conv(dst, src, ac8.color_conv_rgb48_to_bgr24);
    | pix_format_bgr48  : acc.color_conv(dst, src, ac8.color_conv_bgr48_to_bgr24);
    | pix_format_abgr32 : acc.color_conv(dst, src, ac8.color_conv_abgr32_to_bgra32);
    | pix_format_argb32 : acc.color_conv(dst, src, ac8.color_conv_argb32_to_bgra32);
    | pix_format_rgba32 : acc.color_conv(dst, src, ac8.color_conv_rgba32_to_bgra32);
    | pix_format_bgra64 : acc.color_conv(dst, src, ac8.color_conv_bgra64_to_bgra32);
    | pix_format_abgr64 : acc.color_conv(dst, src, ac8.color_conv_abgr64_to_bgra32);
    | pix_format_argb64 : acc.color_conv(dst, src, ac8.color_conv_argb64_to_bgra32);
    | pix_format_rgba64 : acc.color_conv(dst, src, ac8.color_conv_rgba64_to_bgra32);*)
  END;

END convert_pmap;



PROCEDURE (VAR ps: platform_specific) display_pmap*(dc: win.HDC; src: arb.rendering_buffer_ptr);
VAR
  pmap_tmp: awb.pixel_map;
  rbuf_tmp: arb.rendering_buffer_ptr;
BEGIN
  IF ps.sys_format = ps.format THEN
    ps.pmap_window.draw(dc, NIL, NIL)
  ELSE
    pmap_tmp.Construct;
    pmap_tmp.create(ps.pmap_window.width(), ps.pmap_window.height(), ps.sys_bpp, 255);

    NEW(rbuf_tmp);
(*    rbuf_tmp.Construct();*)

    IF ps.flip_y THEN
      rbuf_tmp.attach(pmap_tmp.bmp.buf,  pmap_tmp.Bpp())
    ELSE
      rbuf_tmp.attach(pmap_tmp.bmp.buf, -pmap_tmp.Bpp());
    END;

    convert_pmap (rbuf_tmp, src, ps.format);
    pmap_tmp.draw(dc, NIL, NIL);

    rbuf_tmp.Destruct();
    pmap_tmp.Destruct();
  END;
END display_pmap;



PROCEDURE (VAR ps: platform_specific) load_pmap*(fn: ARRAY OF CHAR; idx: bas.int32; dst: arb.rendering_buffer_ptr ): BOOLEAN;
BEGIN
  RETURN FALSE;
END load_pmap;

PROCEDURE (VAR ps: platform_specific) save_pmap*(fn: ARRAY OF CHAR; idx: bas.int32; src: arb.rendering_buffer_ptr ): BOOLEAN;
VAR
  pmap_tmp: awb.pixel_map;
  rbuf_tmp: arb.rendering_buffer_ptr;
  r: BOOLEAN;
BEGIN
  IF ps.sys_format = ps.format THEN
    RETURN ps.pmap_img[idx].save(fn);
  END;

  pmap_tmp.Construct();
  pmap_tmp.create(ps.pmap_img[idx].width(), ps.pmap_img[idx].height(), ps.sys_bpp, 255);

  NEW(rbuf_tmp);
  rbuf_tmp.Construct();

  IF ps.flip_y THEN
    rbuf_tmp.attach(pmap_tmp.bmp.buf, pmap_tmp.Bpp())
  ELSE
    rbuf_tmp.attach(pmap_tmp.bmp.buf, pmap_tmp.Bpp());
  END;

  convert_pmap(rbuf_tmp, src, ps.format);

  r := pmap_tmp.save(fn);

  rbuf_tmp.Destruct();
  pmap_tmp.Destruct();

  RETURN r;
END save_pmap;

PROCEDURE (VAR ps: platform_specific) translate*(keycode: bas.int32): bas.int32;
BEGIN
  IF keycode > 255 THEN
    ps.last_translated_key := 0
  ELSE
    ps.last_translated_key := ps.keymap[keycode];
  END;
  RETURN ps.last_translated_key;
END translate;


PROCEDURE (VAR psu: platform_support) Construct*(format: bas.int32; flip_y: BOOLEAN);
VAR
  i: bas.int32;
BEGIN
  NEW(psu.specific);
  psu.specific.Construct(format, flip_y);

  psu.ctrls.Construct();

  NEW(psu.rbuf_window);
  psu.rbuf_window.Construct();

  FOR i := 0 TO max_images - 1 DO
    NEW(psu.rbuf_img[i]);
    psu.rbuf_img[i].Construct();
  END;

  NEW(psu.resize_mtx);
  psu.resize_mtx.Construct();

  psu.format := format;

  psu.bpp := psu.specific.bpp;

  psu.window_flags := {};
  psu.wait_mode    := TRUE;
  psu.flip_y       := flip_y;

  psu.initial_width  := 10;
  psu.initial_height := 10;

  psu.caption := 'Anti-Grain Geometry Application';
END Construct;

PROCEDURE (VAR psu: platform_support) Destruct*();
VAR
  i : bas.int32;
BEGIN
  psu.specific.Destruct();
  psu.specific := NIL;

  psu.ctrls.Destruct();
  psu.rbuf_window.Destruct();

  FOR i := 0 TO max_images - 1 DO
    psu.rbuf_img[i].Destruct();
  END;
END Destruct;

(** Setting the windows caption (title). Should be able
    to be called at least before calling init().
    It's perfect if they can be called anytime. *)
PROCEDURE (VAR psu: platform_support) setcaption*(cap: ARRAY OF CHAR);
BEGIN
  COPY(cap, psu.caption);

  IF psu.specific.hwnd # NIL THEN
    win.SetWindowText(psu.specific.hwnd, psu.caption);
  END;
END setcaption;

PROCEDURE (VAR psu: platform_support) on_init*();
BEGIN
END on_init;

PROCEDURE (VAR psu: platform_support) on_resize*(sx, sy: bas.int32);
BEGIN

END on_resize;

PROCEDURE (VAR psu: platform_support) on_idle*();
BEGIN

END on_idle;

PROCEDURE (VAR psu: platform_support) on_ctrl_change*();
BEGIN

END on_ctrl_change;

PROCEDURE (VAR psu: platform_support) on_mouse_move*(x, y: bas.int32; flags: SET);
BEGIN

END on_mouse_move;

PROCEDURE (VAR psu: platform_support) on_mouse_button_down*(x, y: bas.int32; flags: SET);
BEGIN

END on_mouse_button_down;

PROCEDURE (VAR psu: platform_support) on_mouse_button_up  *(x, y: bas.int32; flags: SET);
BEGIN

END on_mouse_button_up  ;

PROCEDURE (VAR psu: platform_support) on_key*(x, y: bas.int32; key: bas.int32; flags: SET);
BEGIN

END on_key;

PROCEDURE (VAR psu: platform_support) on_draw*();
BEGIN

END on_draw;

PROCEDURE (VAR psu: platform_support) on_post_draw*(raw_handler: win.HDC);
BEGIN

END on_post_draw;



(**
These two functions control updating of the window.
force_redraw() is an analog of the Win32 InvalidateRect() function.
Being called it sets a flag (or sends a message) which results
in calling on_draw() and updating the content of the window
when the next event cycle comes.
update_window() results in just putting immediately the content
of the currently rendered buffer to the window without calling
on_draw().
*)
PROCEDURE (VAR psu: platform_support) force_redraw*();
BEGIN
  psu.specific.redraw_flag := TRUE;
  win.InvalidateRect(psu.specific.hwnd, NIL, FALSE);
END force_redraw;

PROCEDURE (VAR psu: platform_support) update_window*();
VAR
  dc: win.HDC;
BEGIN
  dc := win.GetDC(psu.specific.hwnd);
  psu.specific.display_pmap(dc, psu.rbuf_window);
  win.ReleaseDC(psu.specific.hwnd, dc);
END update_window;


(**
Returns file extension used in the implemenation for the particular
system.
*)
PROCEDURE (VAR psu: platform_support) img_ext*(VAR S: ARRAY OF CHAR);
BEGIN
  o2s.Append('.bmp', S);
END img_ext;


PROCEDURE (VAR psu: platform_support) copy_img_to_window*(idx: bas.int32);
BEGIN

END copy_img_to_window;

PROCEDURE (VAR psu: platform_support) copy_window_to_img*(idx: bas.int32);
BEGIN

END copy_window_to_img;

PROCEDURE (VAR psu: platform_support) copy_img_to_img*(idx_to, idx_from: bas.int32);
BEGIN

END copy_img_to_img;


<*+WOFF301*>
(**
These 3 menthods handle working with images. The image
formats are the simplest ones, such as .BMP in Windows or
.ppm in Linux. In the applications the names of the files
should not have any file extensions. Method load_img() can
be called before init(), so, the application could be able
to determine the initial size of the window depending on
the size of the loaded image.
The argument "idx" is the number of the image 0...max_images-1
*)
PROCEDURE (VAR psu: platform_support) load_img  *(idx: bas.int32; file: ARRAY OF CHAR): BOOLEAN;
BEGIN
  IF idx < max_images THEN
    psu.img_ext(file);


    IF ~(fls.Exists(file)) THEN
      o2s.Insert('bmp\', 0, file);
    END;

    RETURN psu.specific.load_pmap(file, idx, psu.rbuf_img[idx]);
  ELSE
    RETURN TRUE;
  END;
END load_img;

PROCEDURE (VAR psu: platform_support) save_img  *(idx: bas.int32; file: ARRAY OF CHAR): BOOLEAN;
BEGIN
  IF idx < max_images THEN
    RETURN psu.specific.save_pmap(file, idx, psu.rbuf_img[idx])
  ELSE
    RETURN TRUE;
  END
END save_img  ;

PROCEDURE (VAR psu: platform_support) create_img*(idx: bas.int32; width: bas.int32; height: bas.int32): BOOLEAN;
BEGIN
  RETURN FALSE;
END create_img;
<*-WOFF301*>



PROCEDURE (VAR psu: platform_support) quit*();
BEGIN
  IF psu.specific.hwnd # NIL THEN
    win.DestroyWindow(psu.specific.hwnd);
  END;
  win.PostQuitMessage(0);
END quit;


(**
Auxiliary functions. trans_affine_resizing() modifier sets up the resizing
matrix on the basis of the given width and height and the initial
width and height of the window. The implementation should simply
call this function every time when it catches the resizing event
passing in the new values of width and height of the window.
Nothing prevents you from "cheating" the scaling matrix if you
call this function from somewhere with wrong arguments.
trans_affine_resizing() accessor simply returns current resizing matrix
which can be used to apply additional scaling of any of your
stuff when the window is being resized.
width(), height(), initial_width(), and initial_height() must be
clear to understand with no comments :-)
*)
PROCEDURE (VAR psu: platform_support) set_trans_affine_resizing*(width, height: bas.int32);
VAR
  vp: atv.trans_viewport_ptr;
  ts: ata.trans_affine_scaling_ptr;
BEGIN
  NEW(vp);
  NEW(ts);
  IF psu.window_flags * window_keep_aspect_ratio # {} THEN
    vp.Construct;
    vp.preserve_aspect_ratio(0.5, 0.5, atv.aspect_ratio_meet);

    vp.device_viewport(0, 0, width, height);
    vp.world_viewport (0, 0, psu.initial_width, psu.initial_height);

    vp.to_affine(psu.resize_mtx);
  ELSE
    ts.ConstructS(width / psu.initial_width, height / psu.initial_height);
    psu.resize_mtx.assign(ts);
  END;
END set_trans_affine_resizing;

PROCEDURE (VAR psu: platform_support) trans_affine_resizing*(): ata.trans_affine_ptr;
BEGIN
  RETURN psu.resize_mtx;
END trans_affine_resizing;


PROCEDURE get_key_flags(wflags : bas.int32) : SET;
VAR
  flags: SET;
  mk: win.MK_SET;
BEGIN
  flags := {};
  mk := sys.VAL(win.MK_SET, wflags);

  IF mk * win.MK_LBUTTON # win.MK_SET{} THEN
    flags := mouse_left;
  END;

  IF mk * win.MK_RBUTTON # win.MK_SET{} THEN
    flags := flags + mouse_right;
  END;

  IF mk * win.MK_SHIFT # win.MK_SET{} THEN
    flags := flags + kbd_shift;
  END;

  IF mk * win.MK_CONTROL # win.MK_SET{} THEN
    flags := flags + kbd_ctrl;
  END;

  RETURN flags;
END get_key_flags;


PROCEDURE [win.CALLBACK] window_proc(Wnd: win.HWND; Msg: win.UINT; WPar: win.WPARAM; LPar: win.LPARAM): win.LRESULT;
VAR
 ps  : win.PAINTSTRUCT;
 app : platform_support_ptr;
 ret : win.LRESULT;
 w,h : win.WORD;
 dc, paintDC: win.HDC;

 left, up, right, down: BOOLEAN;

BEGIN
  left  := FALSE;
  up    := FALSE;
  right := FALSE;
  down  := FALSE;
  app := sys.VAL(platform_support_ptr, win.GetWindowLong(Wnd, win.GWL_USERDATA));

  IF app = NIL THEN
    IF Msg = win.WM_DESTROY THEN
      win.PostQuitMessage(0);
      RETURN 0;
    END;
    RETURN win.DefWindowProc(Wnd, Msg, WPar, LPar);
  END;

  dc := win.GetDC(app.specific.hwnd);

  app.specific.current_dc := dc;

  ret := 0;

  CASE Msg OF
  win.WM_CREATE :
    bas.NoP();

  | win.WM_SIZE :
    w := win.LOWORD(LPar);
    h := win.HIWORD(LPar);
    app.specific.create_pmap(win.LOWORD(LPar), win.HIWORD(LPar), app.rbuf_window);
    app.set_trans_affine_resizing(w, h);
    app.on_resize(win.LOWORD(LPar), win.HIWORD(LPar));
    app.force_redraw();

  | win.WM_ERASEBKGND :
    bas.NoP();

  | win.WM_LBUTTONDOWN :
    win.SetCapture(app.specific.hwnd);
    app.specific.cur_x := win.LOWORD(LPar);

    IF app.flip_y THEN
      app.specific.cur_y := app.rbuf_window.byte_height - win.HIWORD(LPar)
    ELSE
      app.specific.cur_y := win.HIWORD(LPar);
    END;

    app.specific.input_flags := mouse_left + get_key_flags(WPar);

    IF app.ctrls.set_cur(app.specific.cur_x, app.specific.cur_y) THEN
    END;

    IF app.ctrls.on_mouse_button_down(app.specific.cur_x, app.specific.cur_y) THEN
       app.on_ctrl_change();
       app.force_redraw();
    ELSE
     IF app.ctrls.in_rect(app.specific.cur_x, app.specific.cur_y) THEN
      IF app.ctrls.set_cur(app.specific.cur_x, app.specific.cur_y) THEN
        app.on_ctrl_change;
        app.force_redraw;
      END;
     ELSE
      app.on_mouse_button_down(
       app.specific.cur_x,
       app.specific.cur_y,
       app.specific.input_flags);
     END;
    END;

  | win.WM_LBUTTONUP :

    win.ReleaseCapture;

    app.specific.cur_x := win.LOWORD(LPar);

    IF app.flip_y THEN
      app.specific.cur_y := app.rbuf_window.byte_height - win.HIWORD(LPar);
    ELSE
      app.specific.cur_y := win.HIWORD(LPar);
    END;

    app.specific.input_flags := mouse_left + get_key_flags(WPar);

    IF app.ctrls.on_mouse_button_up(app.specific.cur_x, app.specific.cur_y) THEN
      app.on_ctrl_change;
      app.force_redraw;

    END;

    app.on_mouse_button_up(
     app.specific.cur_x,
     app.specific.cur_y,
     app.specific.input_flags);

  | win.WM_RBUTTONDOWN :
    win.SetCapture(app.specific.hwnd);

    app.specific.cur_x := win.LOWORD(LPar);

    IF app.flip_y THEN
      app.specific.cur_y := app.rbuf_window.byte_height - win.HIWORD(LPar);
    ELSE
      app.specific.cur_y := win.HIWORD(LPar);
    END;

    app.specific.input_flags := mouse_right + get_key_flags(WPar);

    app.on_mouse_button_down(
     app.specific.cur_x,
     app.specific.cur_y,
     app.specific.input_flags);

  | win.WM_RBUTTONUP :
    win.ReleaseCapture;

    app.specific.cur_x := win.LOWORD(LPar);

    IF app.flip_y THEN
      app.specific.cur_y := app.rbuf_window.byte_height - win.HIWORD(LPar)
    ELSE
      app.specific.cur_y := win.HIWORD(LPar)
    END;

    app.specific.input_flags := mouse_right + get_key_flags(WPar);

    app.on_mouse_button_up(
     app.specific.cur_x,
     app.specific.cur_y,
     app.specific.input_flags);

  | win.WM_MOUSEMOVE :
    app.specific.cur_x := win.LOWORD(LPar);

    IF app.flip_y THEN
      app.specific.cur_y := app.rbuf_window.byte_height - win.HIWORD(LPar)
    ELSE
      app.specific.cur_y := win.HIWORD(LPar)
    END;

    app.specific.input_flags := get_key_flags(WPar);

    IF app.ctrls.on_mouse_move(
        app.specific.cur_x,
        app.specific.cur_y,
        ((app.specific.input_flags * mouse_left) # {})) THEN

      app.on_ctrl_change;
      app.force_redraw();
    ELSE
      IF ~app.ctrls.in_rect(app.specific.cur_x, app.specific.cur_y) THEN
        app.on_mouse_move(
         app.specific.cur_x,
         app.specific.cur_y,
         app.specific.input_flags);
      END;
    END;

  | win.WM_SYSKEYDOWN, win.WM_KEYDOWN :

    app.specific.last_translated_key := 0;
(*
    IF    WPar = win.VK_CONTROL THEN
      app.specific.input_flags := app.specific.input_flags + kbd_ctrl;

    ELSIF WPar = win.VK_SHIFT THEN
      app.specific.input_flags := app.specific.input_flags + kbd_shift;

    ELSIF (WPar = win.VK_F4) & (bit.and32(LPar, 20000000H) # 0) THEN
      app.quit();
    ELSE
      IF app.specific.translate(WPar) = 0 THEN END;
    END;
*)

    CASE WPar OF
      win.VK_CONTROL:
      app.specific.input_flags := app.specific.input_flags + kbd_ctrl;

    | win.VK_SHIFT:
      app.specific.input_flags := app.specific.input_flags + kbd_shift;

    | win.VK_F4:

      IF bit.and32(LPar, 20000000H) # 0 THEN
        app.quit();
      END;
    ELSE
       IF app.specific.translate(WPar) = 0 THEN
       END;
    END;

    IF app.specific.last_translated_key # 0 THEN
      left  := FALSE;
      up    := FALSE;
      right := FALSE;
      down  := FALSE;

      CASE app.specific.last_translated_key OF
          key_left  : left := TRUE;
        | key_up    : up   := TRUE;
        | key_right : right:= TRUE;
        | key_down  : down := TRUE;
        | key_f2    : app.copy_window_to_img(max_images - 1);
                      IF app.save_img(max_images - 1, 'screenshot.bmp') THEN END;
      ELSE
      END;

    END;

    IF app.window_flags * window_process_all_keys # {} THEN
     app.on_key(
      app.specific.cur_x,
      app.specific.cur_y,
      app.specific.last_translated_key,
      app.specific.input_flags)

    ELSE
     IF app.ctrls.on_arrow_keys(left, right, down, up) THEN
       app.on_ctrl_change();
       app.force_redraw();
     ELSE
      app.on_key(
       app.specific.cur_x,
       app.specific.cur_y,
       app.specific.last_translated_key,
       app.specific.input_flags);
     END;
    END;

  | win.WM_SYSKEYUP, win.WM_KEYUP:

    app.specific.last_translated_key := 0;

    CASE WPar OF
      win.VK_CONTROL :
      app.specific.input_flags := app.specific.input_flags - kbd_ctrl;

     | win.VK_SHIFT :
      app.specific.input_flags := app.specific.input_flags - kbd_shift;
    ELSE
    END;


  | win.WM_CHAR, win.WM_SYSCHAR :
   IF app.specific.last_translated_key = 0 THEN
    app.on_key(
     app.specific.cur_x,
     app.specific.cur_y,
     WPar,
     app.specific.input_flags);
   END;
  | win.WM_PAINT :
     paintDC := win.BeginPaint(Wnd, ps);

    app.specific.current_dc := paintDC;

    IF app.specific.redraw_flag THEN
      app.on_draw();
      app.specific.redraw_flag := FALSE;
    END;

    app.specific.display_pmap(paintDC, app.rbuf_window);
    app.on_post_draw         (paintDC);

    app.specific.current_dc := NIL;

    win.EndPaint(Wnd, ps);


  | win.WM_COMMAND :
    bas.NoP;

  | win.WM_DESTROY :
   win.PostQuitMessage(0);

  ELSE
    ret := win.DefWindowProc(Wnd, Msg, WPar, LPar);

 END;

 app.specific.current_dc := NIL;

 win.ReleaseDC(app.specific.hwnd, dc);

 RETURN ret;

END window_proc;




(**
init() and run(). See description before the class for details.
The necessity of calling init() after creation is that it's
impossible to call the overridden virtual function (on_init())
from the constructor. On the other hand it's very useful to have
some on_init() event handler when the window is created but
not yet displayed. The rbuf_window() method (see below) is
accessible from on_init().
*)
PROCEDURE (VAR psu: platform_support) init*(width, height: bas.int32; flags: SET): BOOLEAN;
VAR
  wc  : win.WNDCLASS;
  rct : win.RECT;

  wStyle: win.WS_SET;
BEGIN
  IF psu.specific.sys_format = pix_format_undefined THEN
    RETURN FALSE
  END;

  psu.window_flags := flags;

  wc.lpszClassName := win.GetPSTR(AGGAppClass);
  wc.lpfnWndProc   := window_proc;
  wc.style         := win.CS_OWNDC + win.CS_VREDRAW + win.CS_HREDRAW;
  wc.hInstance     := win.GetModuleHandle(NIL);
  wc.hIcon         := win.LoadIcon  (NIL, win.IDI_APPLICATION);
  wc.hCursor       := win.LoadCursor(NIL, win.IDC_ARROW);
  wc.hbrBackground := NIL;
  wc.lpszMenuName  := NIL;
  wc.cbClsExtra    := 0;
  wc.cbWndExtra    := 0;

  win.RegisterClass(wc);

  wStyle := win.WS_OVERLAPPED + win.WS_CAPTION + win.WS_SYSMENU + win.WS_MINIMIZEBOX;

  IF psu.window_flags * window_resize # {} THEN
    wStyle := win.WS_OVERLAPPED + win.WS_CAPTION + win.WS_SYSMENU + win.WS_MINIMIZEBOX + win.WS_THICKFRAME + win.WS_MAXIMIZEBOX;
  END;

  psu.specific.hwnd :=
    win.CreateWindow(AGGAppClass, psu.caption, wStyle, 100, 100, width, height, NIL, NIL, win.GetModuleHandle(NIL), NIL);

  IF psu.specific.hwnd = NIL THEN
    RETURN FALSE;
  END;

  win.GetClientRect(psu.specific.hwnd, rct);

  win.MoveWindow(
   psu.specific.hwnd,     (* handle to window    *)
   100,                   (* horizontal position *)
   100,                   (* vertical position   *)
   width + (width - (rct.right - rct.left)),
   height + (height - (rct.bottom - rct.top)),
   FALSE);

  win.SetWindowLong(psu.specific.hwnd, win.GWL_USERDATA, bas.agg_adr(psu));

  psu.specific.create_pmap(width, height, psu.rbuf_window);

  psu.initial_width  := width;
  psu.initial_height := height;

  psu.on_init();

  psu.specific.redraw_flag := TRUE;

  win.ShowWindow(psu.specific.hwnd, win.SW_SHOW);

  RETURN TRUE;
END init;

PROCEDURE (VAR psu: platform_support) run*(): bas.int32;
VAR
  msg: win.MSG;
BEGIN
  LOOP
    IF psu.wait_mode THEN
      IF ~win.GetMessage(msg, NIL, 0, 0) THEN
        EXIT;
      END;
      win.TranslateMessage(msg);
      win.DispatchMessage (msg);
    ELSE
      IF win.PeekMessage(msg, NIL, 0, 0, win.PM_REMOVE) THEN
        win.TranslateMessage(msg);
        IF msg.message = win.WM_QUIT THEN
          EXIT;
        END;
        win.DispatchMessage(msg);
      ELSE
        psu.on_idle;
      END;
    END;
  END;
  RETURN msg.wParam;
END run;

(**
Adding control elements. A control element once added will be
working and reacting to the mouse and keyboard events. Still, you
will have to render them in the on_draw() using function
render_ctrl() because platform_support doesn't know anything about
renderers you use. The controls will be also scaled automatically
if they provide a proper scaling mechanism (all the controls
included into the basic AGG package do).
If you don't need a particular control to be scaled automatically
call ctrl::no_transform() after adding.
*)
PROCEDURE (VAR psu: platform_support) add_ctrl*(c: ctl.ctrl_ptr);
BEGIN
  psu.ctrls.add(c);
  c.transform(psu.resize_mtx);
END add_ctrl;

PROCEDURE (VAR psu: platform_support) widthd*(): bas.double;
BEGIN
  RETURN psu.rbuf_window.byte_width;
END widthd;

PROCEDURE (VAR psu: platform_support) heightd*(): bas.double;
BEGIN
  RETURN psu.rbuf_window.byte_height;
END heightd;

PROCEDURE (VAR psu: platform_support) initial_widthd*(): bas.double;
BEGIN
  RETURN psu.initial_width;
END initial_widthd;

PROCEDURE (VAR psu: platform_support) initial_heightd*(): bas.double;
BEGIN
  RETURN psu.initial_height;
END initial_heightd;

(**
Get raw display handler depending on the system.
For win32 its an HDC, for other systems it can be a pointer to some
structure. See the implementation files for detals.
It's provided "as is", so, first you should check if it's not null.
If it's null the raw_display_handler is not supported. Also, there's
no guarantee that this function is implemented, so, in some
implementations you may have simply an unresolved symbol when linking.
*)
PROCEDURE (VAR psu: platform_support) raw_display_handler*(): win.HDC;
BEGIN
  RETURN psu.specific.current_dc;
END raw_display_handler;
(**
Display message box or print the message to the console
(depending on implementation)
*)
PROCEDURE (VAR psu: platform_support) message*(msg: ARRAY OF CHAR);
BEGIN
  win.MessageBox(psu.specific.hwnd, msg, win.GetPSTR('AGG Message'), win.MB_OK);
END message;

(**
Stopwatch functions. Function elapsed_time() returns time elapsed
since the latest start_timer() invocation in millisecods.
The resolutoin depends on the implementation.
In Win32 it uses QueryPerformanceFrequency() / QueryPerformanceCounter().
*)
PROCEDURE (VAR psu: platform_support) start_timer*();
BEGIN
  win.QueryPerformanceCounter(sw_start);(*hack*)
END start_timer;

PROCEDURE (VAR psu: platform_support) elapsed_time*(): bas.double;
VAR
  stop: win.LARGE_INTEGER;
BEGIN
  win.QueryPerformanceCounter(stop);
  RETURN SHORT((stop.QuadPart - sw_start.QuadPart) * 1000.0 / sw_freq.QuadPart);(*hack*)
END elapsed_time;

(**
Get the full file name. In most cases it simply returns
file_name. As it's appropriate in many systems if you open
a file by its name without specifying the path, it tries to
open it in the current directory. The demos usually expect
all the supplementary files to be placed in the current
directory, that is usually coincides with the directory where
the the executable is. However, in some systems (BeOS) it's not so.
For those kinds of systems full_file_name() can help access files
preserving commonly used policy.
So, it's a good idea to use in the demos the following:
FILE* fd = fopen(full_file_name("some.file"), "r");
instead of
FILE* fd = fopen("some.file", "r"); *)
PROCEDURE (VAR psu: platform_support) full_file_name*(file_name: ARRAY OF CHAR; VAR r: ARRAY OF CHAR);
BEGIN
  COPY(r, file_name);
END full_file_name;
PROCEDURE (VAR psu: platform_support) file_source   *(path, fname: ARRAY OF CHAR; VAR r: ARRAY OF CHAR);
BEGIN

END file_source;


END AggPlatformSupport.
PROCEDURE (VAR psu: platform_support)