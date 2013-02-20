(* Аналог packed *)
<* ALIGNMENT="2"*>
MODULE AggWin32Bmp;

IMPORT
  sys := SYSTEM,
  win := Windows,
  bit := AggBit,
  bas := AggBasics,
  sqf := SeqFile,
  raw := RawIO,
  rts := oberonRTS;

CONST
  org_mono8   * =  8;
  org_color16 * = 16;
  org_color24 * = 24;
  org_color32 * = 32;
  org_color48 * = 48;
  org_color64 * = 64;

TYPE

  RGBQUAD = RECORD
    rgbBlue     : bas.int8u;
    rgbGreen    : bas.int8u;
    rgbRed      : bas.int8u;
    rgbReserved : bas.int8u;
  END;
  PRGBQUAD = POINTER TO RGBQUAD;

  BITMAPINFOHEADER = RECORD
    biSize: bas.int32u;
    biWidth: bas.int32;
    biHeight: bas.int32;
    biPlanes: bas.int16u;
    biBitCount: bas.int16u;
    biCompression: bas.int32u;
    biSizeImage: bas.int32u;
    biXPelsPerMeter: bas.int32;
    biYPelsPerMeter: bas.int32u;
    biClrUsed: bas.int32u;
    biClrImportant: bas.int32u;
  END;
  PBITMAPINFOHEADER = POINTER TO BITMAPINFOHEADER;

  BITMAPINFO = RECORD
    bmiHeader : BITMAPINFOHEADER;
    bmiColors : POINTER TO ARRAY OF RGBQUAD;
  END;
  PBITMAPINFO = POINTER TO BITMAPINFO;

  BITMAPFILEHEADER = RECORD
    bfType: bas.int16u;
    bfSize: bas.int32u;
    bfReserved1: bas.int16u;
    bfReserved2: bas.int16u;
    bfOffBits: bas.int32u;
  END;

  PBITMAP* = POINTER TO BITMAP;
  BITMAP* = RECORD
    bmp: PBITMAPINFO;
    buf-: bas.buffer;
  END;

  pixel_map* = RECORD
    bmp-: PBITMAP;
    bpp: bas.int32;

    is_internal: BOOLEAN;

    img_size: bas.int32;
  END;

PROCEDURE (VAR pm: pixel_map) destroy*();
BEGIN

  pm.bmp := NIL;
  pm.bpp := 0;
  pm.is_internal := FALSE;
  pm.img_size  := 0;

  rts.Collect();
END destroy;

PROCEDURE (VAR pm: pixel_map) Construct*();
BEGIN
  pm.destroy();
END Construct;

PROCEDURE (VAR pm: pixel_map) Destruct*();
BEGIN
  pm.destroy();
END Destruct;

PROCEDURE (VAR pm: pixel_map) calc_row_len(width, bits_per_pixel: bas.int32): bas.int32;
VAR
  n, k: bas.int32;
BEGIN
  n := width;

  CASE bits_per_pixel OF
   1:
     k := n;
     n := bas.shr_int32(n, 3);
     IF bit.and32(k, 7) # 0 THEN
       INC(n);
     END;
  | 4:
     k := n;
     n := bas.shr_int32(n, 1);
     IF bit.and32(k, 3) # 0 THEN
      INC(n);
     END;
  | 8 :;
  |16 : n := n * 2;
  |24 : n := n * 3;
  |32 : n := n * 4;
  |48 : n := n * 6;
  |64 : n := n * 8;
  ELSE
    n:=0;
  END;
  RETURN ASH(bas.shr_int32((n + 3), 2), 2);
END calc_row_len;

PROCEDURE (VAR pm: pixel_map) calc_palette_size(clr_used, bits_per_pixel: bas.int32): bas.int32;
VAR
  palette_size: bas.int32;
BEGIN
  palette_size := 0;

  IF bits_per_pixel <= 8 THEN
    palette_size := clr_used;

    IF palette_size = 0 THEN
      palette_size := ASH(1, bits_per_pixel);
    END;
  END;

  RETURN palette_size;
END calc_palette_size;

PROCEDURE (VAR pm: pixel_map) calc_palette_size_bmp(bm: PBITMAP): bas.int32;
BEGIN
  IF bm = NIL THEN
    RETURN 0
  ELSE
    RETURN pm.calc_palette_size(bm.bmp.bmiHeader.biClrUsed, bm.bmp.bmiHeader.biBitCount);
  END;
END calc_palette_size_bmp;

PROCEDURE (VAR pm: pixel_map) create_bitmap_info(width, height, bits_per_pixel: bas.int32): PBITMAP;
VAR
  bm: PBITMAP;

  line_len,
  img_size,
  rgb_size: bas.int32;
BEGIN
  line_len := pm.calc_row_len(width, bits_per_pixel);
  img_size := height * line_len;
  rgb_size := pm.calc_palette_size(0, bits_per_pixel) * SIZE(RGBQUAD);

  NEW(bm);
  NEW(bm.bmp);
  IF rgb_size # 0 THEN
    NEW(bm.bmp.bmiColors, rgb_size);
  ELSE
    bm.bmp.bmiColors := NIL;
  END;
  NEW(bm.buf, height, line_len);

  bm.bmp.bmiHeader.biSize         :=SIZE(BITMAPINFOHEADER);
  bm.bmp.bmiHeader.biWidth        :=width;
  bm.bmp.bmiHeader.biHeight       :=height;
  bm.bmp.bmiHeader.biPlanes       :=1;
  bm.bmp.bmiHeader.biBitCount     :=SHORT(bits_per_pixel);
  bm.bmp.bmiHeader.biCompression  :=0;
  bm.bmp.bmiHeader.biSizeImage    :=img_size;
  bm.bmp.bmiHeader.biXPelsPerMeter:=0;
  bm.bmp.bmiHeader.biYPelsPerMeter:=0;
  bm.bmp.bmiHeader.biClrUsed      :=0;
  bm.bmp.bmiHeader.biClrImportant :=0;

  RETURN bm;
END create_bitmap_info;

PROCEDURE (VAR pm: pixel_map) create_from_bmp(bm: PBITMAP);
BEGIN
  IF bm # NIL THEN
    pm.img_size := pm.calc_row_len(bm.bmp.bmiHeader.biWidth, bm.bmp.bmiHeader.biBitCount) * bm.bmp.bmiHeader.biHeight;
    pm.bmp := bm;
  END;
END create_from_bmp;

PROCEDURE (VAR pm: pixel_map) create_gray_scale_palette(bm: PBITMAP);
VAR
 i, rgb_size: bas.int32;
 brightness: bas.int8u;
BEGIN
  IF bm = NIL THEN
    RETURN;
  END;

  rgb_size := pm.calc_palette_size_bmp(bm);

  IF rgb_size > 0 THEN
    FOR i := 0 TO rgb_size - 1 DO
      brightness := bas.agg_getbyte(ENTIER((255 * i)  / (rgb_size - 1)));

      bm.bmp.bmiColors[i].rgbBlue  := brightness;
      bm.bmp.bmiColors[i].rgbGreen := brightness;
      bm.bmp.bmiColors[i].rgbRed   := brightness;

      bm.bmp.bmiColors[i].rgbReserved := 0;
    END;
  END;
END create_gray_scale_palette;

PROCEDURE (VAR pm: pixel_map) create*(width, height, org: bas.int32; clear_val: bas.int16u);
VAR
  i, j: bas.int32;
BEGIN
  pm.destroy();

  IF width = 0 THEN
    width := 1;
  END;

  IF height = 0 THEN
    height := 1;
  END;

  pm.bpp := org;
  pm.create_from_bmp(pm.create_bitmap_info(width, height, pm.bpp));
  pm.create_gray_scale_palette(pm.bmp);
  pm.is_internal := TRUE;

  IF clear_val <= 255 THEN
    FOR i := 0 TO LEN(pm.bmp.buf^, 0)-1 DO
      FOR j := 0 TO LEN(pm.bmp.buf^, 1)-1 DO
        pm.bmp.buf[i, j] := bas.agg_getbyte(clear_val);
      END;
    END;
  END;
END create;

PROCEDURE (VAR pm: pixel_map) width*(): bas.int32;
BEGIN
  RETURN pm.bmp.bmp.bmiHeader.biWidth;
END width;

PROCEDURE (VAR pm: pixel_map) height*(): bas.int32;
BEGIN
  RETURN pm.bmp.bmp.bmiHeader.biHeight;
END height;

PROCEDURE (VAR pm: pixel_map) Bpp*(): bas.int32;
BEGIN
  RETURN pm.bpp;
END Bpp;

PROCEDURE (VAR pm: pixel_map) calc_header_size(bm: PBITMAP): bas.int32u;
BEGIN
  IF bm = NIL THEN
    RETURN 0
  ELSE
    RETURN SIZE(BITMAPINFOHEADER) + SIZE(RGBQUAD) * pm.calc_palette_size_bmp(bm);
  END;
END calc_header_size;

PROCEDURE (VAR pm: pixel_map) calc_full_size(bm: PBITMAP): bas.int32;
BEGIN
  IF bm = NIL THEN
    RETURN 0
  ELSE
    RETURN SIZE(BITMAPINFOHEADER) + SIZE(RGBQUAD) * pm.calc_palette_size_bmp(bm) + ENTIER(bm.bmp.bmiHeader.biSizeImage + 0.0);
  END;
END calc_full_size;

PROCEDURE (VAR pm: pixel_map) save*(filename: ARRAY OF CHAR): BOOLEAN;
VAR
  cid: sqf.ChanId;
  res: sqf.OpenResults;
  bmf: BITMAPFILEHEADER;
BEGIN
  sqf.OpenWrite(cid, filename, sqf.raw + sqf.write + sqf.old, res);
  IF ~(res = sqf.opened) THEN
    RETURN FALSE;
  END;

  IF pm.bmp = NIL THEN
    RETURN FALSE;
  ELSE
    bmf.bfType      := 04D42H;
    bmf.bfOffBits   := pm.calc_header_size(pm.bmp) + SIZE(BITMAPFILEHEADER);
    bmf.bfSize      := SHORT(bmf.bfOffBits) + pm.img_size;
    bmf.bfReserved1 := 0;
    bmf.bfReserved2 := 0;

    raw.Write(cid, bmf);
    raw.Write(cid, pm.bmp.bmp.bmiHeader);

    IF pm.bmp.bmp.bmiHeader.biBitCount <= 8 THEN
      raw.Write(cid, pm.bmp.bmp.bmiColors^);
    END;

    raw.Write(cid, pm.bmp.buf^);

    sqf.Close(cid);

    RETURN TRUE;
  END;
END save;

PROCEDURE (VAR pm: pixel_map) load*(filename: ARRAY OF CHAR): BOOLEAN;
VAR
  bmf: BITMAPFILEHEADER;
  bmi: PBITMAP;

  cid: sqf.ChanId;
  res: sqf.OpenResults;
BEGIN
  sqf.OpenRead(cid, filename, sqf.raw + sqf.read + sqf.old, res);
  IF ~(res = sqf.opened) THEN
    RETURN FALSE;
  END;

  raw.Read(cid, bmf);

  IF bmf.bfType # 04D42H THEN
    RETURN FALSE;
  END;

  NEW(bmi);
  NEW(bmi.bmp);
  raw.Read(cid, bmi.bmp.bmiHeader);
  pm.bpp := bmi.bmp.bmiHeader.biBitCount;
  IF (pm.bpp <= 8) & (bmi.bmp.bmiHeader.biClrUsed > 0) THEN
    NEW(bmi.bmp.bmiColors, bmi.bmp.bmiHeader.biClrUsed);
    raw.Read(cid, bmi.bmp.bmiColors^);
  END;
  NEW(bmi.buf, bmi.bmp.bmiHeader.biHeight, pm.calc_row_len(bmi.bmp.bmiHeader.biWidth, bmi.bmp.bmiHeader.biBitCount));

  raw.Read(cid, bmi.buf^);

  pm.create_from_bmp(bmi);
  pm.is_internal := TRUE;

  sqf.Close(cid);

  RETURN TRUE;

END load;

PROCEDURE (VAR pm: pixel_map) draw*(h_dc: win.HDC; device_rect: win.PRECT; bmp_rect: win.PRECT);

TYPE
  tdummy = POINTER TO ARRAY OF bas.int8u;

VAR
  bmp_x,
  bmp_y,

  bmp_width,
  bmp_height,

  dvc_x,
  dvc_y,

  dvc_width,
  dvc_height: bas.int32;

  err: bas.int32;
(*  bok: BOOLEAN;*)

(*  compdc: win.HDC;
  handle,
  backup: win.HBITMAP;
  bminfo: BITMAPINFO; *)

  dummy: tdummy;
(*  buffer: pointer;

  rinc, rgap, size, stride: bas.int32;*)
BEGIN
  IF (pm.bmp = NIL) OR (pm.bmp.buf = NIL) THEN
    RETURN;
  END;

  bmp_x := 0;
  bmp_y := 0;

  bmp_width  := pm.bmp.bmp.bmiHeader.biWidth;
  bmp_height := pm.bmp.bmp.bmiHeader.biHeight;

  IF bmp_rect # NIL THEN
    bmp_x      := bmp_rect.left;
    bmp_y      := bmp_rect.top;
    bmp_width  := bmp_rect.right  - bmp_rect.left;
    bmp_height := bmp_rect.bottom - bmp_rect.top;
  END;

  dvc_x      := bmp_x;
  dvc_y      := bmp_y;
  dvc_width  := bmp_width;
  dvc_height := bmp_height;

  IF device_rect # NIL THEN
    dvc_x      := device_rect.left;
    dvc_y      := device_rect.top;
    dvc_width  := device_rect.right  - device_rect.left;
    dvc_height := device_rect.bottom - device_rect.top;
  END;

  (* The Trick
     Oberon does not allow us to use the whole 2-dimentional array as one long 1-dimentional.
     It gives us the first line only.
     But it gives us the address of first element ([0,0]).
     Windows is happy...

     Ugh!*)
  dummy := sys.VAL(tdummy, pm.bmp.buf);

  IF (dvc_width # bmp_width) OR (dvc_height # bmp_height) THEN

   win.SetStretchBltMode(h_dc, win.COLORONCOLOR);

   win.StretchDIBits(
    h_dc,             (* handle of device context *)
    dvc_x,            (* x-coordinate of upper-left corner of source rect. *)
    dvc_y,            (* y-coordinate of upper-left corner of source rect. *)
    dvc_width,        (* width of source rectangle *)
    dvc_height,       (* height of source rectangle *)
    bmp_x,
    bmp_y,            (* x, y -coordinates of upper-left corner of dest. rect. *)
    bmp_width,        (* width of destination rectangle *)
    bmp_height,       (* height of destination rectangle *)
    dummy^,           (* address of bitmap bits *)
    sys.VAL(win.BITMAPINFOHEADER, pm.bmp.bmp.bmiHeader),       (* address of bitmap data *)
    win.DIB_RGB_COLORS,(* usage *)
    win.SRCCOPY);     (* raster operation code *)
  ELSE

   err := win.SetDIBitsToDevice(
    h_dc,             (* handle to device context *)
    dvc_x,            (* x-coordinate of upper-left corner of *)
    dvc_y,            (* y-coordinate of upper-left corner of *)
    dvc_width,        (* source rectangle width *)
    dvc_height,       (* source rectangle height *)
    bmp_x,            (* x-coordinate of lower-left corner of *)
    bmp_y,            (* y-coordinate of lower-left corner of *)
    0,                (* first scan line in array *)
    bmp_height,       (* number of scan lines *)
    dummy^,           (* address of array with DIB bits *)
    sys.VAL(win.BITMAPINFOHEADER, pm.bmp.bmp.bmiHeader), (* address of structure with bitmap info. *)
    win.DIB_RGB_COLORS); (* RGB or palette indexes *)

  (*hack*)
   (*if err = 0 then
    begin
     compdc:=CreateCompatibleDC(h_dc);

     if compdc <> 0 then
      begin
       fillchar(bminfo, sizeof(TBitmapInfoHeader) , 0);

       bminfo.bmiHeader.biSize       :=m_bmp.bmiHeader.biSize;
       bminfo.bmiHeader.biCompression:=m_bmp.bmiHeader.biCompression;

       bminfo.bmiHeader.biPlanes  :=m_bmp.bmiHeader.biPlanes;
       bminfo.bmiHeader.biBitCount:=m_bmp.bmiHeader.biBitCount;

       bminfo.bmiHeader.biWidth :=m_bmp.bmiHeader.biWidth;
       bminfo.bmiHeader.biHeight:=m_bmp.bmiHeader.biHeight;

       handle:=CreateDIBSection(compdc, bminfo, DIB_RGB_COLORS, buffer, 0, 0);
       stride:=_stride;

       rinc:=((bminfo.bmiHeader.biWidth * bminfo.bmiHeader.biBitCount + 31)  shr 5)  shl 2;
       rgap:=bminfo.bmiHeader.biWidth mod 4;
       size:=rinc * bminfo.bmiHeader.biHeight;

       if handle <> 0 then
        begin
         backup:=SelectObject(compdc, handle);

         if (rinc = stride)  and
            (size = m_img_size)  then
          begin
           move(m_buf^, buffer^, size);

           bok:=BitBlt(
            h_dc, dvc_x, dvc_y, dvc_width, dvc_height,
            compdc, bmp_x, bmp_y,
            SRCCOPY);

          end
         else
          MessageBox(0, 'Cannot draw - different format !', 'pixel_map.draw message', MB_OK);

         if backup <> 0 then
          SelectObject(compdc, backup);

         DeleteObject(handle);

        end;

       DeleteDC(compdc);

      end;

    end;
  *)
  END;
END draw;

END AggWin32Bmp.