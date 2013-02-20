MODULE AggRenderingBuffer;

IMPORT
  bas := AggBasics,
  rts := oberonRTS;

TYPE

  rendering_buffer* = RECORD
    buf          - : bas.buffer;
    byte_width   - : bas.int32; (* ширина в пикселях *)
    byte_height  - : bas.int32; (* высота в пикселях *)
    bpp          - : bas.int32; (* Количество байт-на-пиксель (byte-per-pixel). Может быть отрицательным. *)


    (* *)
    bits_per_pix - : bas.int32;
    pixel_width  - : bas.int32;
    pixel_height - : bas.int32;
  END;

  rendering_buffer_ptr* = POINTER TO rendering_buffer;

(* bppabs *)

PROCEDURE (rc: rendering_buffer_ptr) bppabs*(): bas.int32;
BEGIN
  RETURN ABS(rc.bpp);
END bppabs;

(* row *)

PROCEDURE (rc: rendering_buffer_ptr) row*(y: bas.int32): bas.int32;
BEGIN
  IF (rc.bpp < 0) OR (rc.bits_per_pix < 0) THEN
    RETURN rc.byte_height - y;
  ELSE
    RETURN y;
  END;
END row;

(* Attach *)

PROCEDURE (rc: rendering_buffer_ptr) Attach*(rbuf: rendering_buffer_ptr);
BEGIN
  rc.buf         := rbuf.buf;
  rc.byte_height := rbuf.byte_height;
  rc.byte_width  := rbuf.byte_width;
  rc.bpp         := rbuf.bpp;

  rc.pixel_height:= rbuf.pixel_height;
  rc.pixel_width := rbuf.pixel_width;
  rc.bits_per_pix:= rbuf.bits_per_pix;
END Attach;

PROCEDURE (rc: rendering_buffer_ptr) attach*(buf: bas.buffer; bytes_per_pixel: bas.int32);
BEGIN
  (*ASSERT(LEN(buf^, 1) MOD ABS(bytes_per_bixel) = 0);*)
  (* Подключение к буферу с другим разрешением *)
  rc.buf         := buf;
  rc.bpp         := bytes_per_pixel;
  rc.byte_width  := LEN(buf^, 1) (*DIV ABS(bytes_per_bixel)*);
  rc.byte_height := LEN(buf^, 0);

  rc.pixel_height:= rc.byte_height;
  rc.pixel_width := rc.byte_width DIV bytes_per_pixel;
  rc.bits_per_pix:= bytes_per_pixel * 8;
END attach;

PROCEDURE (rc: rendering_buffer_ptr) attach_bits*(buf: bas.buffer; bits_per_pix: bas.int32);
BEGIN
  (*ASSERT(LEN(buf^, 1) MOD ABS(bytes_per_bixel) = 0);*)
  (* Подключение к буферу с другим разрешением *)
  rc.buf         := buf;
  rc.bpp         := 0;
  rc.byte_width  := LEN(buf^, 1) (*DIV ABS(bytes_per_bixel)*);
  rc.byte_height := LEN(buf^, 0);

  rc.pixel_height:= rc.byte_height;
  rc.pixel_width := ASH(ASH((rc.byte_width * 8), -3), 3) DIV ABS(bits_per_pix);
  rc.bits_per_pix:= bits_per_pix;
END attach_bits;


(* Init *)

PROCEDURE (rc: rendering_buffer_ptr) Construct*();
BEGIN
  rc.buf          := NIL;
  rc.byte_width   := 0;
  rc.byte_height  := 0;
  rc.bpp          := 0;

  rc.bits_per_pix := 0;
  rc.pixel_width  := 0;
  rc.pixel_height := 0;
END Construct;

(* Create *)

PROCEDURE (rc: rendering_buffer_ptr) Construct1*(pixel_height, pixel_width, bytes_per_pixel: bas.int32);
VAR
  byte_width: bas.int32;
BEGIN
  byte_width := pixel_width * ABS(bytes_per_pixel);
  NEW(rc.buf, pixel_height, byte_width);

  rc.byte_width  := byte_width;
  rc.byte_height := pixel_height;
  rc.bpp         := bytes_per_pixel;

  rc.bits_per_pix:= bytes_per_pixel * 8;
  rc.pixel_width := pixel_width;
  rc.pixel_height:= pixel_height;
END Construct1;

PROCEDURE (rc: rendering_buffer_ptr) Construct2*(pixel_height, pixel_width, bits_per_pixel: bas.int32);
VAR
  byte_width: bas.int32;
BEGIN
  (*В данном случае ASH -3 нужно для того,
  чтобы при делении на 8 производилось округление в большую сторону. *)
  byte_width := ASH(pixel_width * ABS(bits_per_pixel), -3) DIV 8;
  NEW(rc.buf, pixel_height, byte_width);

  rc.byte_width  := byte_width;
  rc.byte_height := pixel_height;
  rc.bpp         := 0;

  rc.bits_per_pix:= bits_per_pixel;
  rc.pixel_width := pixel_width;
  rc.pixel_height:= pixel_height;
END Construct2;



(* Destroy.
   Отдаем память сборщику мусора. *)

PROCEDURE (rc: rendering_buffer_ptr) Destruct*();
BEGIN
  rc.Construct();
  rts.Collect
END Destruct;

END AggRenderingBuffer.
