  (* GRAY *)

  amask_no_clip_gray8_ptr* = POINTER TO amask_no_clip_gray8;
  amask_no_clip_gray8* = RECORD(amask_no_clip_u8)
  END;

  (*RGB*)

  amask_no_clip_rgb24r_ptr* = POINTER TO amask_no_clip_rgb24r;
  amask_no_clip_rgb24r* = RECORD(amask_no_clip_u8)
  END;

  amask_no_clip_rgb24g_ptr* = POINTER TO amask_no_clip_rgb24g;
  amask_no_clip_rgb24g* = RECORD(amask_no_clip_u8)
  END;

  amask_no_clip_rgb24b_ptr* = POINTER TO amask_no_clip_rgb24b;
  amask_no_clip_rgb24b* = RECORD(amask_no_clip_u8)
  END;

  (*BGR*)

  amask_no_clip_bgr24r_ptr* = POINTER TO amask_no_clip_bgr24r;
  amask_no_clip_bgr24r* = RECORD(amask_no_clip_u8)
  END;

  amask_no_clip_bgr24g_ptr* = POINTER TO amask_no_clip_bgr24g;
  amask_no_clip_bgr24g* = RECORD(amask_no_clip_u8)
  END;

  amask_no_clip_bgr24b_ptr* = POINTER TO amask_no_clip_bgr24b;
  amask_no_clip_bgr24b* = RECORD(amask_no_clip_u8)
  END;

  (* RGBA *)

  amask_no_clip_rgba32r_ptr* = POINTER TO amask_no_clip_rgba32r;
  amask_no_clip_rgba32r* = RECORD(amask_no_clip_u8)
  END;

  amask_no_clip_rgba32g_ptr* = POINTER TO amask_no_clip_rgba32g;
  amask_no_clip_rgba32g* = RECORD(amask_no_clip_u8)
  END;

  amask_no_clip_rgba32b_ptr* = POINTER TO amask_no_clip_rgba32b;
  amask_no_clip_rgba32b* = RECORD(amask_no_clip_u8)
  END;

  amask_no_clip_rgba32a_ptr* = POINTER TO amask_no_clip_rgba32a;
  amask_no_clip_rgba32a* = RECORD(amask_no_clip_u8)
  END;

  (*ARGB*)

  amask_no_clip_argb32r_ptr* = POINTER TO amask_no_clip_argb32r;
  amask_no_clip_argb32r* = RECORD(amask_no_clip_u8)
  END;

  amask_no_clip_argb32g_ptr* = POINTER TO amask_no_clip_argb32g;
  amask_no_clip_argb32g* = RECORD(amask_no_clip_u8)
  END;

  amask_no_clip_argb32b_ptr* = POINTER TO amask_no_clip_argb32b;
  amask_no_clip_argb32b* = RECORD(amask_no_clip_u8)
  END;

  amask_no_clip_argb32a_ptr* = POINTER TO amask_no_clip_argb32a;
  amask_no_clip_argb32a* = RECORD(amask_no_clip_u8)
  END;

  (* BRGA *)

  amask_no_clip_bgra32r_ptr* = POINTER TO amask_no_clip_bgra32r;
  amask_no_clip_bgra32r* = RECORD(amask_no_clip_u8)
  END;

  amask_no_clip_bgra32g_ptr* = POINTER TO amask_no_clip_bgra32g;
  amask_no_clip_bgra32g* = RECORD(amask_no_clip_u8)
  END;

  amask_no_clip_bgra32b_ptr* = POINTER TO amask_no_clip_bgra32b;
  amask_no_clip_bgra32b* = RECORD(amask_no_clip_u8)
  END;

  amask_no_clip_bgra32a_ptr* = POINTER TO amask_no_clip_bgra32a;
  amask_no_clip_bgra32a* = RECORD(amask_no_clip_u8)
  END;

  (* ABRG *)

  amask_no_clip_abgr32r_ptr* = POINTER TO amask_no_clip_abgr32r;
  amask_no_clip_abgr32r* = RECORD(amask_no_clip_u8)
  END;

  amask_no_clip_abgr32g_ptr* = POINTER TO amask_no_clip_abgr32g;
  amask_no_clip_abgr32g* = RECORD(amask_no_clip_u8)
  END;

  amask_no_clip_abgr32b_ptr* = POINTER TO amask_no_clip_abgr32b;
  amask_no_clip_abgr32b* = RECORD(amask_no_clip_u8)
  END;

  amask_no_clip_abgr32a_ptr* = POINTER TO amask_no_clip_abgr32a;
  amask_no_clip_abgr32a* = RECORD(amask_no_clip_u8)
  END;

  (* GRAY *)

  amask_no_clip_rgb24gray_ptr* = POINTER TO amask_no_clip_rgb24gray;
  amask_no_clip_rgb24gray* = RECORD(amask_no_clip_u8)
  END;

  amask_no_clip_bgr24gray_ptr* = POINTER TO amask_no_clip_bgr24gray;
  amask_no_clip_bgr24gray* = RECORD(amask_no_clip_u8)
  END;


  amask_no_clip_rgba32gray_ptr* = POINTER TO amask_no_clip_rgba32gray;
  amask_no_clip_rgba32gray* = RECORD(amask_no_clip_u8)
  END;

  amask_no_clip_argb32gray_ptr* = POINTER TO amask_no_clip_argb32gray;
  amask_no_clip_argb32gray* = RECORD(amask_no_clip_u8)
  END;

  amask_no_clip_bgra32gray_ptr* = POINTER TO amask_no_clip_bgra32gray;
  amask_no_clip_bgra32gray* = RECORD(amask_no_clip_u8)
  END;

  amask_no_clip_abgr32gray_ptr* = POINTER TO amask_no_clip_abgr32gray;
  amask_no_clip_abgr32gray* = RECORD(amask_no_clip_u8)
  END;


(* GRAY *)

PROCEDURE (au: amask_no_clip_gray8_ptr) Construct*(rbuf: arb.rendering_buffer_ptr);
BEGIN
  au.ConstructRF(rbuf, one_component_mask_u8, 1, 0);
END Construct;

(*RGB*)

PROCEDURE (au: amask_no_clip_rgb24r_ptr) Construct*(rbuf: arb.rendering_buffer_ptr);
BEGIN
  au.ConstructRF(rbuf, one_component_mask_u8, 3, 0);
END Construct;

PROCEDURE (au: amask_no_clip_rgb24g_ptr) Construct*(rbuf: arb.rendering_buffer_ptr);
BEGIN
  au.ConstructRF(rbuf, one_component_mask_u8, 3, 1);
END Construct;

PROCEDURE (au: amask_no_clip_rgb24b_ptr) Construct*(rbuf: arb.rendering_buffer_ptr);
BEGIN
  au.ConstructRF(rbuf, one_component_mask_u8, 3, 2);
END Construct;


(* BGR *)

PROCEDURE (au: amask_no_clip_bgr24r_ptr) Construct*(rbuf: arb.rendering_buffer_ptr);
BEGIN
  au.ConstructRF(rbuf, one_component_mask_u8, 3, 2);
END Construct;

PROCEDURE (au: amask_no_clip_bgr24g_ptr) Construct*(rbuf: arb.rendering_buffer_ptr);
BEGIN
  au.ConstructRF(rbuf, one_component_mask_u8, 3, 1);
END Construct;

PROCEDURE (au: amask_no_clip_bgr24b_ptr) Construct*(rbuf: arb.rendering_buffer_ptr);
BEGIN
  au.ConstructRF(rbuf, one_component_mask_u8, 3, 0);
END Construct;


(* RGBA *)

PROCEDURE (au: amask_no_clip_rgba32r_ptr) Construct*(rbuf: arb.rendering_buffer_ptr);
BEGIN
  au.ConstructRF(rbuf, one_component_mask_u8, 4, 0);
END Construct;

PROCEDURE (au: amask_no_clip_rgba32g_ptr) Construct*(rbuf: arb.rendering_buffer_ptr);
BEGIN
  au.ConstructRF(rbuf, one_component_mask_u8, 4, 1);
END Construct;

PROCEDURE (au: amask_no_clip_rgba32b_ptr) Construct*(rbuf: arb.rendering_buffer_ptr);
BEGIN
  au.ConstructRF(rbuf, one_component_mask_u8, 4, 2);
END Construct;

PROCEDURE (au: amask_no_clip_rgba32a_ptr) Construct*(rbuf: arb.rendering_buffer_ptr);
BEGIN
  au.ConstructRF(rbuf, one_component_mask_u8, 4, 3);
END Construct;

(* ARGB *)

PROCEDURE (au: amask_no_clip_argb32r_ptr) Construct*(rbuf: arb.rendering_buffer_ptr);
BEGIN
  au.ConstructRF(rbuf, one_component_mask_u8, 4, 1);
END Construct;

PROCEDURE (au: amask_no_clip_argb32g_ptr) Construct*(rbuf: arb.rendering_buffer_ptr);
BEGIN
  au.ConstructRF(rbuf, one_component_mask_u8, 4, 2);
END Construct;

PROCEDURE (au: amask_no_clip_argb32b_ptr) Construct*(rbuf: arb.rendering_buffer_ptr);
BEGIN
  au.ConstructRF(rbuf, one_component_mask_u8, 4, 3);
END Construct;

PROCEDURE (au: amask_no_clip_argb32a_ptr) Construct*(rbuf: arb.rendering_buffer_ptr);
BEGIN
  au.ConstructRF(rbuf, one_component_mask_u8, 4, 0);
END Construct;

(* BGRA *)

PROCEDURE (au: amask_no_clip_bgra32r_ptr) Construct*(rbuf: arb.rendering_buffer_ptr);
BEGIN
  au.ConstructRF(rbuf, one_component_mask_u8, 4, 2);
END Construct;

PROCEDURE (au: amask_no_clip_bgra32g_ptr) Construct*(rbuf: arb.rendering_buffer_ptr);
BEGIN
  au.ConstructRF(rbuf, one_component_mask_u8, 4, 1);
END Construct;

PROCEDURE (au: amask_no_clip_bgra32b_ptr) Construct*(rbuf: arb.rendering_buffer_ptr);
BEGIN
  au.ConstructRF(rbuf, one_component_mask_u8, 4, 0);
END Construct;

PROCEDURE (au: amask_no_clip_bgra32a_ptr) Construct*(rbuf: arb.rendering_buffer_ptr);
BEGIN
  au.ConstructRF(rbuf, one_component_mask_u8, 4, 3);
END Construct;

(* ABGR *)

PROCEDURE (au: amask_no_clip_abgr32r_ptr) Construct*(rbuf: arb.rendering_buffer_ptr);
BEGIN
  au.ConstructRF(rbuf, one_component_mask_u8, 4, 3);
END Construct;

PROCEDURE (au: amask_no_clip_abgr32g_ptr) Construct*(rbuf: arb.rendering_buffer_ptr);
BEGIN
  au.ConstructRF(rbuf, one_component_mask_u8, 4, 2);
END Construct;

PROCEDURE (au: amask_no_clip_abgr32b_ptr) Construct*(rbuf: arb.rendering_buffer_ptr);
BEGIN
  au.ConstructRF(rbuf, one_component_mask_u8, 4, 1);
END Construct;

PROCEDURE (au: amask_no_clip_abgr32a_ptr) Construct*(rbuf: arb.rendering_buffer_ptr);
BEGIN
  au.ConstructRF(rbuf, one_component_mask_u8, 4, 0);
END Construct;

(* GRAY *)

PROCEDURE (au: amask_no_clip_rgb24gray_ptr) Construct*(rbuf: arb.rendering_buffer_ptr);
BEGIN
  au.ConstructRF(rbuf, rgb_to_gray_mask_u8_012, 3, 0);
END Construct;

PROCEDURE (au: amask_no_clip_bgr24gray_ptr) Construct*(rbuf: arb.rendering_buffer_ptr);
BEGIN
  au.ConstructRF(rbuf, rgb_to_gray_mask_u8_210, 3, 0);
END Construct;

PROCEDURE (au: amask_no_clip_rgba32gray_ptr) Construct*(rbuf: arb.rendering_buffer_ptr);
BEGIN
  au.ConstructRF(rbuf, rgb_to_gray_mask_u8_012, 4, 0);
END Construct;

PROCEDURE (au: amask_no_clip_argb32gray_ptr) Construct*(rbuf: arb.rendering_buffer_ptr);
BEGIN
  au.ConstructRF(rbuf, rgb_to_gray_mask_u8_012, 4, 1);
END Construct;

PROCEDURE (au: amask_no_clip_bgra32gray_ptr) Construct*(rbuf: arb.rendering_buffer_ptr);
BEGIN
  au.ConstructRF(rbuf, rgb_to_gray_mask_u8_210, 4, 0);
END Construct;

PROCEDURE (au: amask_no_clip_abgr32gray_ptr) Construct*(rbuf: arb.rendering_buffer_ptr);
BEGIN
  au.ConstructRF(rbuf, rgb_to_gray_mask_u8_210, 4, 1);
END Construct;
