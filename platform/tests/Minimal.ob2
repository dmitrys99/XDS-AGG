<* +MAIN *>
<* +O2EXTENSIONS *>
MODULE Minimal;

IMPORT
  Windows, SYSTEM;

CONST AppName = "Generic";
      Title   = AppName + " Application";

(* Resource identifiers (sorry, can't include .h files *)

CONST IDM_NEW         = 100;
      IDM_OPEN        = 101;
      IDM_SAVE        = 102;
      IDM_SAVEAS      = 103;
      IDM_PRINT       = 104;
      IDM_PRINTSETUP  = 105;
      IDM_EXIT        = 106;
      IDM_UNDO        = 200;
      IDM_CUT         = 201;
      IDM_COPY        = 202;

      IDM_PASTE       = 203;
      IDM_LINK        = 204;
      IDM_LINKS       = 205;

PROCEDURE [Windows.CALLBACK] WndProc (hWnd: Windows.HWND; message: Windows.UINT;
                                      wParam: Windows.WPARAM; lParam: Windows.LPARAM): Windows.LRESULT;

VAR wmId : INTEGER;
    ps     : Windows.PAINTSTRUCT;
    hdc    : Windows.HDC;
    pnt    : Windows.POINT;
    hMenu  : Windows.HMENU;
    hBrush : Windows.HBRUSH;
    rect   : Windows.RECT;
BEGIN
    CASE message OF
    | Windows.WM_CREATE:
         hMenu := Windows.GetMenu (hWnd);
         Windows.EnableMenuItem (hMenu, IDM_NEW, Windows.MF_ENABLED + Windows.MF_BYCOMMAND);
         Windows.EnableMenuItem (hMenu, IDM_OPEN, Windows.MF_ENABLED + Windows.MF_BYCOMMAND);

         RETURN 0;
    | Windows.WM_COMMAND:
         wmId    := Windows.LOWORD (wParam);

         (* Parse the menu selections: *)

         CASE wmId OF
         | IDM_EXIT:     Windows.DestroyWindow (hWnd);

         (* These two are not 'real' new and open; they only demonstrate how
          * to react on menu message.
          * Besides, OpenFile procedure demonstrates using of common dialogs
          * (see above)
          *)

         | IDM_NEW:

           Windows.MessageBox (hWnd, "'New' menu item pressed", Windows.GetPSTR(AppName), Windows.MB_OK);

         | IDM_OPEN:

           Windows.MessageBox (hWnd, "'New' menu item pressed", Windows.GetPSTR(AppName), Windows.MB_OK);


         (* Here are all the other possible menu options,
          * all of these are currently disabled:
         *)

         | IDM_SAVE:
         | IDM_SAVEAS:
         | IDM_UNDO:
         | IDM_CUT:
         | IDM_COPY:
         | IDM_PASTE:
         | IDM_LINK:
         | IDM_LINKS:
         ELSE
                         RETURN Windows.DefWindowProc (hWnd, message, wParam, lParam);
         END;

    | Windows.WM_RBUTTONDOWN: (* RightClick in windows client area... *)

         pnt.x := Windows.LOWORD (lParam);
         pnt.y := Windows.HIWORD (lParam);
         Windows.ClientToScreen (hWnd, pnt);

         (* This is where you would determine the appropriate 'context'
         ** menu to bring up. Since this app has no real functionality,
         ** we will just bring up the 'File' menu:
         *)

         hMenu := Windows.GetSubMenu (Windows.GetMenu (hWnd), 0);
         IF hMenu # NIL THEN
             Windows.TrackPopupMenu (hMenu, Windows.TPM_SET {}, pnt.x, pnt.y, 0, hWnd, NIL);
         ELSE
             Windows.MessageBeep (Windows.MB_SET {});
         END;

    | Windows.WM_PAINT:
         hdc := Windows.BeginPaint (hWnd, ps);

         (* This is where you should put all your drawing.
          * Just to demonstrate how it should be done we will
          * paint enture client area in light-blue and draw some
          * white string in the center of it
         *)

         hBrush := Windows.CreateSolidBrush (Windows.RGB (0, 0, 255));
         Windows.GetClientRect (hWnd, rect);
         Windows.FillRect (hdc, rect, hBrush);
         Windows.DeleteObject (hBrush);
         Windows.SetTextColor (hdc, Windows.RGB (255, 255, 255));
         Windows.SetBkMode (hdc, Windows.TRANSPARENT);
         Windows.DrawText (hdc, "Generic application", -1, rect,
                           Windows.DT_CENTER + Windows.DT_VCENTER +
                           Windows.DT_SINGLELINE);


         Windows.EndPaint (hWnd, ps);

    | Windows.WM_DESTROY:
         Windows.PostQuitMessage (0);
    ELSE
         RETURN Windows.DefWindowProc (hWnd, message, wParam, lParam);
    END;
    RETURN 0;
END WndProc;

PROCEDURE InitApplication () : BOOLEAN;
VAR wc : Windows.WNDCLASSEX;
BEGIN

    (* Fill in window class structure with parameters that describe
    ** the main window.
    *)

    wc.style         := Windows.CS_HREDRAW + Windows.CS_VREDRAW;
    wc.lpfnWndProc   := WndProc;
    wc.cbClsExtra    := 0;
    wc.cbWndExtra    := 0;
    wc.hInstance     := Windows.GetModuleHandle(NIL);
    wc.hIcon         := Windows.LoadIcon (wc.hInstance, AppName);
    wc.hCursor       := Windows.LoadCursor (NIL, Windows.IDC_ARROW);
    wc.hbrBackground := NIL; (* No default brush necessary since we handle all the drawing *)
    wc.lpszMenuName  := Windows.GetPSTR(AppName);
    wc.lpszClassName := Windows.GetPSTR(AppName);

    (* Added elements for Windows 95: *)

    wc.cbSize  := SIZE (Windows.WNDCLASSEX);
    wc.hIconSm := Windows.LoadIcon (wc.hInstance, "SMALL");

    RETURN Windows.RegisterClassEx (wc) <> 0;
END InitApplication;

PROCEDURE InitMainWindow () : BOOLEAN;
VAR hWnd : Windows.HWND;
BEGIN
    hWnd := Windows.CreateWindow (AppName, Title, Windows.WS_OVERLAPPEDWINDOW,
                                  Windows.CW_USEDEFAULT, 0, Windows.CW_USEDEFAULT, 0,
                                  NIL, NIL, Windows.GetModuleHandle(NIL), NIL);
    IF hWnd = NIL THEN
                RETURN FALSE;
    END;

    Windows.ShowWindow   (hWnd, Windows.SW_SHOWDEFAULT);
    Windows.UpdateWindow (hWnd);
    RETURN TRUE;
END InitMainWindow;

VAR msg : Windows.MSG;

BEGIN
    IF InitApplication () AND InitMainWindow () THEN
        WHILE Windows.GetMessage (msg, NIL, 0, 0) DO
              Windows.TranslateMessage (msg);
              Windows.DispatchMessage  (msg);
        END;
    END;


END Minimal.