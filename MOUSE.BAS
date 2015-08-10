' Copyright (C) 1991, Stephen Makonin. All Rights Reserved.


TYPE RegType
   ax AS INTEGER
   bx AS INTEGER
   cx AS INTEGER
   dx AS INTEGER
   Bp AS INTEGER
   si AS INTEGER
   di AS INTEGER
   flags AS INTEGER
END TYPE

DECLARE SUB Interrupt (intnum%, inreg AS RegType, outreg AS RegType)
DECLARE SUB Mouse (M1%, M2%, M3%, M4%)

ErrorHandle:
   CLS
   SCREEN 0
   WIDTH 80, 25
   PRINT "Error #"; ERR; " has occured at "; ERL; ";!"
   PRINT
   PRINT "Would you like to save you current game"
   PRINT "before exiting to DOS? (y/n)"
      DO
         Misc1$ = UCASE$(INPUT$(1))
         IF Misc1$ = "Y" THEN
            PRINT "Y"
            Info.x = x
            Info.y = y
            PUT #1, 1, Info
            PUT #1, 2, Info2
            EXIT DO
         END IF
         IF Misc1$ = "N" THEN PRINT "N": EXIT DO
      LOOP
   END

ErrNext:
   RESUME NEXT

SUB MouseHide STATIC
   Mouse 2, 0, 0, 0
END SUB

SUB MouseInches (horizontal%, vertical%) STATIC
   h% = horizontal% * 5 \ 2
   v% = vertical% * 8
   Mouse 15, 0, h%, v%
END SUB

SUB MouseInstall (mflag%) STATIC
   mflage% = 0
   Mouse mflag%, 0, 0, 0
END SUB

SUB MouseLightPen (switch%) STATIC
   IF switch% THEN
      Mouse 13, 0, 0, 0
   ELSE
      Mouse 14, 0, 0, 0
   END IF
END SUB

SUB MouseMickey (Horz%, Vert%) STATIC
   Mouse 11, 0, Horz%, Vert%
END SUB

SUB MouseNow (lefty%, righty%, x%, y%) STATIC
   Mouse 3, M2%, x%, y%
   lefty% = ((M2% AND 1) <> 0)
   righty% = ((M2% AND 2) <> 0)
END SUB

SUB MousePressLeft (lefty%, x%, y%) STATIC
   ml% = 5
   lefty% = 0
   Mouse ml%, lefty%, x%, y%
END SUB

SUB MousePressRight (lefty%, x%, y%) STATIC
   ml% = 5
   lefty% = 1
   Mouse ml%, lefty%, x%, y%
END SUB

SUB MousePut (x%, y%) STATIC
   Mouse 4, 0, x%, y%
END SUB

SUB MouseRange (x1%, y1%, x2%, y2%) STATIC
   Mouse 7, 0, x1%, y1%
   Mouse 8, 0, x2%, y2%
END SUB

SUB MouseReleaseLeft (lefty%, x%, y%) STATIC
   ml% = 6
   lefty% = 0
   Mouse ml%, lefty%, x%, y%
END SUB

SUB MouseReleseRight (lefty%, x%, y%) STATIC
   ml% = 6
   lefty% = 1
   Mouse ml%, lefty%, x%, y%
END SUB

SUB MouseSetGcursor (cursor$) STATIC
   x% = ASC(LEFT$(cursor$, 1))
   y% = ASC(MID$(cursor$, 2, 1))
   ON ERROR GOTO ErrNext
   Mouse 9, x%, y%, SADD(cursor$) + 2
   ON ERROR GOTO ErrorHandle
END SUB

SUB MouseShow STATIC
   Mouse 1, 0, 0, 0
END SUB

SUB MouseSoftCursor (scrn%, Curs%) STATIC
   Mouse 10, 0, scrn%, Curs%
END SUB

SUB MouseWrap (thres%) STATIC
   Mouse 19, 0, 0, thres%
END SUB

SUB TextScroll (row1%, col1%, row2%, col2%, lines%, Attr%) STATIC
   DIM reg AS RegType
   IF lines% > 0 THEN
      reg.ax = &H600 + lines% MOD 256
   ELSE
      reg.ax = &H700 + ABS(lines%) MOD 256
   END IF
   reg.bx = (Attr% * 256&) AND &HFF00
   reg.cx = (row1% - 1) * 256 + col1% - 1
   reg.dx = (row2% - 1) * 256 + col2% - 1
   Interrupt &H10, reg, reg
END SUB

