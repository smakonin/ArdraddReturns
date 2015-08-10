' Copyright (C) 1991, Stephen Makonin. All Rights Reserved.

DECLARE SUB Center (wide!, row!, text$)
DECLARE SUB PressEnter ()
DECLARE SUB FixInput (row!, col!, text$, MaxLen!)
DECLARE SUB MenuBar (Selection$(), TopLocation!, Margin!, ForeGround!, BackGround!, BarForeGround!, BarBackGround!, Ch!)
DECLARE SUB Window1 ()
DECLARE SUB Window2 ()
DECLARE SUB Window3 ()
DECLARE SUB SubMenu (Numb!)
DECLARE SUB CopyHD (Drive$, Direct$)

ON ERROR GOTO ErrorHandle
CLS
DIM SHARED Drive AS STRING * 1
DIM SHARED Direct AS STRING * 8
DIM SHARED help(5) AS STRING * 80
VIEW PRINT 1 TO 25
Drive = "C"
Direct = "QQ1"
Window1
Window2

   choice = 1
   DO
      DIM list$(5)
      list$(1) = "1 Set Directory Path"
      list$(2) = "2 Game Requirements"
      list$(3) = "3 Print Out Manual"
      list$(4) = "4 Copy Files to Path"
      list$(5) = "5 Exit to DOS"
     
      help(1) = "Select this item to change the current directory for installation."
      help(2) = "Select this item to look at the requirements needed to run this game."
      help(3) = "Select this item to print out the manual for this game on your printer."
      help(4) = "Select this item to install this game to the directory " + Drive + ":\" + RTRIM$(Direct) + "."
      help(5) = "Select this item to exit install and return to operating system."
     
      MenuBar list$(), 9, 29, 14, 4, 15, 13, choice
      IF choice = 5 THEN
         EXIT DO
      ELSE
         SubMenu choice
      END IF
   LOOP

COLOR 7, 0
CLS
PRINT "Wasn't that fun!!!"
END

ErrorHandle:
   COLOR 7, 0
   CLS
   PRINT "You've just been FOO-BARD!!!"
   END

ErrorDir:
   RESUME NEXT

SpaceError:
   COLOR 7, 0
   CLS
   PRINT "Not enough space on selected path!"
   END

SUB Center (wide, row, text$)
   wide = INT(wide / 2)
   LOCATE row, wide - (LEN(text$) / 2 + .5)
   PRINT text$;
END SUB

SUB CopyHD (Drive$, Direct$)
   COLOR 7, 0
   LOCATE 25, 1
   PRINT " *** Install game files DO NOT interrupt this procedure.                       ";
   COLOR 15, 5
   Center 80, 16, "Copying/Installing Game Files"
   COLOR 31, 5
   Center 80, 22, "Please wait...."
   COLOR 15, 5
   Center 80, 19, "Creating directory " + Drive + ":\" + RTRIM$(Direct) + "...."
   COLOR 5, 5
   CHDIR Drive$ + ":\"
   ON ERROR GOTO ErrorDir
   MKDIR Drive$ + ":" + Direct$
   CHDIR Drive$ + ":\" + Direct$
   ON ERROR GOTO SpaceError
   COLOR 15, 5
   Center 80, 19, "Copying: *.exe to " + Drive + ":\" + RTRIM$(Direct) + "\*.exe"
   SHELL "copy *.exe " + Drive$ + ":\" + Direct$ + " >nul"
   COLOR 15, 5
   Center 80, 19, "Copying: *.dat to " + Drive + ":\" + RTRIM$(Direct) + "\*.dat"
   SHELL "copy *.dat " + Drive$ + ":\" + Direct$ + " >nul"
   COLOR 15, 5
   Center 80, 19, "Copying: *.ega to " + Drive + ":\" + RTRIM$(Direct) + "\*.ega"
   SHELL "copy *.ega " + Drive$ + ":\" + Direct$ + " >nul"
   COLOR 15, 5
   Center 80, 19, "Copying: *.map to " + Drive + ":\" + RTRIM$(Direct) + "\*.map"
   SHELL "copy *.map " + Drive$ + ":\" + Direct$ + " >nul"
   ON ERROR GOTO ErrorHandle
END SUB

SUB FixInput (row, col, text$, MaxLen) STATIC
   COLOR 1, 0
   LOCATE row, col, 1
   text$ = STRING$(MaxLen, 32)
   PRINT text$;
   LOCATE row, col, 1
   text$ = ""
   DO
      I$ = UCASE$(INKEY$)
      SELECT CASE I$
      CASE CHR$(13)
         EXIT SUB
      CASE CHR$(8)
         IF LEN(text$) > 0 THEN
            text$ = LEFT$(text$, LEN(text$) - 1)
            LOCATE row, col
            PRINT text$; " ";
            LOCATE row, col + LEN(text$)
         END IF
      CASE CHR$(48) TO CHR$(57), CHR$(65) TO CHR$(90)
         IF LEN(text$) < MaxLen THEN
            text$ = text$ + I$
            COLOR 1, 0
            PRINT I$;
         END IF
      END SELECT
   LOOP
   LOCATE , , 0
END SUB

SUB MenuBar (Selection$(), TopLocation, Margin, ForeGround, BackGround, BarForeGround, BarBackGround, Ch) STATIC
   LN = 0
   FOR I = 0 TO UBOUND(Selection$)
      IF LEN(Selection$(I)) > LN THEN LN = LEN(Selection$(I))
   NEXT I
   IF LBOUND(Selection$) = 0 THEN
      IF Selection$(0) <> "" AND TopLocation <> 1 THEN
         LOCATE TopLocation - 2, Margin + ((LN - LEN(Selection$(0))) / 2)
         PRINT Selection$(0);
      END IF
   END IF
   COLOR ForeGround, BackGround
   FOR I = 1 TO UBOUND(Selection$)
      LOCATE TopLocation + I - 1, Margin
      a$ = Selection$(I)
      Selection$(I) = a$ + STRING$(LN - LEN(a$), 32)
      PRINT Selection$(I);
   NEXT I
   CHOOSING = 1
   OLD = Ch
   WHILE CHOOSING = 1
      COLOR ForeGround, BackGround
      LOCATE TopLocation + OLD - 1, Margin
      PRINT Selection$(OLD);
      COLOR BarForeGround, BarBackGround
      LOCATE TopLocation + Ch - 1, Margin
      PRINT Selection$(Ch);
      COLOR 7, 0
      LOCATE 25, 1
      PRINT help(Ch);
      OLD = Ch
      a$ = ""
      WHILE a$ = "": a$ = INKEY$: WEND
      IF LEN(a$) = 2 THEN a = ASC(RIGHT$(a$, 1)) ELSE a = ASC(a$)
      IF a = 13 OR a = 27 THEN CHOOSING = 0
      IF LEN(a$) = 1 THEN
         FOR J = UBOUND(Selection$) TO 1 STEP -1
            IF a = ASC(Selection$(J)) THEN
               Ch = J
               COLOR ForeGround, BackGround
               LOCATE TopLocation + OLD - 1, Margin
               PRINT Selection$(OLD);
               COLOR BarForeGround, BarBackGround
               LOCATE TopLocation + Ch - 1, Margin
               PRINT Selection$(Ch);
               COLOR 7, 0
               LOCATE 25, 1
               PRINT help(Ch);
               CHOOSING = 0
            END IF
         NEXT J
      ELSE
         IF a = 72 OR a = 56 THEN
            Ch = Ch - 1
         ELSEIF a = 80 OR a = 50 THEN
            Ch = Ch + 1
         ELSEIF a = 71 OR a = 73 OR a = 55 OR a = 57 THEN
            Ch = 1
         ELSEIF a = 79 OR a = 81 OR a = 49 OR a = 51 THEN
            Ch = UBOUND(Selection$)
         END IF
      END IF
      IF Ch > UBOUND(Selection$) THEN Ch = 1
      IF Ch < 1 THEN Ch = UBOUND(Selection$)
   WEND
COLOR ForeGround, BackGround
END SUB

SUB PressEnter
   DO
      Misc1$ = UCASE$(INPUT$(1))
   LOOP UNTIL Misc1$ = CHR$(13)
END SUB

SUB SubMenu (Numb)
   DIM list$(3)
   Window3
   choice = 1
   IF Numb = 1 THEN
      DO
         list$(0) = "Copy Game to Hard Disk"
         list$(1) = "1 Drive:  " + Drive + ":"
         list$(2) = "2 Directory:  \" + Direct
         list$(3) = "3 Exit to Main Menu"
         help(1) = "Select this item to change the current drive."
         help(2) = "Select this item to change the current directory."
         help(3) = "Select this item to return to the main menu of this program."
         help(4) = ""
         help(5) = ""
         COLOR 14, 5
         MenuBar list$(), 19, 29, 14, 5, 15, 1, choice
         IF choice = 1 THEN FixInput 19, 39, Drive, 1
         IF choice = 2 THEN FixInput 20, 44, Direct, 8
         IF choice = 3 THEN Window1: Window2: EXIT SUB
      LOOP
   ELSEIF Numb = 2 THEN
      COLOR 15, 5
      Center 80, 16, " Game Requirements:  "
      Center 80, 18, "  EGA 320x200, 64K   "
      Center 80, 19, "286  20MHz or Faster "
      Center 80, 20, "  *MicroSoft Mouse   "
      COLOR 7, 0
      LOCATE 25, 1
      PRINT " *** Not a requirement needed in running the game.                            ";
      COLOR 15, 5
      Center 80, 22, "Press Enter when done...."
      PressEnter
      Window1
      Window2
      EXIT SUB
   ELSEIF Numb = 3 THEN
      COLOR 7, 0
      LOCATE 25, 1
      PRINT " *** Press enter to print out the manual or escape to exit this selection ***";
      COLOR 15, 5
      Center 80, 20, " Setup Printer & Enter Key..."
      DO
         Misc1$ = UCASE$(INPUT$(1))
      LOOP UNTIL Misc1$ = CHR$(13) OR Misc1$ = CHR$(27)
      IF Misc1$ = CHR$(27) THEN Window1: Window2: EXIT SUB
      COLOR 31, 5
      Center 80, 20, "    Printing Manual...       "
      ON ERROR GOTO ErrorDir
      SHELL "COPY MANUAL.DAT PRN: >NUL"
      Window1
      Window2
      EXIT SUB
   ELSEIF Numb = 4 THEN
      CopyHD Drive, Direct
      Window1
      Window2
      EXIT SUB
   END IF
END SUB

SUB Window1
   Misc1 = 13
   Misc2 = 13
   DO
      COLOR 14, 2
      LOCATE Misc1, 1
      PRINT CHR$(179); SPC(78); CHR$(179)
      LOCATE Misc2, 1
      PRINT CHR$(179); SPC(78); CHR$(179)
      Misc1 = Misc1 - 1
      Misc2 = Misc2 + 1
   LOOP UNTIL Misc2 = 25
   LOCATE Misc1, 1
   PRINT CHR$(218); STRING$(78, 196); CHR$(191)
   LOCATE Misc2 - 1, 1
   PRINT CHR$(192); STRING$(78, 196); CHR$(217)
   Center 80, Misc1 + 1, "Quikk Quest 1:  Ardradd Returns"
   Center 80, Misc1 + 2, "Quikk Install Program"
END SUB

SUB Window2
   Misc1 = 13
   Misc2 = 13
   DO
      COLOR 14, 4
      Center 80, Misc1, CHR$(179) + STRING$(26, 32) + CHR$(179)
      Center 80, Misc2, CHR$(179) + STRING$(26, 32) + CHR$(179)
      Misc1 = Misc1 - 1
      Misc2 = Misc2 + 1
   LOOP UNTIL Misc2 = 20
   Center 80, Misc1, CHR$(218) + STRING$(26, 196) + CHR$(191)
   Center 80, Misc2, CHR$(192) + STRING$(26, 196) + CHR$(217)
   Center 80, Misc1 + 1, "Main Menu"
END SUB

SUB Window3
   Misc1 = 19
   Misc2 = 19
   DO
      COLOR 14, 5
      Center 80, Misc1, CHR$(179) + STRING$(46, 32) + CHR$(179)
      Center 80, Misc2, CHR$(179) + STRING$(46, 32) + CHR$(179)
      Misc1 = Misc1 - 1
      Misc2 = Misc2 + 1
   LOOP UNTIL Misc2 = 23
   Center 80, Misc1, CHR$(218) + STRING$(46, 196) + CHR$(191)
   Center 80, Misc2, CHR$(192) + STRING$(46, 196) + CHR$(217)
   Center 80, Misc1 + 1, "Sub Menu"
END SUB

