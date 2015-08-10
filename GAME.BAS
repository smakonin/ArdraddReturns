' Copyright (C) 1991, Stephen Makonin. All Rights Reserved.

DECLARE SUB ArdraddMove (Info AS ANY, Info2 AS ANY, x!, y!)
DECLARE SUB Attack (Info AS ANY, Info2 AS ANY, x!, y!)
DECLARE SUB MonsterMove (Info AS ANY, Info2 AS ANY, x!, y!, x2!, y2!)
DECLARE SUB PlaceTempTiles (Info AS ANY, Info2 AS ANY, x!, y!)
DECLARE SUB DungScroll (Info AS ANY, Info2 AS ANY, Direction$, x!, y!)
DECLARE SUB LookTile (Info AS ANY, Info2 AS ANY, x!, y!)
DECLARE SUB QuitNSave (Info AS ANY, Info2 AS ANY, x!, y!)
DECLARE SUB UseIt (Info AS ANY, Info2 AS ANY, x!, y!)
DECLARE SUB GetMap (Info AS ANY, Info2 AS ANY)
DECLARE SUB LoadMapTiles (TileSet!)
DECLARE SUB MapScroll (Info AS ANY, Info2 AS ANY, Direction$, x!, y!)
DECLARE SUB SpellList (Info AS ANY, Info2 AS ANY, x!, y!)
DECLARE SUB Talking (Info AS ANY, Info2 AS ANY, x!, y!)
DECLARE SUB TextScroll (row1%, col1%, row2%, col2%, lines%, Attr%)
DECLARE SUB MouseNow (lefty%, righty%, x%, y%)
DECLARE SUB MouseHide ()
DECLARE SUB MouseInstall (mflag%)
DECLARE SUB MouseShow ()
DECLARE SUB MouseSetGcursor (cursor$)
DECLARE SUB ReadData (a%)
DECLARE SUB UseIt2 (Info AS ANY, Info2 AS ANY, x!, y!)
DECLARE SUB ClearScreen ()
DECLARE SUB CastingEffect3 ()
DECLARE SUB CastingEffect4 ()
DECLARE SUB LoadScreen (Misc1$)
DECLARE SUB ExitTowne (Info AS ANY, Info2 AS ANY, x!, y!)
DECLARE SUB Enterance (Info AS ANY, Info2 AS ANY, Location AS ANY, x!, y!)
DECLARE SUB ReadyWeapon (Info AS ANY, Info2 AS ANY)
DECLARE SUB Inventory (Info AS ANY, Info2 AS ANY, x!, y!)
DECLARE SUB MainStats (Info AS ANY, Info2 AS ANY)
DECLARE SUB RestoreLife (Info AS ANY, Info2 AS ANY, Location AS ANY, x!, y!)
DECLARE SUB HoldUp (Info AS ANY, Info2 AS ANY, x!, y!)
DECLARE SUB CastingEffect2 ()
DECLARE SUB CastingEffect1 (Info AS ANY, Info2 AS ANY, x!, y!)
DECLARE SUB MenuBar (Selection$(), TopLocation!, Margin!, ForeGround!, BackGround!, BarForeGround!, BarBackGround!, CH!)
DECLARE SUB Prompt ()
DECLARE SUB Ending ()
DECLARE SUB Prompt2 ()
DECLARE SUB TheNews (Info AS ANY, Info2 AS ANY, qx!, qy!, Misc1$)
DECLARE SUB FixInput2 (row!, col!, Misc!, MaxLen!)

TYPE Chrinfo2
   pc2Name AS STRING * 10
   pc2Sex AS STRING * 1
   pc2Role AS STRING * 8
   Worldy2 AS STRING * 2
   Cit2 AS STRING * 2
   Cit3 AS STRING * 1
   Shipx AS INTEGER
   ShipY AS INTEGER
   SSword AS INTEGER
   Dagger AS INTEGER
   Shield AS INTEGER
   SpellBook AS INTEGER
   LSword AS INTEGER
   Bow AS INTEGER
   LBow AS INTEGER
   Sling AS INTEGER
   MDagger AS INTEGER
   MSword AS INTEGER
   Cloth AS INTEGER
   Leather AS INTEGER
   ChainMale AS INTEGER
   PlateMale AS INTEGER
   MagicMale AS INTEGER
   DungLvl AS INTEGER
   NumShips AS INTEGER
   ShipX2 AS INTEGER
   ShipY2 AS INTEGER
   Dead AS INTEGER
   ShipX3 AS INTEGER
   ShipY3 AS INTEGER
   ShipY4 AS INTEGER
   ShipX4 AS INTEGER
END TYPE

TYPE Chrinfo
   pcName AS STRING * 10
   pcSex AS STRING * 1
   pcRole AS STRING * 8
   Worldy AS STRING * 2
   Cit AS STRING * 2
   UnderTile AS STRING * 1
   pcStr AS INTEGER
   pcDex AS INTEGER
   pcINT AS INTEGER
   pcLvl AS INTEGER
   pcExp AS INTEGER
   pcHP AS INTEGER
   pcMHP AS INTEGER
   pcMP AS INTEGER
   pcMMP AS INTEGER
   pcGold AS INTEGER
   pcFood AS INTEGER
   ReadWeapon1 AS INTEGER
   ReadWeapon2 AS INTEGER
   ReadArmor AS INTEGER
   x AS INTEGER
   y AS INTEGER
   x2 AS INTEGER
   y2 AS INTEGER
   Mode AS INTEGER
   Quest1 AS INTEGER
   Quest2 AS INTEGER
   Quest3 AS INTEGER
   Quest4 AS INTEGER
   Quest5 AS INTEGER
   Quest6 AS INTEGER
   Quest7 AS INTEGER
END TYPE

TYPE MiscInfo
   Misc0 AS STRING * 10
END TYPE

TYPE LocateInfo
   Header1 AS STRING * 1
   Header2 AS STRING * 2
   lx1 AS INTEGER
   ly1 AS INTEGER
   lx2 AS INTEGER
   ly2 AS INTEGER
   lx3 AS INTEGER
   ly3 AS INTEGER
END TYPE

TYPE CorInfo
   x AS INTEGER
   y AS INTEGER
END TYPE

TYPE MonInfo
   ID AS STRING * 1
   x AS INTEGER
   y AS INTEGER
   HP AS INTEGER
   Damage AS INTEGER
   Expr AS INTEGER
   AttNum AS INTEGER
   RoundNum AS INTEGER
END TYPE

TYPE MouseInfo
   Curs  AS STRING * 66
   Hotx AS INTEGER
   Hoty AS INTEGER
END TYPE

CONST MaxDim = 10, DOWN = 80, UP = 72, LEFT = 75, Right = 77

DIM SHARED Location AS LocateInfo
DIM SHARED m AS MouseInfo
DIM SHARED Misc10 AS MiscInfo
DIM SHARED Cor AS CorInfo
DIM SHARED Mon(1 TO MaxDim) AS MonInfo
DIM SHARED Weap AS MonInfo
DIM Info AS Chrinfo
DIM Info2  AS Chrinfo2
DIM SHARED Latitude AS STRING * 70
DIM SHARED Inv AS STRING * 40
DIM SHARED ScreenTiles(1 TO 81) AS STRING * 1
DIM SHARED ViewMapScr(1 TO 4000)
DIM SHARED Tiles(45 TO 1215)
DIM SHARED Misc9(1 TO 45)
DIM SHARED Compass AS INTEGER
DIM SHARED DungTiles(1 TO 10) AS STRING * 1
OPEN "SAVEGAME.DAT" FOR RANDOM AS #1 LEN = LEN(Info)
GET #1, 1, Info
GET #1, 2, Info2
OPEN "LOCATE.DAT" FOR RANDOM AS #3 LEN = LEN(Location)
OPEN "LOOK.DAT" FOR RANDOM AS #4 LEN = LEN(Misc10)
OPEN "DOTOUT.DAT" FOR RANDOM AS #5 LEN = LEN(Cor)
OPEN "SCREEN01.DAT" FOR RANDOM AS #6 LEN = LEN(Inv)
OPEN "MOUSE.DAT" FOR RANDOM AS #8 LEN = LEN(m)
OPEN "FAKEINT.DAT" FOR RANDOM AS #9 LEN = LEN(Mon(1))
CLS
SCREEN 7, , 0, 0
MouseInstall mflag%
MouseShow
GET #8, 2, m
MouseSetGcursor m.Curs
DIM SHARED Bubble(1 TO 45)
DEF SEG = VARSEG(Bubble(1))
BLOAD "BUBBLE.EGA", VARPTR(Bubble(1))
DEF SEG
GetMap Info, Info2
IF Info.Cit = "00" THEN
   LoadMapTiles 1
ELSE
   LoadMapTiles 2
   Times1 = 1
   CitB$ = Info2.Cit3 + Info2.Cit2
   DO
      GET #3, Times1, Location
      IF CitB$ = Location.Header1 + Location.Header2 THEN EXIT DO
      Times1 = Times1 + 1
   LOOP UNTIL Times1 = LOF(3) \ LEN(Location) + 1
END IF
PALETTE 3, 0
PALETTE 5, 7
PALETTE 11, 0
PALETTE 13, 9
PALETTE 14, 0
PALETTE 12, 0
Misc3 = 1
Misc33 = 1
ON TIMER(1) GOSUB ChangeColor
TIMER ON
x = Info.x
y = Info.y
MouseHide
LoadScreen "MAINS1.EGA"
LOCATE 22, 3
PRINT "  Quikk Quest 1 "
LOCATE 23, 3
PRINT " Ardradd Returns"
MainStats Info, Info2
Compass = 1
IF Info.Mode = 3 THEN DungScroll Info, Info2, "Redraw", x, y ELSE MapScroll Info, Info2, "Mdraw", x, y

ON ERROR GOTO ErrorHandle

Misc44 = 1
Prompt
DO
   DO WHILE INKEY$ <> "": LOOP
   MouseShow
   DO
      IF Info.Mode = 1 AND Info.Cit <> "00" THEN
         IF x = Location.lx1 OR x = Location.lx2 THEN ExitTowne Info, Info2, x, y: Prompt
         IF y = Location.ly1 OR y = Location.ly2 THEN ExitTowne Info, Info2, x, y: Prompt
      END IF
      Choice$ = INKEY$
      MouseNow leftButton%, rightButton%, xMouse%, yMouse%
      IF xMouse% / 2 >= 9 AND xMouse% / 2 <= 150 AND yMouse% >= 164 AND yMouse% <= 188 THEN
         GET #8, 8, m
         MouseSetGcursor m.Curs
         IF leftButton% = -1 THEN Choice$ = "Q"
      ELSEIF xMouse% / 2 >= 164 AND xMouse% / 2 <= 308 AND yMouse% >= 9 AND yMouse% <= 84 THEN
         GET #8, 7, m
         MouseSetGcursor m.Curs
         IF leftButton% = -1 THEN Choice$ = "I"
      ELSEIF yMouse% >= 9 AND yMouse% <= 73 AND xMouse% / 2 >= 9 + (yMouse% - 9) AND xMouse% / 2 <= 151 - (yMouse% - 9) THEN
         GET #8, 3, m
         MouseSetGcursor m.Curs
         IF leftButton% = -1 THEN Choice$ = " " + CHR$(72)
      ELSEIF yMouse% >= 89 AND yMouse% <= 151 AND xMouse% / 2 >= 9 + ABS(yMouse% - 151) AND xMouse% / 2 <= 151 - ABS(yMouse% - 151) THEN
         GET #8, 4, m
         MouseSetGcursor m.Curs
         IF leftButton% = -1 THEN Choice$ = " " + CHR$(80)
      ELSEIF xMouse% / 2 >= 9 AND xMouse% / 2 <= 73 AND yMouse% >= 9 + (xMouse% / 2 - 9) AND yMouse% <= 151 - (xMouse% / 2 - 9) THEN
         GET #8, 6, m
         MouseSetGcursor m.Curs
         IF leftButton% = -1 THEN Choice$ = " " + CHR$(75)
      ELSEIF xMouse% / 2 >= 89 AND xMouse% / 2 <= 151 AND yMouse% >= 9 + ABS(xMouse% / 2 - 151) AND yMouse% <= 151 - ABS(xMouse% / 2 - 151) THEN
         GET #8, 5, m
         MouseSetGcursor m.Curs
         IF leftButton% = -1 THEN Choice$ = " " + CHR$(77)
      ELSE
         GET #8, 1, m
         MouseSetGcursor m.Curs
         IF leftButton% = -1 THEN Choice$ = " "
      END IF
      IF Misc44 = 15 THEN GOSUB WaterMove: Misc44 = 1
      Misc44 = Misc44 + 1
   LOOP WHILE Choice$ = ""
   MouseHide
   IF LEN(Choice$) = 1 THEN
   SELECT CASE ASC(Choice$)
   CASE 32 'Space bar  Pass a turn
      PRINT "Pass";
      IF Info.Mode = 3 THEN DungScroll Info, Info2, "Redraw", x, y ELSE MapScroll Info, Info2, "Mdraw", x, y
      Prompt
   CASE 65, 97    'A Attack
      IF Info.Cit = "00" OR Info.Cit = "05" THEN
         Attack Info, Info2, x, y
         MapScroll Info, Info2, "MRedraw", x, y
      ELSE
         PRINT "Not Here!";
      END IF
      Prompt
   CASE 67, 99    'C Cast
      IF Info.pcRole = "Wizard  " THEN SpellList Info, Info2, x, y ELSE PRINT "No spells!";
      Prompt
   CASE 69, 101   'E Enter
      IF Info.Cit = "00" THEN
         Enterance Info, Info2, Location, x, y
      ELSE
         PRINT "Not Here!";
      END IF
      Prompt
   CASE 72, 104   'H Hold Up & Camp
      IF Info.pcFood > 0 THEN
         IF Info.Cit = "00" AND Info.Mode = 1 OR Info.Mode = 3 THEN
            IF Info.UnderTile = "B" OR Info.UnderTile = "C" OR Info.Mode = 3 THEN
               HoldUp Info, Info2, x, y
            ELSE
               PRINT "Can't sleep here!";
            END IF
         ELSE
            PRINT "Can't sleep here!";
         END IF
      ELSE
         PRINT "Need food!";
      END IF
      IF Info.Cit = "00" AND Info.Mode <> 3 THEN MapScroll Info, Info2, "Mdraw", x, y
      Prompt
   CASE 73, 105   'I Inventory
      PRINT "Inventory";
      Inventory Info, Info2, x, y
      IF Info.Mode = 3 THEN DungScroll Info, Info2, "Redraw", x, y ELSE MapScroll Info, Info2, "Redraw", x, y
      Prompt
   CASE 76, 108   'L Look
      LookTile Info, Info2, x, y
      Prompt
   CASE 81, 113   'Q Quit & Save
      QuitNSave Info, Info2, x, y
      Prompt
   CASE 82, 114   'R Ready
      IF Info.Quest7 < 69 THEN ReadyWeapon Info, Info2 ELSE PRINT "You can't ready!";
      IF Info.Cit = "00" AND Info.Mode <> 3 THEN MapScroll Info, Info2, "Mdraw", x, y
      Prompt
   CASE 84, 116   'T Talk
      Talking Info, Info2, x, y
      Prompt
   CASE 85, 117   'U Use
      UseIt Info, Info2, x, y
      Prompt
   END SELECT
   ELSE
   Choice$ = RIGHT$(Choice$, 1)
   SELECT CASE Choice$
   CASE CHR$(DOWN)
      IF Info.Mode = 3 THEN DungScroll Info, Info2, "BackUp", x, y ELSE MapScroll Info, Info2, "Down", x, y
   CASE CHR$(UP)
      IF Info.Mode = 3 THEN DungScroll Info, Info2, "Forward", x, y ELSE MapScroll Info, Info2, "Up", x, y
   CASE CHR$(Right)
      IF Info.Mode = 3 THEN DungScroll Info, Info2, "TurnRight", x, y ELSE MapScroll Info, Info2, "Right", x, y
   CASE CHR$(LEFT)
      IF Info.Mode = 3 THEN DungScroll Info, Info2, "TurnLeft", x, y ELSE MapScroll Info, Info2, "Left", x, y
   END SELECT
   END IF
LOOP

ChangeColor:
   IF Misc33 = 1 THEN
      PALETTE 3, 7
      PALETTE 5, 0
      Misc33 = 2
      RETURN
   END IF
   IF Misc33 = 2 THEN
      PALETTE 3, 0
      PALETTE 5, 7
      Misc33 = 1
      RETURN
   END IF
   RETURN

WaterMove:
   IF Misc3 = 1 THEN
      PALETTE 11, 0
      PALETTE 14, 0
      PALETTE 12, 0
      PALETTE 13, 9
      Misc3 = 2
      RETURN
   END IF
   IF Misc3 = 2 THEN
      PALETTE 13, 0
      PALETTE 11, 9
      PALETTE 14, 0
      PALETTE 12, 0
      Misc3 = 3
      RETURN
   END IF
   IF Misc3 = 3 THEN
      PALETTE 13, 0
      PALETTE 11, 0
      PALETTE 14, 9
      PALETTE 12, 0
      Misc3 = 4
      RETURN
   END IF
   IF Misc3 = 4 THEN
      PALETTE 13, 0
      PALETTE 11, 0
      PALETTE 14, 0
      PALETTE 12, 9
      Misc3 = 1
      RETURN
   END IF
   RETURN

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

SUB ArdraddMove (Info AS Chrinfo, Info2 AS Chrinfo2, x, y)
   Num = 1
   Misc = 1
      IF Mon(Num).ID = " " AND Info.Cit = "05" THEN
         GET #9, 13, Mon(Num)
         Mon(Num).x = 35
         Mon(Num).y = 11
      ELSEIF y < 16 THEN
         tx = 73
         ty = 73
         qx = 73 + (16 * (Mon(Num).x - x))
         qy = 73 + (16 * (Mon(Num).y - y))
         Prompt2
         x3 = qx
         y3 = qy
         x4 = qx
         y4 = qy
         IF qx - tx = 0 THEN s = 0 ELSE s = (qy - ty) / (qx - tx + C)
         my3 = s * x3
         B = y3 - my3
         DO
            y3 = s * x3 + B
            IF s = 0 THEN y4 = qy: y3 = ty
            IF ABS(y4 - y3) >= 1 THEN
               DO
                  PUT (x4, y4), Bubble, XOR
                  FOR Misc12 = 1 TO 15
                  NEXT
                  PUT (x4, y4), Bubble, XOR
                  IF qy < ty THEN
                     y4 = y4 + 1
                  ELSEIF qy > ty THEN
                     y4 = y4 - 1
                  END IF
               LOOP UNTIL INT(y4) = INT(y3)
            END IF
            x4 = x3
            y4 = y3
            PUT (x3, y3), Bubble, XOR
            FOR Misc12 = 1 TO 15
            NEXT
            PUT (x3, y3), Bubble, XOR
            IF qx < tx THEN
               x3 = x3 + 1
            ELSEIF qx > tx THEN
               x3 = x3 - 1
            END IF
         LOOP UNTIL x3 = tx
         Misc6 = INT(RND * 25 + 2) + 5
         IF Info.Cit = "05" AND Info.Quest7 < 69 THEN
            Info.pcHP = 0
            Prompt2
            PRINT "'Good bye, ";
            Prompt2
            PRINT "foolish mortal!',";
            Prompt2
            PRINT "says Lord Ardradd";
            EXIT SUB
         ELSEIF Misc6 > Info.pcDex THEN
            Info.pcHP = Info.pcHP - Mon(Num).Damage + 10
            PRINT "Ardradd Hits!  ";
            PUT (qx, qy), Tiles(765), XOR
            FOR Misc12 = 1 TO 30
            NEXT
            PUT (qx, qy), Tiles(765), XOR
            MainStats Info, Info2
         ELSE
            PRINT "Ardradd Misses!";
         END IF
      END IF
END SUB

SUB Attack (Info AS Chrinfo, Info2 AS Chrinfo2, x, y) STATIC
   PRINT "Attack with";
   Prompt2
   IF Info.Mode = 2 THEN
      PRINT "Cannons";
   ELSEIF Info.ReadWeapon1 = 26 THEN
      PRINT "Hands";
   ELSE
      ReadData Info.ReadWeapon1
   END IF
   PRINT ":  ";
   Misc = Info.ReadWeapon1 + 15
   IF Info.ReadWeapon1 = 26 THEN Misc = 40
   IF Info.Mode = 2 THEN Misc = 42
   GET #9, Misc, Weap
   tx = x
   ty = y
   PUT (73 + (16 * (tx - x)), 73 + (16 * (ty - y))), Bubble, XOR
   Misc55 = 1
   DO
      DO
         Choice$ = INKEY$
      LOOP WHILE Choice$ = ""
      IF LEN(Choice$) = 1 THEN
      SELECT CASE ASC(Choice$)
      CASE 13, 65, 97
         PUT (73 + (16 * (tx - x)), 73 + (16 * (ty - y))), Bubble, XOR
         Misc55 = 2
      CASE 27, 32
         PRINT "NOT!!!";
         PUT (73 + (16 * (tx - x)), 73 + (16 * (ty - y))), Bubble, XOR
         MapScroll Info, Info2, "Mdraw", x, y
         EXIT SUB
      END SELECT
      ELSE
      Choice$ = RIGHT$(Choice$, 1)
      SELECT CASE Choice$
      CASE CHR$(UP)
         PUT (73 + (16 * (tx - x)), 73 + (16 * (ty - y))), Bubble, XOR
         ty = ty - 1
         IF ABS(ty - y) > Weap.AttNum THEN ty = ty + 1
         PUT (73 + (16 * (tx - x)), 73 + (16 * (ty - y))), Bubble, XOR
      CASE CHR$(DOWN)
         PUT (73 + (16 * (tx - x)), 73 + (16 * (ty - y))), Bubble, XOR
         ty = ty + 1
         IF ABS(ty - y) > Weap.AttNum THEN ty = ty - 1
         PUT (73 + (16 * (tx - x)), 73 + (16 * (ty - y))), Bubble, XOR
      CASE CHR$(LEFT)
         PUT (73 + (16 * (tx - x)), 73 + (16 * (ty - y))), Bubble, XOR
         tx = tx - 1
         IF ABS(tx - x) > Weap.AttNum THEN tx = tx + 1
         PUT (73 + (16 * (tx - x)), 73 + (16 * (ty - y))), Bubble, XOR
      CASE CHR$(Right)
         PUT (73 + (16 * (tx - x)), 73 + (16 * (ty - y))), Bubble, XOR
         tx = tx + 1
         IF ABS(tx - x) > Weap.AttNum THEN tx = tx - 1
         PUT (73 + (16 * (tx - x)), 73 + (16 * (ty - y))), Bubble, XOR
      END SELECT
      END IF
   LOOP WHILE Misc55 = 1
      Num = 1
      DO
         IF tx = Mon(Num).x AND ty = Mon(Num).y THEN Num = Num: GOTO AttTry
         Num = Num + 1
      LOOP UNTIL Num = MaxDim + 1
AttTry:
      qx = 73
      qy = 73
      tx = 73 + (16 * (tx - x))
      ty = 73 + (16 * (ty - y))
      x3 = qx
      y3 = qy
      x4 = qx
      y4 = qy
      IF qx - tx = 0 THEN s = 0 ELSE s = (qy - ty) / (qx - tx + C)
      my3 = s * x3
      B = y3 - my3
      Misc6 = INT(RND * 20 + 1 + (Info.pcDex / 10))
      DO
         y3 = s * x3 + B
         IF s = 0 THEN y4 = qy: y3 = ty
         IF ABS(y4 - y3) >= 1 THEN
            DO
               PUT (x4, y4), Bubble, XOR
               FOR Misc12 = 1 TO 15
               NEXT
               PUT (x4, y4), Bubble, XOR
               IF qy < ty THEN
                  y4 = y4 + 1
               ELSEIF qy > ty THEN
                  y4 = y4 - 1
               END IF
            LOOP UNTIL INT(y4) = INT(y3)
         END IF
         x4 = x3
         y4 = y3
         PUT (x3, y3), Bubble, XOR
         FOR Misc12 = 1 TO 15
         NEXT
         PUT (x3, y3), Bubble, XOR
         IF x3 <= tx THEN x5 = INT((x3 - 9) / 16 + 1)
         IF x3 > tx THEN x5 = ABS(INT(-(x3 - 9) / 16 - 1))
         IF y3 <= ty THEN y5 = INT((y3 - 9) / 16)
         IF y3 > ty THEN y5 = ABS(INT(-(y3 - 9) / 16))
         tt = 9 * y5 + x5
         IF ScreenTiles(tt) = "D" THEN Misc6 = 9999: EXIT DO
         IF qx < tx THEN
            x3 = x3 + 1
         ELSEIF qx > tx THEN
            x3 = x3 - 1
         END IF
      LOOP UNTIL x3 = tx
      IF Num = MaxDim + 1 THEN Misc6 = 9999
      IF Misc6 < Info.pcDex THEN
         Mon(Num).HP = Mon(Num).HP - Weap.Damage - INT(25 - Info.pcStr)
         PRINT "Hit!!!";
         PUT (qx, qy), Tiles(765), XOR
         FOR Misc12 = 1 TO 30
         NEXT
         PUT (qx, qy), Tiles(765), XOR
         IF Mon(Num).HP <= 0 THEN
            IF Info.Mode <> 2 THEN Info.pcExp = Info.pcExp + Mon(Num).Expr ELSE Info.pcExp = Info.pcExp + INT(Mon(Num).Expr / 4)
            IF Info.Mode <> 2 THEN Info.pcGold = Info.pcGold + INT(Mon(Num).Expr / 10)
            Mon(Num).x = 0
            Mon(Num).y = 0
            Mon(Num).HP = 0
            Mon(Num).Damage = 0
            Mon(Num).Expr = 0
            Mon(Num).AttNum = 0
            Mon(Num).RoundNum = 0
            IF Info.Cit = "05" THEN
               Prompt2
               Prompt2
               PRINT "Lord Ardradd is";
               Prompt2
               PRINT "Dead!!!";
               Info.Quest7 = 99
               Prompt2
               Prompt2
               PRINT "Press a key...";
               a$ = INPUT$(1)
               Prompt2
               CastingEffect2
               CastingEffect2
               CastingEffect3
               MapScroll Info, Info2, "Special", x, y
               CastingEffect4
               MainStats Info, Info2
               ClearScreen
               SHELL "QQ1 ±²³"
               SCREEN 0
               WIDTH 80, 25
               PRINT "Be " + "on " + "then " + "look " + "out " + "for" + ":"
               PRINT
               PRINT "Qu" + "ikk" + " Q" + "ue" + "st" + " 2:" + "  " + "En" + "tr" + "an" + "ce " + "Do" + "wnU" + "nd" + "er"
               END
            ELSE
               Prompt2
               Prompt2
               Misc6 = INT(RND * 25 + 1)
               Liner = ASC(Mon(Num).ID) - 64
               GET #4, Liner, Misc10
               a$ = UCASE$(RTRIM$(LEFT$(RIGHT$(Misc10.Misc0, 8), 1))) + RTRIM$(RIGHT$(Misc10.Misc0, 7))
               a = LEN(a$)
               IF a$ <> "Skeleton" THEN MID$(a$, a) = " " ELSE a$ = "Skeleton "
               PRINT a$; "is Dead!!";
               Mon(Num).ID = ""
            END IF
         END IF
         MapScroll Info, Info2, "Mdraw", x, y
         MainStats Info, Info2
      ELSE
         PRINT "Miss!";
         MapScroll Info, Info2, "Mdraw", x, y
      END IF
END SUB

SUB DungScroll (Info AS Chrinfo, Info2 AS Chrinfo2, Direction$, x, y) STATIC
   IF Direction$ = "Forward" THEN
      IF Compass = 1 THEN
         Tempx = x - 4
         Tempy = y - 5
      ELSEIF Compass = 2 THEN
         Tempx = x - 3
         Tempy = y - 4
      ELSEIF Compass = 3 THEN
         Tempx = x - 4
         Tempy = y - 3
      ELSEIF Compass = 4 THEN
         Tempx = x - 5
         Tempy = y - 4
      END IF
      GOTO Body2
   END IF
   IF Direction$ = "BackUp" THEN
      IF Compass = 3 THEN
         Tempx = x - 4
         Tempy = y - 5
      ELSEIF Compass = 4 THEN
         Tempx = x - 3
         Tempy = y - 4
      ELSEIF Compass = 1 THEN
         Tempx = x - 4
         Tempy = y - 3
      ELSEIF Compass = 2 THEN
         Tempx = x - 5
         Tempy = y - 4
      END IF
      GOTO Body2
   END IF
   IF Direction$ = "TurnLeft" THEN
      Compass = Compass - 1
      IF Compass = 0 THEN Compass = 4
      DungScroll Info, Info2, "Redraw", x, y
      EXIT SUB
   END IF
   IF Direction$ = "TurnRight" THEN
      Compass = Compass + 1
      IF Compass = 5 THEN Compass = 1
      DungScroll Info, Info2, "Redraw", x, y
      EXIT SUB
   END IF
   IF Direction$ = "Redraw" THEN
      Tempx = x - 4
      Tempy = y - 4
      GOTO Body2
   END IF
   IF Direction$ = "Special" THEN
      Tempx = x - 4
      Tempy = y - 4
      GOTO Body2
   END IF
 
Body2:
   IF Direction$ <> "Redraw" AND Direction$ <> "Special" AND Direction$ <> "TurnLeft" AND Direction$ <> "TurnRight" THEN
      IF Compass = 1 THEN PRINT "North ";
      IF Compass = 2 THEN PRINT "East ";
      IF Compass = 3 THEN PRINT "South ";
      IF Compass = 4 THEN PRINT "West ";
   END IF
   GOSUB GetThem2
   SELECT CASE Info.UnderTile
   CASE "D"
      PRINT "Blocked!";
      Misc2 = 1
   CASE "P"
      Misc2 = 1
      PRINT "Talk!";
      IF x = 21 AND y = 22 THEN
         Prompt2
         PRINT "Kelemore says";
         Prompt2
         IF Info.Quest1 < 67 THEN
            PRINT "If you find me my";
            Prompt2
            PRINT "Sword I will give";
            Prompt2
            PRINT "you the Magic Gem.";
         ELSEIF Info.Quest1 < 69 THEN
            PRINT "Thanks for finding";
            Prompt2
            PRINT "my Sword here is";
            Prompt2
            PRINT "the Magic Gem.";
            Info.Quest1 = 69
         ELSE
            PRINT "Good Luck!";
         END IF
      END IF
      IF x = 41 AND y = 38 THEN
         Prompt2
         PRINT "James says";
         Prompt2
         IF Info.Quest6 < 67 THEN
            PRINT "If you find me ";
            Prompt2
            PRINT "some keys I will";
            Prompt2
            PRINT "give you the Staff";
            Prompt2
            PRINT "of Death to kill";
            Prompt2
            PRINT "Evil Jester #2";
         ELSEIF Info.Quest6 < 69 THEN
            PRINT "Thanks for getting";
            Prompt2
            PRINT "me some keys to";
            Prompt2
            PRINT "get out of my";
            Prompt2
            PRINT "shackles. Here is";
            Prompt2
            PRINT "the Staff like I";
            Prompt2
            PRINT "promised.";
            Info.Quest6 = 69
         ELSE
            PRINT "Good Luck!";
         END IF
      END IF
   CASE ELSE
      Misc2 = 2
   END SELECT
   IF Misc2 = 1 THEN
      Tempx = x - 4
      Tempy = y - 4
      GOSUB GetThem2
      GOTO Renew2
   END IF
   x = Tempx + 4
   y = Tempy + 4
   SCREEN 7, , 1, 0
   CLS
   IF Compass = 1 THEN
      DungTiles(1) = ScreenTiles(40)
      DungTiles(2) = ScreenTiles(41)
      DungTiles(3) = ScreenTiles(42)
      DungTiles(4) = ScreenTiles(31)
      DungTiles(5) = ScreenTiles(32)
      DungTiles(6) = ScreenTiles(33)
      DungTiles(7) = ScreenTiles(22)
      DungTiles(8) = ScreenTiles(23)
      DungTiles(9) = ScreenTiles(24)
      DungTiles(10) = ScreenTiles(14)
   ELSEIF Compass = 2 THEN
      DungTiles(1) = ScreenTiles(32)
      DungTiles(2) = ScreenTiles(41)
      DungTiles(3) = ScreenTiles(50)
      DungTiles(4) = ScreenTiles(33)
      DungTiles(5) = ScreenTiles(42)
      DungTiles(6) = ScreenTiles(51)
      DungTiles(7) = ScreenTiles(34)
      DungTiles(8) = ScreenTiles(43)
      DungTiles(9) = ScreenTiles(52)
      DungTiles(10) = ScreenTiles(44)
   ELSEIF Compass = 3 THEN
      DungTiles(1) = ScreenTiles(42)
      DungTiles(2) = ScreenTiles(41)
      DungTiles(3) = ScreenTiles(40)
      DungTiles(4) = ScreenTiles(51)
      DungTiles(5) = ScreenTiles(50)
      DungTiles(6) = ScreenTiles(49)
      DungTiles(7) = ScreenTiles(60)
      DungTiles(8) = ScreenTiles(59)
      DungTiles(9) = ScreenTiles(58)
      DungTiles(10) = ScreenTiles(68)
   ELSEIF Compass = 4 THEN
      DungTiles(1) = ScreenTiles(50)
      DungTiles(2) = ScreenTiles(41)
      DungTiles(3) = ScreenTiles(32)
      DungTiles(4) = ScreenTiles(49)
      DungTiles(5) = ScreenTiles(40)
      DungTiles(6) = ScreenTiles(31)
      DungTiles(7) = ScreenTiles(48)
      DungTiles(8) = ScreenTiles(39)
      DungTiles(9) = ScreenTiles(30)
      DungTiles(10) = ScreenTiles(38)
   END IF
   DungTiles(2) = Info.UnderTile
   IF DungTiles(1) = "D" THEN
      DRAW "C7 S7 BM76,76 BH27 BR59 BD59 BL59 NG11 BU59 NH11"
   ELSE
      DRAW "C7 S7 BM76,76 BH27 NL11 D59 L11"
   END IF
   IF DungTiles(3) = "D" THEN
      DRAW "C7 S7 BM76,76 BH27 BR59 NE11 BD59 NF11 BL59 BU59"
   ELSE
      DRAW "C7 S7 BM75,76 BR6 BE27 NR11 D59 R11"
   END IF
   IF DungTiles(5) = "D" AND DungTiles(1) = "D" AND DungTiles(3) = "D" THEN
      DRAW "C7 S7 BM76,76 BH27 R59 D59 L59 U59"
   ELSEIF DungTiles(5) = "D" AND DungTiles(1) <> "D" AND DungTiles(3) <> "D" THEN
      DRAW "C7 S7 BM76,76 BH27 R59 C0 D59 C7 L59 U1 C0 U58 U1 C7 R59"
   ELSEIF DungTiles(5) = "D" AND DungTiles(1) = "D" AND DungTiles(3) <> "D" THEN
      DRAW "C7 S7 BM76,76 BH27 R59 C0 D59 C7 L59 U59 R59"
   ELSEIF DungTiles(5) = "D" AND DungTiles(1) <> "D" AND DungTiles(3) = "D" THEN
      DRAW "C7 S7 BM76,76 BH27 R59 D59 L59 U1 C0 U58"
   END IF
   IF DungTiles(2) = "E" THEN
      DRAW "C7 S7 BM45,10 R40 G10 L21 H10 R10 D1 C6 D5 NR20 D20 NR20 D20 ND10 R21 ND10 U45"
   END IF
   IF DungTiles(2) = "F" THEN
      DRAW "C7 S7 BM115,150 TA180 R40 G10 L21 H10 R10 D1 C6 D5 NR20 D20 NR20 D20 ND10 R21 ND10 U45 TA0"
   END IF
   IF DungTiles(2) = "T" THEN
      DRAW "C6 S7 BM45,150 NU20 R20 E10 NU 20 G10 U20 NE10 L10 BD5 R2 D4 L3 U4 R2 BU5 L11 E10 NR20 D1 C0 G9"
   END IF
   IF DungTiles(5) = "D" THEN GOTO Skipping
  
   IF DungTiles(4) = "D" THEN
      DRAW "C7 S4 BM76,76 BH25 BR59 BD59 BL59 NG22 BU59 NH22"
   ELSE
      DRAW "C7 S4 BM76,76 BH25 NL21 D59 L22 ND22 NU81"
   END IF
   IF DungTiles(6) = "D" THEN
      DRAW "C7 S4 BM76,76 BH25 BR59 NE22 BD59 NF22 BL59 BU59"
   ELSE
      DRAW "C7 S4 BM76,76 BR9 BE25 NR21 D59 R22 ND22 NU81"
   END IF
   IF DungTiles(8) = "D" AND DungTiles(4) = "D" AND DungTiles(6) = "D" THEN
      DRAW "C7 S4 BM76,76 BH25 R59 D59 L59 U59"
   ELSEIF DungTiles(8) = "D" AND DungTiles(4) <> "D" AND DungTiles(6) <> "D" THEN
      DRAW "C7 S4 BM76,76 BH25 R59 D1 C0 D58 C7 L59 U1 C0 U57"
   ELSEIF DungTiles(8) = "D" AND DungTiles(4) = "D" AND DungTiles(6) <> "D" THEN
      DRAW "C7 S4 BM76,76 BH25 R59 D1 C0 D58 C7 L59 U59"
   ELSEIF DungTiles(8) = "D" AND DungTiles(4) <> "D" AND DungTiles(6) = "D" THEN
      DRAW "C7 S4 BM76,76 BH25 R59 D59 L59 U1 C0 U57"
   END IF
   IF DungTiles(5) = "E" THEN
      DRAW "C7 S4 BM62,40 R40 G10 L21 H10 R10 D1 C6 D5 NR20 D20 NR20 D20 ND10 R21 ND10 U45"
   END IF
   IF DungTiles(5) = "F" THEN
      DRAW "C7 S4 BM100,120 TA180 R40 G10 L21 H10 R10 D1 C6 D5 NR20 D20 NR20 D20 ND10 R21 ND10 U45 TA0"
   END IF
   IF DungTiles(5) = "T" THEN
      DRAW "C6 S4 BM62,120 NU20 R20 E10 NU 20 G10 U20 NE10 L10 BD5 R2 D4 L3 U4 R2 BU5 L11 E10 R20"
   END IF
   IF DungTiles(8) = "D" THEN GOTO Skipping
  
   IF DungTiles(7) = "D" THEN
      DRAW "C7 S2 BM76,76 BH20 BR59 BD59 BL59 NG29 BU59 NH29"
   ELSE
      DRAW "C7 S2 BM76,76 BH20 NL31 D59 L30 ND30 NU88"
   END IF
   IF DungTiles(9) = "D" THEN
      DRAW "C7 S2 BM76,76 BH20 BR59 NE29 BD59 NF29 BL59 BU59"
   ELSE
      DRAW "C7 S2 BM76,76 BR19 BE20 NR31 D59 R30 ND30 NU88"
   END IF
   IF DungTiles(10) = "D" AND DungTiles(7) = "D" AND DungTiles(9) = "D" THEN
      DRAW "C7 S2 BM76,76 BH20 R59 D59 L59 U59"
   ELSEIF DungTiles(10) = "D" AND DungTiles(7) <> "D" AND DungTiles(9) <> "D" THEN
      DRAW "C7 S2 BM76,76 BH20 R59 D2 C0 D57 C7 L59 U2 C0 U55"
   ELSEIF DungTiles(10) = "D" AND DungTiles(7) = "D" AND DungTiles(9) <> "D" THEN
      DRAW "C7 S2 BM76,76 BH20 R59 D2 C0 D57 C7 L59 U59"
   ELSEIF DungTiles(10) = "D" AND DungTiles(7) <> "D" AND DungTiles(9) = "D" THEN
      DRAW "C7 S2 BM76,76 BH20 R59 D59 L59 U2 C0 U55"
   END IF
   IF DungTiles(8) = "E" THEN
      DRAW "C7 S2 BM72,60 R40 G10 L21 H10 R10 D2 C6 D5 NR20 D20 NR20 D20 ND10 R21 ND10 U44" '
   END IF
   IF DungTiles(8) = "F" THEN
      DRAW "C7 S2 BM91,100 TA180 R40 G10 L21 H10 R10 D2 C6 D5 NR20 D20 NR20 D20 ND10 R21 ND10 U44 TA0"
   END IF
   IF DungTiles(8) = "T" THEN
      DRAW "C6 S2 BM72,100 NU20 R20 E10 NU 20 G10 U20 NE10 L10 BD5 R2 D4 L3 U4 R2 BU5 L12 E10 R20"
   END IF
   IF DungTiles(10) = "D" THEN GOTO Skipping
  
   DRAW "C7 S7 BM80,80 R1 NE8 D1 NF8 L1 NG8 U1 NH8"

Skipping:
   GET (9, 9)-(152, 152), ViewMapScr
   SCREEN 7, , 0, 0
   IF Direction$ = "Special" THEN EXIT SUB
   PUT (9, 9), ViewMapScr, PSET
Renew2:
   IF Info.pcHP <= 0 THEN RestoreLife Info, Info2, Location, x, y
   IF Direction$ <> "Redraw" AND Direction$ <> "Special" AND Direction$ <> "TurnLeft" AND Direction$ <> "TurnRight" THEN Prompt
   EXIT SUB

GetThem2:
   IF Tempx >= 71 THEN Tempx = 1
   IF Tempx <= 0 THEN Tempx = 70
   IF Tempy >= 71 THEN Tempy = 1
   IF Tempy <= 0 THEN Tempy = 70
   Liner = Tempy
   taba = 1
   Misc5 = 1
   DO
      GET #2, Liner, Latitude
      PlaceTempTiles Info, Info2, Tempx, Liner
      Misc2 = 0
      DO
         Misc4 = Tempx + Misc2
         IF Misc4 >= 71 THEN Misc4 = Misc4 - 70
         ScreenTiles(Misc5) = RIGHT$(LEFT$(Latitude, Misc4), 1)
         Misc2 = Misc2 + 1
         Misc5 = Misc5 + 1
      LOOP UNTIL Misc2 = 9
      Liner = Liner + 1
      taba = taba + 1
      IF Liner >= 71 THEN Liner = 1
      IF Liner <= 0 THEN Liner = 70
   LOOP UNTIL taba = 10
   Info.UnderTile = ScreenTiles(41)
   IF Info.pcRole = "Warrior " THEN ScreenTiles(41) = "P"
   IF Info.pcRole = "Wizard  " THEN ScreenTiles(41) = "O"
   IF Info.Mode = 2 THEN ScreenTiles(41) = "I"
   RETURN
END SUB

SUB GetMap (Info AS Chrinfo, Info2 AS Chrinfo2) STATIC
   IF Info.Cit = "00" THEN
      Misc1$ = Info.Worldy
      Num = 1
      DO
         Mon(Num).ID = ""
         Mon(Num).x = 0
         Mon(Num).y = 0
         Mon(Num).HP = 0
         Mon(Num).Damage = 0
         Mon(Num).Expr = 0
         Mon(Num).AttNum = 0
         Mon(Num).RoundNum = 0
         Num = Num + 1
      LOOP UNTIL Num = MaxDim + 1
   ELSE
      Misc1$ = Info.Cit
   END IF
   IF Info.Cit = "05" THEN
      Num = 1
      DO
         Mon(Num).ID = ""
         Mon(Num).x = 0
         Mon(Num).y = 0
         Mon(Num).HP = 0
         Mon(Num).Damage = 0
         Mon(Num).Expr = 0
         Mon(Num).AttNum = 0
         Mon(Num).RoundNum = 0
         Num = Num + 1
      LOOP UNTIL Num = MaxDim + 1
   END IF
   Misc1$ = "WORLD" + Misc1$ + ".MAP"
   CLOSE #2
   OPEN Misc1$ FOR RANDOM AS #2 LEN = LEN(Latitude)
END SUB

SUB LoadMapTiles (TileSet)
   a$ = "TILES" + LEFT$(MKI$(TileSet + 48), 1) + ".EGA"
   DEF SEG = VARSEG(Tiles(45))
   BLOAD a$, VARPTR(Tiles(45))
   DEF SEG
END SUB

SUB LookTile (Info AS Chrinfo, Info2 AS Chrinfo2, x, y)
   PRINT "Look....";
   tx = x
   ty = y
   PUT (73 + (16 * (tx - x)), 73 + (16 * (ty - y))), Bubble, XOR
   Misc55 = 1
   DO
      DO
         Choice$ = INKEY$
      LOOP WHILE Choice$ = ""
      IF LEN(Choice$) = 1 THEN
      SELECT CASE ASC(Choice$)
      CASE 13, 76, 108
         PUT (73 + (16 * (tx - x)), 73 + (16 * (ty - y))), Bubble, XOR
         Misc55 = 2
      CASE 27, 32
         PRINT "NOT!!!";
         PUT (73 + (16 * (tx - x)), 73 + (16 * (ty - y))), Bubble, XOR
         EXIT SUB
      END SELECT
      ELSE
      Choice$ = RIGHT$(Choice$, 1)
      SELECT CASE Choice$
      CASE CHR$(UP)
         PUT (73 + (16 * (tx - x)), 73 + (16 * (ty - y))), Bubble, XOR
         ty = ty - 1
         IF ABS(ty - y) > 4 THEN ty = ty + 1
         PUT (73 + (16 * (tx - x)), 73 + (16 * (ty - y))), Bubble, XOR
      CASE CHR$(DOWN)
         PUT (73 + (16 * (tx - x)), 73 + (16 * (ty - y))), Bubble, XOR
         ty = ty + 1
         IF ABS(ty - y) > 4 THEN ty = ty - 1
         PUT (73 + (16 * (tx - x)), 73 + (16 * (ty - y))), Bubble, XOR
      CASE CHR$(LEFT)
         PUT (73 + (16 * (tx - x)), 73 + (16 * (ty - y))), Bubble, XOR
         tx = tx - 1
         IF ABS(tx - x) > 4 THEN tx = tx + 1
         PUT (73 + (16 * (tx - x)), 73 + (16 * (ty - y))), Bubble, XOR
      CASE CHR$(Right)
         PUT (73 + (16 * (tx - x)), 73 + (16 * (ty - y))), Bubble, XOR
         tx = tx + 1
         IF ABS(tx - x) > 4 THEN tx = tx - 1
         PUT (73 + (16 * (tx - x)), 73 + (16 * (ty - y))), Bubble, XOR
      END SELECT
      END IF
   LOOP WHILE Misc55 = 1
   Prompt2
   IF ty > 70 THEN ty = ty - 70
   IF ty < 1 THEN ty = ty + 70
   IF tx > 70 THEN tx = tx - 70
   IF tx < 1 THEN tx = tx + 70
   GET #2, ty, Latitude
   PlaceTempTiles Info, Info2, tx, ty
   FOR a = 1 TO tx
      Tiler$ = MID$(Latitude, a, 1)
   NEXT
   Liner = ASC(Tiler$) - 64
   IF Info.Cit <> "00" THEN Liner = Liner + 26
   GET #4, Liner, Misc10
   Prompt2
   IF tx = x AND ty = y THEN Misc10.Misc0 = "Yourself!"
   PRINT "You see " + Misc10.Misc0;
END SUB

SUB MapScroll (Info AS Chrinfo, Info2 AS Chrinfo2, Direction$, x, y) STATIC
   IF Direction$ = "Up" THEN
      Tempx = x - 4
      Tempy = y - 5
      PRINT "North ";
      GOTO Body1
   END IF
   IF Direction$ = "Down" THEN
      Tempx = x - 4
      Tempy = y - 3
      PRINT "South ";
      GOTO Body1
   END IF
   IF Direction$ = "Left" THEN
      Tempx = x - 5
      Tempy = y - 4
      PRINT "West ";
      GOTO Body1
   END IF
   IF Direction$ = "Right" THEN
      Tempx = x - 3
      Tempy = y - 4
      PRINT "East ";
      GOTO Body1
   END IF
   IF Direction$ = "Special" OR Direction$ = "Redraw" OR Direction$ = "Mdraw" THEN
      Tempx = x - 4
      Tempy = y - 4
      GOTO Body1
   END IF
   EXIT SUB

Body1:
   GOSUB GetThem
   IF Info.Mode = 1 THEN
      IF Info.Cit <> "00" THEN
         SELECT CASE Info.UnderTile
         CASE "A", "C", "D", "H", "K" TO "P", "R", "U" TO "Z"
            PRINT "Blocked!";
            Misc2 = 1
         CASE "L", "S"
            Info.pcHP = Info.pcHP - 5
         CASE ELSE
            Misc2 = 2
         END SELECT
      ELSE
         SELECT CASE Info.UnderTile
         CASE "A", "D", "N", "O", "P", "U" TO "Z"
            PRINT "Blocked!";
            Misc2 = 1
         CASE "L", "S"
            Info.pcHP = Info.pcHP - 5
         CASE ELSE
            Misc2 = 2
         END SELECT
      END IF
   END IF
   IF Info.Mode = 2 THEN
      SELECT CASE Info.UnderTile
      CASE "A"
         Misc2 = 2
      CASE ELSE
         PRINT "Blocked!";
         Misc2 = 1
      END SELECT
   END IF
   IF Misc2 = 1 THEN
      Tempx = x - 4
      Tempy = y - 4
      GOSUB GetThem
      GOTO ReNew
   END IF
   x2 = x
   y2 = y
   x = Tempx + 4
   y = Tempy + 4
   IF Direction$ <> "Special" AND Direction$ <> "Redraw" AND Info.Cit = "00" THEN MonsterMove Info, Info2, x, y, x2, y2
   IF Direction$ <> "Special" AND Direction$ <> "Redraw" AND Info.Cit = "05" THEN ArdraddMove Info, Info2, x2, y2
   GOSUB GetThem
   C = 9
   a = C
   B = 9
   D% = 1
   SCREEN 7, , 1, 0
   PCOPY 0, 1
   DO
      Misc7 = (ASC(ScreenTiles(D%)) - 64) * 45
      PUT (a, B), Tiles(Misc7), PSET
      a = a + 16
      D% = D% + 1
      IF a >= C + (16 * 9) THEN B = B + 16: a = 9
   LOOP UNTIL D% = 82
   SCREEN 7, , 0, 0
   IF Direction$ = "Special" THEN EXIT SUB
   PCOPY 1, 0
ReNew:
   MainStats Info, Info2
   IF Info.pcHP <= 0 THEN RestoreLife Info, Info2, Location, x, y
   IF Direction$ <> "Redraw" AND Direction$ <> "Mdraw" THEN Prompt
   EXIT SUB

GetThem:
   IF Tempx >= 71 THEN Tempx = 1
   IF Tempx <= 0 THEN Tempx = 70
   IF Tempy >= 71 THEN Tempy = 1
   IF Tempy <= 0 THEN Tempy = 70
   Liner = Tempy
   taba = 1
   Misc5 = 1
   DO
      GET #2, Liner, Latitude
      PlaceTempTiles Info, Info2, Tempx, Liner
      Misc2 = 0
      DO
         Misc4 = Tempx + Misc2
         IF Misc4 >= 71 THEN Misc4 = Misc4 - 70
         ScreenTiles(Misc5) = RIGHT$(LEFT$(Latitude, Misc4), 1)
         Misc2 = Misc2 + 1
         Misc5 = Misc5 + 1
      LOOP UNTIL Misc2 = 9
      Liner = Liner + 1
      taba = taba + 1
      IF Liner >= 71 THEN Liner = 1
      IF Liner <= 0 THEN Liner = 70
   LOOP UNTIL taba = 10
   Info.UnderTile = ScreenTiles(41)
   IF Info.pcRole = "Warrior " THEN ScreenTiles(41) = "P"
   IF Info.pcRole = "Wizard  " THEN ScreenTiles(41) = "O"
   IF Info.Mode = 2 THEN ScreenTiles(41) = "I"
   RETURN
END SUB

SUB MonsterMove (Info AS Chrinfo, Info2 AS Chrinfo2, x, y, x2, y2)
   Num = 1
   Misc = 1
   DO
      Misc = 1
      Tempx = Mon(Num).x
      Tempy = Mon(Num).y
      IF Mon(Num).x = 0 AND Mon(Num).y = 0 THEN
         RANDOMIZE TIMER
         IF Info.pcLvl = 1 THEN Misc2 = INT(RND * 11 + 1)
         IF Info.pcLvl = 2 THEN Misc2 = INT(RND * 11 + 1)
         IF Info.pcLvl = 3 THEN Misc2 = INT(RND * 11 + 1)
         IF Info.pcLvl = 4 THEN Misc2 = INT(RND * 11 + 1)
         IF Info.pcLvl = 5 THEN Misc2 = INT(RND * 15 + 1)
         IF Info.pcLvl = 6 THEN Misc2 = INT(RND * 15 + 1)
         IF Info.pcLvl = 7 THEN Misc2 = INT(RND * 15 + 1)
         IF Info.pcLvl = 8 THEN Misc2 = INT(RND * 18 + 1)
         IF Info.pcLvl = 9 THEN Misc2 = INT(RND * 19 + 1)
         IF Info.pcLvl = 10 THEN Misc2 = INT(RND * 20 + 1)
         IF Misc2 > 10 THEN
            Misc9 = 0
            DO
               RANDOMIZE TIMER
               MiscX = INT((65 - 5 + 1) * RND + 5)
               MiscY = INT((65 - 5 + 1) * RND + 5)
               GET #2, MiscY, Latitude
               Misc1$ = RIGHT$(LEFT$(Latitude, MiscX), 1)
               IF Info.Worldy = "01" AND MiscX <> x AND MiscY <> y AND Misc1$ = "B" THEN Misc9 = 1
               IF Info.Worldy = "01" AND MiscX <> x AND MiscY <> y AND Misc1$ = "C" THEN Misc9 = 1
               IF Info.Worldy = "02" AND MiscX <> x AND MiscY <> y AND Misc1$ = "S" THEN Misc9 = 1
               IF Info.Cit = "05" AND MiscX <> x AND MiscY <> y AND Misc1$ = "Q" THEN Misc9 = 1
            LOOP UNTIL Misc9 = 1
         RANDOMIZE TIMER
         IF Info.Worldy = "01" THEN
            Misc4 = INT(RND * 6 + 1)
         ELSEIF Info.Cit = "00" AND Info.Worldy = "02" THEN
            Misc4 = INT(RND * 2 + 1)
            IF Misc4 = 1 THEN Misc4 = 6
            IF Misc4 = 2 THEN Misc4 = 1
         END IF
         IF Misc4 > 0 THEN GET #9, Misc4, Mon(Num)
            Mon(Num).x = MiscX
            Mon(Num).y = MiscY
         END IF
         Misc = 2
      ELSEIF Mon(Num).y = y AND ABS(Mon(Num).x - x) <= Mon(Num).AttNum THEN
         Misc = 5
      ELSEIF Mon(Num).x = x AND ABS(Mon(Num).y - y) <= Mon(Num).AttNum THEN
         Misc = 5
      ELSEIF ABS(Mon(Num).x - x) <= Mon(Num).AttNum AND ABS(Mon(Num).y - y) <= Mon(Num).AttNum THEN
         Misc = 5
      ELSEIF Misc = 1 AND Mon(Num).x = x THEN
         IF Mon(Num).y < y THEN Tempy = Mon(Num).y + 1
         IF Mon(Num).y > y THEN Tempy = Mon(Num).y - 1
      ELSEIF Misc = 1 AND Mon(Num).y = y THEN
         IF Mon(Num).x < x THEN Tempx = Mon(Num).x + 1
         IF Mon(Num).x > x THEN Tempx = Mon(Num).x - 1
      ELSEIF Misc = 1 THEN
         IF Mon(Num).x > x AND Mon(Num).y > y THEN
            RANDOMIZE TIMER
            Misc5 = INT(RND * 3 + 1)
            IF Misc5 = 1 THEN Tempx = Mon(Num).x - 1: Tempy = Mon(Num).y - 1
            IF Misc5 = 2 THEN Tempx = Mon(Num).x - 1: Tempy = Mon(Num).y
            IF Misc5 = 2 THEN Tempx = Mon(Num).x: Tempy = Mon(Num).y - 1
         ELSEIF Mon(Num).x < x AND Mon(Num).y < y THEN
            RANDOMIZE TIMER
            Misc5 = INT(RND * 3 + 1)
            IF Misc5 = 1 THEN Tempx = Mon(Num).x + 1: Tempy = Mon(Num).y + 1
            IF Misc5 = 2 THEN Tempx = Mon(Num).x + 1: Tempy = Mon(Num).y
            IF Misc5 = 2 THEN Tempx = Mon(Num).x: Tempy = Mon(Num).y + 1
         ELSEIF Mon(Num).x < x AND Mon(Num).y > y THEN
            RANDOMIZE TIMER
            Misc5 = INT(RND * 3 + 1)
            IF Misc5 = 1 THEN Tempx = Mon(Num).x + 1: Tempy = Mon(Num).y - 1
            IF Misc5 = 2 THEN Tempx = Mon(Num).x + 1: Tempy = Mon(Num).y
            IF Misc5 = 2 THEN Tempx = Mon(Num).x: Tempy = Mon(Num).y - 1
         ELSEIF Mon(Num).x > x AND Mon(Num).y < y THEN
            RANDOMIZE TIMER
            Misc5 = INT(RND * 3 + 1)
            IF Misc5 = 1 THEN Tempx = Mon(Num).x - 1: Tempy = Mon(Num).y + 1
            IF Misc5 = 2 THEN Tempx = Mon(Num).x - 1: Tempy = Mon(Num).y
            IF Misc5 = 2 THEN Tempx = Mon(Num).x: Tempy = Mon(Num).y + 1
         END IF
      END IF
      IF Misc = 1 THEN GET #2, Tempy, Latitude
      IF Misc = 1 THEN Misc1$ = RIGHT$(LEFT$(Latitude, Tempx), 1)
      IF Mon(Num).AttNum < 3 AND Misc1$ = "A" THEN Misc = 2
      IF Mon(Num).AttNum < 2 AND Misc1$ = "D" THEN Misc = 2
      IF Misc1$ = "N" THEN Misc = 2
      IF Misc1$ = "P" THEN Misc = 2
      IF Misc1$ = "O" THEN Misc = 2
      IF Misc1$ = "U" THEN Misc = 2
      IF Misc1$ = "V" THEN Misc = 2
      IF Misc1$ = "W" THEN Misc = 2
      IF Misc1$ = "X" THEN Misc = 2
      IF Misc1$ = "Y" THEN Misc = 2
      IF Misc1$ = "Z" THEN Misc = 2
      IF Misc = 1 THEN
         Misc3 = 1
         DO
            IF Tempx = Mon(Misc3).x AND Tempy = Mon(Misc3).y THEN Misc = 2
            Misc3 = Misc3 + 1
         LOOP UNTIL Misc3 = MaxDim + 1
      END IF
      IF Tempx = x AND Tempy = y THEN Misc = 2
      IF Misc = 1 THEN Mon(Num).x = Tempx: Mon(Num).y = Tempy
      Mon(Num).RoundNum = Mon(Num).RoundNum + 1
      IF Mon(Num).RoundNum > 90 - Info.pcLvl AND Mon(Num).x <= x - 10 AND Mon(Num).y <= y - 10 AND Mon(Num).x >= x + 10 AND Mon(Num).y >= y + 10 THEN
         Mon(Num).ID = ""
         Mon(Num).x = 0
         Mon(Num).y = 0
         Mon(Num).HP = 0
         Mon(Num).Damage = 0
         Mon(Num).Expr = 0
         Mon(Num).AttNum = 0
         Mon(Num).RoundNum = 0
      END IF
      IF Misc = 5 THEN
         tx = 73
         ty = 73
         qx = 73 + (16 * (Mon(Num).x - x2))
         qy = 73 + (16 * (Mon(Num).y - y2))
         x3 = qx
         y3 = qy
         x4 = qx
         y4 = qy
         IF qx - tx = 0 THEN s = 0 ELSE s = (qy - ty) / (qx - tx + C)
         my3 = s * x3
         B = y3 - my3
         DO
            y3 = s * x3 + B
            IF s = 0 THEN y4 = qy: y3 = ty
            IF ABS(y4 - y3) >= 1 THEN
               DO
                  PUT (x4, y4), Bubble, XOR
                  FOR Misc12 = 1 TO 15
                  NEXT
                  PUT (x4, y4), Bubble, XOR
                  IF qy < ty THEN
                     y4 = y4 + 1
                  ELSEIF qy > ty THEN
                     y4 = y4 - 1
                  END IF
               LOOP UNTIL INT(y4) = INT(y3)
            END IF
            x4 = x3
            y4 = y3
            PUT (x3, y3), Bubble, XOR
            FOR Misc12 = 1 TO 15
            NEXT
            PUT (x3, y3), Bubble, XOR
            IF qx < tx THEN
               x3 = x3 + 1
            ELSEIF qx > tx THEN
               x3 = x3 - 1
            END IF
         LOOP UNTIL x3 = tx
         Misc6 = INT(RND * 25 + 1)
         Liner = ASC(Mon(Num).ID) - 64
         GET #4, Liner, Misc10
         Prompt2
         a$ = UCASE$(RTRIM$(LEFT$(RIGHT$(Misc10.Misc0, 8), 1))) + RTRIM$(RIGHT$(Misc10.Misc0, 7))
         a = LEN(a$)
         IF a$ <> "Skeleton" THEN MID$(a$, a) = " " ELSE a$ = "Skeleton "
         PRINT a$;
         IF Misc6 > Info.pcDex THEN
            IF Mon(Num).ID = "V" THEN
               PRINT "Steals!";
               Info.pcGold = Info.pcGold - INT(RND * 45 + 5)
               MainStats Info, Info2
            ELSE
               Info.pcHP = Info.pcHP - Mon(Num).Damage - INT(25 - Info.pcStr)
               PRINT "Hits!";
               PUT (qx, qy), Tiles(765), XOR
               FOR Misc12 = 1 TO 30
               NEXT
               PUT (qx, qy), Tiles(765), XOR
               MainStats Info, Info2
               EXIT SUB
            END IF
         ELSE
            PRINT "Misses!";
         END IF
      END IF
      Num = Num + 1
   LOOP UNTIL Num = MaxDim + 1
END SUB

SUB PlaceTempTiles (Info AS Chrinfo, Info2 AS Chrinfo2, x, y) STATIC
      IF Info.Cit = "00" THEN
         Num = 1
         DO
            IF y = Mon(Num).y THEN
               Misc1$ = Mon(Num).ID
               qx = Mon(Num).x
               IF qx > 70 THEN qx = qx - 70
               GOSUB Place
            END IF
            Num = Num + 1
         LOOP UNTIL Num = MaxDim + 1
      END IF
      IF Info.Worldy = "01" AND Info.Cit = "00" THEN
         Misc8 = Info2.NumShips
         FOR Misc3 = 1 TO INT(Info2.NumShips / 10)
            Misc8 = Misc8 - 10
         NEXT
         Misc1$ = "I"
         IF y = Info2.ShipY AND Misc8 <> 1 THEN
            qx = Info2.Shipx
            IF qx > 70 THEN qx = qx - 70
            GOSUB Place
         END IF
         IF y = Info2.ShipY2 AND Misc8 <> 2 THEN
            qx = Info2.ShipX2
            IF qx > 70 THEN qx = qx - 70
            GOSUB Place
         END IF
         IF y = Info2.ShipY3 AND Misc8 <> 3 THEN
            qx = Info2.ShipX3
            IF qx > 70 THEN qx = qx - 70
            GOSUB Place
         END IF
         IF y = Info2.ShipY4 AND Misc8 <> 4 THEN
            qx = Info2.ShipX4
            IF qx > 70 THEN qx = qx - 70
            GOSUB Place
         END IF
         IF y = 4 AND Info.Quest3 = 99 THEN
            Misc1$ = "B"
            qx = 38
            GOSUB Place
         END IF
      ELSEIF Info.Cit = "04" THEN
         IF y = 63 AND Info.Quest4 < 69 THEN
            Misc1$ = "V"
            qx = 34
            GOSUB Place
         END IF
         IF y = 62 AND Info.Quest4 < 89 THEN
            Misc1$ = "T"
            qx = 26
            GOSUB Place
         END IF
      ELSEIF Info.Cit = "05" THEN
         IF y = 58 AND Info.Quest6 < 99 THEN
            Misc1$ = "V"
            qx = 35
            GOSUB Place
         END IF
         IF y = 11 AND Info.Quest7 < 99 THEN
            Misc1$ = "U"
            qx = 35
            GOSUB Place
         END IF
      ELSEIF Info.Cit = "08" THEN
         IF y = 56 AND Info.Quest5 < 67 THEN
            Misc1$ = "T"
            qx = 51
            GOSUB Place
         END IF
      ELSEIF Info.Cit = "L3" THEN
         IF y = 47 AND Info.Quest7 < 67 THEN
            Misc1$ = "T"
            qx = 8
            GOSUB Place
         END IF
         IF y = 40 AND Info.Quest2 < 67 THEN
            Misc1$ = "T"
            qx = 49
            GOSUB Place
         END IF
         IF y = 37 AND Info.Quest6 < 67 THEN
            Misc1$ = "T"
            qx = 25
            GOSUB Place
         END IF
      ELSEIF Info.Cit = "L4" THEN
         IF y = 25 AND Info.Quest1 < 67 THEN
            Misc1$ = "T"
            qx = 24
            GOSUB Place
         END IF
         IF y = 39 AND Info.Quest3 < 67 THEN
            Misc1$ = "T"
            qx = 41
            GOSUB Place
         END IF
         IF y = 64 AND Info.Quest4 < 67 THEN
            Misc1$ = "T"
            qx = 33
            GOSUB Place
         END IF
         IF y = 11 AND Info.Quest7 = 99 THEN
            Misc1$ = "Q"
            qx = 35
            GOSUB Place
         END IF
      END IF
   EXIT SUB
Place:
   Lx$ = LEFT$(Latitude, qx - 1)
   Ly$ = RIGHT$(Latitude, ABS(70 - qx))
   Latitude = Lx$ + Misc1$ + Ly$
   RETURN
END SUB

SUB QuitNSave (Info AS Chrinfo, Info2 AS Chrinfo2, x, y)
      PRINT "Save Game?";
      DO
         Misc1$ = UCASE$(INPUT$(1))
         IF Misc1$ = "Y" THEN
            PRINT "Y";
            Info.x = x
            Info.y = y
            PUT #1, 1, Info
            PUT #1, 2, Info2
            EXIT DO
         END IF
         IF Misc1$ = "N" THEN PRINT "N"; : EXIT DO
      LOOP
      Prompt2
      PRINT "Exit to DOS?";
      DO
         Misc1$ = UCASE$(INPUT$(1))
         IF Misc1$ = "Y" THEN PRINT "Y"; : Ending
         IF Misc1$ = "N" THEN PRINT "N"; : EXIT DO
      LOOP
END SUB

SUB SpellList (Info AS Chrinfo, Info2 AS Chrinfo2, x, y) STATIC
PRINT "Spell List:";
Choice = 1
TextScroll 13, 21, 25, 40, 12, 0
start:
   a$ = "Exit Spell List"
   DIM list$(11)
   list$(1) = " Light Heal"
   list$(2) = " Light Wound"
   list$(3) = " Great Heal"
   list$(4) = " Great Wound"
   list$(5) = " Great Tremor"
   list$(6) = " Great Blink"
   list$(7) = " Fire Ball"
   list$(8) = " Dungeon Exit"
   list$(9) = " Create Food"
   list$(10) = " Change Money"
   list$(11) = " Exit List"
   CALL MenuBar(list$(), 15, 22, 15, 0, 4, 0, Choice)
   Prompt2
   IF Choice = 1 THEN
      IF Info.pcMP < 2 THEN a$ = "Not Enough MP!": GOTO ExitList
      CastingEffect2
      FOR a = 1 TO INT(RND * 20 + 5)
         Info.pcHP = Info.pcHP + 1
         MainStats Info, Info2
      NEXT
      IF Info.pcHP > Info.pcMHP THEN Info.pcHP = Info.pcMHP
      Info.pcMP = Info.pcMP - 2
      MainStats Info, Info2
      CastingEffect2
      a$ = "Spell successful!"
   END IF
   IF Choice = 2 THEN
      IF Info.pcMP < 5 THEN a$ = "Not Enough MP!": GOTO ExitList
      CastingEffect2
      Info.pcMP = Info.pcMP - 5
      Num = 1
      DO
         IF Mon(Num).ID <> " " THEN Mon(Num).HP = Mon(Num).HP - 10
         IF Mon(Num).HP <= 0 THEN
            Mon(Num).ID = ""
            Mon(Num).x = 0
            Mon(Num).y = 0
            Mon(Num).HP = 0
            Mon(Num).Damage = 0
            Mon(Num).Expr = 0
            Mon(Num).AttNum = 0
            Mon(Num).RoundNum = 0
         END IF
         Num = Num + 1
      LOOP UNTIL Num = MaxDim + 1
      MainStats Info, Info2
      CastingEffect2
      a$ = "Spell successful!"
   END IF
   IF Choice = 3 THEN
      IF Info.pcMP < 5 THEN a$ = "Not Enough MP!": GOTO ExitList
      CastingEffect2
      FOR a = 1 TO INT(Info.pcMHP / 4) STEP 10
         Info.pcHP = Info.pcHP + 10
         MainStats Info, Info2
      NEXT
      Info.pcMP = Info.pcMP - 5
      MainStats Info, Info2
      CastingEffect2
      a$ = "Spell successful!"
   END IF
   IF Choice = 4 THEN
      IF Info.pcMP < 10 THEN a$ = "Not Enough MP!": GOTO ExitList
      CastingEffect2
      Info.pcMP = Info.pcMP - 10
      Num = 1
      DO
         IF Mon(Num).ID <> " " THEN Mon(Num).HP = Mon(Num).HP - INT((25 - 15 + 1) + RND * 15 + 2)
         IF Mon(Num).HP <= 0 THEN
            Mon(Num).ID = ""
            Mon(Num).x = 0
            Mon(Num).y = 0
            Mon(Num).HP = 0
            Mon(Num).Damage = 0
            Mon(Num).Expr = 0
            Mon(Num).AttNum = 0
            Mon(Num).RoundNum = 0
         END IF
         Num = Num + 1
      LOOP UNTIL Num = MaxDim + 1
      MainStats Info, Info2
      CastingEffect2
      a$ = "Spell successful!"
   END IF
   IF Choice = 7 THEN
      IF Info.pcMP < 10 THEN a$ = "Not Enough MP!": GOTO ExitList
      CastingEffect2
      Info.pcMP = Info.pcMP - 10
      MainStats Info, Info2
      CastingEffect2
      r = .5
      DO
         th = (r + COS(r) / 100) * .2
         zx = 73 + (r * COS(th))
         zy = 73 + (r * SIN(th))
         zx2 = 73 + -1 * (r * COS(th))
         zy2 = 73 + -1 * (r * SIN(th))
         PUT (zx, zy), Bubble, XOR
         PUT (zx2, zy2), Bubble, XOR
         FOR Misc12 = 1 TO 50
         NEXT
         PUT (zx, zy), Bubble, XOR
         PUT (zx2, zy2), Bubble, XOR
         r = r + .5
      LOOP UNTIL INT(zx) <= 9 OR INT(zx) >= 137 OR INT(zy) <= 9 OR INT(zy) >= 137
      Num = 1
      DO
         IF Mon(Num).x >= x - 4 AND Mon(Num).y >= y - 4 AND Mon(Num).x <= x + 4 AND Mon(Num).y <= y + 4 THEN Mon(Num).HP = Mon(Num).HP - INT((75 - 55 + 1) + RND * 55 + 2)
         IF Mon(Num).HP <= 0 THEN
            Mon(Num).ID = ""
            Mon(Num).x = 0
            Mon(Num).y = 0
            Mon(Num).HP = 0
            Mon(Num).Damage = 0
            Info.pcExp = Info.pcExp + Mon(Num).Expr
            Mon(Num).Expr = 0
            Mon(Num).AttNum = 0
            Mon(Num).RoundNum = 0
         END IF
         Num = Num + 1
      LOOP UNTIL Num = MaxDim + 1
      a$ = "Spell successful!"
   END IF
   IF Choice = 6 THEN
      IF Info.pcMP < 20 THEN a$ = "Not Enough MP!": GOTO ExitList
      CastingEffect2
      Info.pcMP = Info.pcMP - 20
      MainStats Info, Info2
      CastingEffect2
      IF Info.Cit = "00" THEN
         Misc9 = 0
         DO
            RANDOMIZE TIMER
            MiscX = INT((60 - 10 + 1) * RND + 10)
            MiscY = INT((60 - 10 + 1) * RND + 10)
            GET #2, MiscY, Latitude
            Misc1$ = RIGHT$(LEFT$(Latitude, MiscX), 1)
            IF Info.Mode = 1 AND Info.Worldy = "01" AND Misc1$ = "B" THEN Misc9 = 1
            IF Info.Mode = 1 AND Info.Worldy = "01" AND Misc1$ = "C" THEN Misc9 = 1
            IF Info.Mode = 1 AND Info.Worldy = "02" AND Misc1$ = "S" THEN Misc9 = 1
            IF Info.Mode = 2 AND Info.Worldy = "01" AND Misc1$ = "A" THEN Misc9 = 1
         LOOP UNTIL Misc9 = 1
         x = MiscX
         y = MiscY
         MapScroll Info, Info2, "Redraw", x, y
         a$ = "Spell successful!"
      ELSE
         a$ = "Spell Fails!"
      END IF
   END IF
   IF Choice = 8 THEN
      IF Info.pcMP < 50 THEN a$ = "Not Enough MP!": GOTO ExitList
      CastingEffect2
      Info.pcHP = Info.pcHP - 200
      IF Info.pcHP < 0 THEN Info.pcHP = 0
      Info.pcMP = Info.pcMP - 50
      MainStats Info, Info2
      CastingEffect2
      IF Info.Cit <> "00" THEN
         x = Info.x2
         y = Info.y2
         Info.Mode = 1
         Info.Cit = "00"
         GetMap Info, Info2
         LoadMapTiles 1
         MapScroll Info, Info2, "Redraw", x, y
         a$ = "Spell successful!"
      ELSE
         a$ = "Spell Fails!"
      END IF
   END IF
   IF Choice = 9 THEN
      IF Info.pcMP < 30 THEN a$ = "Not Enough MP!": GOTO ExitList
         CastingEffect2
         Info.pcFood = Info.pcFood + 1
         Info.pcMP = Info.pcMP - 30
         MainStats Info, Info2
         CastingEffect2
         a$ = "Spell successful!"
   END IF
   IF Choice = 10 THEN
      Prompt2
      Prompt2
      PRINT "How Much Gold?";
      Prompt2
      FixInput2 25, 22, Misc, 3
      Misc = INT(Misc / 10)
      IF Misc > Info.pcGold THEN Misc = Info.pcGold
      IF Misc <= 0 THEN Misc = 0
      CastingEffect2
      FOR a = 1 TO Misc
         Info.pcMP = Info.pcMP + 1
         Info.pcGold = Info.pcGold - 10
         MainStats Info, Info2
      NEXT
      MainStats Info, Info2
      CastingEffect2
      a$ = "Spell successful!"
   END IF
   IF Choice = 5 THEN
      IF Info.pcMP < 20 THEN a$ = "Not Enough MP!": GOTO ExitList
         CastingEffect2
         Info.pcMP = Info.pcMP - 20
         MainStats Info, Info2
         CastingEffect2
         GET (9, 14)-(151, 146), ViewMapScr
         Num = 1
         RANDOMIZE TIMER
         DO
            PUT (9, 9 + INT(RND * 10)), ViewMapScr, PSET
            Num = Num + 1
         LOOP UNTIL Num = 25
         IF Info.Mode = 3 THEN DungScroll Info, Info2, "Redraw", x, y ELSE MapScroll Info, Info2, "Mdraw", x, y
         Num = 1
         DO
            IF Mon(Num).x >= x - 4 AND Mon(Num).y >= y - 4 AND Mon(Num).x <= x + 4 AND Mon(Num).y <= y + 4 THEN Mon(Num).HP = Mon(Num).HP - INT((60 - 45 + 1) + RND * 45 + 2)
            IF Mon(Num).HP <= 0 THEN
               Mon(Num).ID = ""
               Mon(Num).x = 0
               Mon(Num).y = 0
               Mon(Num).HP = 0
               Mon(Num).Damage = 0
               Info.pcExp = Info.pcExp + Mon(Num).Expr
               Info.pcGold = Info.pcGold + INT(Mon(Num).Expr / 10)
               Mon(Num).Expr = 0
               Mon(Num).AttNum = 0
               Mon(Num).RoundNum = 0
            END IF
            Num = Num + 1
         LOOP UNTIL Num = MaxDim + 1
         a$ = "Spell successful!"
         IF LEFT$(Info.Cit, 1) = "L" THEN
            Prompt2
            Prompt2
            PRINT "The walls of the";
            Prompt2
            PRINT "dungeon collase";
            Prompt2
            PRINT "upon you!";
            RestoreLife Info, Info2, Location, x, y
            MainStats Info, Info2
            EXIT SUB
         END IF
   END IF
ExitList:
   MainStats Info, Info2
   IF Info.Mode = 3 THEN DungScroll Info, Info2, "Redraw", x, y ELSE MapScroll Info, Info2, "Mdraw", x, y
   Prompt2
   Prompt2
   PRINT a$;
   IF Info.pcHP = 0 THEN RestoreLife Info, Info2, Location, x, y
END SUB

SUB Talking (Info AS Chrinfo, Info2 AS Chrinfo2, x, y) STATIC
   PRINT "Talk....";
   qx = x
   qy = y
   PUT (73 + (16 * (qx - x)), 73 + (16 * (qy - y))), Bubble, XOR
   Misc55 = 1
   DO
      DO
         Choice$ = INKEY$
      LOOP WHILE Choice$ = ""
      IF LEN(Choice$) = 1 THEN
      SELECT CASE ASC(Choice$)
      CASE 13, 84, 116
         PUT (73 + (16 * (qx - x)), 73 + (16 * (qy - y))), Bubble, XOR
         Misc55 = 2
      CASE 27, 32
         PRINT "NOT!!!";
         PUT (73 + (16 * (qx - x)), 73 + (16 * (qy - y))), Bubble, XOR
         EXIT SUB
      END SELECT
      ELSE
      Choice$ = RIGHT$(Choice$, 1)
      SELECT CASE Choice$
      CASE CHR$(UP)
         PUT (73 + (16 * (qx - x)), 73 + (16 * (qy - y))), Bubble, XOR
         qy = qy - 1
         IF ABS(qy - y) > 4 THEN qy = qy + 1
         PUT (73 + (16 * (qx - x)), 73 + (16 * (qy - y))), Bubble, XOR
      CASE CHR$(DOWN)
         PUT (73 + (16 * (qx - x)), 73 + (16 * (qy - y))), Bubble, XOR
         qy = qy + 1
         IF ABS(qy - y) > 4 THEN qy = qy - 1
         PUT (73 + (16 * (qx - x)), 73 + (16 * (qy - y))), Bubble, XOR
      CASE CHR$(LEFT)
         PUT (73 + (16 * (qx - x)), 73 + (16 * (qy - y))), Bubble, XOR
         qx = qx - 1
         IF ABS(qx - x) > 4 THEN qx = qx + 1
         PUT (73 + (16 * (qx - x)), 73 + (16 * (qy - y))), Bubble, XOR
      CASE CHR$(Right)
         PUT (73 + (16 * (qx - x)), 73 + (16 * (qy - y))), Bubble, XOR
         qx = qx + 1
         IF ABS(qx - x) > 4 THEN qx = qx - 1
         PUT (73 + (16 * (qx - x)), 73 + (16 * (qy - y))), Bubble, XOR
      END SELECT
      END IF
   LOOP WHILE Misc55 = 1
   PRINT ;
   GET #2, qy, Latitude
   PlaceTempTiles Info, Info2, qx, qy
   FOR a = 1 TO qx
      Misc1$ = MID$(Latitude, a, 1)
   NEXT
   TheNews Info, Info2, qx, qy, Misc1$
END SUB

SUB UseIt (Info AS Chrinfo, Info2 AS Chrinfo2, x, y) STATIC
   IF Info.Mode = 3 THEN
      IF Info.UnderTile <> "T" AND LEFT$(Info.Cit, 1) = "L" THEN
         IF Info.UnderTile = "E" THEN Info2.DungLvl = Info2.DungLvl - 1
         IF Info.UnderTile = "F" THEN Info2.DungLvl = Info2.DungLvl + 1
         IF Info2.DungLvl = 0 THEN ExitTowne Info, Info2, x, y: EXIT SUB
         IF Info2.DungLvl = 1 THEN Info.Cit = "L1"
         IF Info2.DungLvl = 2 THEN Info.Cit = "L2"
         IF Info2.DungLvl = 3 THEN Info.Cit = "L3"
         IF Info2.DungLvl = 4 THEN Info.Cit = "L4"
         PRINT "Use ladder";
      END IF
      IF Info.Cit = "05" AND Info2.Cit3 = "E" AND x = 35 AND y = 40 THEN
         PRINT "Climb up!";
         x = 35
         y = 17
         Info.Mode = 1
         MapScroll Info, Info2, "Redraw", x, y
         EXIT SUB
      END IF
      IF Info.Cit = "05" AND Info2.Cit3 = "E" AND x = 35 AND y = 33 THEN
         PRINT "Climb down!";
         x = 35
         y = 56
         Info.Mode = 1
         MapScroll Info, Info2, "Redraw", x, y
         EXIT SUB
      END IF
      GetMap Info, Info2
      DungScroll Info, Info2, "Redraw", x, y
   END IF
   IF LEFT$(Info.Cit, 1) <> "L" THEN
      IF Info.Cit <> "00" AND Info.UnderTile = "E" THEN
         PRINT "Climb up!";
         IF Info.Cit = "05" AND Info2.Cit3 = "E" AND x = 35 AND y = 56 THEN
            x = 35
            y = 33
            Info.Mode = 3
            DungScroll Info, Info2, "Redraw", x, y
            EXIT SUB
         END IF
         IF Info.Cit <> "05" AND Info2.Cit3 = "E" AND x = 35 AND y = 56 THEN x = 35: y = 33
         IF Info.Cit <> "05" AND Info2.Cit3 = "E" AND x = 35 AND y = 40 THEN x = 35: y = 17
         IF Info2.Cit3 = "K" AND x = 19 AND y = 10 THEN x = 43: y = 12
         IF Info2.Cit3 = "K" AND x = 43 AND y = 10 THEN x = 19: y = 30
         IF Info2.Cit3 = "K" AND x = 19 AND y = 28 THEN x = 43: y = 30
         IF Info2.Cit3 = "K" AND x = 43 AND y = 28 THEN x = 19: y = 48
         IF Info2.Cit3 = "K" AND x = 19 AND y = 46 THEN x = 43: y = 48
         MapScroll Info, Info2, "Redraw", x, y
         EXIT SUB
      ELSEIF Info.Cit <> "00" AND Info.UnderTile = "F" THEN
         PRINT "Climb down!";
         IF Info.Cit <> "05" AND Info2.Cit3 = "E" AND x = 35 AND y = 33 THEN x = 35: y = 56
         IF Info.Cit = "05" AND Info2.Cit3 = "E" AND x = 35 AND y = 17 THEN
            x = 35
            y = 40
            Info.Mode = 3
            DungScroll Info, Info2, "Redraw", x, y
            EXIT SUB
         END IF
         IF Info.Cit <> "05" AND Info2.Cit3 = "E" AND x = 35 AND y = 17 THEN x = 35: y = 40
         IF Info2.Cit3 = "K" AND x = 43 AND y = 12 THEN x = 19: y = 10
         IF Info2.Cit3 = "K" AND x = 19 AND y = 30 THEN x = 43: y = 10
         IF Info2.Cit3 = "K" AND x = 43 AND y = 30 THEN x = 19: y = 28
         IF Info2.Cit3 = "K" AND x = 19 AND y = 48 THEN x = 43: y = 28
         IF Info2.Cit3 = "K" AND x = 43 AND y = 48 THEN x = 19: y = 46
         MapScroll Info, Info2, "Redraw", x, y
         EXIT SUB
      END IF
   END IF
   IF Info.UnderTile = "H" THEN
      IF Info.Quest1 = 99 THEN
         PRINT "Zap!!!";
         Times1 = 1
         DO
            GET #3, Times1, Location
            IF Location.Header1 = "H" THEN
               IF Location.Header2 <> "12" THEN
                  IF x = Location.lx1 AND y = Location.ly1 THEN
                     x = Location.lx2
                     y = Location.ly2
                     CastingEffect1 Info, Info2, x, y
                     EXIT SUB
                  END IF
                  IF x = Location.lx2 AND y = Location.ly2 THEN
                     x = Location.lx1
                     y = Location.ly1
                     CastingEffect1 Info, Info2, x, y
                     EXIT SUB
                  END IF
               ELSEIF Info.Quest4 = 99 THEN
                  IF x = Location.lx1 AND y = Location.ly1 THEN
                     x = Location.lx2
                     y = Location.ly2
                     Info.Worldy = "02"
                     GetMap Info, Info2
                     CastingEffect1 Info, Info2, x, y
                     EXIT SUB
                  END IF
                  IF x = Location.lx2 AND y = Location.ly2 THEN
                     x = Location.lx1
                     y = Location.ly1
                     Info.Worldy = "01"
                     GetMap Info, Info2
                     CastingEffect1 Info, Info2, x, y
                     EXIT SUB
                  END IF
               ELSE
                  Prompt2
                  PRINT "The power has ";
                  Prompt2
                  PRINT "been drained from";
                  Prompt2
                  PRINT "this gate!!!";
                  EXIT SUB
               END IF
            END IF
            Times1 = Times1 + 1
         LOOP UNTIL Times1 = LOF(3) \ LEN(Location) + 1
         EXIT SUB
      ELSE
         PRINT "For SOME reason";
         Prompt2
         PRINT "the gates do not";
         Prompt2
         PRINT "work!!!";
         EXIT SUB
      END IF
   END IF
   IF Info.UnderTile = "T" THEN
      PRINT "You open the Chest";
      IF Info.Cit = "08" AND Info2.Cit2 = "D " THEN Info.Quest5 = 67
      IF Info.Cit = "04" THEN Info.Quest4 = 89
      IF Info.Cit = "L3" AND x = 8 AND y = 47 THEN Info.Quest7 = 67
      IF Info.Cit = "L3" AND x = 49 AND y = 40 THEN Info.Quest2 = 67
      IF Info.Cit = "L4" AND x = 24 AND y = 25 THEN Info.Quest1 = 67
      IF Info.Cit = "L4" AND x = 41 AND y = 39 THEN Info.Quest3 = 67
      IF Info.Cit = "L4" AND x = 33 AND y = 64 THEN Info.Quest4 = 67
      IF Info.Cit = "L3" AND x = 25 AND y = 37 THEN Info.Quest6 = 67
      IF Info.Mode = 3 THEN DungScroll Info, Info2, "Redraw", x, y ELSE MapScroll Info, Info2, "Redraw", x, y
      EXIT SUB
   END IF
   IF Info.Mode = 1 OR Info.Mode = 3 AND Info.UnderTile = "Q" THEN
      UseIt2 Info, Info2, x, y
   ELSEIF Info.Mode = 2 THEN
      PRINT "Not here!";
   END IF
END SUB

SUB ViewMap (Info AS Chrinfo, Info2 AS Chrinfo2, x, y)
   qx = x
   qy = y
   CLOSE #7
   OPEN "WORLD" + Info.Worldy + ".MAP" FOR RANDOM AS #7 LEN = LEN(Latitude)
   Liner = 1
   DO
      GET #7, Liner, Latitude
      PlaceTempTiles Info, Info2, Tempx, Liner
      FOR a = 1 TO LEN(Latitude)
      Tiler$ = MID$(Latitude, a, 1)
      Misc7 = ASC(Tiler$) - 65
      IF Misc7 = 0 THEN Misc7 = 1: GOTO qwe
      IF Misc7 = 1 THEN Misc7 = 2: GOTO qwe
      IF Misc7 = 2 THEN Misc7 = 10: GOTO qwe
      IF Misc7 = 3 THEN Misc7 = 7: GOTO qwe
      IF Misc7 = 4 THEN Misc7 = 15: GOTO qwe
      IF Misc7 = 5 THEN Misc7 = 15: GOTO qwe
      IF Misc7 = 6 THEN Misc7 = 15: GOTO qwe
      IF Misc7 = 7 THEN Misc7 = 15: GOTO qwe
      IF Misc7 = 8 THEN Misc7 = 8: GOTO qwe
      IF Misc7 = 9 THEN Misc7 = 15: GOTO qwe
      IF Misc7 = 10 THEN Misc7 = 15: GOTO qwe
      IF Misc7 = 11 THEN Misc7 = 9: GOTO qwe
      IF Misc7 = 12 THEN Misc7 = 15: GOTO qwe
      IF Misc7 = 13 THEN Misc7 = 15: GOTO qwe
      IF Misc7 = 14 THEN Misc7 = 8: GOTO qwe
      IF Misc7 = 15 THEN Misc7 = 8: GOTO qwe
      IF Misc7 = 18 THEN Misc7 = 4: GOTO qwe
      IF Misc7 >= 19 THEN Misc7 = 0: GOTO qwe
qwe:
      PSET (a + 185, Liner + 85), Misc7
      NEXT
      Liner = Liner + 1
      a$ = INKEY$
      IF a$ <> "" THEN EXIT SUB
   LOOP UNTIL Liner = 71
   IF Info.Cit <> "00" THEN qx = Info.x2: qy = Info.y2
   IF qx > 70 THEN qx = qx - 70
   IF qy > 70 THEN qy = qy - 70
   PSET (qx + 185, qy + 85), 8
   LOCATE 21, 25
   IF Info.Worldy = "01" THEN PRINT "Allasia" ELSE PRINT "The Sun"
   a$ = INPUT$(1)
END SUB

