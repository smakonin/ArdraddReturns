' Copyright (C) 1991, Stephen Makonin. All Rights Reserved.

DECLARE SUB DungScroll (Info AS ANY, Info2 AS ANY, Direction$, x!, y!)
DECLARE SUB GetMap (Info AS ANY, Info2 AS ANY)
DECLARE SUB LoadMapTiles (TileSet!)
DECLARE SUB MapScroll (Info AS ANY, Info2 AS ANY, Direction$, x!, y!)
DECLARE SUB ViewMap (Info AS ANY, Info2 AS ANY, x!, y!)
DECLARE SUB TextScroll (row1%, col1%, row2%, col2%, lines%, Attr%)
DECLARE SUB MouseHide ()
DECLARE SUB MouseShow ()
DECLARE SUB ReadData (a%)
DECLARE SUB CastingEffect3 ()
DECLARE SUB CastingEffect4 ()
DECLARE SUB MainStats (Info AS ANY, Info2 AS ANY)
DECLARE SUB CastingEffect2 ()
DECLARE SUB MenuBar (Selection$(), TopLocation!, Margin!, ForeGround!, BackGround!, BarForeGround!, BarBackGround!, CH!)
DECLARE SUB Prompt ()
DECLARE SUB Prompt2 ()
DECLARE FUNCTION Convert% (Text$)
DECLARE SUB ThePub (Info AS ANY, Info2 AS ANY)
DECLARE SUB Shires (Info AS ANY, Info2 AS ANY)
DECLARE SUB Shipry (Info AS ANY, Info2 AS ANY)
DECLARE SUB Weaponry (Info AS ANY, Info2 AS ANY)
DECLARE SUB Armory (Info AS ANY, Info2 AS ANY)
DECLARE SUB JoesInn (Info AS ANY, Info2 AS ANY, x!, y!)
DECLARE SUB JoesDiner (Info AS ANY, Info2 AS ANY)
DECLARE SUB YesOrNo (Choice!)
DECLARE SUB FixInput (row!, col!, Text$, MaxLen!)
DECLARE SUB Prompt3 ()

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
   Shipx2 AS INTEGER
   Shipy2 AS INTEGER
   Dead AS INTEGER
   Shipx3 AS INTEGER
   Shipy3 AS INTEGER
   Shipy4 AS INTEGER
   Shipx4 AS INTEGER
END TYPE

TYPE Chrinfo
   pcName AS STRING * 10
   pcSex AS STRING * 1
   pcRole AS STRING * 8
   Worldy AS STRING * 2
   Cit AS STRING * 2
   UnderTile AS STRING * 1
   pcSTR AS INTEGER
   pcDEX AS INTEGER
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

CONST esc = 27, DOWN = 80, UP = 72, LEFT = 75, Right = 77

DIM SHARED ViewMapScr(1 TO 4000)
DIM SHARED Location AS LocateInfo
DIM SHARED Misc10 AS MiscInfo
DIM SHARED Cor AS CorInfo
DIM Info AS Chrinfo
DIM Info2  AS Chrinfo2
DIM SHARED Latitude AS STRING * 70
DIM SHARED Inv AS STRING * 40
DIM SHARED ScreenTiles(1 TO 81) AS STRING * 1
DIM SHARED Tiles(45 TO 1215)
DIM SHARED Misc9(1 TO 45)
DIM SHARED Compass AS INTEGER
DIM SHARED DungTiles(1 TO 10) AS STRING * 1

DATA "S Sword", "Dagger", "Shield", "Spell Book", "L Sword", "Short Bow", "Long Bow"
DATA "Sling", "M Dagger", "M Sword", "Cloth", "Leather", "Chain", "Plate"
DATA "Magical", "Silver Key", "Magic Gem", "Lion's Sword", "Mountain Remover"
DATA "Urn of Power", "Ring of Power", "Finger o' Death", "Staff o' Death"
DATA "Staff o' Opening", "Lock Picking Key", "Nothing", "Magic Stuff"

SUB Armory (Info AS Chrinfo, Info2 AS Chrinfo2) STATIC
   DIM list$(3)
   DIM List2$(5)
   DIM List3$(5)
   DO
      MainStats Info, Info2
      PRINT "Welcome to another";
      Prompt2
      PRINT "of Joe's Armory";
      TextScroll 13, 21, 25, 40, 10, 0
      Choice = 1
      list$(1) = "1 Buy Armor"
      list$(2) = "2 Sell Armor"
      list$(3) = "3 Leave Shop"
      CALL MenuBar(list$(), 17, 22, 15, 0, 4, 0, Choice)
      IF Choice = 1 THEN
         Prompt2
         PRINT "What Type?";
         TextScroll 13, 21, 25, 40, 11, 0
         choice3 = 1
         List2$(1) = "Cloth         100gp"
         List2$(2) = "Leather       250gp"
         List2$(3) = "Chain Mail    400gp"
         List2$(4) = "Plate Mail    500gp"
         List2$(5) = "Not Interested!"
         CALL MenuBar(List2$(), 16, 22, 15, 0, 4, 0, choice3)
         IF choice3 = 1 AND Info.pcGold >= 100 THEN
            Info.pcGold = Info.pcGold - 100
            Info2.Cloth = Info2.Cloth + 1
         ELSEIF choice3 = 2 AND Info.pcGold >= 250 THEN
            Info.pcGold = Info.pcGold - 250
            Info2.Leather = Info2.Leather + 1
         ELSEIF choice3 = 3 AND Info.pcGold >= 400 THEN
            Info.pcGold = Info.pcGold - 400
            Info2.ChainMale = Info2.ChainMale + 1
         ELSEIF choice3 = 4 AND Info.pcGold >= 500 THEN
            Info.pcGold = Info.pcGold - 500
            Info2.PlateMale = Info2.PlateMale + 1
         END IF
      END IF
      IF Choice = 2 THEN
         Prompt2
         PRINT "Sell what?";
         TextScroll 13, 21, 25, 40, 11, 0
         choice3 = 1
         List3$(1) = "----------"
         List3$(2) = "----------"
         List3$(3) = "----------"
         List3$(4) = "----------"
         List3$(5) = "No, not now."
         IF Info2.Cloth > 0 THEN List3$(1) = "Cloth          75gp"
         IF Info2.Leather > 0 THEN List3$(2) = "Leather       200gp"
         IF Info2.ChainMale > 0 THEN List3$(3) = "Chain Mail    300gp"
         IF Info2.PlateMale > 0 THEN List3$(4) = "Plate Mail    450gp"
         CALL MenuBar(List3$(), 16, 22, 15, 0, 4, 0, choice3)
         IF choice3 = 1 AND Info2.Cloth > 0 THEN
            Info.pcGold = Info.pcGold + 75
            Info2.Cloth = Info2.Cloth - 1
            IF Info2.Cloth = 0 AND Info.ReadArmor = 11 THEN Info.ReadArmor = 26
         ELSEIF choice3 = 2 AND Info2.Leather > 0 THEN
            Info.pcGold = Info.pcGold + 200
               Info2.Leather = Info2.Leather - 1
            IF Info2.Leather = 0 AND Info.ReadArmor = 12 THEN Info.ReadArmor = 26
         ELSEIF choice3 = 3 AND Info2.ChainMale > 0 THEN
            Info.pcGold = Info.pcGold + 300
            Info2.ChainMale = Info2.ChainMale - 1
            IF Info2.ChainMale = 0 AND Info.ReadArmor = 13 THEN Info.ReadArmor = 26
         ELSEIF choice3 = 4 AND Info2.PlateMale > 0 THEN
            Info.pcGold = Info.pcGold + 450
            Info2.PlateMale = Info2.PlateMale - 1
            IF Info2.PlateMale = 0 AND Info.ReadArmor = 14 THEN Info.ReadArmor = 26
         END IF
      END IF
   LOOP UNTIL Choice = 3
   ERASE list$
   ERASE List2$
   ERASE List3$
   MainStats Info, Info2
END SUB

SUB CastingEffect1 (Info AS Chrinfo, Info2 AS Chrinfo2, x, y) STATIC
   qw = 8
   qw2 = 153
   a = 1
   DO
      LINE (qw + a, qw + a)-(qw2 - a, qw2 - a), 0, B
      a = a + 1
   LOOP UNTIL a = 82
   MapScroll Info, Info2, "Redraw", x, y
END SUB

SUB CastingEffect2
   GET (9, 9)-(152, 152), ViewMapScr
   PUT (9, 9), ViewMapScr, PRESET
END SUB

SUB CastingEffect3
   B = 1
   DO
      GET #5, B, Cor
      PSET (Cor.x, Cor.y), 0
      B = B + 1
   LOOP UNTIL B = 20481
END SUB

SUB CastingEffect4
   B = 20480
   DO
      GET #5, B, Cor
      SCREEN 7, , 1, 0
      a = POINT(Cor.x, Cor.y)
      IF a > 0 THEN
         SCREEN 7, , 0, 0
         PSET (Cor.x, Cor.y), a
      END IF
      B = B - 1
   LOOP UNTIL B = 0
   SCREEN 7, , 0, 0
END SUB

SUB ClearScreen
   x = 1
   y = 1
   x2 = 320
   y2 = 200
   DO
      LINE (x, y)-(x2, y2), 0, B
      x = x + 1
      y = y + 1
      x2 = x2 - 1
      y2 = y2 - 1
   LOOP UNTIL y = 101
END SUB

FUNCTION Convert% (Text$)
   a = LEN(RTRIM$(Text$))
   Misc% = 0
   I = 1
   DO
      Misc% = Misc% + ((ASC(MID$(Text$, I, 1)) - 48) * (10 ^ ABS(I - a)))
      I = I + 1
   LOOP UNTIL I = a + 1
   Convert% = Misc%
END FUNCTION

SUB Ending
   SCREEN 0
   WIDTH 80, 25
   CLS
   PRINT "Quikk Quest 1 : Ardradd Returns  Ver. 7.1"
   PRINT
   PRINT "By:  Stephen William Makonin & F.F.S."
   PRINT "Address:  F.F.S."
   PRINT "          Site 3, C-63, RR#2"
   PRINT "          Castlegar, BC, Canada"
   PRINT "          V1N 3L4,  (604)399-4773"
   PRINT
   PRINT "If you have any comments, questions or donations write to me!"
   PRINT "A registration package ($15.00) is avalable!"
   PRINT "The QQ1 Hint Book ($5.00) is also available!"
   PRINT
   PRINT "I hope you had as much fun as I did!"
   END
END SUB

SUB Enterance (Info AS Chrinfo, Info2 AS Chrinfo2, Location AS LocateInfo, x, y) STATIC
      IF Info.UnderTile = "E" THEN
         Info2.Cit3 = "E"
         PRINT "Welcome to:";
         Prompt2
         IF Info.Worldy = "01" AND x = 35 AND y = 34 THEN PRINT "The Castle"; : Info.Cit = "03": Info2.Cit2 = "  "
         IF Info.Worldy = "01" AND x = 63 AND y = 55 THEN PRINT "Evil Castle"; : Info.Cit = "04": Info2.Cit2 = "  "
         IF Info.Quest5 < 99 AND Info.Worldy = "02" AND x = 28 AND y = 44 THEN EXIT SUB
         IF Info.Worldy = "02" AND x = 28 AND y = 44 THEN PRINT "Other Evil Castle"; : Info.Cit = "05": Info2.Cit2 = "  "
      ELSEIF Info.UnderTile = "F" THEN
         Info2.Cit3 = "F"
         PRINT "Welcome to:";
         Prompt2
         IF Info.Worldy = "01" AND x = 22 AND y = 44 THEN PRINT "Amber"; : Info.Cit = "09": Info2.Cit2 = "A "
         IF Info.Worldy = "01" AND x = 50 AND y = 62 THEN PRINT "Bath"; : Info.Cit = "09": Info2.Cit2 = "B "
         IF Info.Worldy = "01" AND x = 19 AND y = 59 THEN PRINT "Done"; : Info.Cit = "09": Info2.Cit2 = "C "
         IF Info.Worldy = "01" AND x = 74 AND y = 51 THEN PRINT "Narcon"; : Info.Cit = "09": Info2.Cit2 = "D "
         IF Info.Worldy = "01" AND x = 50 AND y = 17 THEN PRINT "Rhohan"; : Info.Cit = "10": Info2.Cit2 = "A "
         IF Info.Worldy = "01" AND x = 55 AND y = 43 THEN PRINT "Evilen"; : Info.Cit = "10": Info2.Cit2 = "B "
         IF Info.Worldy = "01" AND x = 17 AND y = 39 THEN PRINT "Rhalha"; : Info.Cit = "10": Info2.Cit2 = "C "
      ELSEIF Info.UnderTile = "G" THEN
         Info2.Cit3 = "F"
         PRINT "Welcome to:";
         Prompt2
         IF Info.Worldy = "01" AND x = 36 AND y = 12 THEN PRINT "Young One's Hut"; : Info.Cit = "08": Info2.Cit2 = "C "
         IF Info.Worldy = "01" AND x = 25 AND y = 67 THEN PRINT "A Hut"; : Info.Cit = "08": Info2.Cit2 = "D "
      ELSEIF Info.UnderTile = "J" THEN
         PRINT "Welcome to:";
         Prompt2
         PRINT "Dungeon of";
         Prompt2
         IF Info.Worldy = "01" AND x = 46 AND y = 6 THEN PRINT "the Ice Man";
         IF Info.Worldy = "01" AND x = 27 AND y = 73 THEN PRINT "Frozen Frost";
         IF Info.Worldy = "01" AND x = 40 AND y = 68 THEN PRINT "the Ice Nixie";
         IF Info.Worldy = "01" AND x = 17 AND y = 16 THEN PRINT "No Echo";
         IF Info.Worldy = "01" AND x = 39 AND y = 26 THEN PRINT "Nothing";
         IF Info.Worldy = "01" AND x = 46 AND y = 26 THEN PRINT "Something";
         IF Info.Worldy = "01" AND x = 13 AND y = 42 THEN PRINT "This & That";
         IF Info.Worldy = "02" AND x = 41 AND y = 40 THEN PRINT "Nalixama";
         IF Info.Worldy = "02" AND x = 19 AND y = 36 THEN PRINT "Tempera";
         Info.Cit = "L1"
         Info.Mode = 3
         Info.x2 = x
         Info.y2 = y
         GetMap Info, Info2
         LoadMapTiles 2
         Info2.DungLvl = 1
         Compass = 1
         DungScroll Info, Info2, "Redraw", x, y
         EXIT SUB
      ELSEIF Info.UnderTile = "I" THEN
         qx = x
         qy = y
         IF qx > 70 THEN qx = qx - 70
         IF qy > 70 THEN qy = qy - 70
         IF Info.Mode = 1 THEN
            PRINT "Enter ship!";
            Info.Mode = 2
            IF qx = Info2.Shipx AND qy = Info2.ShipY THEN Info2.NumShips = Info2.NumShips + 1
            IF qx = Info2.Shipx2 AND qy = Info2.Shipy2 THEN Info2.NumShips = Info2.NumShips + 2
            IF qx = Info2.Shipx3 AND qy = Info2.Shipy3 THEN Info2.NumShips = Info2.NumShips + 3
            IF qx = Info2.Shipx4 AND qy = Info2.Shipy4 THEN Info2.NumShips = Info2.NumShips + 4
            MapScroll Info, Info2, "Redraw", x, y
            EXIT SUB
         END IF
      ELSEIF Info.Mode = 2 THEN
         qx = x
         qy = y
         IF qx > 70 THEN qx = qx - 70
         IF qy > 70 THEN qy = qy - 70
         PRINT "Exit ship!";
         Info.Mode = 1
         Misc8 = Info2.NumShips
         FOR Misc3 = 1 TO INT(Info2.NumShips / 10)
            Misc8 = Misc8 - 10
         NEXT
         IF Misc8 = 1 THEN
            Info2.NumShips = Info2.NumShips - 1
            Info2.Shipx = qx
            Info2.ShipY = qy
         ELSEIF Misc8 = 2 THEN
            Info2.NumShips = Info2.NumShips - 2
            Info2.Shipx2 = qx
            Info2.Shipy2 = qy
         ELSEIF Misc8 = 3 THEN
            Info2.NumShips = Info2.NumShips - 3
            Info2.Shipx3 = qx
            Info2.Shipy3 = qy
         ELSEIF Misc8 = 4 THEN
            Info2.NumShips = Info2.NumShips - 4
            Info2.Shipx4 = qx
            Info2.Shipy4 = qy
         END IF
         MapScroll Info, Info2, "Redraw", x, y
         EXIT SUB
      ELSEIF Info.UnderTile = "K" THEN
         Info2.Cit3 = "K"
         PRINT "Welcome to:";
         Prompt2
         IF Info.Worldy = "01" AND x = 32 AND y = 55 THEN PRINT "Emerald Tower"; : Info.Cit = "06": Info2.Cit2 = "  "
         IF Info.Worldy = "01" AND x = 70 AND y = 70 THEN PRINT "Lava Tower"; : Info.Cit = "07": Info2.Cit2 = "  "
      ELSEIF Info.UnderTile = "M" THEN
         IF Info.Quest2 > 67 THEN
            Info2.Cit3 = "F"
            PRINT "Welcome to:";
            Prompt2
            IF Info.Worldy = "01" AND Info.pcRole = "Warrior " AND x = 29 AND y = 25 THEN
               PRINT "Shrine of Im";
               Info.Cit = "08"
               Info2.Cit2 = "A "
            ELSEIF Info.Worldy = "01" AND Info.pcRole = "Wizard  " AND x = 33 AND y = 41 THEN
               PRINT "Shrine of Om";
               Info.Cit = "08"
               Info2.Cit2 = "B "
            ELSE
               PRINT "NOT ALLOWED!!!";
               EXIT SUB
            END IF
         ELSE
            PRINT "The doors to ";
            Prompt2
            PRINT "the Shrines are";
            Prompt2
            PRINT "LOCKED!!!";
            EXIT SUB
         END IF
      ELSE
         PRINT "Please, no out of";
         Prompt2
         PRINT "body experiences!";
         EXIT SUB
      END IF
   Times1 = 1
   CitB$ = Info2.Cit3 + Info2.Cit2
   DO
      GET #3, Times1, Location
      IF CitB$ = Location.Header1 + Location.Header2 THEN EXIT DO
      Times1 = Times1 + 1
   LOOP UNTIL Times1 = LOF(3) \ LEN(Location) + 1
   Info.x2 = x
   Info.y2 = y
   x = Location.lx3
   y = Location.ly3
   GetMap Info, Info2
   LoadMapTiles 2
   MapScroll Info, Info2, "Redraw", x, y
END SUB

SUB ExitTowne (Info AS Chrinfo, Info2 AS Chrinfo2, x, y) STATIC
   MouseHide
   PRINT "Leaving...";
   x = Info.x2
   y = Info.y2
   Info.Mode = 1
   Info.Cit = "00"
   GetMap Info, Info2
   LoadMapTiles 1
   lx1 = -1
   ly1 = -1
   lx2 = -1
   ly2 = -1
   lx3 = -1
   ly3 = -1
   MapScroll Info, Info2, "Redraw", x, y
   MouseShow
END SUB

SUB FixInput (row, col, Text$, MaxLen) STATIC
   LOCATE row, col, 1
   Text$ = ""
   PRINT Text$;
   DO
      I$ = UCASE$(INKEY$)
      SELECT CASE I$
      CASE CHR$(13)
         EXIT SUB
      CASE CHR$(8)
         IF LEN(Text$) > 0 THEN
            Text$ = LEFT$(Text$, LEN(Text$) - 1)
            LOCATE row, col
            PRINT Text$; " ";
            LOCATE row, col + LEN(Text$)
         END IF
      CASE CHR$(32), CHR$(48) TO CHR$(57), CHR$(65) TO CHR$(90)
         IF LEN(Text$) < MaxLen THEN
            Text$ = Text$ + I$
            COLOR 15
            PRINT I$;
         END IF
      END SELECT
   LOOP
   LOCATE , , 0
END SUB

SUB FixInput2 (row, col, Misc, MaxLen) STATIC
   LOCATE row, col, 1
   Text$ = ""
   PRINT Text$;
   DO
      I$ = UCASE$(INKEY$)
      SELECT CASE I$
      CASE CHR$(13)
         Misc = Convert%(Text$)
         EXIT SUB
      CASE CHR$(8)
         IF LEN(Text$) > 0 THEN
            Text$ = LEFT$(Text$, LEN(Text$) - 1)
            LOCATE row, col
            PRINT Text$; " ";
            LOCATE row, col + LEN(Text$)
         END IF
      CASE CHR$(48) TO CHR$(57)
         IF LEN(Text$) < MaxLen THEN
            Text$ = Text$ + I$
            COLOR 15
            PRINT I$;
         END IF
      END SELECT
   LOOP
   LOCATE , , 0
END SUB

SUB HoldUp (Info AS Chrinfo, Info2 AS Chrinfo2, x, y)
      PRINT "Hold up & Camp";
      IF Info.Mode <> 3 THEN
         Info.Cit = "10"
         Info.x2 = x
         Info.y2 = y
         x = 55
         y = 60
         GetMap Info, Info2
         LoadMapTiles 2
         MapScroll Info, Info2, "Redraw", x, y
      END IF
      Misc2 = 1
      Prompt2
      Prompt2
      PRINT "ZZZZZZZZZZZ...";
      DO
         Misc2 = Misc2 + 1
      LOOP WHILE Misc2 <= 10000
      Info.pcHP = Info.pcHP + INT(Info.pcMHP / 3)
      IF Info.pcHP > Info.pcMHP THEN Info.pcHP = Info.pcMHP
      Info.pcMP = Info.pcMP + INT(Info.pcMMP / 3)
      IF Info.pcMP > Info.pcMMP THEN Info.pcMP = Info.pcMMP
      Info.pcFood = Info.pcFood - 1
      MainStats Info, Info2
      IF Info.Mode <> 3 THEN
         x = Info.x2
         y = Info.y2
         Info.Cit = "00"
         GetMap Info, Info2
         LoadMapTiles 1
         MapScroll Info, Info2, "Redraw", x, y
      END IF
      Prompt2
      Prompt2
      PRINT "About time";
      Prompt2
      PRINT "you awoke!";
END SUB

SUB Inventory (Info AS Chrinfo, Info2 AS Chrinfo2, x, y)
      SCREEN 7, , 1, 0
      CLS
      SCREEN 7, , 1, 1
      Misc14 = 1
      COLOR 8
      DO
         GET #6, Misc14, Inv
         LOCATE Misc14, 1
         LOCATE Misc14, 1
         PRINT Inv
         Misc14 = Misc14 + 1
      LOOP UNTIL Misc14 > 23
      COLOR 15
      LOCATE 1, 15
      PRINT Info.pcName
      LOCATE 4, 12
      ReadData Info.ReadWeapon1
      LOCATE 5, 12
      ReadData Info.ReadWeapon2
      LOCATE 6, 12
      ReadData Info.ReadArmor
      LOCATE 9, 16
      PRINT Info2.SSword
      LOCATE 10, 16
      PRINT Info2.Dagger
      LOCATE 11, 16
      PRINT Info2.LSword
      LOCATE 12, 16
      PRINT Info2.Bow
      LOCATE 13, 16
      PRINT Info2.LBow
      LOCATE 14, 16
      PRINT Info2.Sling
      LOCATE 17, 16
      PRINT Info2.Cloth
      LOCATE 18, 16
      PRINT Info2.Leather
      LOCATE 19, 16
      PRINT Info2.ChainMale
      LOCATE 20, 16
      PRINT Info2.PlateMale
      Liner = 4
      IF Info.Quest1 = 67 THEN
         LOCATE Liner, 23
         ReadData 18
         Liner = Liner + 1
      END IF
      IF Info.Quest1 = 69 THEN
         LOCATE Liner, 23
         ReadData 17
         Liner = Liner + 1
      END IF
      IF Info.Quest2 = 67 THEN
         LOCATE Liner, 23
         ReadData 16
         Liner = Liner + 1
      END IF
      IF Info.Quest3 = 67 THEN
         LOCATE Liner, 23
         ReadData 20
         Liner = Liner + 1
      END IF
      IF Info.Quest3 = 69 THEN
         LOCATE Liner, 23
         ReadData 19
         Liner = Liner + 1
      END IF
      IF Info.Quest4 = 67 THEN
         LOCATE Liner, 23
         ReadData 22
         Liner = Liner + 1
      END IF
      IF Info.Quest4 = 89 THEN
         LOCATE Liner, 23
         ReadData 21
         Liner = Liner + 1
      END IF
      IF Info.Quest5 = 67 THEN
         LOCATE Liner, 23
         ReadData 24
         Liner = Liner + 1
      END IF
      IF Info.Quest6 = 67 THEN
         LOCATE Liner, 23
         ReadData 25
         Liner = Liner + 1
      END IF
      IF Info.Quest6 = 69 THEN
         LOCATE Liner, 23
         ReadData 23
         Liner = Liner + 1
      END IF
      IF Info.Quest7 = 67 THEN
         LOCATE Liner, 23
         ReadData 27
         Liner = Liner + 1
      END IF
      ViewMap Info, Info2, x, y
END SUB

SUB JoesDiner (Info AS Chrinfo, Info2 AS Chrinfo2) STATIC
   Misc9 = 25
   Misc8 = 1
   Prompt2
   PRINT "Welcome to another";
   Prompt2
   PRINT "Joe's Diner!!!";
   Prompt2
   PRINT "I can sell"; Misc8; "food";
   Prompt2
   PRINT "for"; Misc9; "GP.";
   Prompt2
   YesOrNo Choice
   IF Choice = 1 THEN
      IF Info.pcGold >= Misc9 THEN
         Info.pcFood = Info.pcFood + Misc8
         Info.pcGold = Info.pcGold - Misc9
         MainStats Info, Info2
         Prompt2
         PRINT "Come again!!";
      ELSE
         PRINT "Need more money!";
      END IF
   END IF
END SUB

SUB JoesInn (Info AS Chrinfo, Info2 AS Chrinfo2, x, y) STATIC
   Misc9 = 100
   Prompt2
   PRINT "Welcome to another";
   Prompt2
   PRINT "Joe's Inn!!!";
   Prompt2
   PRINT "You can sleep here";
   Prompt2
   PRINT "for"; Misc9; "GP.";
   Prompt2
   YesOrNo Choice
   IF Choice = 1 THEN
      IF Info.pcGold >= Misc9 THEN
         Info.pcGold = Info.pcGold - Misc9
         MainStats Info, Info2
         Prompt2
         PRINT "Good Night!!!";
         IF Info.Cit = "09" AND Info2.Cit2 = "A " THEN x = 13: y = 18
         IF Info.Cit = "09" AND Info2.Cit2 = "B " THEN x = 47: y = 9
         IF Info.Cit = "10" AND Info2.Cit2 = "C " THEN x = 27: y = 61
         MapScroll Info, Info2, "Redraw", x, y
         SLEEP 5
         Info.pcHP = Info.pcHP + (Info.pcMHP / 2)
         IF Info.pcHP > Info.pcMHP THEN Info.pcHP = Info.pcMHP
         Info.pcMP = Info.pcMP + (Info.pcMMP / 2)
         IF Info.pcMP > Info.pcMMP THEN Info.pcMP = Info.pcMMP
         MainStats Info, Info2
      ELSE
         PRINT "Need more money!";
      END IF
   END IF
END SUB

SUB LoadScreen (Misc1$)
   DIM FullScr(1 TO 9000)
   DEF SEG = VARSEG(FullScr(1))
   BLOAD Misc1$, VARPTR(FullScr(1))
   DEF SEG
   PUT (1, 1), FullScr, PSET
   ERASE FullScr
END SUB

SUB MainStats (Info AS Chrinfo, Info2 AS Chrinfo2) STATIC
   LOCATE 3, 22
   PRINT "Name: "; Info.pcName
   LOCATE 4, 22
   IF Info.pcSex = "M" THEN PRINT "Male    ";  ELSE PRINT "Female  ";
   PRINT Info.pcRole
   LOCATE 5, 22
   PRINT "Strength.......";
   PRINT USING "##"; Info.pcSTR
   LOCATE 6, 22
   PRINT "Dexterity......";
   PRINT USING "##"; Info.pcDEX
   LOCATE 7, 22
   PRINT "Intelligence...";
   PRINT USING "##"; Info.pcINT
   LOCATE 8, 22
   PRINT "Lvl  ";
   PRINT USING "##"; Info.pcLvl;
   PRINT "  Exp ";
   IF Info.pcExp > 9999 THEN Info.pcExp = 9999
   PRINT USING "####"; Info.pcExp
   IF Info.pcHP > Info.pcMHP THEN Info.pcHP = Info.pcMHP
   IF Info.pcHP < 0 THEN Info.pcHP = 0
   LOCATE 9, 22
   PRINT "Hp ";
   PRINT USING "####"; Info.pcHP;
   IF Info.pcMP > Info.pcMMP THEN Info.pcMP = Info.pcMMP
   IF Info.pcFood < 0 THEN Info.pcFood = 0
   PRINT "  Mp   ";
   PRINT USING "###"; Info.pcMP
   LOCATE 10, 22
   IF Info.pcFood > 999 THEN Info.pcFood = 999
   IF Info.pcFood < 0 THEN Info.pcFood = 0
   PRINT "F  ";
   PRINT USING "####"; Info.pcFood;
   IF Info.pcGold > 999 THEN Info.pcGold = 999
   IF Info.pcGold < 0 THEN Info.pcGold = 0
   PRINT "  Gp ";
   PRINT USING "$####"; Info.pcGold
   Prompt3
END SUB

SUB MenuBar (Selection$(), TopLocation, Margin, ForeGround, BackGround, BarForeGround, BarBackGround, CH) STATIC
   LN = 0
   FOR I = 0 TO UBOUND(Selection$)
      IF LEN(Selection$(I)) > LN THEN LN = LEN(Selection$(I))
   NEXT I
   IF LBOUND(Selection$) = 0 THEN
      IF Selection$(0) <> "" AND TopLocation <> 1 THEN
         LOCATE TopLocation - 1, Margin + ((LN - LEN(Selection$(0))) / 2)
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
   OLD = CH
   WHILE CHOOSING = 1
      COLOR ForeGround, BackGround
      LOCATE TopLocation + OLD - 1, Margin
      PRINT Selection$(OLD);
      COLOR BarForeGround, BarBackGround
      LOCATE TopLocation + CH - 1, Margin
      PRINT Selection$(CH);
      OLD = CH
      a$ = ""
      WHILE a$ = "": a$ = INKEY$: WEND
      IF LEN(a$) = 2 THEN a = ASC(RIGHT$(a$, 1)) ELSE a = ASC(a$)
      IF a = 13 OR a = 27 THEN CHOOSING = 0
      IF LEN(a$) = 1 THEN
         FOR J = UBOUND(Selection$) TO 1 STEP -1
            IF a = ASC(Selection$(J)) THEN
               CH = J
               COLOR ForeGround, BackGround
               LOCATE TopLocation + OLD - 1, Margin
               PRINT Selection$(OLD);
               COLOR BarForeGround, BarBackGround
               LOCATE TopLocation + CH - 1, Margin
               PRINT Selection$(CH);
               CHOOSING = 0
            END IF
         NEXT J
      ELSE
         IF a = 72 OR a = 56 THEN
            CH = CH - 1
         ELSEIF a = 80 OR a = 50 THEN
            CH = CH + 1
         ELSEIF a = 71 OR a = 73 OR a = 55 OR a = 57 THEN
            CH = 1
         ELSEIF a = 79 OR a = 81 OR a = 49 OR a = 51 THEN
            CH = UBOUND(Selection$)
         END IF
      END IF
      IF CH > UBOUND(Selection$) THEN CH = 1
      IF CH < 1 THEN CH = UBOUND(Selection$)
   WEND
COLOR ForeGround, BackGround
END SUB

SUB Prompt STATIC
   TextScroll 13, 21, 25, 40, 2, 0
   COLOR 1, 0
   LOCATE 25, 21
   PRINT CHR$(16);
   COLOR 15, 0
END SUB

SUB Prompt2 STATIC
   TextScroll 13, 21, 25, 40, 1, 0
   COLOR 1, 0
   LOCATE 25, 22
   PRINT ;
   COLOR 15, 0
END SUB

SUB Prompt3 STATIC
   COLOR 1, 0
   LOCATE 25, 22
   PRINT ;
   COLOR 15, 0
END SUB

SUB ReadData (a%) STATIC
   C% = 1
   DO
      READ a$
      C% = C% + 1
   LOOP UNTIL C% > a%
   RESTORE
   PRINT a$;
END SUB

SUB ReadyWeapon (Info AS Chrinfo, Info2 AS Chrinfo2) STATIC
   DIM list$(3)
   DIM List2$(6)
   DIM List3$(4)
   IF Info.pcRole = "Warrior " THEN Misc12 = 1
   IF Info.pcRole = "Wizard  " THEN Misc12 = 2
   DO
      Prompt2
      PRINT "Ready List:";
      TextScroll 13, 21, 25, 40, 11, 0
      Choice = 1
      list$(1) = "1 Ready Weapon"
      list$(2) = "2 Ready Armor"
      list$(3) = "3 Exit List"
      CALL MenuBar(list$(), 16, 22, 15, 0, 4, 0, Choice)
      IF Choice = 1 THEN
         Prompt2
         PRINT "Ready Weapon:";
         TextScroll 13, 21, 25, 40, 11, 0
         Choice2 = 1
         List2$(1) = "----------"
         List2$(2) = "----------"
         List2$(3) = "----------"
         List2$(4) = "----------"
         List2$(5) = "----------"
         List2$(6) = "----------"
         IF Misc12 = 1 AND Info2.SSword > 0 THEN List2$(1) = "Short Sword"
         IF Info2.Dagger > 0 THEN List2$(2) = "Dagger"
         IF Misc12 = 1 AND Info2.LSword > 0 THEN List2$(3) = "Long Sword"
         IF Misc12 = 1 AND Info2.Bow > 0 THEN List2$(4) = "Short Bow"
         IF Misc12 = 1 AND Info2.LBow > 0 THEN List2$(5) = "Long Bow"
         IF Info2.Sling > 0 THEN List2$(6) = "Sling"
         CALL MenuBar(List2$(), 16, 22, 15, 0, 4, 0, Choice2)
         IF Choice2 = 1 AND Info2.SSword > 0 THEN Info.ReadWeapon1 = 1
         IF Choice2 = 2 AND Info2.Dagger > 0 THEN Info.ReadWeapon1 = 2
         IF Choice2 = 3 AND Info2.LSword > 0 THEN Info.ReadWeapon1 = 5
         IF Choice2 = 4 AND Info2.Bow > 0 THEN Info.ReadWeapon1 = 6
         IF Choice2 = 5 AND Info2.LBow > 0 THEN Info.ReadWeapon1 = 7
         IF Choice2 = 6 AND Info2.Sling > 0 THEN Info.ReadWeapon1 = 8
      END IF
      IF Choice = 2 THEN
         Prompt2
         PRINT "Ready Armor:";
         TextScroll 13, 21, 25, 40, 11, 0
         choice3 = 1
         List3$(1) = "----------"
         List3$(2) = "----------"
         List3$(3) = "----------"
         List3$(4) = "----------"
         IF Info2.Cloth > 0 THEN List3$(1) = "Cloth"
         IF Info2.Leather > 0 THEN List3$(2) = "Leather"
         IF Misc12 = 1 AND Info2.ChainMale > 0 THEN List3$(3) = "Chain Mail"
         IF Misc12 = 1 AND Info2.PlateMale > 0 THEN List3$(4) = "Plate Mail"
         CALL MenuBar(List3$(), 16, 22, 15, 0, 4, 0, choice3)
         IF choice3 = 1 AND Info2.Cloth > 0 THEN Info.ReadArmor = 11
         IF choice3 = 2 AND Info2.Leather > 0 THEN Info.ReadArmor = 12
         IF choice3 = 3 AND Info2.ChainMale > 0 THEN Info.ReadArmor = 13
         IF choice3 = 4 AND Info2.PlateMale > 0 THEN Info.ReadArmor = 14
      END IF
   LOOP UNTIL Choice = 3
   ERASE list$
   ERASE List2$
   ERASE List3$
END SUB

SUB RestoreLife (Info AS Chrinfo, Info2 AS Chrinfo2, Location AS LocateInfo, x, y) STATIC
   TIMER OFF
   IF Info.Mode = 2 THEN
      qx = x
      qy = y
      IF qx > 70 THEN qx = qx - 70
      IF qy > 70 THEN qy = qy - 70
      Misc8 = Info2.NumShips
      FOR Misc3 = 1 TO INT(Info2.NumShips / 10)
         Misc8 = Misc8 - 10
      NEXT
      IF Misc8 = 1 THEN
            Info2.NumShips = Info2.NumShips - 11
            Info2.Shipx = Info2.Shipx2
            Info2.ShipY = Info2.Shipy2
            Info2.Shipx2 = Info2.Shipx3
            Info2.Shipy2 = Info2.Shipy3
            Info2.Shipx3 = Info2.Shipx4
            Info2.Shipy3 = Info2.Shipy4
            Info2.Shipx4 = 0
            Info2.Shipy4 = 0
         ELSEIF Misc8 = 2 THEN
            Info2.NumShips = Info2.NumShips - 12
            Info2.Shipx2 = Info2.Shipx3
            Info2.Shipy2 = Info2.Shipy3
            Info2.Shipx3 = Info2.Shipx4
            Info2.Shipy3 = Info2.Shipy4
            Info2.Shipx4 = 0
            Info2.Shipy4 = 0
         ELSEIF Misc8 = 3 THEN
            Info2.NumShips = Info2.NumShips - 13
            Info2.Shipx3 = Info2.Shipx4
            Info2.Shipy3 = Info2.Shipy4
            Info2.Shipx4 = 0
            Info2.Shipy4 = 0
         ELSEIF Misc8 = 4 THEN
            Info2.NumShips = Info2.NumShips - 14
            Info2.Shipx4 = 0
            Info2.Shipy4 = 0
         END IF
   END IF
   Info.pcHP = 1
   IF Info.Cit <> "00" THEN
      x = Info.x2
      y = Info.y2
      Info.Cit = "00"
      GetMap Info, Info2
      LoadMapTiles 1
      lx1 = -1
      ly1 = -1
      lx2 = -1
      ly2 = -1
      lx3 = -1
      ly3 = -1
   END IF
   Info.Mode = 1
   MapScroll Info, Info2, "Redraw", x, y
   Info.pcHP = 0
   Prompt2
   Prompt2
   PRINT "You are dead!";
   Prompt2
   Prompt2
   PRINT "Press a key...";
   a$ = INPUT$(1)
   MainStats Info, Info2
   Info.pcHP = 1
   Prompt
   PRINT "Darkness surrounds";
   Prompt2
   PRINT "you...";
   CastingEffect3
   Prompt
   PRINT "Hey, wait a";
   Prompt2
   PRINT "minute...";
   SLEEP 5
   Prompt
   PRINT "I wonder where";
   Prompt2
   PRINT "I am...";
   SLEEP 5
   Prompt
   PRINT "It looks like";
   Prompt2
   PRINT "I am dead...";
   SLEEP 5
   Prompt
   PRINT "You hear";
   Prompt2
   PRINT "something!!!";
   SLEEP 5
   Prompt
   PRINT "You hear a";
   Prompt2
   PRINT "voice!!!";
   SLEEP 5
   Prompt
   PRINT "It Says:";
   SLEEP 5
   Prompt2
   Prompt2
   PRINT "NO STUPID!!!";
   Prompt2
   PRINT "You're Alive!!!";
   Info.Cit = "05"
   tre$ = Info.UnderTile
   Info.UnderTile = "E"
   x = 35
   y = 34
   Info2.Cit3 = "E"
   Info.Cit = "03"
   Info2.Cit2 = "  "
   Times1 = 1
   CitB$ = Info2.Cit3 + Info2.Cit2
   DO
      GET #3, Times1, Location
      IF CitB$ = Location.Header1 + Location.Header2 THEN EXIT DO
      Times1 = Times1 + 1
   LOOP UNTIL Times1 = LOF(3) \ LEN(Location) + 1
   Info.x2 = x
   Info.y2 = y
   x = 35
   y = 13
   Location.lx3 = x
   Location.ly3 = y
   Info.Worldy = "01"
   GetMap Info, Info2
   LoadMapTiles 2
   MapScroll Info, Info2, "Special", x, y
   IF Info.pcLvl = 2 THEN Info.pcExp = 201
   IF Info.pcLvl = 3 THEN Info.pcExp = 601
   IF Info.pcLvl = 4 THEN Info.pcExp = 1101
   IF Info.pcLvl = 5 THEN Info.pcExp = 1501
   IF Info.pcLvl = 6 THEN Info.pcExp = 2301
   IF Info.pcLvl = 7 THEN Info.pcExp = 3001
   IF Info.pcLvl = 8 THEN Info.pcExp = 4001
   IF Info.pcLvl = 9 THEN Info.pcExp = 5001
   IF Info.pcLvl = 10 THEN Info.pcExp = 7001
   MainStats Info, Info2
   CastingEffect4
   Prompt
   PRINT "Do be more";
   Prompt2
   PRINT "careful!!!";
   TIMER ON
END SUB

SUB Shipry (Info AS Chrinfo, Info2 AS Chrinfo2) STATIC
   Misc9 = 500
   IF Info2.NumShips = 20 THEN Misc9 = 700
   IF Info2.NumShips = 30 THEN Misc9 = 1000
   IF Info2.NumShips = 40 THEN Misc9 = 9999
   Prompt2
   PRINT "Welcome to";
   Prompt2
   PRINT "4B's Shippry a";
   Prompt2
   PRINT "ship costs"; Misc9; "GP.";
   Prompt2
   YesOrNo Choice
   IF Choice = 1 THEN
      IF Info.pcGold >= Misc9 THEN
         Info2.NumShips = Info2.NumShips + 10
         IF Info2.NumShips >= 50 THEN Info2.Shipx = 3: Info2.ShipY = 50: Info2.NumShips = 10
         IF Info2.NumShips = 10 THEN Info2.Shipx = 3: Info2.ShipY = 50
         IF Info2.NumShips = 20 THEN Info2.Shipx2 = 3: Info2.Shipy2 = 51
         IF Info2.NumShips = 30 THEN Info2.Shipx3 = 3: Info2.Shipy3 = 52
         IF Info2.NumShips = 40 THEN Info2.Shipx4 = 4: Info2.Shipy4 = 53
         Info.pcGold = Info.pcGold - Misc9
         MainStats Info, Info2
         Prompt2
         PRINT "Come again!!";
      ELSE
         PRINT "Need more money!";
      END IF
   END IF
END SUB

SUB Shires (Info AS Chrinfo, Info2 AS Chrinfo2) STATIC
   Prompt2
   IF Info2.Cit2 = "A " THEN PRINT "I am Mr. Im";  ELSE PRINT "I am Mr. Om";
   SLEEP 5
   Prompt2
   PRINT "Let me look";
   Prompt2
   PRINT "within yourself.";
   SLEEP 5
   Prompt2
   PRINT "I see that...";
   SLEEP 5
   IF Info.pcLvl = 1 AND Info.pcExp > 200 THEN
      Info.pcLvl = Info.pcLvl + 1
      IF Info.pcRole = "Warrior " THEN Misc1 = 100 ELSE Misc1 = 50
      Info.pcMHP = Info.pcMHP + Misc1
      Info.pcHP = Info.pcMHP
      IF Info.pcRole = "Warrior " THEN Misc1 = 0 ELSE Misc1 = 10
      Info.pcMMP = Info.pcMMP + Misc1
      Info.pcMP = Info.pcMMP
   END IF
   IF Info.pcLvl < 3 AND Info.pcExp > 600 THEN
      Info.pcLvl = Info.pcLvl + 1
      IF Info.pcRole = "Warrior " THEN Misc1 = 100 ELSE Misc1 = 50
      Info.pcMHP = Info.pcMHP + Misc1
      Info.pcHP = Info.pcMHP
      IF Info.pcRole = "Warrior " THEN Misc1 = 0 ELSE Misc1 = 10
      Info.pcMMP = Info.pcMMP + Misc1
      Info.pcMP = Info.pcMMP
   END IF
   IF Info.pcLvl < 4 AND Info.pcExp > 1100 THEN
      Info.pcLvl = Info.pcLvl + 1
      IF Info.pcRole = "Warrior " THEN Misc1 = 100 ELSE Misc1 = 50
      Info.pcMHP = Info.pcMHP + Misc1
      Info.pcHP = Info.pcMHP
      IF Info.pcRole = "Warrior " THEN Misc1 = 0 ELSE Misc1 = 10
      Info.pcMMP = Info.pcMMP + Misc1
      Info.pcMP = Info.pcMMP
   END IF
   IF Info.pcLvl < 5 AND Info.pcExp > 1500 THEN
      Info.pcLvl = Info.pcLvl + 1
      IF Info.pcRole = "Warrior " THEN Misc1 = 100 ELSE Misc1 = 50
      Info.pcMHP = Info.pcMHP + Misc1
      Info.pcHP = Info.pcMHP
      IF Info.pcRole = "Warrior " THEN Misc1 = 0 ELSE Misc1 = 10
      Info.pcMMP = Info.pcMMP + Misc1
      Info.pcMP = Info.pcMMP
   END IF
   IF Info.pcLvl < 6 AND Info.pcExp > 2300 THEN
      Info.pcLvl = Info.pcLvl + 1
      IF Info.pcRole = "Warrior " THEN Misc1 = 100 ELSE Misc1 = 50
      Info.pcMHP = Info.pcMHP + Misc1
      Info.pcHP = Info.pcMHP
      IF Info.pcRole = "Warrior " THEN Misc1 = 0 ELSE Misc1 = 10
      Info.pcMMP = Info.pcMMP + Misc1
      Info.pcMP = Info.pcMMP
   END IF
   IF Info.pcLvl < 7 AND Info.pcExp > 3000 THEN
      Info.pcLvl = Info.pcLvl + 1
      IF Info.pcRole = "Warrior " THEN Misc1 = 100 ELSE Misc1 = 50
      Info.pcMHP = Info.pcMHP + Misc1
      Info.pcHP = Info.pcMHP
      IF Info.pcRole = "Warrior " THEN Misc1 = 0 ELSE Misc1 = 10
      Info.pcMMP = Info.pcMMP + Misc1
      Info.pcMP = Info.pcMMP
   END IF
   IF Info.pcLvl < 8 AND Info.pcExp > 4000 THEN
      Info.pcLvl = Info.pcLvl + 1
      IF Info.pcRole = "Warrior " THEN Misc1 = 100 ELSE Misc1 = 50
      Info.pcMHP = Info.pcMHP + Misc1
      Info.pcHP = Info.pcMHP
      IF Info.pcRole = "Warrior " THEN Misc1 = 0 ELSE Misc1 = 10
      Info.pcMMP = Info.pcMMP + Misc1
      Info.pcMP = Info.pcMMP
   END IF
   IF Info.pcLvl < 9 AND Info.pcExp > 5000 THEN
      Info.pcLvl = Info.pcLvl + 1
      IF Info.pcRole = "Warrior " THEN Misc1 = 100 ELSE Misc1 = 50
      Info.pcMHP = Info.pcMHP + Misc1
      Info.pcHP = Info.pcMHP
      IF Info.pcRole = "Warrior " THEN Misc1 = 0 ELSE Misc1 = 10
      Info.pcMMP = Info.pcMMP + Misc1
      Info.pcMP = Info.pcMMP
   END IF
   IF Info.pcLvl < 10 AND Info.pcExp > 7000 THEN
      Info.pcLvl = Info.pcLvl + 1
      IF Info.pcRole = "Warrior " THEN Misc1 = 100 ELSE Misc1 = 50
      Info.pcMHP = Info.pcMHP + Misc1
      Info.pcHP = Info.pcMHP
      IF Info.pcRole = "Warrior " THEN Misc1 = 0 ELSE Misc1 = 10
      Info.pcMMP = Info.pcMMP + Misc1
      Info.pcMP = Info.pcMMP
      Info.Quest2 = 99
   END IF
   Prompt2
   PRINT "You are now:";
   Prompt2
   PRINT "Level"; Info.pcLvl;
   MainStats Info, Info2
   SLEEP 5
   Prompt2
   PRINT "You may go now.";
END SUB

SUB TheNews (Info AS Chrinfo, Info2 AS Chrinfo2, qx, qy, Misc1$) STATIC
      IF Info.Cit <> "00" AND Misc1$ = "X" THEN
         IF Info.Cit = "10" AND Info2.Cit2 = "B " THEN
            Prompt2
            PRINT "The Guard says:";
            Prompt2
            PRINT "Leave or be";
            Prompt2
            PRINT "arrested for";
            Prompt2
            PRINT "loitering!";
         ELSE
            Prompt2
            PRINT "The Guard says:";
            Prompt2
            PRINT "Can't you see";
            Prompt2
            PRINT "I'm busy!";
         END IF
      END IF
      IF Info.Cit <> "00" AND Misc1$ = "V" THEN
         IF Info.Cit = "04" THEN
            Prompt2
            PRINT "Evil Jester #1 says";
            Prompt2
            PRINT "You will never";
            Prompt2
            PRINT "kill me, ha! ha!";
         ELSEIF Info.Cit = "05" THEN
            Prompt2
            PRINT "Evil Jester#2 says";
            Prompt2
            PRINT "You will never get";
            Prompt2
            PRINT "past me, ha! ha!";
         ELSE
            Prompt2
            PRINT "The Jester says:";
            Prompt2
            PRINT "This Game is FUN!";
         END IF
      END IF
      IF Info.Worldy = "01" AND Info.Cit = "00" AND Misc1$ = "N" THEN
         IF Info.Quest3 = 3 THEN
            Prompt2
            PRINT "Booga says:";
            Prompt2
            PRINT "Before I turned";
            Prompt2
            PRINT "to stone I hid";
            Prompt2
            PRINT "it in the Dungeon";
            Prompt2
            PRINT "of This and That.";
         ELSE
            Prompt2
            PRINT "Booga says:";
            Prompt2
            PRINT "Learn wise sayings";
            Prompt2
            PRINT "They are good!";
         END IF
      END IF
      IF Info.Cit <> "00" AND Misc1$ = "Z" THEN
         IF Info.Cit = "08" THEN
            Prompt2
            PRINT "The Young-One says";
            Prompt2
            IF Info.Quest3 < 69 THEN
               PRINT "To get to the gate";
               Prompt2
               PRINT "you must remove";
               Prompt2
               PRINT "the mountains in";
               Prompt2
               PRINT "your way. Get the";
               Prompt2
               PRINT "Potion of Mountain";
               Prompt2
               PRINT "Removing from";
               Prompt2
               PRINT "Anthony Firesword";
               Info.Quest3 = 3
            ELSEIF Info.Quest3 < 99 THEN
               PRINT "Now use the Potion";
               Prompt2
               PRINT "on the mountian by";
               Prompt2
               PRINT "the Gate!";
            ELSEIF Info.Quest3 = 99 AND Info.Quest4 < 89 THEN
               PRINT "To use the gate";
               Prompt2
               PRINT "you must have the";
               Prompt2
               PRINT "Ring of Power that";
               Prompt2
               PRINT "is located in the";
               Prompt2
               PRINT "Evil Castle!";
            ELSE
               PRINT "I have not much";
               Prompt2
               PRINT "more to say!";
            END IF
         ELSEIF Info.Cit = "10" AND Info2.Cit2 = "B " THEN
            Prompt2
            PRINT "The Peasant says:";
            Prompt2
            PRINT "Leave me alone or";
            Prompt2
            PRINT "I will call the";
            Prompt2
            PRINT "Guards over!";
         ELSE
            Prompt2
            PRINT "The Peasant says:";
            Prompt2
            PRINT "Welcome brave";
            Prompt2
            PRINT "hero!!!";
         END IF
      END IF
      IF Info.Cit = "03" AND Misc1$ = "P" THEN
         Prompt2
         PRINT "Lancis of Lot says";
         Prompt2
         IF Info.Quest7 < 67 THEN
            PRINT "You will need the";
            Prompt2
            PRINT "Magic Armor and";
            Prompt2
            PRINT "Weapons located in";
            Prompt2
            PRINT "the Dungeon of ";
            Prompt2
            PRINT "Frozen Frost";
         ELSE
            PRINT "Good luck!";
         END IF
      END IF
      IF Info.Cit = "09" AND Info2.Cit2 = "A " AND Misc1$ = "P" THEN
         Prompt2
         PRINT "Excavior says";
         Prompt2
         IF Info.Quest2 < 67 THEN
            PRINT "Did you know that";
            Prompt2
            PRINT "to open the Shrines";
            Prompt2
            PRINT "you need the Silver";
            Prompt2
            PRINT "Key which is in the";
            Prompt2
            PRINT "Dungeon of";
            Prompt2
            PRINT "Something!";
         ELSE
            PRINT "Good luck!";
         END IF
      END IF
      IF Info.Cit = "09" AND Info2.Cit2 = "B " AND Misc1$ = "P" THEN
         Prompt2
         PRINT "Excalibor says";
         Prompt2
         IF Info.Quest2 < 67 THEN
            PRINT "Did you know that";
            Prompt2
            PRINT "to open the Gates";
            Prompt2
            PRINT "you need the Magic";
            Prompt2
            PRINT "Gem which is in the";
            Prompt2
            PRINT "Dungeon of No Echo!";
         ELSE
            PRINT "Good luck!";
         END IF
      END IF
      IF Info.Cit = "10" AND Info2.Cit2 = "B " AND Misc1$ = "P" THEN
         Prompt2
         PRINT "Kel Dranstack says";
         Prompt2
         IF Info.Quest2 < 67 THEN
            PRINT "Did you know that";
            Prompt2
            PRINT "to kill Evil Jester";
            Prompt2
            PRINT "#2 you need the";
            Prompt2
            PRINT "Staff of Death in";
            Prompt2
            PRINT "the Dungeon of";
            Prompt2
            PRINT "Nalixama in the";
            Prompt2
            PRINT "Sea of Lava.";
         ELSE
            PRINT "What do you think I";
            Prompt2
            PRINT "am, a plethra of";
            Prompt2
            PRINT "information?";
         END IF
      END IF
      IF Info.Cit = "10" AND Info2.Cit2 = "C " AND Misc1$ = "P" THEN
         Prompt2
         PRINT "Sir Fur says";
         Prompt2
         IF Info.Quest5 < 99 THEN
            PRINT "Did you know that";
            Prompt2
            PRINT "you must use the";
            Prompt2
            PRINT "Staff o' Opening";
            Prompt2
            PRINT "to enter the ";
            Prompt2
            PRINT "Other Evil ";
            Prompt2
            PRINT "Castle in the";
            Prompt2
            PRINT "Sea of Lava.";
         ELSE
            PRINT "Have fun!";
         END IF
      END IF
      IF Info.Cit = "07" AND Misc1$ = "P" THEN
         Prompt2
         PRINT "Shamoo says";
         Prompt2
         IF Info.Quest2 < 67 THEN
            PRINT "Did you know that";
            Prompt2
            PRINT "to kill Evil Jester";
            Prompt2
            PRINT "#1 you need the";
            Prompt2
            PRINT "Finger of Death ";
            Prompt2
            PRINT "in the Dungeon of";
            Prompt2
            PRINT "the Ice Man.";
         ELSE
            PRINT "Good luck!";
         END IF
      END IF
      IF Info.Cit <> "00" AND Misc1$ = "Y" THEN
         Misc9 = 200
         Prompt2
         PRINT "I can heal you";
         Prompt2
         PRINT "for 200 Gold.";
         Prompt2
         YesOrNo Choice
         IF Choice = 1 THEN
            IF Info.pcGold >= Misc9 THEN
               Info.pcHP = Info.pcMHP
               Info.pcMP = Info.pcMMP
               Info.pcGold = Info.pcGold - Misc9
               CastingEffect2
               MainStats Info, Info2
               CastingEffect2
               Prompt2
               PRINT "Presto!";
            ELSE
               PRINT "Need more money!";
            END IF
         ELSE
            EXIT SUB
         END IF
      END IF
      IF Info.Cit <> "00" AND Info2.Cit3 = "F" AND Misc1$ = "O" THEN
         IF Info.Cit = "10" AND Info2.Cit2 = "C " AND Misc1$ = "O" THEN
            Prompt2
            PRINT "Polaris says:";
            Prompt2
            PRINT "Remember the";
            Prompt2
            PRINT "word:";
            Prompt2
            PRINT "     QWERTY";
         ELSE
            Misc9 = 500
            Misc8 = 200
            Prompt2
            PRINT "I can raise 200";
            Prompt2
            PRINT "Exp for 500 GP";
            Prompt2
            YesOrNo Choice
            IF Choice = 1 THEN
               IF Info.pcGold >= Misc9 THEN
                  Info.pcExp = Info.pcExp + Misc8
                  IF Info.pcExp > 9999 THEN Info.pcExp = 9999
                  Info.pcGold = Info.pcGold - Misc9
                  CastingEffect2
                  MainStats Info, Info2
                  CastingEffect2
                  Prompt2
                  PRINT "Blammo!";
               ELSE
                  PRINT "Need more money!";
               END IF
            ELSE
               EXIT SUB
            END IF
         END IF
      END IF
      IF Info.Cit = "03" AND Misc1$ = "U" THEN
         Prompt2
         PRINT "King Stephen says:";
         Prompt2
         PRINT "This is a very";
         Prompt2
         PRINT "important Quest,";
         Prompt2
         PRINT "Don't waste time!";
      END IF
      IF Info.Cit = "05" AND Misc1$ = "U" THEN
         Prompt2
         PRINT "Lord Ardradd says:";
         Prompt2
         PRINT "It is time for";
         Prompt2
         PRINT "you to die!!!";
      END IF
      IF Info.Cit = "06" AND Misc1$ = "O" THEN
         IF Info.Quest3 = 3 THEN
            Prompt2
            PRINT "Anthony says:";
            Prompt2
            PRINT "Get me my Urn";
            Prompt2
            PRINT "of Power from";
            Prompt2
            PRINT "Booga first!";
         ELSEIF Info.Quest3 = 67 THEN
            Prompt2
            PRINT "Anthony says";
            Prompt2
            PRINT "Thanks for";
            Prompt2
            PRINT "getting my Urn";
            Prompt2
            PRINT "back, here is";
            Prompt2
            PRINT "the Potion you";
            Prompt2
            PRINT "wanted.";
            Info.Quest3 = 69
         ELSE
            Prompt2
            PRINT "Anthony says";
            Prompt2
            PRINT "Welcome!";
            Prompt2
            PRINT "Adventurer!";
         END IF
      END IF
      IF Info.Cit = "08" AND Misc1$ = "N" THEN
         Shires Info, Info2
      END IF
      IF Info.Cit <> "00" AND Misc1$ = "W" THEN
         IF Info.Cit = "09" THEN
            IF qx = 27 AND qy = 12 THEN
               JoesDiner Info, Info2
            ELSEIF qx = 8 AND qy = 12 THEN
               JoesInn Info, Info2, x, y
            ELSEIF qx = 14 AND qy = 5 THEN
               Armory Info, Info2
            ELSEIF qx = 20 AND qy = 5 THEN
               Weaponry Info, Info2
            ELSEIF qx = 66 AND qy = 29 THEN
               JoesDiner Info, Info2
            ELSEIF qx = 60 AND qy = 4 THEN
               ThePub Info, Info2
            ELSEIF qx = 47 AND qy = 4 THEN
               JoesInn Info, Info2, x, y
            ELSEIF qx = 27 AND qy = 63 THEN
               JoesDiner Info, Info2
            ELSEIF qx = 31 AND qy = 39 THEN
               Armory Info, Info2
            ELSEIF qx = 63 AND qy = 55 THEN
               ThePub Info, Info2
            ELSEIF qx = 44 AND qy = 60 THEN
               Armory Info, Info2
            ELSEIF qx = 45 AND qy = 48 THEN
               Weaponry Info, Info2
            ELSEIF qx = 58 AND qy = 44 THEN
               Shipry Info, Info2
            ELSE
               Prompt2
               PRINT "Sorry, I";
               Prompt2
               PRINT "can't help you!";
            END IF
         ELSEIF Info.Cit = "10" THEN
            IF qx = 9 AND qy = 6 THEN
               JoesDiner Info, Info2
            ELSEIF qx = 9 AND qy = 15 THEN
               ThePub Info, Info2
            ELSEIF qx = 62 AND qy = 28 THEN
               JoesDiner Info, Info2
            ELSEIF qx = 45 AND qy = 28 THEN
               Armory Info, Info2
            ELSEIF qx = 45 AND qy = 8 THEN
               Weaponry Info, Info2
            ELSEIF qx = 29 AND qy = 41 THEN
               JoesDiner Info, Info2
            ELSEIF qx = 10 AND qy = 62 THEN
               ThePub Info, Info2
            ELSEIF qx = 26 AND qy = 52 THEN
               JoesInn Info, Info2, x, y
            ELSE
               Prompt2
               PRINT "Sorry, I";
               Prompt2
               PRINT "can't help you!";
            END IF
         END IF
      END IF
END SUB

SUB ThePub (Info AS Chrinfo, Info2 AS Chrinfo2) STATIC
   Prompt2
   PRINT "Welcome to another";
   Prompt2
   PRINT "Joe's Pub!!!";
   Prompt2
   PRINT "Want to buy some";
   Prompt2
   PRINT "information?";
   Choice = 1
   Prompt2
   Prompt2
   Prompt2
   DIM list$(2)
   list$(1) = "Yes, I do!"
   list$(2) = "No thanks!"
   CALL MenuBar(list$(), 24, 22, 15, 0, 4, 0, Choice)
   IF Choice = 1 THEN
      Prompt2
      PRINT "Info List";
      TextScroll 13, 21, 25, 40, 11, 0
      DIM List2$(8)
      List2$(1) = "The Gates"
      List2$(2) = "The Shrines"
      List2$(3) = "An Abandon Hut"
      List2$(4) = "Magic Stuff"
      List2$(5) = "The Lava Sea"
      List2$(6) = "Evil Jesters"
      List2$(7) = "Sir Kelemore"
      List2$(8) = "Exit this List"
      CALL MenuBar(List2$(), 16, 22, 15, 0, 4, 0, Choice)
      IF Info.pcGold >= 10 THEN
         IF Choice = 8 THEN Prompt2: PRINT "Exit List"; : EXIT SUB
         Info.pcGold = Info.pcGold - 10
         MainStats Info, Info2
         Prompt
         PRINT "Well last time";
         Prompt2
         PRINT "I heard any thing";
         Prompt2
         PRINT "about:";
         Prompt2
         PRINT RTRIM$(List2$(Choice)) + " was";
         Prompt2
         IF Choice = 1 THEN
            PRINT "when Excalibor was";
            Prompt2
            PRINT "talking about them!";
         ELSEIF Choice = 2 THEN
            PRINT "when Excavior was";
            Prompt2
            PRINT "talking about them!";
         ELSEIF Choice = 3 THEN
            PRINT "when I overheard";
            Prompt2
            PRINT "a person say there";
            Prompt2
            PRINT "was something very";
            Prompt2
            PRINT "interesting in the";
            Prompt2
            PRINT "Hut on the Southern";
            Prompt2
            PRINT "Island!";
         ELSEIF Choice = 4 THEN
            PRINT "when I overheard a";
            Prompt2
            PRINT "person say that you";
            Prompt2
            PRINT "can only survive an";
            Prompt2
            PRINT "attack by Ardradd";
            Prompt2
            PRINT "if you wear it!";
         ELSEIF Choice = 5 THEN
            PRINT "when I overheard";
            Prompt2
            PRINT "a person say that";
            Prompt2
            PRINT "you must use the ";
            Prompt2
            PRINT "gate in the moun-";
            Prompt2
            PRINT "tains to get there!";
         ELSEIF Choice = 6 THEN
            PRINT "when Dranctack was";
            Prompt2
            PRINT "talking about them!";
         ELSEIF Choice = 7 THEN
            PRINT "when I overheard";
            Prompt2
            PRINT "a person say that";
            Prompt2
            PRINT "he is lost in the ";
            Prompt2
            PRINT "Dungeon of No Echo!";
         END IF
      ELSE
         PRINT "Need 5 gold!";
      END IF
   END IF
   ERASE list$
   ERASE List2$
END SUB

SUB UseIt2 (Info AS Chrinfo, Info2 AS Chrinfo2, x, y) STATIC
      DIM List2$(8)
      Prompt2
      PRINT "Use Item Menu";
      TextScroll 13, 21, 25, 40, 11, 0
      Choice = 1
      List2$(1) = "------------------"
      List2$(2) = "------------------"
      List2$(3) = "------------------"
      List2$(4) = "------------------"
      List2$(5) = "------------------"
      List2$(6) = "------------------"
      List2$(7) = "------------------"
      List2$(8) = "Exit Menu"
      IF Info.Quest1 = 67 THEN List2$(1) = "Lion's Sword"
      IF Info.Quest1 = 69 THEN List2$(1) = "Magic Gem"
      IF Info.Quest2 = 67 THEN List2$(2) = "Silver Key"
      IF Info.Quest3 = 67 THEN List2$(2) = "Urn of Power"
      IF Info.Quest3 = 69 THEN List2$(3) = "Mountain Remover"
      IF Info.Quest4 = 67 THEN List2$(4) = "Finger o' Death"
      IF Info.Quest4 = 89 THEN List2$(4) = "Ring of Power"
      IF Info.Quest5 = 67 THEN List2$(5) = "Staff o' Opening"
      IF Info.Quest6 = 67 THEN List2$(6) = "Lock Picking Key"
      IF Info.Quest6 = 69 THEN List2$(6) = "Staff o' Death"
      IF Info.Quest7 = 67 THEN List2$(7) = "Magic Stuff"
      CALL MenuBar(List2$(), 16, 22, 15, 0, 4, 0, Choice)
      Prompt2
      IF Choice = 1 AND Info.Quest1 = 69 THEN
         IF Info.Cit = "00" THEN
            CastingEffect2
            Info.Quest1 = 99
            Info.pcExp = Info.pcExp + 100
            IF Info.pcExp > 9999 THEN Info.pcExp = 9999
            MainStats Info, Info2
            CastingEffect2
            PRINT "The Magic Gem";
            Prompt2
            PRINT "has now opened";
            Prompt2
            PRINT "the Gates of";
            Prompt2
            PRINT "Allasia!!!";
         ELSE
            Prompt2
            PRINT "Has no effect!";
         END IF
      ELSEIF Choice = 2 AND Info.Quest2 = 67 THEN
         IF Info.Cit = "00" AND Info.UnderTile = "M" THEN
            CastingEffect2
            Info.Quest2 = 69
            Info.pcExp = Info.pcExp + 100
            IF Info.pcExp > 9999 THEN Info.pcExp = 9999
            MainStats Info, Info2
            CastingEffect2
            PRINT "The Silver";
            Prompt2
            PRINT "has now opened";
            Prompt2
            PRINT "the Shrines of";
            Prompt2
            PRINT "Allasia!!!";
         ELSE
            Prompt2
            PRINT "Has no effect.";
         END IF
      ELSEIF Choice = 3 AND Info.Quest3 = 69 THEN
         IF Info.Cit = "00" AND x = 39 AND y = 74 THEN
            CastingEffect2
            Info.Quest3 = 99
            Info.pcExp = Info.pcExp + 100
            IF Info.pcExp > 9999 THEN Info.pcExp = 9999
            MainStats Info, Info2
            CastingEffect2
            PRINT "The Potion of";
            Prompt2
            PRINT "Mountian Removing";
            Prompt2
            PRINT "has make a path to";
            Prompt2
            PRINT "the secret Gate!";
         ELSE
            Prompt2
            PRINT "Has no effect.";
         END IF
      ELSEIF Choice = 4 AND Info.Quest4 = 67 THEN
         IF Info.Cit = "04" THEN
            CastingEffect2
            Info.Quest4 = 69
            Info.pcExp = Info.pcExp + 100
            IF Info.pcExp > 9999 THEN Info.pcExp = 9999
            MainStats Info, Info2
            CastingEffect2
            PRINT "Evil Jester #1";
            Prompt2
            PRINT "is now dead!";
         ELSE
            Prompt2
            PRINT "Has no effect.";
         END IF
      ELSEIF Choice = 4 AND Info.Quest4 = 89 THEN
         IF Info.Cit = "00" THEN
            Info.Quest4 = 99
            Info.pcExp = Info.pcExp + 100
            IF Info.pcExp > 9999 THEN Info.pcExp = 9999
            MainStats Info, Info2
            PRINT "You now wear the";
            Prompt2
            PRINT "Ring of Power!";
         ELSE
            Prompt2
            PRINT "Has no effect.";
         END IF
      ELSEIF Choice = 5 AND Info.Quest5 = 67 THEN
         IF Info.Worldy = "02" AND x = 28 AND y = 44 THEN
            CastingEffect2
            CastingEffect2
            PRINT "A voice rings";
            Prompt2
            PRINT "out and says:";
            Prompt2
            PRINT "What is soon to";
            Prompt2
            PRINT "come but not yet";
            Prompt2
            PRINT "has???";
            Prompt2
            Prompt2
            FixInput 25, 22, Misc1$, 19
            IF Misc1$ <> "THE E" + "N" + "D" THEN
               Prompt2
               PRINT "ACCESS DENIED!!!";
               EXIT SUB
            END IF
            CastingEffect2
            CastingEffect2
            Info.Quest5 = 99
            Info.pcExp = Info.pcExp + 100
            IF Info.pcExp > 9999 THEN Info.pcExp = 9999
            MainStats Info, Info2
            Prompt
            PRINT "The Other Evil";
            Prompt2
            PRINT "Castle is now";
            Prompt2
            PRINT "open for you to";
            Prompt2
            PRINT "enter!";
         ELSE
            Prompt2
            PRINT "Has no effect.";
         END IF
      ELSEIF Choice = 6 AND Info.Quest6 = 69 THEN
         IF Info.Cit = "05" AND x = 35 AND y = 59 THEN
            CastingEffect2
            Info.Quest6 = 99
            Info.pcExp = Info.pcExp + 100
            IF Info.pcExp > 9999 THEN Info.pcExp = 9999
            MainStats Info, Info2
            CastingEffect2
            PRINT "Evil Jester #2";
            Prompt2
            PRINT "is now dead!";
         ELSE
            Prompt2
            PRINT "Has no effect.";
         END IF
      ELSEIF Choice = 7 AND Info.Quest7 = 67 THEN
         Info.Quest7 = 69
         Info.pcExp = Info.pcExp + 100
         IF Info.pcExp > 9999 THEN Info.pcExp = 9999
         IF Info.pcRole = "Worrior " THEN Info.ReadWeapon1 = 10
         IF Info.pcRole = "Wizard  " THEN Info.ReadWeapon1 = 9
         Info.ReadArmor = 15
         MainStats Info, Info2
         PRINT "You now wear the";
         Prompt2
         PRINT "Magic Stuff!";
      ELSEIF Choice = 8 THEN
         EXIT SUB
      ELSE
         Prompt2
         PRINT "Has no effect!";
      END IF
      ERASE List2$
      IF Info.Mode = 3 THEN DungScroll Info, Info2, "Redraw", x, y ELSE MapScroll Info, Info2, "Redraw", x, y
END SUB

SUB Weaponry (Info AS Chrinfo, Info2 AS Chrinfo2) STATIC
   DIM list$(3)
   DIM List2$(7)
   DIM List3$(7)
   DO
      MainStats Info, Info2
      PRINT "Welcome to another";
      Prompt2
      PRINT "of Joe's Weaponry";
      Choice = 1
      TextScroll 13, 21, 25, 40, 10, 0
      list$(1) = "1 Buy Weapons"
      list$(2) = "2 Sell Weapons"
      list$(3) = "3 Leave Shop"
      CALL MenuBar(list$(), 17, 22, 15, 0, 4, 0, Choice)
      IF Choice = 1 THEN
         Prompt2
         PRINT "What kind?";
         TextScroll 13, 21, 25, 40, 11, 0
         choice3 = 1
         List2$(1) = "Short Sword    50gp"
         List2$(2) = "Dagger         25gp"
         List2$(3) = "Long Sword     75gp"
         List2$(4) = "Short Bow      50gp"
         List2$(5) = "Long Bow      100gp"
         List2$(6) = "Sling          50gp"
         List2$(7) = "Not Interested!"
         CALL MenuBar(List2$(), 16, 22, 15, 0, 4, 0, choice3)
         IF choice3 = 1 AND Info.pcGold >= 50 THEN
            Info.pcGold = Info.pcGold - 50
            Info2.SSword = Info2.SSword + 1
         ELSEIF choice3 = 2 AND Info.pcGold >= 25 THEN
            Info.pcGold = Info.pcGold - 25
            Info2.Dagger = Info2.Dagger + 1
         ELSEIF choice3 = 3 AND Info.pcGold >= 75 THEN
            Info.pcGold = Info.pcGold - 75
            Info2.LSword = Info2.LSword + 1
         ELSEIF choice3 = 4 AND Info.pcGold >= 50 THEN
            Info.pcGold = Info.pcGold - 50
            Info2.Bow = Info2.Bow + 1
         ELSEIF choice3 = 5 AND Info.pcGold >= 100 THEN
            Info.pcGold = Info.pcGold - 100
            Info2.LBow = Info2.LBow + 1
         ELSEIF choice3 = 6 AND Info.pcGold >= 50 THEN
            Info.pcGold = Info.pcGold - 50
            Info2.Sling = Info2.Sling + 1
         END IF
      END IF
      IF Choice = 2 THEN
         Prompt2
         PRINT "Sell what?";
         TextScroll 13, 21, 25, 40, 11, 0
         choice3 = 1
         List3$(1) = "----------"
         List3$(2) = "----------"
         List3$(3) = "----------"
         List3$(4) = "----------"
         List3$(5) = "----------"
         List3$(6) = "----------"
         List3$(7) = "No, not now."
         IF Info2.SSword > 0 THEN List3$(1) = "Short Sword    20gp"
         IF Info2.Dagger > 0 THEN List3$(2) = "Dagger          5gp"
         IF Info2.LSword > 0 THEN List3$(3) = "Long Sword     20gp"
         IF Info2.Bow > 0 THEN List3$(4) = "Short Bow      15gp"
         IF Info2.LBow > 0 THEN List3$(5) = "Long Bow       23gp"
         IF Info2.Sling > 0 THEN List3$(6) = "Sling           7gp"
         CALL MenuBar(List3$(), 16, 22, 15, 0, 4, 0, choice3)
         IF choice3 = 1 AND Info2.SSword > 0 THEN
            Info.pcGold = Info.pcGold + 20
            Info2.SSword = Info2.SSword - 1
            IF Info2.SSword = 0 AND Info.ReadWeapon1 = 1 THEN Info.ReadWeapon1 = 26
         ELSEIF choice3 = 2 AND Info2.Dagger > 0 THEN
            Info.pcGold = Info.pcGold + 5
            Info2.Dagger = Info2.Dagger - 1
            IF Info2.Dagger = 0 AND Info.ReadWeapon1 = 2 THEN Info.ReadWeapon1 = 26
         ELSEIF choice3 = 3 AND Info2.LSword > 0 THEN
            Info.pcGold = Info.pcGold + 20
            Info2.LSword = Info2.LSword - 1
            IF Info2.LSword = 0 AND Info.ReadWeapon1 = 5 THEN Info.ReadWeapon1 = 26
         ELSEIF choice3 = 4 AND Info2.Bow > 0 THEN
            Info.pcGold = Info.pcGold + 15
            Info2.Bow = Info2.Bow - 1
            IF Info2.Bow = 0 AND Info.ReadWeapon1 = 6 THEN Info.ReadWeapon1 = 26
         ELSEIF choice3 = 5 AND Info2.LBow > 0 THEN
            Info.pcGold = Info.pcGold + 23
            Info2.LBow = Info2.LBow - 1
            IF Info2.LBow = 0 AND Info.ReadWeapon1 = 7 THEN Info.ReadWeapon1 = 26
         ELSEIF choice3 = 6 AND Info2.Sling > 0 THEN
            Info.pcGold = Info.pcGold + 7
            Info2.Sling = Info2.Sling - 1
            IF Info2.Sling = 0 AND Info.ReadWeapon1 = 8 THEN Info.ReadWeapon1 = 26
         END IF
      END IF
   LOOP UNTIL Choice = 3
   ERASE list$
   ERASE List2$
   ERASE List3$
   MainStats Info, Info2
END SUB

SUB YesOrNo (Choice) STATIC
   Prompt2
   PRINT "Do you want to?";
   Prompt2
   Prompt2
   Choice = 1
   DIM list$(2)
   list$(1) = "              Yes"
   list$(2) = "              No"
   CALL MenuBar(list$(), 24, 22, 15, 0, 4, 0, Choice)
END SUB

