' Copyright (C) 1991, Stephen Makonin. All Rights Reserved.

DECLARE SUB Conclusion ()
DECLARE SUB LoadScreen2 (Misc1$)
DECLARE SUB Center (row!, Text$)
DECLARE SUB LoadScreen (Misc1$)
DECLARE SUB FixInput (row!, col!, Text$, MaxLen!)
DECLARE SUB ClearScreen ()
DECLARE SUB Pause2 (Misc2!)
DECLARE SUB MainMenu ()
DECLARE SUB Intro ()
DECLARE SUB MakeChr ()
DECLARE SUB AbilityScore (a%)
DECLARE SUB CopyProtection ()
DECLARE SUB ShowTitles ()
DECLARE SUB ShowNames ()
DECLARE SUB Fade (text1$, text2$, text3$, text4$)
DECLARE SUB Fade2 (a$, B$, c$, cc$, d$, e$, f$, g$, h$)

ON ERROR GOTO errorhandler

TYPE ChrInfo
   YourName AS STRING * 10
   YourSex AS STRING * 1
   YourRole AS STRING * 8
   Worldy AS STRING * 2
   Cit AS STRING * 2
   UnderTile AS STRING * 1
   YourSTR AS INTEGER
   YourDEX AS INTEGER
   YourINT AS INTEGER
   YourLvl AS INTEGER
   YourExp AS INTEGER
   YourHP AS INTEGER
   YourMHP AS INTEGER
   YourMP AS INTEGER
   YourMMP AS INTEGER
   YourGold AS INTEGER
   YourFood AS INTEGER
   ReadWeapon1 AS INTEGER
   ReadWeapon2 AS INTEGER
   ReadArmor AS INTEGER
   x AS INTEGER
   Y AS INTEGER
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

TYPE ChrInfo2
   pc2Name AS STRING * 10
   pc2Sex AS STRING * 1
   pc2Role AS STRING * 8
   Worldy2 AS STRING * 2
   Cit2 AS STRING * 2
   UnderTile2 AS STRING * 1
   ShipX AS INTEGER
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
   Mode AS INTEGER
   Shipx2 AS INTEGER
   Shipy2 AS INTEGER
   Dead AS INTEGER
   Shipx3 AS INTEGER
   Shipy3 AS INTEGER
   Shipy4 AS INTEGER
   Shipx4 AS INTEGER
END TYPE

DIM SHARED Info AS ChrInfo
DIM SHARED Info2 AS ChrInfo2

IF COMMAND$ <> "±²³" THEN
   SCREEN 7
   ON KEY(1) GOSUB xc
   KEY(1) ON
   ShowTitles
   END
ELSE
   SCREEN 7
   Conclusion
   END
END IF

errorhandler:
 SCREEN 0
 WIDTH 80, 25
 CLS
 PRINT "Error #"; ERR; " has occurred please check manual for more details."
 END

xc:
   ClearScreen
   PALETTE 1, 1
   PALETTE 2, 2
   PALETTE 3, 3
   PALETTE 4, 4
   PALETTE 5, 5
   PALETTE 6, 6
   PALETTE 7, 7
   PALETTE 8, 8
   PALETTE 9, 9
   PALETTE 10, 10
   PALETTE 11, 11
   PALETTE 12, 12
   PALETTE 13, 13
   PALETTE 14, 14
   PALETTE 15, 15
   SCREEN 7, , 0, 0
   MainMenu

SUB AbilityScore (a%) STATIC
   RANDOMIZE TIMER
   a% = INT(RND * 20) + 1
   IF a% > 20 THEN LET a% = 20
   IF a% < 1 THEN LET a% = 1
END SUB

SUB Center (row, Text$)
   LOCATE row, 20 - (LEN(Text$) / 2 + .5)
   PRINT Text$;
END SUB

SUB ClearScreen
   x = 1
   Y = 1
   x2 = 320
   y2 = 200
   DO
      LINE (x, Y)-(x2, y2), 0, B
      x = x + 1
      Y = Y + 1
      x2 = x2 - 1
      y2 = y2 - 1
   LOOP UNTIL Y = 101
END SUB

SUB Conclusion
   OPEN "SAVEGAME.DAT" FOR RANDOM AS #4 LEN = LEN(Info)
   GET #4, 1, Info
   ClearScreen
   LoadScreen "ENDING.EGA"
   IF Info.YourRole = "Warrior " THEN Fade2 "As you pry your", "sword from the", "body of Ardradd", "you feel the", "castle shake and", "break apart. You", "Take Ardradd's", "sword and quikkly", "leave his castle."
   IF Info.YourRole = "Wizard  " THEN Fade2 "As you finish", "casting your spell", "Ardradd falls to", "the floor dead.", "You decide to", "take his sword as", "evidence. You", "quikkly leave the", "castle."
   Fade2 "As you run away", "you glance back to", "see the Other Evil", "Castle sink slowly", "into the lava.", "", "", "", ""
   Fade2 "You quikkly run to", "the Castle of King", "Stephen where you", "are proclaimed a", "Hero, you are then", "rewarded 1000 GP &", "are ask to attend", "a great Celebration", "in your name."
   LoadScreen "ENDING.EGA"
   END
END SUB

SUB Fade (text1$, text2$, text3$, text4$)
   a = 0
   DO
      COLOR a
      Pause2 100
      Center 11, text1$
      Center 12, text2$
      Center 13, text3$
      Center 14, text4$
      a = a + 1
   LOOP UNTIL a = 16
   Pause2 10000
   a = 15
   DO
      COLOR a
      Pause2 100
      Center 11, text1$
      Center 12, text2$
      Center 13, text3$
      Center 14, text4$
      a = a - 1
   LOOP UNTIL a = -1
END SUB

SUB Fade2 (a$, B$, c$, cc$, d$, e$, f$, g$, h$)
   a = 0
   DO
      COLOR a
      LOCATE 8, 12
      PRINT a$
      LOCATE 9, 12
      PRINT B$
      LOCATE 10, 12
      PRINT c$
      LOCATE 11, 12
      PRINT cc$
      LOCATE 12, 12
      PRINT d$
      LOCATE 13, 12
      PRINT e$
      LOCATE 14, 12
      PRINT f$
      LOCATE 15, 12
      PRINT g$
      LOCATE 16, 12
      PRINT h$
      a = a + 1
   LOOP UNTIL a = 16
   LOCATE 1, 1
   Misc1$ = INPUT$(1)
   a = 15
   DO
      COLOR a
      LOCATE 8, 12
      PRINT a$
      LOCATE 9, 12
      PRINT B$
      LOCATE 10, 12
      PRINT c$
      LOCATE 11, 12
      PRINT cc$
      LOCATE 12, 12
      PRINT d$
      LOCATE 13, 12
      PRINT e$
      LOCATE 14, 12
      PRINT f$
      LOCATE 15, 12
      PRINT g$
      LOCATE 16, 12
      PRINT h$
      a = a - 1
   LOOP UNTIL a = -1
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

SUB Intro
   ClearScreen
   LoadScreen "INTRO1.EGA"
   Fade2 "It is now the", "year 1021 and not", "all is well...", "", "King Stephen has", "been in power for", "100 years since", "the banishment", "of Lord Ardradd"
   Fade2 "We thought he", "would never come", "back...", "", "But we were wrong", "", "So we call upon a", "brave Hero to go", "forth & kill him."
   Fade2 "One day while", "visiting the", "town of Amber you", "see a sign that", "says:", "      WANTED", "Any brave soul to", "kill the evil Lord", "Inquire at Castle"
   Fade2 "As you leave Amber", "a thought of fame", "and fortune rushes", "through your mind,", "a thought of", "killing the evil", "Lord Ardradd.", "", ""
   Fade2 "Entering the", "Castle you meet", "with the king.", "He tells you of", "your mission, and", "then wishes you", "", "GOOD LUCK!!!", ""
END SUB

SUB LoadScreen (Misc1$)
   DIM FullScr(1 TO 9000)
   DEF SEG = VARSEG(FullScr(1))
   BLOAD Misc1$, VARPTR(FullScr(1))
   DEF SEG
   PUT (1, 1), FullScr, PSET
   ERASE FullScr
END SUB

SUB LoadScreen2 (Misc1$)
   DIM FullScr(1 TO 9000)
   DEF SEG = VARSEG(FullScr(1))
   BLOAD Misc1$, VARPTR(FullScr(1))
   DEF SEG
   PUT (0, 0), FullScr, PSET
   ERASE FullScr
END SUB

SUB MainMenu
   DO
      LoadScreen "TITLE3.EGA"
      a$ = UCASE$(INPUT$(1))
      IF a$ = "M" THEN MakeChr
      IF a$ = "C" THEN ClearScreen: RUN "GAME"
      IF a$ = "I" THEN Intro
      IF a$ = "Q" THEN ClearScreen: END
   LOOP
END SUB

SUB MakeChr
   OPEN "SAVEGAME.DAT" FOR RANDOM AS #2 LEN = LEN(Info)
   ClearScreen
   LoadScreen2 "MKCHR1.EGA"
   DO
      LOCATE 11, 24
      PRINT "          "
      LOCATE 13, 29
      PRINT "      "
      LOCATE 17, 5
      PRINT " "
      LOCATE 18, 5
      PRINT " "
      LOCATE 20, 30
      PRINT " "
      LINE (25, 148)-(283, 163), 0, B
      LINE (25, 74)-(283, 92), 4, B
      FixInput 11, 24, Info.YourName, 10
      LINE (25, 74)-(283, 92), 0, B
      LINE (25, 92)-(283, 108), 4, B
      DO
         Info.YourSex = UCASE$(INPUT$(1))
         IF Info.YourSex = "M" THEN a$ = "Male": EXIT DO
         IF Info.YourSex = "F" THEN a$ = "Female": EXIT DO
      LOOP
      LOCATE 13, 29
      PRINT a$
      LINE (25, 92)-(283, 108), 0, B
      LINE (25, 108)-(283, 148), 4, B
      DO
         B$ = UCASE$(INPUT$(1))
         IF B$ = "1" THEN Info.YourRole = "Wizard": LOCATE 17, 5: EXIT DO
         IF B$ = "2" THEN Info.YourRole = "Warrior": LOCATE 18, 5: EXIT DO
      LOOP
      PRINT "X"
      LINE (25, 108)-(283, 148), 0, B
      LINE (25, 148)-(283, 163), 4, B
      a$ = UCASE$(INPUT$(1))
      IF a$ <> "Y" THEN a$ = "N"
      LOCATE 20, 30
      PRINT a$
      IF a$ = "Y" THEN EXIT DO
   LOOP
   ClearScreen
   LoadScreen2 "MKCHR2.EGA"
   LOCATE 10, 21
   Info.YourLvl = 1
   PRINT Info.YourLvl
   LOCATE 11, 21
   Info.YourExp = 0
   PRINT Info.YourExp
   LOCATE 12, 21
   IF Info.YourRole = "Warrior " THEN LET Info.YourHP = 100: Info.YourMHP = 100
   IF Info.YourRole = "Wizard  " THEN LET Info.YourHP = 50: Info.YourMHP = 50
   PRINT Info.YourHP
   LOCATE 13, 21
   IF Info.YourRole = "Wizard  " THEN LET Info.YourMP = 10: Info.YourMMP = 10
   IF Info.YourRole = "Warrior " THEN LET Info.YourMP = 0: Info.YourMMP = 0
   PRINT Info.YourMP
   LOCATE 14, 21
   IF Info.YourRole = "Warrior " THEN LET Info.YourGold = 150
   IF Info.YourRole = "Wizard  " THEN LET Info.YourGold = 80
   PRINT Info.YourGold
   LOCATE 15, 21
   Info.YourFood = 5
   PRINT Info.YourFood
   LOCATE 11, 28
   IF Info.YourRole = "Warrior " THEN
      Info.ReadArmor = 26
      Info2.Leather = 1
      PRINT "Leather"
   END IF
   IF Info.YourRole = "Wizard  " THEN
      Info.ReadArmor = 26
      Info2.Cloth = 1
      PRINT "Cloth"
   END IF
   LOCATE 13, 28
   IF Info.YourRole = "Warrior " THEN
      Info.ReadWeapon1 = 26
      Info2.SSword = 1
      PRINT "S Sword"
   END IF
   IF Info.YourRole = "Wizard  " THEN
      Info.ReadWeapon1 = 26
      Info2.Dagger = 1
      PRINT "Dagger"
   END IF
   LOCATE 15, 28
   IF Info.YourRole = "Warrior " THEN
      Info.ReadWeapon2 = 3
      PRINT "Shield"
   END IF
   IF Info.YourRole = "Wizard  " THEN
      Info.ReadWeapon2 = 4
      PRINT "Spell Book"
   END IF
   DO
      LOCATE 20, 32
      PRINT " "
      AbilityScore Info.YourSTR
      LOCATE 12, 9
      PRINT "  "
      LOCATE 12, 9
      PRINT USING "##"; Info.YourSTR
      AbilityScore Info.YourDEX
      LOCATE 13, 9
      PRINT "  "
      LOCATE 13, 9
      PRINT USING "##"; Info.YourDEX
      AbilityScore Info.YourINT
      LOCATE 14, 9
      PRINT "  "
      LOCATE 14, 9
      PRINT USING "##"; Info.YourINT
      a$ = UCASE$(INPUT$(1))
   LOOP UNTIL a$ = "N"
   LOCATE 20, 32
   PRINT a$
   Info.x = 34
   Info.Y = 34
   Info.Cit = "00"
   Info.Worldy = "01"
   Info.Mode = 1
   ClearScreen
   COLOR 15
   LOCATE 10, 8
   PRINT "Save this Character? (Y/N)"
   DO
      tre$ = UCASE$(INPUT$(1))
      IF tre$ = "Y" THEN PUT #2, 1, Info: PUT #2, 2, Info2: ClearScreen: EXIT SUB
      IF tre$ = "N" THEN ClearScreen: EXIT SUB
   LOOP
END SUB

SUB Pause2 (Misc2)
   Misc4 = 1
   DO
      Misc4 = Misc4 + 1
   LOOP UNTIL Misc4 = Misc2
END SUB

SUB ShowNames
   Fade "Staring", "(in official ramdom order)", "", ""
   Fade "YOU, Ya you!", "as", "THE HERO", ""
   Fade "Jeremy Fleet", "as", "Sir Kelemore Lionsbait", ""
   Fade "Dave Cichowski", "as", "Kel Dranstack ", ""
   Fade "Mike Lane", "as", "Sir Lancis of Lot", ""
   Fade "Stephen William Makonin", "as", "King Stephen", ""
   Fade "Scott Clifford", "as", "Anthony Firesword", ""
   Fade "Brian Port", "as", "THE Young - One", ""
   Fade "Steven Brown", "as", "THE Slave", ""
   Fade "Roddy Hayashi", "as", "Excaveor Goldglove", ""
   Fade "Dan Fodor", "as", "Excalibor Britebane", ""
   Fade "Mr. J. Ritchie", "as", "Lord Ardradd ", "(Bad Guy)"
   Fade "Mr. D. Hickey", "as", "The Great Rock", "(Booga)"
   Fade "Chris Voykin ", "as", "Evil Jester #1", ""
   Fade "Chris Voykin ", "as", "Evil Jester #2", "(is this possible)"
   Fade "Ralf Terpin", "as", "The Great Shamoo", ""
   Fade "", "and", "", ""
   Fade "Greg Akselson", "as", "Buccaneer Billy Beer Belly", "(4B) "
   Fade "Director ", "", "Stephen W. Makonin", ""
   Fade "Programming  ", "", "Stephen W. Makonin", ""
   Fade "Art  ", "", "Stephen W. Makonin", ""
   Fade "Music & Sound Effects", "", "What music and sound effects?", ""
   Fade "Quality Assurance", "", "Louise Makonin", "Mike Lane"
   Fade "Manuals ", "", "Stephen W. Makonin", ""
   Fade "Executive Producer", "", "Ms. Hill ", ""
   COLOR 7
END SUB

SUB ShowTitles
   SCREEN 7, , 0, 1
   LoadScreen "TITLE1.EGA"
   SCREEN 7, , 0, 0
   x = 63
   Y = 108
   DO
      PSET (x, Y), 14
      PSET (x + 1, Y - 2), 14
      PSET (x + 2, Y), 14
      Pause2 150
      Y = Y - 1
   LOOP UNTIL Y = 50
   PSET (63, 50), 14
   PSET (65, 50), 14
   PSET (63, 49), 14
   PSET (65, 49), 14
   PSET (63, 48), 14
   PSET (65, 48), 14
   Pause2 100
   LINE (10, 10)-(62, 44), 10
   LINE (11, 9)-(62, 43), 9
   LINE (12, 9)-(62, 42), 12
   Pause2 50
   LINE (118, 12)-(67, 43), 2
   LINE (118, 11)-(66, 43), 1
   LINE (118, 10)-(66, 42), 4
   Pause2 9000
   ClearScreen
   PALETTE 1, 0
   PALETTE 2, 0
   PALETTE 3, 0
   PALETTE 4, 0
   PALETTE 5, 0
   PALETTE 6, 0
   PALETTE 7, 0
   PALETTE 8, 0
   PALETTE 9, 0
   PALETTE 10, 0
   PALETTE 11, 0
   PALETTE 12, 0
   PALETTE 13, 0
   PALETTE 14, 0
   PALETTE 15, 0
   LoadScreen "TITLE2.EGA"
   PALETTE 1, 9
   Pause2 5000
   PALETTE 2, 9
   Pause2 5000
   PALETTE 3, 9
   Pause2 5000
   PALETTE 4, 9
   Pause2 5000
   PALETTE 5, 9
   Pause2 5000
   PALETTE 6, 9
   Pause2 5000
   PALETTE 7, 9
   Pause2 5000
   PALETTE 8, 9
   Pause2 5000
   PALETTE 9, 9
   Pause2 5000
   PALETTE 10, 9
   Pause2 5000
   PALETTE 11, 9
   Pause2 5000
   PALETTE 12, 9
   Pause2 5000
   PALETTE 13, 9
   Pause2 5000
   PALETTE 14, 4
   Pause2 5000
   PALETTE 15, 4
   Pause2 20000
   ClearScreen
   PALETTE 1, 1
   PALETTE 2, 2
   PALETTE 3, 3
   PALETTE 4, 4
   PALETTE 5, 5
   PALETTE 6, 6
   PALETTE 7, 7
   PALETTE 8, 8
   PALETTE 9, 9
   PALETTE 10, 10
   PALETTE 11, 11
   PALETTE 12, 12
   PALETTE 13, 13
   PALETTE 14, 14
   PALETTE 15, 15
   ShowNames
   ClearScreen
   Intro
   ClearScreen
   MainMenu
END SUB

