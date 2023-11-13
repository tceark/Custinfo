       CBL DATA(24)                                                     00000010
       IDENTIFICATION DIVISION.                                         00000020
       PROGRAM-ID.       TU003.                                         00000030
       DATE-WRITTEN.  05/19/89.                                         00000040
       DATE-COMPILED.                                                   00000050
 INFO *---------------START OF INFORMATION BLOCK----------------------* 00000060
            EJECT                                                       00000290
      *------------------------PROGRAM PURPOSE-------------------------*00000300
      *                                                                *00000310
      *  PROGRAM TITLE: TU003                                          *00000320
      *  PROGRAM TEXT: ONLINE TUTORIAL DISPLAY PROGRAM                 *00000330
      *                                                                *00000340
      *   THIS PROGRAM IS TO DISPLAY FIELD , SCREEN OR MESSAGE         *00000350
      *   HELP WHEN CONTROL IS PASSED FROM AN APPLICATION              *00000360
      *                                                                *00000370
      * ISSUBSTR - TO FORMAT VARIABLE LENGTH FIELDS                    *00000640
      *                                                                *00000650
      *                                                                *00000660
       ENVIRONMENT DIVISION.                                            00000980
       CONFIGURATION SECTION.                                           00000990
       INPUT-OUTPUT SECTION.                                            00001000
       FILE-CONTROL.                                                    00001010
                                                                        00001020
       DATA DIVISION.                                                   00001030
       FILE SECTION.                                                    00001040
       WORKING-STORAGE SECTION.                                         00001050
                                                                        00001060
       77  WS-ISEIB-ROUTINE                 PIC X(8) VALUE 'ISEIB   '.  00001070
       77  WS-ISSUBSTR                      PIC X(8) VALUE 'ISSUBSTR'.  00001080
           COPY AUDCICS.                                                00001090
                                                                        00001100
       01  CARR-DESCRIPTION-ENTRIES.                                    00001110
           05  CARR-DESCFIELD-TABLE OCCURS 20 INDEXED BY CARR-INDEX.    00001120
               10 CARR-DESC-FIELD-NAME           PIC X(35).             00001130
               10 CARR-DESC-FIELD-LENGTH         PIC S9(4) COMP.        00001140
               10 CARR-DESC-FIELD-KEY            PIC X(1).              00001150
               10 CARR-DESC-FIELD-TYPE           PIC X(1).              00001160
                                                                        00001170
       01  WS-MISC.                                                     00001180
           05  PAGE-FORWARD-SW            PIC X.                        00001190
           05  FIRST-PASS-SW              PIC X.                        00001200
           05  WS-SUB                     PIC S9(4) COMP VALUE ZERO.    00001210
           05  WS-SUB2                    PIC S9(4) COMP VALUE ZERO.    00001220
           05  WS-SUB3                    PIC S9(4) COMP VALUE ZERO.    00001230
           05  DARK-SUB                   PIC S9(4) COMP VALUE ZERO.    00001240
           05  WS-CURSOR-POSITION         PIC S9(4) COMP VALUE ZERO.    00001250
           05  HOLD-FOURTEEN              PIC X(14) VALUE SPACE.        00001260
           05  HOLD-CARRIER-RECORD        PIC X(1720)    VALUE SPACE.   00001270
           05  HOLD-CARRIER-KEY           PIC X(20)      VALUE SPACE.   00001280
           05  TRUNCATE-DATE-KEY.                                       00001290
               10  TRUNCATE-FOURTEEN      PIC X(14) VALUE SPACE.        00001300
               10  TRUNCATE-DATE-SIX      PIC X(6)  VALUE SPACE.        00001310
           05  FILLER                     PIC X(30) VALUE SPACE.        00001320
           05  WS-HOLD-YY                 PIC 99.                       00001330
           05  WS-DATE-WORK               PIC 9(6)  VALUE ZERO.         00001340
           05  WS-DATE-WORK2 REDEFINES WS-DATE-WORK.                    00001350
               10 WS-YY                  PIC 99.                        00001360
               10 WS-MM                  PIC 99.                        00001370
               10 WS-DD                  PIC 99.                        00001380
           05  WS-DATE-OUT                PIC 99B99B99.                 00001390
           05  WS-DATE-OUTX REDEFINES WS-DATE-OUT PIC X(8).             00001400
           05  WS-TITLE-WORK              PIC X(35) VALUE SPACES.       00001410
           05  WS-TITLE-RDF REDEFINES WS-TITLE-WORK.                    00001420
               10  WS-TITLE-CHAR OCCURS 35 PIC X.                       00001430
           05  WS-LAST-FIELD-ID.                                        00001440
               10  WS-FIELD-FILE          PIC XX VALUE SPACE.           00001450
               10  WS-FIELD-NUMBER        PIC 99999 VALUE ZERO.         00001460
           05  WS-ALIAS-ID.                                             00001470
               10  WS-ALIAS-FILE          PIC XX VALUE SPACE.           00001480
               10  WS-ALIAS-NUMBER        PIC 99999 VALUE ZERO.         00001490
           05  WS-MAX-LENGTH              PIC S9(4) COMP VALUE +1720.   00001500
           05  WS-LENGTH                  PIC S9(4) COMP VALUE +1720.   00001510
           05  WS-MESSAGE-AREA.                                         00001520
               10  WS-SYSTEM-CODE         PIC XX.                       00001530
               10  WS-MESSAGE-NUMBER      PIC 999.                      00001540
               10  WS-SEVERITY-CODE       PIC X.                        00001550
               10  FILLER                 PIC X VALUE SPACE.            00001560
               10  WS-MESSAGE-TEXT        PIC X(35).                    00001570
           05  WS-TURBO-DESCRIPTIONS      PIC X(35).                    00001580
               88  WS-TURBO-DESC          VALUES ARE                    00001590
                   'KEY FILLER                         '                00001600
                   'RECORD EFFECTIVE DATE              '                00001610
                   'CURRENT RECORD INDICATOR           '                00001620
                   'OUT-FOR-UPDATE INDICATOR           '                00001630
                   'USER LOGONID                       '.               00001640
           05  WS-TABLE                   PIC 9(4) VALUE ZERO.          00001650
           05  WS-DISPLAY                 PIC S9(4) COMP VALUE ZERO.    00001660
           05  WS-AID                     PIC X          VALUE SPACE.   00001670
           05  WS-STICKY                  PIC S9(4) COMP VALUE ZERO.    00001680
           05  WS-STICKY-SUB              PIC S9(4) COMP VALUE ZERO.    00001690
           05  WS-RECEIVE-FIELD-POSITION  PIC S9(4) COMP VALUE ZERO.    00001700
           05  WS-SEND-FIELD-POSITION     PIC S9(4) COMP VALUE ZERO.    00001710
           05  WS-FIELD-LENGTH            PIC S9(4) COMP VALUE ZERO.    00001720
           05  WS-TITLE-LENGTH            PIC S9(4) COMP VALUE ZERO.    00001730
           05  WS-FIELD-VALUES.                                         00001740
             15  WS-DISPLAY1                PIC S9(4) COMP VALUE ZERO.  00001750
             15  WS-SEND-FIELD-POSITION1    PIC S9(4) COMP VALUE ZERO.  00001760
             15  WS-RECEIVE-FIELD-POSITION1 PIC S9(4) COMP VALUE ZERO.  00001770
             15  WS-FIELD-LENGTH1           PIC S9(4) COMP VALUE ZERO.  00001780
             15  WS-TITLE-LENGTH1           PIC S9(4) COMP VALUE ZERO.  00001790
             15  WS-DISPLAY2                PIC S9(4) COMP VALUE ZERO.  00001800
             15  WS-SEND-FIELD-POSITION2    PIC S9(4) COMP VALUE ZERO.  00001810
             15  WS-RECEIVE-FIELD-POSITION2 PIC S9(4) COMP VALUE ZERO.  00001820
             15  WS-FIELD-LENGTH2           PIC S9(4) COMP VALUE ZERO.  00001830
             15  WS-TITLE-LENGTH2           PIC S9(4) COMP VALUE ZERO.  00001840
             15  WS-DISPLAY3                PIC S9(4) COMP VALUE ZERO.  00001850
             15  WS-SEND-FIELD-POSITION3    PIC S9(4) COMP VALUE ZERO.  00001860
             15  WS-RECEIVE-FIELD-POSITION3 PIC S9(4) COMP VALUE ZERO.  00001870
             15  WS-FIELD-LENGTH3           PIC S9(4) COMP VALUE ZERO.  00001880
             15  WS-TITLE-LENGTH3           PIC S9(4) COMP VALUE ZERO.  00001890
             15  WS-DISPLAY4                PIC S9(4) COMP VALUE ZERO.  00001900
             15  WS-SEND-FIELD-POSITION4    PIC S9(4) COMP VALUE ZERO.  00001910
             15  WS-RECEIVE-FIELD-POSITION4 PIC S9(4) COMP VALUE ZERO.  00001920
             15  WS-FIELD-LENGTH4           PIC S9(4) COMP VALUE ZERO.  00001930
             15  WS-TITLE-LENGTH4           PIC S9(4) COMP VALUE ZERO.  00001940
             15  WS-DISPLAY5                PIC S9(4) COMP VALUE ZERO.  00001950
             15  WS-SEND-FIELD-POSITION5    PIC S9(4) COMP VALUE ZERO.  00001960
             15  WS-RECEIVE-FIELD-POSITION5 PIC S9(4) COMP VALUE ZERO.  00001970
             15  WS-FIELD-LENGTH5           PIC S9(4) COMP VALUE ZERO.  00001980
             15  WS-TITLE-LENGTH5           PIC S9(4) COMP VALUE ZERO.  00001990
           05  WT-FIELD-TABLE REDEFINES WS-FIELD-VALUES.                00002000
             10  WT-FIELD-OCCURS OCCURS 5 INDEXED BY WT-FIELD-INDEX.    00002010
               15  WT-DISPLAY                 PIC S9(4) COMP.           00002020
               15  WT-SEND-FIELD-POSITION     PIC S9(4) COMP.           00002030
               15  WT-RECEIVE-FIELD-POSITION  PIC S9(4) COMP.           00002040
               15  WT-FIELD-LENGTH            PIC S9(4) COMP.           00002050
               15  WT-TITLE-LENGTH            PIC S9(4) COMP.           00002060
           05  WS-RECORD-SWITCH               PIC X VALUE 'N'.          00002070
           05  WS-FIELD-DESC1                 PIC X(4) VALUE SPACE.     00002080
           05  WS-TYPE                        PIC X(2) VALUE SPACE.     00002090
           COPY SECCOMMC.                                               00002100
          EJECT                                                         00002110
           COPY TU001M1.                                                00002120
          EJECT                                                         00002130
           COPY DFHAID.                                                 00002140
           COPY DFHBMSCA.                                               00002150
          EJECT                                                         00002160
           COPY TURBDESC.                                               00002170
           COPY TURBDATA.                                               00002180
       01  WT-TABLES.                                                   00002190
           COPY TURB0010.                                               00002200
           COPY DEMOCOMM.                                               00002210
           COPY CAWSINC.                                                00002220
          EJECT                                                         00002230
       01  WS-STARTCD             PIC X(2) VALUE SPACES.                00002240
       01  FOUND-SW               PIC X VALUE 'N'.                      00002250
       01  WS-CTLCHAR             PIC X VALUE 'B'.                      00002260
       01  WS-SUBSCRIPTS.                                               00002270
           05  WS-CALEN           PIC S9(4) COMP VALUE +0.              00002280
           05  WS-BUFFER-LEN      PIC S9(4) COMP VALUE +0.              00002290
           05  WS-RETRIEVE-LEN    PIC S9(4) COMP VALUE +22.             00002300
           05  WS-MSGBUFF-SUB     PIC S9(4) COMP VALUE +0.              00002310
           05  WS-MSGPOS-SUB      PIC S9(4) COMP VALUE +0.              00002320
           05  WS-MSGWORK-SUB     PIC S9(4) COMP VALUE +0.              00002330
           05  WS-MSGTEXT-SUB     PIC S9(4) COMP VALUE +0.              00002340
                                                                        00002350
           COPY TU003COM.                                               00002360
           05  T3-TRANID                  PIC X(4) VALUE SPACES.        00002370
          EJECT                                                         00002380
                                                                        00002390
       01  WS-SET-ATTR.                                                 00002400
           05  WS-SET-ATTR-COMP PIC S9(4) COMP VALUE +29.               00002410
           05  WS-SET-ATTR-RCOMP REDEFINES WS-SET-ATTR-COMP.            00002420
               10  FILLER             PIC X.                            00002430
               10  WS-SET-ATTR-BYTE   PIC X.                            00002440
       01  WS-MESSAGE-3               PIC 999.                          00002450
                                                                        00002460
       01  WS-MSG-WORK.                                                 00002470
           05  WS-MSGWORK-CHAR OCCURS 6 TIMES PIC X.                    00002480
                                                                        00002490
       01  WS-MSG-POSITIONS.                                            00002500
           05  WS-MSG-POS OCCURS 4 TIMES PIC S9(4) COMP.                00002510
                                                                        00002520
       01  TCTUAL                        PIC S9(4) COMP.                00002530
           88  INVALID-TCTUAL            VALUE 0 THRU 135.              00002540
                                                                        00002550
       01  LS-COMMAREA.                                                 00002560
           05  TC-CARRIER      PIC X(2).                                00002570
           05  TC-SCREEN-NAME  PIC X(8).                                00002580
           05  TC-COMMAND-LINE PIC X.                                   00002590
           05  TC-FIELD-FILE   PIC XX.                                  00002600
           05  TC-FIELD-NUMBER PIC 9(5).                                00002610
           05  TC-TRANID       PIC X(4).                                00002620
           05  TC-PAGE-NUMBER  PIC S9(4) COMP.                          00002630
           05  TC-LAST-KEY.                                             00002640
               10  TC-LAST-TABLE  PIC 9(4).                             00002650
               10  TC-LAST-REST   PIC X(16).                            00002660
           05  TC-PREV-KEY.                                             00002670
               10  TC-PREV-TABLE  PIC 9(4).                             00002680
               10  TC-PREV-REST   PIC X(16).                            00002690
           05  TC-CURSOR-POSITION  PIC S9(4) COMP.                      00002700
           05  TC-STICKY-AREA.                                          00002710
               10 TC-STICKY-FIELD-ID   PIC X(7).                        00002720
               10 TC-STICKY-POSITION   PIC 9(3).                        00002730
               10 TC-STICKY-LENGTH     PIC 9(3).                        00002740
               10 TC-STICKY-TYPE       PIC X.                           00002750
               10 TC-STICKY-VALUE      PIC X(30).                       00002760
           05  TC-STICKY-TABLE OCCURS 24.                               00002770
               10 TC-STICKY-LINE       PIC 9(2).                        00002780
               10 TC-STICKY-VALUE-TABLE  PIC X(30).                     00002790
           05  LS-COMMLEN PIC S9(4) COMP.                               00002800
           05  LS-BUFFLEN PIC S9(4) COMP.                               00002810
           05  LS-CA-COMMSAVE.                                          00002820
               10  LS-COMM-CHAR  PIC X OCCURS 0 TO 4096 TIMES           00002830
                             DEPENDING ON LS-COMMLEN.                   00002840
           05  LS-CA-BUFFSAVE.                                          00002850
               10  LS-BUFF-CHAR  PIC X OCCURS 0 TO 4096 TIMES           00002860
                             DEPENDING ON LS-BUFFLEN.                   00002870
                                                                        00002880
       LINKAGE SECTION.                                                 00002890
       01  DFHCOMMAREA.                                                 00002900
           05  COMM-CHAR  PIC X  OCCURS 0 TO 4096 TIMES                 00002910
                           DEPENDING ON EIBCALEN.                       00002920
                                                                        00002930
       01  LS-BUFFER-IMAGE.                                             00002940
           05  LS-BCHAR  PIC X OCCURS 0 TO 4096 TIMES                   00002950
                             DEPENDING ON WS-BUFFER-LEN.                00002960
                                                                        00002970
       01  TCTUAR.                                                      00002980
           05  FILLER                        PIC X(36).                 00002990
           05  TCTUA-DEMO-DATA.                                         00003000
               10  FILLER                    PIC S9(4) COMP.            00003010
                   88  INVALID-DEMO-DATA                    VALUE 0.    00003020
               10  FILLER                    PIC X(98).                 00003030
          EJECT                                                         00003040
                                                                        00003050
       PROCEDURE DIVISION.                                              00003060
                                                                        00003070
      *  IF THIS IS NOT RUNNING UNDER MY TRANSID THEN I CAN ASSUME      00003080
      *  I WAS LINKED TO WITH THE REQUIRED INFORMATION IN MY COMMAREA.  00003090
      *  I THEN START MYSELF PASSING THE INFORMATION I WAS GIVEN ALONG  00003100
      *  WITH ANY ADDITIONAL INFO THAT I WILL NEED SO THAT AFTER I      00003110
      *  RETURN TO THE CALLER OF ME, HE WILL RETURN TO CICS AS IF       00003120
      *  NOTHING EVER HAPPENED, AND I WILL RESTART IMMEDIATELY AFTER.   00003130
                                                                        00003140
           IF EIBTRNID = 'TU03'                                         00003150
               GO TO 0010-DO-TUTORIAL.                                  00003160
                                                                        00003170
       0000-DO-STUB.                                                    00003180
                                                                        00003190
           IF EIBCALEN > 0                                              00003200
               MOVE DFHCOMMAREA TO T3-COMMAREA                          00003210
           ELSE                                                         00003220
               MOVE 'N'  TO T3-COMMAND-LINE                             00003230
               MOVE 'ZZ' TO T3-CARRIER                                  00003240
               MOVE 'ZZZZZZZZ' TO T3-SCREEN-NAME.                       00003250
                                                                        00003260
           MOVE EIBTRNID TO T3-TRANID.                                  00003270
           EXEC CICS START TRANSID('TU03')                              00003280
                           TERMID(EIBTRMID)                             00003290
                           FROM(T3-COMMAREA)                            00003300
                           END-EXEC.                                    00003310
           EXEC CICS RETURN END-EXEC.                                   00003320
                                                                        00003330
       0010-DO-TUTORIAL.                                                00003340
                                                                        00003350
           EXEC CICS ASSIGN STARTCODE(WS-STARTCD) END-EXEC.             00003360
                                                                        00003370
           IF WS-STARTCD NOT = 'SD'                                     00003380
               GO TO 0020-NOT-FIRST-TIME-IN.                            00003390
                                                                        00003400
                                                                        00003570
           EXEC CICS RECEIVE BUFFER ASIS                                00003580
                             SET(ADDRESS OF LS-BUFFER-IMAGE)            00003590
                             LENGTH(WS-BUFFER-LEN)                      00003600
                             END-EXEC.                                  00003610
                                                                        00003620
                                                                        00003700
           EXEC CICS RETRIEVE                                           00003710
                     INTO(LS-COMMAREA)                                  00003720
                     LENGTH(WS-RETRIEVE-LEN)                            00003730
                     END-EXEC.                                          00003740
                                                                        00003750
           MOVE 1 TO TC-PAGE-NUMBER.                                    00003760
                                                                        00003770
           IF TC-SCREEN-NAME = 'IS548M2' OR 'IS548M3'                   00003780
               COMPUTE TC-CURSOR-POSITION = EIBCPOSN - 480              00003790
           ELSE                                                         00003800
               MOVE EIBCPOSN TO TC-CURSOR-POSITION.                     00003810
                                                                        00003820
           MOVE EIBCALEN TO LS-COMMLEN.                                 00003830
           MOVE WS-BUFFER-LEN TO LS-BUFFLEN.                            00003840
           MOVE DFHCOMMAREA TO LS-CA-COMMSAVE.                          00003850
           MOVE LS-BUFFER-IMAGE TO LS-CA-BUFFSAVE.                      00003860
                                                                        00003870
           GO TO 0030-BEGIN-ACTUAL-TUTORIAL.                            00003880
                                                                        00003890
       0020-NOT-FIRST-TIME-IN.                                          00003900
                                                                        00003910
           IF EIBCALEN = 0                                              00003920
               EXEC CICS RETURN END-EXEC.                               00003930
                                                                        00003940
                                                                        00004010
           MOVE DFHCOMMAREA TO LS-COMMAREA.                             00004020
                                                                        00004030
           EJECT                                                        00004040
       0030-BEGIN-ACTUAL-TUTORIAL.                                      00004050
                                                                        00004060
           MOVE LOW-VALUES TO TU001M1O.                                 00004070
                                                                        00004200
           MOVE EIBAID TO WS-AID.                                       00004210
           IF EIBAID EQUAL DFHPF17 OR DFHPF5 OR DFHCLEAR                00004220
               GO TO 0900-RETURN-TO-CALLER.                             00004230
                                                                        00004240
           IF (TC-CURSOR-POSITION LESS 79 AND                           00004250
               TC-COMMAND-LINE NOT EQUAL 'Y') OR                        00004260
              (TC-CURSOR-POSITION LESS 159 AND GREATER 79 AND           00004270
               TC-COMMAND-LINE  EQUAL 'Y')                              00004280
               PERFORM 0200-SCREEN-HELP THRU 0200-EXIT                  00004290
           ELSE                                                         00004300
           IF TC-CURSOR-POSITION GREATER 1759                           00004310
               PERFORM 0300-MESSAGE-HELP                                00004320
           ELSE                                                         00004330
               PERFORM 0400-FIELD-HELP THRU 0400-EXIT.                  00004340
                                                                        00004350
           GO TO 0500-SEND-MAP.                                         00004360
                                                                        00004370
          EJECT                                                         00004380
       0200-SCREEN-HELP.                                                00004390
      *---------------------------------------------------------------* 00004400
      * THIS SECTION IS TO DISPLAY HELP ABOUT THE SCREEN THE USER IS  * 00004410
      * ON. THE SCREEN NAME IS USED AS A KEY TO THE SCREEN DESCRIPTION* 00004420
      * TABLE FILE AND THE DESCRIPTION IS DISPLAYED. IF PAGE FORWARD  * 00004430
      * IS ENTERED THEN THE FILE IS CHECKED FOR MULTI-PAGE DESCRIPTION* 00004440
      *---------------------------------------------------------------* 00004450
           MOVE 'SCREEN' TO MAP-TYPEO.                                  00004460
      *---------------------------------------------------------------* 00004470
      * GET SCREEN DESCRIPTOR RECORD                                  * 00004480
      *---------------------------------------------------------------* 00004490
           MOVE SPACE TO TUTOR-SD-KEY.                                  00004500
           MOVE  820  TO TUTOR-SD-RECORD-TYPE.                          00004510
           MOVE TC-SCREEN-NAME TO TUTOR-SD-SCREEN-NAME.                 00004520
                                                                        00004530
           IF EIBTRNID NOT = 'TU03'                                     00004540
              NEXT SENTENCE                                             00004550
           ELSE                                                         00004560
              IF EIBAID EQUAL DFHPF19 OR DFHPF7                         00004570
                  IF TC-PAGE-NUMBER = 1                                 00004580
                     NEXT SENTENCE                                      00004590
                  ELSE                                                  00004600
                     SUBTRACT 1 FROM TC-PAGE-NUMBER                     00004610
              ELSE                                                      00004620
                  IF EIBAID EQUAL DFHPF20 OR DFHPF8                     00004630
                      ADD 1 TO TC-PAGE-NUMBER.                          00004640
                                                                        00004650
      *---------------------------------------------------------------* 00004660
      * MOVE SCREEN DESCRIPTION TO DISPLAY                            * 00004670
      *---------------------------------------------------------------* 00004680
           MOVE TC-PAGE-NUMBER TO TUTOR-SD-PAGE-NUMBER.                 00004690
           MOVE TUTOR-SD-SCREEN-NAME TO MAP-SCREEN-NAMEO.               00004700
           MOVE TUTOR-SD-PAGE-NUMBER TO MAP-PAGE-NUMBERO.               00004710
                                                                        00004720
           EXEC CICS HANDLE CONDITION NOTFND(0290-NOTFND)               00004730
                                      ENDFILE  (0492-ENDFILE)           00004740
                                      NOTOPEN  (0494-NOTOPEN)           00004750
                                      DISABLED (0494-NOTOPEN)           00004760
                                      END-EXEC.                         00004770
                                                                        00004780
           EXEC CICS STARTBR DATASET('TURBDATA')                        00004790
                             RIDFLD(CARR-KEY-GENERIC)                   00004800
                             END-EXEC.                                  00004810
                                                                        00004820
           MOVE WS-MAX-LENGTH TO WS-LENGTH.                             00004830
           EXEC CICS READNEXT DATASET('TURBDATA')                       00004840
                              RIDFLD(CARR-KEY-GENERIC)                  00004850
                              INTO(CARRIER-CONTROL-RECORD)              00004860
      ****                    LENGTH(WS-LENGTH)                         00004870
                              LENGTH(LENGTH OF CARRIER-CONTROL-RECORD)  00004880
                              END-EXEC.                                 00004890
                                                                        00004900
           PERFORM 0210-READ-NEXT-820-RECORD THRU 0210-EXIT             00004910
              UNTIL CARR-CURRENT-IND = 'C'.                             00004920
                                                                        00004930
           EXEC CICS ENDBR DATASET('TURBDATA')                          00004940
                     END-EXEC.                                          00004950
                                                                        00004960
      *    IF TUTOR-SD-SCREEN-NAME NOT = TC-SCREEN-NAME                 00004970
      *       GO TO 0290-NOTFND.                                        00004980
                                                                        00004990
           IF TUTOR-SD-RECORD-TYPE NOT = 820                            00005000
              OR TUTOR-SD-SCREEN-NAME NOT = TC-SCREEN-NAME              00005010
              OR TUTOR-SD-PAGE-NUMBER NOT = TC-PAGE-NUMBER              00005020
                 GO TO 0290-NOTFND.                                     00005030
                                                                        00005040
           MOVE TUTOR-SD-SCREEN-TITLE TO MAP-TITLEO.                    00005050
                                                                        00005060
           PERFORM 0250-FORMAT-DATA THRU 0250-EXIT                      00005070
               VARYING WS-SUB FROM 1 BY 1                               00005080
               UNTIL WS-SUB GREATER 20.                                 00005090
                                                                        00005100
       0200-EXIT.                                                       00005110
           EXIT.                                                        00005120
                                                                        00005130
       0210-READ-NEXT-820-RECORD.                                       00005140
                                                                        00005150
           MOVE WS-MAX-LENGTH TO WS-LENGTH.                             00005160
           EXEC CICS READNEXT DATASET('TURBDATA')                       00005170
                              RIDFLD(CARR-KEY-GENERIC)                  00005180
                              INTO(CARRIER-CONTROL-RECORD)              00005190
      ***                     LENGTH(WS-LENGTH)                         00005200
                              LENGTH(LENGTH OF CARRIER-CONTROL-RECORD)  00005210
           END-EXEC.                                                    00005220
                                                                        00005230
       0210-EXIT.                                                       00005240
           EXIT.                                                        00005250
                                                                        00005260
       0250-FORMAT-DATA.                                                00005270
                                                                        00005280
           MOVE TUTOR-SD-SCREEN-DESC (WS-SUB) TO                        00005290
                MAP-SCREEN-DESCO (WS-SUB).                              00005300
                                                                        00005310
       0250-EXIT.                                                       00005320
           EXIT.                                                        00005330
                                                                        00005340
       0290-NOTFND.                                                     00005350
           PERFORM 0493-DARK-SCREEN-FOR-ERR-MSG                         00005360
               VARYING DARK-SUB FROM 1 BY 1                             00005370
               UNTIL DARK-SUB > 18.                                     00005380
                                                                        00005390
           MOVE 'NO MORE PAGES TO DISPLAY' TO MAP-SCREEN-DESCO (19).    00005400
           GO TO 0500-SEND-MAP.                                         00005410
                                                                        00005420
          EJECT                                                         00005430
       0300-MESSAGE-HELP.                                               00005440
      *---------------------------------------------------------------* 00005450
      * THIS SECTION IS TO DISPLAY EXPLANATIONS OF ERROR MESSAGES     * 00005460
      * THE APPLICATION WILL PASS THE TUTORIAL SYSTEM UP TO FOUR      * 00005470
      * ERROR MESSAGE NUMBERS. THE CURSOR POSITION IS USED TO         * 00005480
      * DETERMINE WHICH MESSAGE HELP WAS REQUESTED ON.                * 00005490
      * THEN THE ERROR MESSAGE TABLE RECORD FOR THAT MESSAGE IS READ  * 00005500
      * AND THE EXPLANATION IS DISPLAYED                              * 00005510
      *---------------------------------------------------------------* 00005520
           MOVE 'MESSAGES' TO MAP-TYPEO.                                00005530
                                                                        00005540
           MOVE SPACE TO ERROR-KEY.                                     00005550
           MOVE 9999  TO ERROR-RECORD-TYPE.                             00005560
      *---------------------------------------------------------------* 00005570
      * IF PF19 OR PF7 PRESSED, DISPLAY PAGE 1                        * 00005580
      *---------------------------------------------------------------* 00005590
           IF EIBAID = DFHPF19 OR DFHPF7                                00005600
               MOVE 1 TO TC-PAGE-NUMBER.                                00005610
      *---------------------------------------------------------------* 00005620
      * FIND WHICH PAGE WE ARE ABOUT TO BUILD                         * 00005630
      *---------------------------------------------------------------* 00005640
           IF TC-PAGE-NUMBER = 1                                        00005650
               MOVE 1 TO WS-MSGTEXT-SUB                                 00005660
               PERFORM 0900-MOVE-IN-MESSAGE                             00005670
           ELSE                                                         00005680
               MOVE 3 TO WS-MSGTEXT-SUB                                 00005690
               PERFORM 0900-MOVE-IN-MESSAGE.                            00005700
                                                                        00005710
           MOVE TC-PAGE-NUMBER       TO MAP-PAGE-NUMBERO.               00005720
                                                                        00005730
      *---------------------------------------------------------------* 00005740
      * READ THE MESSAGE DESCRIPTION RECORD                           * 00005750
      *---------------------------------------------------------------* 00005760
           IF WS-SYSTEM-CODE NOT EQUAL SPACE AND LOW-VALUES             00005770
               MOVE WS-SYSTEM-CODE TO ERROR-SYSTEM-CODE                 00005780
               MOVE WS-MESSAGE-NUMBER TO ERROR-MESSAGE-NUMBER           00005790
               EXEC CICS HANDLE CONDITION NOTFND(0390-NOTFND)           00005800
                                          ENDFILE  (0492-ENDFILE)       00005810
                                          NOTOPEN  (0494-NOTOPEN)       00005820
                                          DISABLED (0494-NOTOPEN)       00005830
                                          END-EXEC                      00005840
               EXEC CICS STARTBR DATASET('TURBDATA')                    00005850
                                 RIDFLD(CARR-KEY-GENERIC)               00005860
                                 END-EXEC                               00005870
                                                                        00005880
               MOVE WS-MAX-LENGTH TO WS-LENGTH                          00005890
               EXEC CICS READNEXT DATASET('TURBDATA')                   00005900
                                  RIDFLD(CARR-KEY-GENERIC)              00005910
                                  INTO(CARRIER-CONTROL-RECORD)          00005920
      **                          LENGTH(WS-LENGTH)                     00005930
                              LENGTH(LENGTH OF CARRIER-CONTROL-RECORD)  00005940
                                  END-EXEC                              00005950
                                                                        00005960
               MOVE CARRIER-CONTROL-RECORD TO HOLD-CARRIER-RECORD       00005970
               MOVE CARR-KEY-GENERIC       TO HOLD-CARRIER-KEY          00005980
               PERFORM 482-CHECK-CURRENT-0810-RECORD                    00005990
                   UNTIL HOLD-CARRIER-KEY NOT = CARR-KEY-GENERIC        00006000
               MOVE HOLD-CARRIER-RECORD    TO CARRIER-CONTROL-RECORD    00006010
                                                                        00006020
               EXEC CICS ENDBR DATASET('TURBDATA')                      00006030
                                         END-EXEC                       00006040
                                                                        00006050
               MOVE ERROR-MESSAGE-TEXT TO WS-MESSAGE-TEXT               00006060
               MOVE ERROR-SEVERITY-CODE TO WS-SEVERITY-CODE             00006070
               MOVE WS-MESSAGE-AREA    TO MAP-TITLEO                    00006080
      *---------------------------------------------------------------* 00006090
      * MOVE THE MESSAGE DESCRIPTION TO THE DISPLAY                   * 00006100
      *---------------------------------------------------------------* 00006110
R03022         MOVE 1 TO WS-SUB3                                        00006120
               PERFORM 0350-FORMAT-DATA                                 00006130
                   VARYING WS-SUB FROM 1 BY 1                           00006140
                   UNTIL WS-SUB GREATER 10.                             00006150
                                                                        00006160
           IF TC-PAGE-NUMBER = 1                                        00006170
               MOVE 2 TO WS-MSGTEXT-SUB                                 00006180
               PERFORM 0900-MOVE-IN-MESSAGE                             00006190
           ELSE                                                         00006200
               MOVE 4 TO WS-MSGTEXT-SUB                                 00006210
               PERFORM 0900-MOVE-IN-MESSAGE.                            00006220
                                                                        00006230
      *---------------------------------------------------------------* 00006240
      * READ SECOND MESSAGE FOR THIS PAGE                             * 00006250
      *---------------------------------------------------------------* 00006260
           MOVE SPACE TO ERROR-KEY.                                     00006270
           MOVE 9999  TO ERROR-RECORD-TYPE.                             00006280
           MOVE WS-SYSTEM-CODE TO ERROR-SYSTEM-CODE.                    00006290
           MOVE WS-MESSAGE-NUMBER TO ERROR-MESSAGE-NUMBER.              00006300
                                                                        00006310
           IF WS-SYSTEM-CODE NOT EQUAL SPACE AND LOW-VALUES             00006320
               EXEC CICS HANDLE CONDITION NOTOPEN  (0494-NOTOPEN)       00006330
                                          DISABLED (0494-NOTOPEN)       00006340
                                          ENDFILE  (0492-ENDFILE)       00006350
                                          END-EXEC                      00006360
               EXEC CICS STARTBR DATASET('TURBDATA')                    00006370
                                 RIDFLD(CARR-KEY-GENERIC)               00006380
                                 END-EXEC                               00006390
                                                                        00006400
               MOVE WS-MAX-LENGTH TO WS-LENGTH                          00006410
               EXEC CICS READNEXT DATASET('TURBDATA')                   00006420
                                  RIDFLD(CARR-KEY-GENERIC)              00006430
                                  INTO(CARRIER-CONTROL-RECORD)          00006440
      ***                         LENGTH(WS-LENGTH)                     00006450
                              LENGTH(LENGTH OF CARRIER-CONTROL-RECORD)  00006460
                                  END-EXEC                              00006470
                                                                        00006480
               MOVE CARRIER-CONTROL-RECORD TO HOLD-CARRIER-RECORD       00006490
               MOVE CARR-KEY-GENERIC       TO HOLD-CARRIER-KEY          00006500
               PERFORM 482-CHECK-CURRENT-0810-RECORD                    00006510
                   UNTIL HOLD-CARRIER-KEY NOT = CARR-KEY-GENERIC        00006520
               MOVE HOLD-CARRIER-RECORD    TO CARRIER-CONTROL-RECORD    00006530
                                                                        00006540
               EXEC CICS ENDBR DATASET('TURBDATA')                      00006550
                                         END-EXEC                       00006560
                                                                        00006570
               MOVE ERROR-MESSAGE-TEXT TO WS-MESSAGE-TEXT               00006580
               MOVE ERROR-SEVERITY-CODE TO WS-SEVERITY-CODE             00006590
               MOVE WS-MESSAGE-AREA    TO MAP-SCREEN-DESCO (11)         00006600
      *---------------------------------------------------------------* 00006610
      * MOVE THE MESSAGE DESCRIPTION TO THE DISPLAY                   * 00006620
      *---------------------------------------------------------------* 00006630
               MOVE 1 TO WS-SUB3                                        00006640
               PERFORM 0350-FORMAT-DATA                                 00006650
                   VARYING WS-SUB FROM 12 BY 1                          00006660
                   UNTIL WS-SUB GREATER 20.                             00006670
                                                                        00006680
           ADD 1 TO TC-PAGE-NUMBER.                                     00006690
           IF TC-PAGE-NUMBER GREATER 2                                  00006700
              MOVE 1 TO TC-PAGE-NUMBER.                                 00006710
                                                                        00006720
       0350-FORMAT-DATA.                                                00006730
                                                                        00006740
           MOVE ERROR-MESSAGE-DESC (WS-SUB3) TO                         00006750
                MAP-SCREEN-DESCO (WS-SUB).                              00006760
           ADD 1 TO WS-SUB3.                                            00006770
                                                                        00006780
       0390-NOTFND.                                                     00006790
                                                                        00006800
           MOVE SPACE TO WS-MESSAGE-AREA.                               00006810
           MOVE WS-SYSTEM-CODE TO ERROR-SYSTEM-CODE.                    00006820
           MOVE WS-MESSAGE-NUMBER TO ERROR-MESSAGE-NUMBER.              00006830
           MOVE WS-MESSAGE-AREA    TO MAP-TITLEO.                       00006840
           MOVE 1 TO TC-PAGE-NUMBER.                                    00006850
           PERFORM 0493-DARK-SCREEN-FOR-ERR-MSG                         00006860
               VARYING DARK-SUB FROM 1 BY 1                             00006870
               UNTIL DARK-SUB > 18.                                     00006880
                                                                        00006890
           MOVE 'ERROR MESSAGE EXPLANATION NOT FOUND'                   00006900
               TO MAP-SCREEN-DESCO (19).                                00006910
           GO TO 0500-SEND-MAP.                                         00006920
                                                                        00006930
          EJECT                                                         00006940
       0400-FIELD-HELP.                                                 00006950
      *---------------------------------------------------------------* 00006960
      * THIS SECTION IS TO DISPLAY HELP ON A FIELD LEVEL.             * 00006970
      * FIRST THE COMMAREA IS CHECKED TO SEE IF THIS IS A REQUEST FOR * 00006980
      * THE FIRST PAGE OR NOT. IF THIS IS THE FIRST PAGE THEN THE     * 00006990
      * SCREEN/FIELD CROSS REFERENCE FILE IS READ TO FIND THE FIELD   * 00007000
      * NUMBER OF THE FIELD THAT CORRESPONDS TO THE CURSOR POSITION   * 00007010
      * THEN THE FIELD DESCRIPTION FILE IS READ AND THE FIELD         * 00007020
      * DESCRIPTION IS DISPLAY IF THE FIELD DESCRIPTION RECORD        * 00007030
      * SHOWS THAT THERE IS A TABLE OF VALUES FOR THIS FIELD          * 00007040
      * THEN THE TABLE FILE IS READ TO SHOW THE VALID VALUES FOR THIS * 00007050
      * FIELD.                                                        * 00007060
      * ON SECOND THRU NTH PAGE THE TABLE VALUES ARE READ FUTHER TO SEE 00007070
      * IF MORE VALUES EXIST FOR THIS FIELD                           * 00007080
      *---------------------------------------------------------------* 00007090
           MOVE 'FIELD' TO MAP-TYPEO.                                   00007100
      *---------------------------------------------------------------* 00007110
      *  IF PF19 PRESSED THEN DISPLAY PREVIOUS PAGE NOT NEXT PAGE     * 00007120
      *---------------------------------------------------------------* 00007130
                                                                        00007140
           IF EIBAID EQUAL DFHPF19 OR DFHPF7                            00007150
               IF TC-PAGE-NUMBER LESS 3                                 00007160
                   MOVE 1 TO TC-PAGE-NUMBER                             00007170
               ELSE                                                     00007180
                  SUBTRACT 2 FROM TC-PAGE-NUMBER.                       00007190
                                                                        00007200
      *---------------------------------------------------------------* 00007210
      * IF FIRST PAGE DISPLAY FIELD DESCRIPTION                       * 00007220
      * IF NOT CONTINUE FIELD VALUE DISPLAY                           * 00007230
      *---------------------------------------------------------------* 00007240
           IF TC-PAGE-NUMBER EQUAL 1                                    00007250
               MOVE SPACE TO EIBAID                                     00007260
               PERFORM 0410-FIRST-PAGE THRU 0410-EXIT                   00007270
           ELSE                                                         00007280
      *---------------------------------------------------------------* 00007290
      * GET FIELD DESCRIPTION RECORD                                  * 00007300
      *---------------------------------------------------------------* 00007310
               MOVE SPACE TO TUTOR-FD-KEY                               00007320
               MOVE  810  TO TUTOR-FD-RECORD-TYPE                       00007330
               MOVE TC-FIELD-FILE   TO TUTOR-FD-FIELD-FILE              00007340
                                       WS-FIELD-FILE                    00007350
               MOVE TC-FIELD-NUMBER TO TUTOR-FD-FIELD-NUMBER            00007360
                                       WS-FIELD-NUMBER                  00007370
               EXEC CICS HANDLE CONDITION NOTFND(0490-NOTFND)           00007380
                                          ENDFILE  (0492-ENDFILE)       00007390
                                          NOTOPEN  (0494-NOTOPEN)       00007400
                                          DISABLED (0494-NOTOPEN)       00007410
                                          END-EXEC                      00007420
               EXEC CICS STARTBR DATASET('TURBDATA')                    00007430
                                 RIDFLD(CARR-KEY-GENERIC)               00007440
                                 END-EXEC                               00007450
                                                                        00007460
               MOVE WS-MAX-LENGTH TO WS-LENGTH                          00007470
               EXEC CICS READNEXT DATASET('TURBDATA')                   00007480
                                  RIDFLD(CARR-KEY-GENERIC)              00007490
                                  INTO(CARRIER-CONTROL-RECORD)          00007500
      ***                         LENGTH(WS-LENGTH)                     00007510
                                LENGTH(LENGTH OF CARRIER-CONTROL-RECORD)00007520
                                  END-EXEC                              00007530
                                                                        00007540
               MOVE CARRIER-CONTROL-RECORD TO HOLD-CARRIER-RECORD       00007550
               MOVE CARR-KEY-GENERIC       TO HOLD-CARRIER-KEY          00007560
               PERFORM 482-CHECK-CURRENT-0810-RECORD                    00007570
                   UNTIL HOLD-CARRIER-KEY NOT = CARR-KEY-GENERIC        00007580
               MOVE HOLD-CARRIER-RECORD    TO CARRIER-CONTROL-RECORD    00007590
                                                                        00007600
               EXEC CICS ENDBR DATASET('TURBDATA')                      00007610
                                         END-EXEC                       00007620
               MOVE 2 TO WS-SUB                                         00007630
               MOVE TUTOR-FD-FIELD-NAME TO MAP-TITLEO                   00007640
               MOVE TUTOR-FD-TABLE TO WS-TABLE                          00007650
               MOVE TUTOR-FD-TABLE-DISPLAY1 TO WS-DISPLAY1              00007660
               MOVE TUTOR-FD-TABLE-DISPLAY2 TO WS-DISPLAY2              00007670
               MOVE TUTOR-FD-TABLE-DISPLAY3 TO WS-DISPLAY3              00007680
               MOVE TUTOR-FD-TABLE-DISPLAY4 TO WS-DISPLAY4              00007690
               MOVE TUTOR-FD-TABLE-DISPLAY5 TO WS-DISPLAY5.             00007700
                                                                        00007710
           MOVE WS-LAST-FIELD-ID     TO MAP-SCREEN-NAMEO.               00007720
           MOVE TC-PAGE-NUMBER       TO MAP-PAGE-NUMBERO.               00007730
      *---------------------------------------------------------------* 00007740
      * IF STICKY CURSOR REQUESTED RETURN TO USER WITH PROPER VALUE   * 00007750
      *---------------------------------------------------------------* 00007760
           IF WS-AID EQUAL DFHPF24                                      00007770
               GO TO 0600-RETURN-STICKY.                                00007780
                                                                        00007790
      *---------------------------------------------------------------* 00007800
      * CHECK IF THERE IS A TABLE OF VALUES FOR THIS FIELD            * 00007810
      * IF YES GET THE TABLE DESCRIPTOR RECORD TO FIND FORMAT OF TABLE* 00007820
      * AND GET THE TABLE FIELD TITLES.                               * 00007830
      *---------------------------------------------------------------* 00007840
                                                                        00007850
           IF WS-TABLE NOT EQUAL ZERO                                   00007860
               PERFORM 0430-READ-DESCRIPTOR                             00007870
                                                                        00007880
               IF EIBAID EQUAL DFHPF19 OR DFHPF7                        00007890
                   ADD 1 TO TC-PAGE-NUMBER                              00007900
                   MOVE TC-PREV-KEY TO TC-LAST-KEY                      00007910
                   MOVE WS-SUB TO WS-SUB2                               00007920
                   MOVE 'N' TO WS-RECORD-SWITCH                         00007930
                   PERFORM 0476-SELECT-RECORD                           00007940
                       UNTIL WS-RECORD-SWITCH EQUAL 'Y'                 00007950
                          OR CARR-RECORD-TYPE NOT EQUAL WS-TABLE        00007960
                   PERFORM 0470-FORMAT-VALUES                           00007970
                       VARYING WS-SUB FROM 20 BY -1                     00007980
                       UNTIL WS-SUB LESS WS-SUB2                        00007990
                          OR CARR-RECORD-TYPE NOT EQUAL WS-TABLE        00008000
               ELSE                                                     00008010
                   MOVE WS-MAX-LENGTH TO WS-LENGTH                      00008020
                   EXEC CICS READNEXT DATASET('TURBDATA')               00008030
                                      RIDFLD(TRUNCATE-DATE-KEY)         00008040
                                      INTO(CARRIER-CONTROL-RECORD)      00008050
      **                              LENGTH(WS-LENGTH)                 00008060
                                LENGTH(LENGTH OF CARRIER-CONTROL-RECORD)00008070
                                      END-EXEC                          00008080
                                                                        00008090
                   MOVE CARRIER-CONTROL-RECORD TO HOLD-CARRIER-RECORD   00008100
                   MOVE TRUNCATE-FOURTEEN      TO HOLD-FOURTEEN         00008110
                   PERFORM 483-CHECK-CURRENT-CARR-RECORD                00008120
                       UNTIL TRUNCATE-FOURTEEN NOT = HOLD-FOURTEEN      00008130
                   MOVE HOLD-CARRIER-RECORD TO CARRIER-CONTROL-RECORD   00008140
                                                                        00008150
                   EXEC CICS RESETBR DATASET('TURBDATA')                00008160
                                     RIDFLD(CARR-KEY-GENERIC)           00008170
                                     END-EXEC                           00008180
                                                                        00008190
                   ADD 1 TO TC-PAGE-NUMBER                              00008330
                   MOVE TC-LAST-KEY TO TC-PREV-KEY                      00008340
                   MOVE WS-SUB TO WS-SUB2                               00008350
                   MOVE 'N' TO WS-RECORD-SWITCH                         00008360
                   PERFORM 0476-SELECT-RECORD                           00008370
                       UNTIL WS-RECORD-SWITCH EQUAL 'Y'                 00008380
                          OR CARR-RECORD-TYPE NOT EQUAL WS-TABLE        00008390
                   PERFORM 0470-FORMAT-VALUES                           00008400
                       VARYING WS-SUB FROM WS-SUB2 BY 1                 00008410
                       UNTIL WS-SUB GREATER 20                          00008420
                          OR CARR-RECORD-TYPE NOT EQUAL WS-TABLE        00008430
           ELSE                                                         00008440
               MOVE 1 TO TC-PAGE-NUMBER.                                00008450
                                                                        00008460
       0400-EXIT.                                                       00008470
           EXIT.                                                        00008480
                                                                        00008490
                                                                        00008500
       0410-FIRST-PAGE.                                                 00008510
      *---------------------------------------------------------------* 00008520
      * GET THE FIELD DESCRIPTION RECORD AND DISPLAY THE TEN          * 00008530
      * LINES OF DESCRIPTION                                          * 00008540
      *                                                               * 00008550
      *                                                               * 00008560
      *---------------------------------------------------------------* 00008570
           MOVE 'Y' TO FIRST-PASS-SW.                                   00008580
                                                                        00008590
           IF TC-FIELD-FILE > SPACES AND                                00008600
              TC-FIELD-NUMBER > SPACES                                  00008610
                 MOVE TC-FIELD-FILE TO WS-FIELD-FILE                    00008620
                 MOVE TC-FIELD-NUMBER TO WS-FIELD-NUMBER                00008630
                 GO TO 0410-SKIP-800-LOOKUP.                            00008640
                                                                        00008650
                                                                        00008660
      **** MOVE SPACE TO CARR-KEY-GENERIC.                              00008670
           MOVE SPACE TO TUTOR-SF-KEY.                                  00008680
           MOVE 800   TO TUTOR-SF-RECORD-TYPE.                          00008690
           MOVE TC-SCREEN-NAME TO TUTOR-SF-SCREEN-NAME.                 00008700
           DIVIDE TC-CURSOR-POSITION BY 80                              00008710
              GIVING TUTOR-SF-SCREEN-ROW                                00008720
              REMAINDER TUTOR-SF-SCREEN-COLUMN.                         00008730
           ADD 1 TO TUTOR-SF-SCREEN-ROW.                                00008740
      **** MOVE 'Y' TO FIRST-PASS-SW.                                   00008750
           EXEC CICS HANDLE CONDITION NOTFND(0490-NOTFND)               00008760
                                      ENDFILE  (0492-ENDFILE)           00008770
                                      NOTOPEN  (0494-NOTOPEN)           00008780
                                      DISABLED (0494-NOTOPEN)           00008790
                                      END-EXEC.                         00008800
           EXEC CICS STARTBR DATASET('TURBDATA')                        00008810
                             RIDFLD(CARR-KEY-GENERIC)                   00008820
                             END-EXEC.                                  00008830
                                                                        00008840
           MOVE WS-MAX-LENGTH TO WS-LENGTH.                             00008850
           EXEC CICS READNEXT DATASET('TURBDATA')                       00008860
                              RIDFLD(CARR-KEY-GENERIC)                  00008870
                              INTO(CARRIER-CONTROL-RECORD)              00008880
      **                      LENGTH(WS-LENGTH)                         00008890
                              LENGTH(LENGTH OF CARRIER-CONTROL-RECORD)  00008900
                              END-EXEC.                                 00008910
                                                                        00008920
           MOVE CARRIER-CONTROL-RECORD TO HOLD-CARRIER-RECORD.          00008930
           MOVE CARR-KEY-GENERIC       TO HOLD-CARRIER-KEY.             00008940
           PERFORM 482-CHECK-CURRENT-0810-RECORD                        00008950
               UNTIL HOLD-CARRIER-KEY NOT = CARR-KEY-GENERIC.           00008960
                                                                        00008970
           MOVE HOLD-CARRIER-RECORD    TO CARRIER-CONTROL-RECORD.       00008980
                                                                        00008990
                                                                        00009000
           MOVE TUTOR-SF-FIELD-FILE   TO WS-FIELD-FILE.                 00009010
           MOVE TUTOR-SF-FIELD-NUMBER TO WS-FIELD-NUMBER.               00009020
                                                                        00009030
           IF TUTOR-SF-SCREEN-NAME EQUAL TC-SCREEN-NAME                 00009040
               COMPUTE WS-CURSOR-POSITION =                             00009050
                                ((TUTOR-SF-SCREEN-ROW - 1) * 80)        00009060
                                + (TUTOR-SF-SCREEN-COLUMN).             00009070
                                                                        00009080
           IF TC-CURSOR-POSITION LESS WS-CURSOR-POSITION                00009090
           OR TUTOR-SF-SCREEN-NAME NOT EQUAL TC-SCREEN-NAME             00009100
               MOVE WS-MAX-LENGTH TO WS-LENGTH                          00009110
               EXEC CICS READPREV DATASET('TURBDATA')                   00009120
                                  RIDFLD(CARR-CONTROL-KEY)              00009130
                                  INTO(CARRIER-CONTROL-RECORD)          00009140
      **                          LENGTH(WS-LENGTH)                     00009150
                              LENGTH(LENGTH OF CARRIER-CONTROL-RECORD)  00009160
                                  END-EXEC                              00009170
               MOVE WS-MAX-LENGTH TO WS-LENGTH                          00009180
               EXEC CICS READPREV DATASET('TURBDATA')                   00009190
                                  RIDFLD(CARR-CONTROL-KEY)              00009200
                                  INTO(CARRIER-CONTROL-RECORD)          00009210
      **                          LENGTH(WS-LENGTH)                     00009220
                              LENGTH(LENGTH OF CARRIER-CONTROL-RECORD)  00009230
                                  END-EXEC                              00009240
               MOVE TUTOR-SF-FIELD-FILE   TO WS-FIELD-FILE              00009250
               MOVE TUTOR-SF-FIELD-NUMBER TO WS-FIELD-NUMBER.           00009260
                                                                        00009270
           IF TUTOR-SF-SCREEN-NAME NOT EQUAL TC-SCREEN-NAME             00009280
               GO TO 0490-NOTFND.                                       00009290
                                                                        00009300
           EXEC CICS ENDBR DATASET('TURBDATA')                          00009310
                     END-EXEC.                                          00009320
                                                                        00009330
      *---------------------------------------------------------------* 00009340
      * THIS IS SPECIAL CODING TO TRANSLATE CODES 3, 4, 5, AND 6      * 00009350
      *                                                               * 00009360
      *---------------------------------------------------------------* 00009370
           IF WS-LAST-FIELD-ID EQUAL 'CM00975' OR                       00009380
                                     'CM01025' OR                       00009390
                                     'CM01065' OR                       00009400
                                     'CM01105'                          00009410
               PERFORM 0420-SPECIAL-ALIAS THRU 0420-EXIT.               00009420
                                                                        00009430
       0410-SKIP-800-LOOKUP.                                            00009440
      *---------------------------------------------------------------* 00009450
      * AFTER GETING FIELD ID THEN GET FIELD DESCRIPTION RECORD       * 00009460
      *---------------------------------------------------------------* 00009470
           MOVE SPACE TO TUTOR-FD-KEY.                                  00009480
           MOVE  810  TO TUTOR-FD-RECORD-TYPE.                          00009490
           MOVE WS-FIELD-FILE   TO TUTOR-FD-FIELD-FILE                  00009500
                                   TC-FIELD-FILE.                       00009510
           MOVE WS-FIELD-NUMBER TO TUTOR-FD-FIELD-NUMBER                00009520
                                   TC-FIELD-NUMBER.                     00009530
           EXEC CICS HANDLE CONDITION NOTFND(0490-NOTFND)               00009540
                                      ENDFILE  (0492-ENDFILE)           00009550
                                      NOTOPEN  (0494-NOTOPEN)           00009560
                                      DISABLED (0494-NOTOPEN)           00009570
                                      END-EXEC.                         00009580
                                                                        00009590
           EXEC CICS STARTBR DATASET('TURBDATA')                        00009600
                             RIDFLD(CARR-KEY-GENERIC)                   00009610
                             END-EXEC.                                  00009620
                                                                        00009630
           MOVE WS-MAX-LENGTH TO WS-LENGTH.                             00009640
           EXEC CICS READNEXT DATASET('TURBDATA')                       00009650
                              RIDFLD(CARR-KEY-GENERIC)                  00009660
                              INTO(CARRIER-CONTROL-RECORD)              00009670
      **                      LENGTH(WS-LENGTH)                         00009680
                              LENGTH(LENGTH OF CARRIER-CONTROL-RECORD)  00009690
                              END-EXEC.                                 00009700
                                                                        00009710
                                                                        00009720
           MOVE CARRIER-CONTROL-RECORD TO HOLD-CARRIER-RECORD.          00009730
           MOVE CARR-KEY-GENERIC       TO HOLD-CARRIER-KEY.             00009740
           PERFORM 482-CHECK-CURRENT-0810-RECORD                        00009750
               UNTIL HOLD-CARRIER-KEY NOT = CARR-KEY-GENERIC.           00009760
           MOVE HOLD-CARRIER-RECORD    TO CARRIER-CONTROL-RECORD.       00009770
                                                                        00009780
           EXEC CICS ENDBR DATASET('TURBDATA')                          00009790
                     END-EXEC.                                          00009800
                                                                        00009810
      *---------------------------------------------------------------* 00009820
      * MOVE FIELD NAME TO DISPLAY                                    * 00009830
      *---------------------------------------------------------------* 00009840
           MOVE TUTOR-FD-FIELD-NAME TO MAP-TITLEO.                      00009850
           MOVE TUTOR-FD-TABLE TO WS-TABLE.                             00009860
           MOVE TUTOR-FD-TABLE-DISPLAY1 TO WS-DISPLAY1.                 00009870
           MOVE TUTOR-FD-TABLE-DISPLAY2 TO WS-DISPLAY2.                 00009880
           MOVE TUTOR-FD-TABLE-DISPLAY3 TO WS-DISPLAY3.                 00009890
           MOVE TUTOR-FD-TABLE-DISPLAY4 TO WS-DISPLAY4.                 00009900
           MOVE TUTOR-FD-TABLE-DISPLAY5 TO WS-DISPLAY5.                 00009910
           IF TUTOR-FD-TABLE-STICKY NUMERIC                             00009920
               MOVE TUTOR-FD-TABLE-STICKY   TO WS-STICKY.               00009930
      *---------------------------------------------------------------* 00009940
      * CHECK IF THIS FIELD HAS AN ALIAS IF SO GET DESCRIPTION FROM   * 00009950
      * THE ALIAS FIELD.                                              * 00009960
      *                                                               * 00009970
      *---------------------------------------------------------------* 00009980
           IF TUTOR-FD-FIELD-ALIAS NOT EQUAL SPACE AND LOW-VALUES       00009990
               MOVE TUTOR-FD-FIELD-ALIAS TO WS-ALIAS-ID                 00010000
               MOVE SPACE TO TUTOR-FD-KEY                               00010010
               MOVE  810  TO TUTOR-FD-RECORD-TYPE                       00010020
               MOVE WS-ALIAS-FILE   TO TUTOR-FD-FIELD-FILE              00010030
               MOVE WS-ALIAS-NUMBER TO TUTOR-FD-FIELD-NUMBER            00010040
               EXEC CICS HANDLE CONDITION NOTOPEN  (0494-NOTOPEN)       00010050
                                          DISABLED (0494-NOTOPEN)       00010060
                                          ENDFILE  (0492-ENDFILE)       00010070
                                          END-EXEC                      00010080
               EXEC CICS STARTBR DATASET('TURBDATA')                    00010090
                                 RIDFLD(CARR-KEY-GENERIC)               00010100
                                 END-EXEC                               00010110
                                                                        00010120
               MOVE WS-MAX-LENGTH TO WS-LENGTH                          00010130
               EXEC CICS READNEXT DATASET('TURBDATA')                   00010140
                                  RIDFLD(CARR-KEY-GENERIC)              00010150
                                  INTO(CARRIER-CONTROL-RECORD)          00010160
      **                          LENGTH(WS-LENGTH)                     00010170
                                LENGTH(LENGTH OF CARRIER-CONTROL-RECORD)00010180
                                  END-EXEC                              00010190
                                                                        00010200
               MOVE CARRIER-CONTROL-RECORD TO HOLD-CARRIER-RECORD       00010210
               MOVE CARR-KEY-GENERIC       TO HOLD-CARRIER-KEY          00010220
               PERFORM 482-CHECK-CURRENT-0810-RECORD                    00010230
                   UNTIL HOLD-CARRIER-KEY NOT = CARR-KEY-GENERIC        00010240
               MOVE HOLD-CARRIER-RECORD    TO CARRIER-CONTROL-RECORD    00010250
                                                                        00010260
               EXEC CICS ENDBR DATASET('TURBDATA')                      00010270
                                         END-EXEC.                      00010280
                                                                        00010290
      *---------------------------------------------------------------* 00010300
      * DISPLAY THE TEN LINE OF DESCRIPTION FROM THE FIELD DESCRIPTOR * 00010310
      * RECORD                                                        * 00010320
      *---------------------------------------------------------------* 00010330
           PERFORM 0450-FORMAT-DATA                                     00010340
               VARYING WS-SUB FROM 1 BY 1                               00010350
               UNTIL WS-SUB GREATER 10.                                 00010360
                                                                        00010370
      *---------------------------------------------------------------* 00010380
      * FIND THE LAST UNUSED BLANK LINE TO START FIELD VALUES TITLES  * 00010390
      *---------------------------------------------------------------* 00010400
           PERFORM 0460-FIND-BLANK                                      00010410
               VARYING WS-SUB FROM 10 BY -1                             00010420
               UNTIL WS-SUB EQUAL 1 OR                                  00010430
                     (MAP-SCREEN-DESCO (WS-SUB) NOT EQUAL SPACES AND    00010440
                      MAP-SCREEN-DESCO (WS-SUB) NOT EQUAL LOW-VALUES).  00010450
           ADD 2 TO WS-SUB.                                             00010460
                                                                        00010470
       0410-EXIT.                                                       00010480
           EXIT.                                                        00010490
                                                                        00010500
                                                                        00010510
       0420-SPECIAL-ALIAS.                                              00010520
      *---------------------------------------------------------------* 00010530
      * THIS SPECIAL CODING TRANSLATES CODES 3 4 5 AND 6 TO           * 00010540
      * THE CORRECT COVERAGE TYPE AND PLAN TABLE                      * 00010550
      *---------------------------------------------------------------* 00010560
      *---------------------------------------------------------------* 00010570
      *    MOVE TC-CARRIER TO WT-C0010-CARRIER.                       * 00010580
      *    MOVE WT-CNTL0010 TO TURBO-CNTL-AREA.                       * 00010590
      *    PERFORM 10000-CALL-GSF.                                    * 00010600
      *    MOVE TURBO-CNTL-AREA TO WT-CNTL0010.                       * 00010610
      *    IF WT-C0010-RETURN NOT EQUAL ZERO                          * 00010620
      *        GO TO 0420-EXIT.                                       * 00010630
      *---------------------------------------------------------------* 00010640
                                                                        00010650
           MOVE SPACES     TO CARR-CONTROL-KEY.                         00010660
           MOVE 0010       TO CARR-RECORD-TYPE.                         00010670
           MOVE TC-CARRIER TO CARR-CARRIER-CODE.                        00010680
           MOVE ZEROES     TO CARR-EFF-DATE.                            00010690
                                                                        00010700
           EXEC CICS HANDLE CONDITION NOTFND(0290-NOTFND)               00010710
                                      ENDFILE  (0492-ENDFILE)           00010720
                                      NOTOPEN  (0494-NOTOPEN)           00010730
                                      DISABLED (0494-NOTOPEN)           00010740
                                      END-EXEC.                         00010750
                                                                        00010760
           EXEC CICS STARTBR DATASET('TURBDATA')                        00010770
                             RIDFLD(CARR-KEY-GENERIC)                   00010780
                             END-EXEC.                                  00010790
                                                                        00010800
           MOVE WS-MAX-LENGTH TO WS-LENGTH.                             00010810
           EXEC CICS READNEXT DATASET('TURBDATA')                       00010820
                              RIDFLD(CARR-KEY-GENERIC)                  00010830
                              INTO(CARRIER-CONTROL-RECORD)              00010840
      **                      LENGTH(WS-LENGTH)                         00010850
                              LENGTH(LENGTH OF CARRIER-CONTROL-RECORD)  00010860
                              END-EXEC.                                 00010870
                                                                        00010880
           PERFORM 0421-READ-NEXT-010-RECORD THRU 0421-EXIT             00010890
              UNTIL CARR-CURRENT-IND = 'C'.                             00010900
                                                                        00010910
           EXEC CICS ENDBR DATASET('TURBDATA')                          00010920
                     END-EXEC.                                          00010930
                                                                        00010940
           IF (CARR-RECORD-TYPE NOT = 0010) OR                          00010950
              (CARR-CARRIER-CODE NOT = TC-CARRIER)                      00010960
                 GO TO 0420-EXIT.                                       00010970
                                                                        00010980
           MOVE CARR-RECORD-DATA-OLD TO WT-C0010-DATA.                  00010990
                                                                        00011000
           IF WS-LAST-FIELD-ID EQUAL 'CM00975'                          00011010
               MOVE WT-C0010-CODE3-TYPE TO WS-TYPE                      00011020
           ELSE                                                         00011030
           IF WS-LAST-FIELD-ID EQUAL 'CM01025'                          00011040
               MOVE WT-C0010-CODE4-TYPE TO WS-TYPE                      00011050
           ELSE                                                         00011060
           IF WS-LAST-FIELD-ID EQUAL 'CM01065'                          00011070
               MOVE WT-C0010-CODE5-TYPE TO WS-TYPE                      00011080
           ELSE                                                         00011090
           IF WS-LAST-FIELD-ID EQUAL 'CM01105'                          00011100
               MOVE WT-C0010-CODE6-TYPE TO WS-TYPE.                     00011110
                                                                        00011120
           IF WS-TYPE EQUAL 'DN'                                        00011130
                MOVE 'MC00085' TO WS-LAST-FIELD-ID                      00011140
           ELSE                                                         00011150
           IF WS-TYPE EQUAL 'DS'                                        00011160
                MOVE 'MC00095' TO WS-LAST-FIELD-ID                      00011170
           ELSE                                                         00011180
           IF WS-TYPE EQUAL 'PC'                                        00011190
                MOVE 'MC00100' TO WS-LAST-FIELD-ID                      00011200
           ELSE                                                         00011210
           IF WS-TYPE EQUAL 'DL' OR 'DV'                                00011220
                MOVE 'MC00105' TO WS-LAST-FIELD-ID                      00011230
           ELSE                                                         00011240
           IF WS-TYPE EQUAL 'LT'                                        00011250
                MOVE 'MC00110' TO WS-LAST-FIELD-ID                      00011260
           ELSE                                                         00011270
           IF WS-TYPE EQUAL 'SA'                                        00011280
                MOVE 'MC00115' TO WS-LAST-FIELD-ID                      00011290
           ELSE                                                         00011300
           IF WS-TYPE EQUAL 'VN'                                        00011310
                MOVE 'MC00120' TO WS-LAST-FIELD-ID                      00011320
           ELSE                                                         00011330
           IF WS-TYPE EQUAL 'BT'                                        00011340
                MOVE 'MC00125' TO WS-LAST-FIELD-ID                      00011350
           ELSE                                                         00011360
           IF WS-TYPE EQUAL 'OT'                                        00011370
                MOVE 'MC00555' TO WS-LAST-FIELD-ID                      00011380
           ELSE                                                         00011390
           IF WS-TYPE EQUAL 'AD'                                        00011400
                MOVE 'MC00645' TO WS-LAST-FIELD-ID.                     00011410
                                                                        00011420
       0420-EXIT.                                                       00011430
           EXIT.                                                        00011440
                                                                        00011450
                                                                        00011460
       0421-READ-NEXT-010-RECORD.                                       00011470
                                                                        00011480
           MOVE WS-MAX-LENGTH TO WS-LENGTH.                             00011490
           EXEC CICS READNEXT DATASET('TURBDATA')                       00011500
                              RIDFLD(CARR-KEY-GENERIC)                  00011510
                              INTO(CARRIER-CONTROL-RECORD)              00011520
      ****                    LENGTH(WS-LENGTH)                         00011530
                              LENGTH(LENGTH OF CARRIER-CONTROL-RECORD)  00011540
                              END-EXEC.                                 00011550
                                                                        00011560
       0421-EXIT.                                                       00011570
           EXIT.                                                        00011580
                                                                        00011590
       0430-READ-DESCRIPTOR.                                            00011600
      *---------------------------------------------------------------* 00011610
      * READ TABLE FILE DESCRIPTOR RECORD FOR THE FIELD VALUES TABLE  * 00011620
      *                                                               * 00011630
      *---------------------------------------------------------------* 00011640
           MOVE 'T'        TO TURBO-DESC-TYPE.                          00011650
           MOVE 'TRBO'     TO TURBO-DESC-FILLER.                        00011660
           MOVE 'TB'       TO TURBO-DESC-FILE-PREFIX.                   00011670
           MOVE ZEROS      TO TURBO-DESC-FIELD-NUMBER.                  00011680
           MOVE WS-TABLE   TO TURBO-DESC-TABLE CARR-RECORD-TYPE.        00011690
                                                                        00011700
           EXEC CICS HANDLE CONDITION NOTFND   (0490-NOTFND)            00011710
                                      NOTOPEN  (0491-NOTOPEN)           00011720
                                      DISABLED (0491-NOTOPEN)           00011730
                                      ENDFILE  (0492-ENDFILE)           00011740
                                      END-EXEC.                         00011750
                                                                        00011760
           EXEC CICS STARTBR DATASET('TURBDESC')                        00011770
                             RIDFLD (TURBO-DESC-KEY)                    00011780
                             END-EXEC.                                  00011790
                                                                        00011800
           SET CARR-INDEX TO 1.                                         00011810
           PERFORM 0485-LOAD-CARRIER-DESC-ENTRIES                       00011820
               UNTIL WS-TABLE NOT = TURBO-DESC-TABLE OR                 00011830
                   CARR-INDEX > 20.                                     00011840
                                                                        00011850
           EXEC CICS ENDBR DATASET('TURBDESC')                          00011860
                                    END-EXEC.                           00011870
                                                                        00011880
           PERFORM 0431-PROCESS-FIELD                                   00011890
               VARYING WT-FIELD-INDEX FROM 1 BY 1                       00011900
               UNTIL WT-FIELD-INDEX GREATER 5.                          00011910
                                                                        00011920
                                                                        00011930
           MOVE 1 TO WS-RECEIVE-FIELD-POSITION.                         00011940
           MOVE 1 TO WT-RECEIVE-FIELD-POSITION (1).                     00011950
           MOVE WT-DISPLAY (1) TO WS-DISPLAY.                           00011960
           MOVE WT-FIELD-LENGTH (1) TO WS-FIELD-LENGTH.                 00011970
           MOVE WT-TITLE-LENGTH (1) TO WS-TITLE-LENGTH.                 00011980
           MOVE CARR-DESC-FIELD-NAME (1) TO WS-FIELD-DESC1.             00011990
                                                                        00012000
           PERFORM 0432-GET-POSITION                                    00012010
               VARYING WT-FIELD-INDEX FROM 2 BY 1                       00012020
               UNTIL WT-FIELD-INDEX GREATER 5.                          00012030
                                                                        00012040
           PERFORM 0433-MOVE-TITLE                                      00012050
               VARYING WT-FIELD-INDEX FROM 1 BY 1                       00012060
               UNTIL WT-FIELD-INDEX GREATER 5.                          00012070
                                                                        00012080
           ADD 1 TO WS-SUB.                                             00012090
      *---------------------------------------------------------------* 00012100
      * START BROWSE ON THE VALUES TABLE                              * 00012110
      *                                                               * 00012120
      *---------------------------------------------------------------* 00012130
           IF TC-PAGE-NUMBER EQUAL 1                                    00012140
               IF WS-FIELD-LENGTH1 EQUAL 2 AND                          00012150
                  WS-FIELD-DESC1   EQUAL 'CARR' AND                     00012160
                  TC-CARRIER   NOT EQUAL SPACE                          00012170
                     MOVE SPACE        TO CARR-KEY-GENERIC              00012180
                     MOVE WS-TABLE     TO CARR-RECORD-TYPE              00012190
                     MOVE TC-CARRIER   TO CARR-CARRIER-CODE             00012200
               ELSE                                                     00012210
                   MOVE SPACE          TO CARR-KEY-GENERIC              00012220
                   MOVE WS-TABLE       TO CARR-RECORD-TYPE              00012230
           ELSE                                                         00012240
               IF EIBAID EQUAL DFHPF19 OR DFHPF7                        00012250
                   MOVE TC-PREV-KEY TO CARR-KEY-GENERIC                 00012260
               ELSE                                                     00012270
                   MOVE TC-LAST-KEY TO CARR-KEY-GENERIC.                00012280
                                                                        00012290
           EXEC CICS HANDLE CONDITION NOTFND(0490-NOTFND)               00012300
                                      ENDFILE  (0492-ENDFILE)           00012310
                                      NOTOPEN  (0494-NOTOPEN)           00012320
                                      DISABLED (0494-NOTOPEN)           00012330
                                      END-EXEC.                         00012340
           MOVE CARR-KEY-GENERIC TO TRUNCATE-DATE-KEY.                  00012350
           MOVE SPACES           TO TRUNCATE-DATE-SIX.                  00012360
           EXEC CICS STARTBR DATASET('TURBDATA')                        00012370
                             RIDFLD(TRUNCATE-DATE-KEY)                  00012380
                             END-EXEC.                                  00012390
           MOVE TRUNCATE-DATE-KEY TO CARR-KEY-GENERIC.                  00012400
                                                                        00012410
           IF EIBAID EQUAL DFHPF19 OR DFHPF7                            00012420
               MOVE WS-MAX-LENGTH TO WS-LENGTH                          00012430
               EXEC CICS READNEXT DATASET('TURBDATA')                   00012440
                                  RIDFLD(TRUNCATE-DATE-KEY)             00012450
                                  INTO(CARRIER-CONTROL-RECORD)          00012460
      ****                        LENGTH(WS-LENGTH)                     00012470
                              LENGTH(LENGTH OF CARRIER-CONTROL-RECORD)  00012480
                                  END-EXEC                              00012490
               MOVE WS-MAX-LENGTH TO WS-LENGTH                          00012500
               EXEC CICS READPREV DATASET('TURBDATA')                   00012510
                                  RIDFLD(CARR-KEY-GENERIC)              00012520
                                  INTO(CARRIER-CONTROL-RECORD)          00012530
      ***                         LENGTH(WS-LENGTH)                     00012540
                              LENGTH(LENGTH OF CARRIER-CONTROL-RECORD)  00012550
                                  END-EXEC.                             00012560
                                                                        00012570
      *---------------------------------------------------------------* 00012580
      * IF START BROWSE DOES NOT FIND A RECORD FOR THE CARRIER        * 00012590
      * ENTERED JUST EXIT AND DONT DISPLAY ANY RECORDS                * 00012600
      *---------------------------------------------------------------* 00012610
           IF WS-FIELD-LENGTH1 EQUAL 2 AND                              00012620
              WS-FIELD-DESC1   EQUAL 'CARR' AND                         00012630
              TC-CARRIER   NOT EQUAL SPACE AND                          00012640
              TC-CARRIER   NOT EQUAL CARR-CARRIER-CODE                  00012650
                  GO TO 0500-SEND-MAP.                                  00012660
                                                                        00012670
      *---------------------------------------------------------------* 00012680
      * CHECK TO SEE IF USER CAN LOOK AT THIS CARRIERS RECORDS        * 00012690
      *---------------------------------------------------------------* 00012700
           IF WS-FIELD-LENGTH1 EQUAL 2 AND                              00012710
              WS-FIELD-DESC1   EQUAL 'CARR' AND                         00012720
              TC-CARRIER   NOT EQUAL SPACE                              00012730
      *          MOVE CARR-CARRIER-CODE TO SEC-CARRIER-CODE             00012740
      *          MOVE 'Y' TO SEC-CHK-CARRIER                            00012750
                 MOVE SPACE TO SEC-RETURN-CODE                          00012760
                 MOVE 'I' TO SEC-FUNCTION-CODE                          00012770
                 EXEC CICS LINK                                         00012780
                      PROGRAM('SECCHECK')                               00012790
                      COMMAREA(SECURITY-COMM-AREA)                      00012800
      **              LENGTH(31)                                        00012810
                      LENGTH(LENGTH OF SECURITY-COMM-AREA)              00012820
                      END-EXEC                                          00012830
                 IF SEC-RETURN-CODE = 'C'                               00012840
                     GO TO 0500-SEND-MAP.                               00012850
                                                                        00012860
       0431-PROCESS-FIELD.                                              00012870
      *---------------------------------------------------------------* 00012880
      * COMPUTE THE POSITION ON THE SCREEN EACH VALUE FIELD WILL      * 00012890
      * DISPLAY ON                                                    * 00012900
      * THEN GET FIELD DISPLACEMENTS WITHIN THE TABLE RECORD          * 00012910
      * AND THE COMPUTE TITLE LENGTH AND FIELD LENGTH                 * 00012920
      *---------------------------------------------------------------* 00012930
           IF WT-DISPLAY (WT-FIELD-INDEX) NOT EQUAL ZERO                00012940
               MOVE WT-DISPLAY (WT-FIELD-INDEX) TO WS-DISPLAY           00012950
               PERFORM 0435-FIND-FIELD-DISPLACEMENT                     00012960
               MOVE WS-SEND-FIELD-POSITION                              00012970
                    TO WT-SEND-FIELD-POSITION (WT-FIELD-INDEX)          00012980
               MOVE WS-FIELD-LENGTH TO                                  00012990
                    WT-FIELD-LENGTH (WT-FIELD-INDEX)                    00013000
               MOVE CARR-DESC-FIELD-NAME (WS-DISPLAY)  TO               00013010
                    WS-TITLE-WORK                                       00013020
               PERFORM 0438-FIND-TITLE-LENGTH                           00013030
                   VARYING WS-SUB2 FROM 35 BY -1                        00013040
                   UNTIL (WS-TITLE-CHAR (WS-SUB2) NOT EQUAL SPACE       00013050
                     AND  WS-TITLE-CHAR (WS-SUB2) NOT EQUAL LOW-VALUES) 00013060
                      OR WS-SUB2 EQUAL 1                                00013070
               MOVE WS-SUB2 TO WT-TITLE-LENGTH (WT-FIELD-INDEX).        00013080
                                                                        00013090
       0432-GET-POSITION.                                               00013100
      *---------------------------------------------------------------* 00013110
      * CALCULATE THE SCREEN RECIEVING POSITIONS BY TAKING THE EITHER * 00013120
      * THE LENGTH OF THE TITLE OF THE LAST FIELD OR THE LENGTH OF THE* 00013130
      * LAST FIELD WHICHEVER IS LARGER AND ADDING IT TO THE LAST USED * 00013140
      * LINE POSITION ADDING 2 FOR TWO SPACES IN BETWEEN.             * 00013150
      *---------------------------------------------------------------* 00013160
           IF WS-DISPLAY NOT EQUAL ZERO                                 00013170
               IF WS-TITLE-LENGTH GREATER WS-FIELD-LENGTH               00013180
                   COMPUTE WT-RECEIVE-FIELD-POSITION (WT-FIELD-INDEX) = 00013190
                                           WS-RECEIVE-FIELD-POSITION    00013200
                                                + WS-TITLE-LENGTH       00013210
                                                + 2                     00013220
                   MOVE WT-RECEIVE-FIELD-POSITION (WT-FIELD-INDEX)      00013230
                       TO WS-RECEIVE-FIELD-POSITION                     00013240
               ELSE                                                     00013250
                   COMPUTE WT-RECEIVE-FIELD-POSITION  (WT-FIELD-INDEX) =00013260
                                          WS-RECEIVE-FIELD-POSITION     00013270
                                                + WS-FIELD-LENGTH       00013280
                                                + 2                     00013290
                   MOVE WT-RECEIVE-FIELD-POSITION (WT-FIELD-INDEX)      00013300
                       TO WS-RECEIVE-FIELD-POSITION.                    00013310
                                                                        00013320
           IF WT-RECEIVE-FIELD-POSITION (WT-FIELD-INDEX) GREATER THAN 8000013330
               MOVE ZERO TO WT-RECEIVE-FIELD-POSITION (WT-FIELD-INDEX). 00013340
                                                                        00013350
           IF (WT-RECEIVE-FIELD-POSITION (WT-FIELD-INDEX) +             00013360
               WT-FIELD-LENGTH (WT-FIELD-INDEX) )                       00013370
               GREATER 80                                               00013380
              COMPUTE WT-FIELD-LENGTH (WT-FIELD-INDEX) =                00013390
                      80 - WT-RECEIVE-FIELD-POSITION (WT-FIELD-INDEX).  00013400
                                                                        00013410
           IF (WT-RECEIVE-FIELD-POSITION (WT-FIELD-INDEX) +             00013420
               WT-TITLE-LENGTH (WT-FIELD-INDEX) )                       00013430
               GREATER 80                                               00013440
              COMPUTE WT-TITLE-LENGTH (WT-FIELD-INDEX) =                00013450
                      80 - WT-RECEIVE-FIELD-POSITION (WT-FIELD-INDEX).  00013460
                                                                        00013470
           MOVE WT-DISPLAY (WT-FIELD-INDEX) TO WS-DISPLAY.              00013480
           MOVE WT-TITLE-LENGTH (WT-FIELD-INDEX) TO WS-TITLE-LENGTH.    00013490
           MOVE WT-FIELD-LENGTH (WT-FIELD-INDEX) TO WS-FIELD-LENGTH.    00013500
                                                                        00013510
       0433-MOVE-TITLE.                                                 00013520
      *---------------------------------------------------------------* 00013530
      * CALL SUBSTRING SUBROUTINE TO MOVE TITLES TO LINE              * 00013540
      * USING THE LENGTHS JUST CALCULATED AND THE FIELD START         * 00013550
      * POSITIONS                                                     * 00013560
      *---------------------------------------------------------------* 00013570
           MOVE WT-DISPLAY (WT-FIELD-INDEX) TO WS-DISPLAY.              00013580
           MOVE WT-RECEIVE-FIELD-POSITION (WT-FIELD-INDEX) TO           00013590
                WS-RECEIVE-FIELD-POSITION.                              00013600
           MOVE WT-TITLE-LENGTH (WT-FIELD-INDEX) TO                     00013610
                WS-TITLE-LENGTH.                                        00013620
      *    IF WS-DISPLAY NOT EQUAL ZERO                                 00013630
      *        MOVE 1 TO WS-SEND-FIELD-POSITION                         00013640
      *        CALL WS-ISSUBSTR USING MAP-SCREEN-DESCO (WS-SUB)         00013650
      *                              CARR-DESC-FIELD-NAME               00013660
      *                                  (WS-DISPLAY)                   00013670
      *                              WS-RECEIVE-FIELD-POSITION          00013680
      *                              WS-SEND-FIELD-POSITION             00013690
      *                              WS-TITLE-LENGTH.                   00013700
                                                                        00013710
       0435-FIND-FIELD-DISPLACEMENT.                                    00013720
      *---------------------------------------------------------------* 00013730
      * THIS PARAGRAPH IS TO CALCULATE THE VALUE FIELD POSITION IN    * 00013740
      * THE TABLE FILE FROM THE TABLE DESCRIPTOR RECORD               * 00013750
      *---------------------------------------------------------------* 00013760
           IF CARR-DESC-FIELD-KEY (WS-DISPLAY) EQUAL 'Y'                00013770
               MOVE 5 TO WS-SEND-FIELD-POSITION                         00013780
           ELSE                                                         00013790
               MOVE 1 TO WS-SEND-FIELD-POSITION.                        00013800
                                                                        00013810
           PERFORM 0436-FIND-FIELD-DISPLACEMENT                         00013820
                   VARYING WS-SUB2 FROM 1 BY 1                          00013830
                   UNTIL WS-SUB2 EQUAL WS-DISPLAY.                      00013840
           MOVE CARR-DESC-FIELD-LENGTH (WS-DISPLAY) TO                  00013850
                    WS-FIELD-LENGTH.                                    00013860
           IF CARR-DESC-FIELD-TYPE (WS-DISPLAY) EQUAL 'D'               00013870
                MOVE 8 TO WS-FIELD-LENGTH.                              00013880
                                                                        00013890
       0436-FIND-FIELD-DISPLACEMENT.                                    00013900
           IF CARR-DESC-FIELD-KEY (WS-DISPLAY) EQUAL 'Y'                00013910
               IF CARR-DESC-FIELD-KEY (WS-SUB2) EQUAL 'Y'               00013920
                   ADD CARR-DESC-FIELD-LENGTH (WS-SUB2) TO              00013930
                       WS-SEND-FIELD-POSITION                           00013940
               ELSE                                                     00013950
                   NEXT SENTENCE                                        00013960
           ELSE                                                         00013970
               IF CARR-DESC-FIELD-KEY (WS-SUB2) NOT EQUAL 'Y'           00013980
                   ADD CARR-DESC-FIELD-LENGTH (WS-SUB2) TO              00013990
                       WS-SEND-FIELD-POSITION.                          00014000
                                                                        00014010
       0438-FIND-TITLE-LENGTH.                                          00014020
                                                                        00014030
       0450-FORMAT-DATA.                                                00014040
           MOVE TUTOR-FD-FIELD-DESC (WS-SUB) TO                         00014050
                MAP-SCREEN-DESCO (WS-SUB).                              00014060
                                                                        00014070
       0460-FIND-BLANK.                                                 00014080
                                                                        00014090
       0470-FORMAT-VALUES.                                              00014100
                                                                        00014110
           PERFORM 0475-FORMAT-VALUES                                   00014120
              VARYING WT-FIELD-INDEX FROM 1 BY 1                        00014130
              UNTIL WT-FIELD-INDEX GREATER 5.                           00014140
                                                                        00014150
           MOVE 'N' TO WS-RECORD-SWITCH.                                00014160
           PERFORM 0476-SELECT-RECORD                                   00014170
               UNTIL WS-RECORD-SWITCH EQUAL 'Y'                         00014180
                  OR CARR-RECORD-TYPE NOT EQUAL WS-TABLE.               00014190
           IF EIBAID EQUAL DFHPF19 OR DFHPF7                            00014200
               IF CARR-RECORD-TYPE NOT EQUAL WS-TABLE                   00014210
                   MOVE 1 TO WS-SUB                                     00014220
               ELSE                                                     00014230
      ****         MOVE CARR-KEY-GENERIC TO TC-PREV-KEY                 00014240
                   NEXT SENTENCE                                        00014250
           ELSE                                                         00014260
               IF CARR-RECORD-TYPE NOT EQUAL WS-TABLE                   00014270
                   MOVE 1 TO TC-PAGE-NUMBER                             00014280
                   MOVE 21 TO WS-SUB                                    00014290
                ELSE                                                    00014300
                   MOVE CARR-KEY-GENERIC TO TC-LAST-KEY.                00014310
                                                                        00014320
                                                                        00014330
       0475-FORMAT-VALUES.                                              00014340
      *---------------------------------------------------------------* 00014350
      * THIS PARAGRAPH CALLS THE SUBSTRING SUBROUTINE TO FORMAT       * 00014360
      * EACH FIELD FROM THE VALUE TABLE ENTRY ONTO THE SCREEN LINE    * 00014370
      * AND READS THE NEXT TABLE ENTRY UNTIL THE END OF THE SCREEN OR * 00014380
      * UNTIL THE END OF THE TABLE VALUES FOR THIS TABLE              * 00014390
      *---------------------------------------------------------------* 00014400
           MOVE WT-DISPLAY (WT-FIELD-INDEX) TO WS-DISPLAY.              00014410
           MOVE WT-RECEIVE-FIELD-POSITION (WT-FIELD-INDEX)              00014420
                TO WS-RECEIVE-FIELD-POSITION.                           00014430
           MOVE WT-SEND-FIELD-POSITION (WT-FIELD-INDEX)                 00014440
                TO WS-SEND-FIELD-POSITION.                              00014450
           MOVE WT-FIELD-LENGTH (WT-FIELD-INDEX)                        00014460
                TO WS-FIELD-LENGTH.                                     00014470
           IF WS-DISPLAY NOT EQUAL ZERO                                 00014480
               IF CARR-DESC-FIELD-TYPE (WS-DISPLAY) EQUAL 'D'           00014490
                   PERFORM 0480-FORMAT-DATE.                            00014500
      *        ELSE                                                     00014510
      *            IF CARR-DESC-FIELD-KEY (WS-DISPLAY) EQUAL 'Y'        00014520
      *                CALL WS-ISSUBSTR USING MAP-SCREEN-DESCO (WS-SUB) 00014530
      *                                      CARR-KEY-GENERIC           00014540
      *                                      WS-RECEIVE-FIELD-POSITION  00014550
      *                                      WS-SEND-FIELD-POSITION     00014560
      *                                      WS-FIELD-LENGTH            00014570
      *            ELSE                                                 00014580
      *                CALL WS-ISSUBSTR USING MAP-SCREEN-DESCO (WS-SUB) 00014590
      *                                      CARR-RECORD-DATA-OLD       00014600
      *                                      WS-RECEIVE-FIELD-POSITION  00014610
      *                                      WS-SEND-FIELD-POSITION     00014620
      *                                      WS-FIELD-LENGTH.           00014630
      *                                                                 00014640
      *    IF WS-DISPLAY NOT EQUAL ZERO                                 00014650
      *        PERFORM 0475-CHECK-STICKY.                               00014660
                                                                        00014670
       0475-CHECK-STICKY.                                               00014680
           IF WS-STICKY EQUAL WS-DISPLAY                                00014690
               ADD 1 TO WS-STICKY-SUB                                   00014700
               MOVE WS-SUB TO TC-STICKY-LINE (WS-STICKY-SUB)            00014710
               MOVE SPACE  TO TC-STICKY-VALUE-TABLE (WS-STICKY-SUB)     00014720
               MOVE 1      TO WS-RECEIVE-FIELD-POSITION                 00014730
               IF CARR-DESC-FIELD-KEY (WS-DISPLAY) EQUAL 'Y'            00014740
                   CALL WS-ISSUBSTR USING                               00014750
                                 TC-STICKY-VALUE-TABLE (WS-STICKY-SUB)  00014760
                                        CARR-KEY-GENERIC                00014770
                                        WS-RECEIVE-FIELD-POSITION       00014780
                                        WS-SEND-FIELD-POSITION          00014790
                                        WS-FIELD-LENGTH                 00014800
               ELSE                                                     00014810
                   CALL WS-ISSUBSTR USING                               00014820
                                 TC-STICKY-VALUE-TABLE (WS-STICKY-SUB)  00014830
                                        CARR-RECORD-DATA-OLD            00014840
                                        WS-RECEIVE-FIELD-POSITION       00014850
                                        WS-SEND-FIELD-POSITION          00014860
                                        WS-FIELD-LENGTH.                00014870
                                                                        00014880
                                                                        00014890
       0476-SELECT-RECORD.                                              00014900
      *---------------------------------------------------------------* 00014910
      * CHECK FOR CARRIER RECORD SELECTION IS THE CARRIER CODE        * 00014920
      * THE FIRST FIELD IN THE RECORD                                 * 00014930
      * AND IS THIS RECORDS CARRIER CODE EQUAL TO SELECT CARRIER CODE * 00014940
      * IF SO DISPLAY THIS RECORD.                                    * 00014950
      *                                                               * 00014960
      * THIS SECTION OF CODE READS RECORDS TO BE DISPLAYED AS DETAIL. * 00014970
      * IN ORDER FOR THIS CODE TO EXECUTE PROPERLY THE RECORD TO BE   * 00014980
      * DISPLAYED MUST ONE BEHIND THE NEXT READ. BECAUSE DATE APPEARS * 00014990
      * IN THE KEY FIELD YOU MUST READ ALL THE RECORDS FOR A          * 00015000
      * PARTICULAR TYPE AND SELECT THE LAST ONE, THAT WILL BE THE     * 00015010
      * CURRENT TURBO RECORD.                                         * 00015020
      * THE FIRST-PASS-SW AND PAGE-FORWARD-SW INSURES THAT THE        * 00015030
      * PROGRAM WILL DISPLAY THE FIRST RECORD ON THE INITIAL PASS IF  * 00015040
      * THESE SWITCHES ARE REMOVED RESULTS ARE UNRELIABLE             * 00015050
      *---------------------------------------------------------------* 00015060
      **** IF EIBAID EQUAL DFHPF20 AND PAGE-FORWARD-SW NOT = 'N'        00015070
           IF EIBAID EQUAL DFHPF19 OR DFHPF7                            00015080
               NEXT SENTENCE                                            00015090
           ELSE                                                         00015100
           IF PAGE-FORWARD-SW NOT = 'N'                                 00015110
               MOVE 'Y' TO FIRST-PASS-SW.                               00015120
           IF EIBAID EQUAL DFHPF19 OR DFHPF7                            00015130
               EXEC CICS HANDLE CONDITION NOTOPEN  (0494-NOTOPEN)       00015140
                                          DISABLED (0494-NOTOPEN)       00015150
                                          ENDFILE  (0492-ENDFILE)       00015160
                                          END-EXEC                      00015170
               MOVE CARR-KEY-GENERIC TO TC-PREV-KEY                     00015180
               MOVE ' ' TO CARR-CURRENT-IND                             00015190
               PERFORM 484-READ-PREV-CURRENT                            00015200
                   UNTIL CARR-CURRENT-IND = 'C'                         00015210
           ELSE                                                         00015220
              IF FIRST-PASS-SW = 'Y'                                    00015230
                   EXEC CICS HANDLE CONDITION NOTFND(0290-NOTFND)       00015240
                                              ENDFILE  (0492-ENDFILE)   00015250
                                              NOTOPEN  (0494-NOTOPEN)   00015260
                                              DISABLED (0494-NOTOPEN)   00015270
                                              END-EXEC                  00015280
                   MOVE WS-MAX-LENGTH TO WS-LENGTH                      00015290
                   EXEC CICS READNEXT DATASET('TURBDATA')               00015300
                                      RIDFLD(CARR-KEY-GENERIC)          00015310
                                      INTO(CARRIER-CONTROL-RECORD)      00015320
      **                              LENGTH(WS-LENGTH)                 00015330
                               LENGTH(LENGTH OF CARRIER-CONTROL-RECORD) 00015340
                                      END-EXEC                          00015350
                                                                        00015360
                   MOVE CARRIER-CONTROL-RECORD TO HOLD-CARRIER-RECORD   00015370
                   MOVE CARR-KEY-GENERIC       TO HOLD-CARRIER-KEY      00015380
                   PERFORM 482-CHECK-CURRENT-0810-RECORD                00015390
                       UNTIL HOLD-CARRIER-KEY NOT = CARR-KEY-GENERIC    00015400
                   MOVE HOLD-CARRIER-RECORD  TO CARRIER-CONTROL-RECORD  00015410
                   MOVE 'N' TO FIRST-PASS-SW                            00015420
                   MOVE 'N' TO PAGE-FORWARD-SW                          00015430
           ELSE                                                         00015440
               EXEC CICS HANDLE CONDITION NOTFND(0290-NOTFND)           00015450
                                          ENDFILE  (0492-ENDFILE)       00015460
                                          NOTOPEN  (0494-NOTOPEN)       00015470
                                          DISABLED (0494-NOTOPEN)       00015480
                                          END-EXEC                      00015490
                                                                        00015500
               MOVE WS-MAX-LENGTH TO WS-LENGTH                          00015510
               EXEC CICS READNEXT DATASET('TURBDATA')                   00015520
                                  RIDFLD(CARR-KEY-GENERIC)              00015530
                                  INTO(CARRIER-CONTROL-RECORD)          00015540
      ***                         LENGTH(WS-LENGTH)                     00015550
                                LENGTH(LENGTH OF CARRIER-CONTROL-RECORD)00015560
                                  END-EXEC                              00015570
                                                                        00015580
               MOVE CARRIER-CONTROL-RECORD TO HOLD-CARRIER-RECORD       00015590
               MOVE CARR-KEY-GENERIC       TO HOLD-CARRIER-KEY          00015600
               PERFORM 482-CHECK-CURRENT-0810-RECORD                    00015610
                   UNTIL HOLD-CARRIER-KEY NOT = CARR-KEY-GENERIC        00015620
                                                                        00015630
               MOVE CARRIER-CONTROL-RECORD TO HOLD-CARRIER-RECORD       00015640
               MOVE CARR-KEY-GENERIC       TO HOLD-CARRIER-KEY          00015650
               PERFORM 482-CHECK-CURRENT-0810-RECORD                    00015660
                   UNTIL HOLD-CARRIER-KEY NOT = CARR-KEY-GENERIC        00015670
               MOVE HOLD-CARRIER-RECORD    TO CARRIER-CONTROL-RECORD.   00015680
                                                                        00015690
           IF WS-FIELD-LENGTH1 EQUAL 2 AND                              00015700
              WS-FIELD-DESC1   EQUAL 'CARR' AND                         00015710
              TC-CARRIER       EQUAL SPACE                              00015720
      *          MOVE CARR-CARRIER-CODE TO SEC-CARRIER-CODE             00015730
      *          MOVE 'Y' TO SEC-CHK-CARRIER                            00015740
                 MOVE SPACE TO SEC-RETURN-CODE                          00015750
                 MOVE 'I' TO SEC-FUNCTION-CODE                          00015760
                 EXEC CICS LINK                                         00015770
                      PROGRAM('SECCHECK')                               00015780
                      COMMAREA(SECURITY-COMM-AREA)                      00015790
      **              LENGTH(31)                                        00015800
                      LENGTH(LENGTH OF SECURITY-COMM-AREA)              00015810
                      END-EXEC                                          00015820
                 IF SEC-RETURN-CODE = 'C'                               00015830
                     MOVE 'N' TO WS-RECORD-SWITCH                       00015840
                 ELSE                                                   00015850
                     MOVE 'Y' TO WS-RECORD-SWITCH                       00015860
           ELSE                                                         00015870
               MOVE 'Y' TO WS-RECORD-SWITCH.                            00015880
                                                                        00015890
           IF WS-FIELD-LENGTH1 EQUAL 2 AND                              00015900
              WS-FIELD-DESC1   EQUAL 'CARR' AND                         00015910
              TC-CARRIER   NOT EQUAL SPACE AND                          00015920
              TC-CARRIER   NOT EQUAL CARR-CARRIER-CODE                  00015930
                   MOVE 9999 TO CARR-RECORD-TYPE.                       00015940
                                                                        00015950
       0480-FORMAT-DATE.                                                00015960
           MOVE 1 TO WS-RECEIVE-FIELD-POSITION.                         00015970
           MOVE 6 TO WS-FIELD-LENGTH.                                   00015980
           IF CARR-DESC-FIELD-KEY (WS-DISPLAY) EQUAL 'Y'                00015990
               CALL WS-ISSUBSTR USING WS-DATE-WORK                      00016000
                                     CARR-KEY-GENERIC                   00016010
                                     WS-RECEIVE-FIELD-POSITION          00016020
                                     WS-SEND-FIELD-POSITION             00016030
                                     WS-FIELD-LENGTH                    00016040
           ELSE                                                         00016050
               CALL WS-ISSUBSTR USING WS-DATE-WORK                      00016060
                                     CARR-RECORD-DATA-OLD               00016070
                                     WS-RECEIVE-FIELD-POSITION          00016080
                                     WS-SEND-FIELD-POSITION             00016090
                                     WS-FIELD-LENGTH.                   00016100
                                                                        00016110
           IF WS-DATE-WORK EQUAL ZERO                                   00016120
               MOVE SPACE TO WS-DATE-OUTX                               00016130
           ELSE                                                         00016140
               MOVE WS-YY TO WS-HOLD-YY                                 00016150
               MOVE WS-MM TO WS-YY                                      00016160
               MOVE WS-DD TO WS-MM                                      00016170
               MOVE WS-HOLD-YY TO WS-DD                                 00016180
               MOVE WS-DATE-WORK TO WS-DATE-OUT                         00016190
               INSPECT WS-DATE-OUT REPLACING ALL SPACE BY '/'.          00016200
                                                                        00016210
           MOVE WT-RECEIVE-FIELD-POSITION (WT-FIELD-INDEX)              00016220
               TO WS-RECEIVE-FIELD-POSITION.                            00016230
           MOVE WT-FIELD-LENGTH (WT-FIELD-INDEX)                        00016240
                TO WS-FIELD-LENGTH.                                     00016250
           MOVE 1 TO WS-SEND-FIELD-POSITION.                            00016260
           CALL WS-ISSUBSTR USING MAP-SCREEN-DESCO (WS-SUB)             00016270
                                 WS-DATE-OUTX                           00016280
                                 WS-RECEIVE-FIELD-POSITION              00016290
                                 WS-SEND-FIELD-POSITION                 00016300
                                 WS-FIELD-LENGTH.                       00016310
                                                                        00016320
                                                                        00016330
       482-CHECK-CURRENT-0810-RECORD.                                   00016340
           MOVE WS-MAX-LENGTH TO WS-LENGTH.                             00016350
           EXEC CICS READNEXT DATASET('TURBDATA')                       00016360
                              RIDFLD(CARR-KEY-GENERIC)                  00016370
                              INTO(CARRIER-CONTROL-RECORD)              00016380
                              LENGTH(WS-LENGTH)                         00016390
                              END-EXEC.                                 00016400
                                                                        00016410
           IF CARR-KEY-GENERIC = HOLD-CARRIER-KEY                       00016420
               MOVE CARRIER-CONTROL-RECORD TO HOLD-CARRIER-RECORD       00016430
               MOVE CARR-KEY-GENERIC       TO HOLD-CARRIER-KEY.         00016440
                                                                        00016450
       483-CHECK-CURRENT-CARR-RECORD.                                   00016460
           MOVE WS-MAX-LENGTH TO WS-LENGTH.                             00016470
           EXEC CICS READNEXT DATASET('TURBDATA')                       00016480
                              RIDFLD(TRUNCATE-FOURTEEN)                 00016490
                              INTO(CARRIER-CONTROL-RECORD)              00016500
                              LENGTH(WS-LENGTH)                         00016510
                              END-EXEC.                                 00016520
                                                                        00016530
           IF TRUNCATE-FOURTEEN = HOLD-FOURTEEN                         00016540
               MOVE CARRIER-CONTROL-RECORD TO HOLD-CARRIER-RECORD       00016550
               MOVE TRUNCATE-FOURTEEN      TO HOLD-FOURTEEN.            00016560
                                                                        00016570
       484-READ-PREV-CURRENT.                                           00016580
           MOVE WS-MAX-LENGTH TO WS-LENGTH.                             00016590
           EXEC CICS READPREV DATASET('TURBDATA')                       00016600
                              RIDFLD(CARR-KEY-GENERIC)                  00016610
                              INTO(CARRIER-CONTROL-RECORD)              00016620
                              LENGTH(WS-LENGTH)                         00016630
                              END-EXEC.                                 00016640
                                                                        00016650
       0485-LOAD-CARRIER-DESC-ENTRIES.                                  00016660
           EXEC CICS READNEXT DATASET('TURBDESC')                       00016670
                              RIDFLD(TURBO-DESC-KEY)                    00016680
                              INTO(TURBO-DESC-RECORD)                   00016690
                              END-EXEC.                                 00016700
           MOVE TURBO-DESC-FIELD-DESCRIPTION TO                         00016710
                  WS-TURBO-DESCRIPTIONS.                                00016720
           IF NOT WS-TURBO-DESC                                         00016730
              MOVE TURBO-DESC-FIELD-DESCRIPTION TO CARR-DESC-FIELD-NAME 00016740
                                                (CARR-INDEX)            00016750
              MOVE TURBO-DESC-KEY-FLAG          TO CARR-DESC-FIELD-KEY  00016760
                                                (CARR-INDEX)            00016770
              MOVE TURBO-DESC-FORMAT            TO CARR-DESC-FIELD-TYPE 00016780
                                                (CARR-INDEX)            00016790
              MOVE TURBO-DESC-FIELD-LENGTH     TO CARR-DESC-FIELD-LENGTH00016800
                                                (CARR-INDEX)            00016810
                                                                        00016820
              SET CARR-INDEX UP BY 1.                                   00016830
                                                                        00016840
       0490-NOTFND.                                                     00016850
           MOVE WS-LAST-FIELD-ID     TO MAP-SCREEN-NAMEO.               00016860
           MOVE TC-PAGE-NUMBER       TO MAP-PAGE-NUMBERO.               00016870
           PERFORM 0493-DARK-SCREEN-FOR-ERR-MSG                         00016880
               VARYING DARK-SUB FROM 1 BY 1                             00016890
               UNTIL DARK-SUB > 18.                                     00016900
                                                                        00016910
           MOVE 'FIELD EXPLANATION NOT FOUND'                           00016920
               TO MAP-SCREEN-DESCO (19).                                00016930
           GO TO 0500-SEND-MAP.                                         00016940
                                                                        00016950
       0491-NOTOPEN.                                                    00016960
           MOVE WS-LAST-FIELD-ID     TO MAP-SCREEN-NAMEO.               00016970
           MOVE TC-PAGE-NUMBER       TO MAP-PAGE-NUMBERO.               00016980
           PERFORM 0493-DARK-SCREEN-FOR-ERR-MSG                         00016990
               VARYING DARK-SUB FROM 1 BY 1                             00017000
               UNTIL DARK-SUB > 18.                                     00017010
                                                                        00017020
           MOVE 'TURBDESC FILE NOT OPEN - TRY AGAIN LATER'              00017030
               TO MAP-SCREEN-DESCO (19).                                00017040
           GO TO 0500-SEND-MAP.                                         00017050
                                                                        00017060
       0492-ENDFILE.                                                    00017070
           MOVE WS-LAST-FIELD-ID     TO MAP-SCREEN-NAMEO.               00017080
           MOVE TC-PAGE-NUMBER       TO MAP-PAGE-NUMBERO.               00017090
           PERFORM 0493-DARK-SCREEN-FOR-ERR-MSG                         00017100
               VARYING DARK-SUB FROM 1 BY 1                             00017110
               UNTIL DARK-SUB > 18.                                     00017120
      **** MOVE 'DISCRIPTOR LOOKUP ERROR - ENDFILE CONDITION'           00017130
           MOVE 'NO TUTORIAL AVAILABLE'                                 00017140
               TO MAP-SCREEN-DESCO (19).                                00017150
           GO TO 0500-SEND-MAP.                                         00017160
                                                                        00017170
       0493-DARK-SCREEN-FOR-ERR-MSG.                                    00017180
           MOVE DFHBMDAR TO MAP-SCREEN-DESCA (DARK-SUB).                00017190
                                                                        00017200
       0494-NOTOPEN.                                                    00017210
           MOVE WS-LAST-FIELD-ID     TO MAP-SCREEN-NAMEO.               00017220
           MOVE TC-PAGE-NUMBER       TO MAP-PAGE-NUMBERO.               00017230
           PERFORM 0493-DARK-SCREEN-FOR-ERR-MSG                         00017240
               VARYING DARK-SUB FROM 1 BY 1                             00017250
               UNTIL DARK-SUB > 18.                                     00017260
                                                                        00017270
           MOVE 'TURBDATA FILE NOT OPEN - TRY AGAIN LATER'              00017280
               TO MAP-SCREEN-DESCO (19).                                00017290
           GO TO 0500-SEND-MAP.                                         00017300
                                                                        00017310
       0600-RETURN-STICKY.                                              00017320
           COMPUTE WS-SUB = EIBCPOSN / 80 - 1.                          00017330
           PERFORM 0605-CHECK-NUMERIC                                   00017340
               VARYING WS-STICKY-SUB FROM 1 BY 1                        00017350
               UNTIL WS-STICKY-SUB GREATER 24.                          00017360
           PERFORM 0610-FIND-LINE                                       00017370
               VARYING WS-STICKY-SUB FROM 1 BY 1                        00017380
               UNTIL WS-STICKY-SUB GREATER 20 OR                        00017390
               WS-SUB EQUAL TC-STICKY-LINE (WS-STICKY-SUB).             00017400
                                                                        00017410
           IF WS-SUB EQUAL TC-STICKY-LINE (WS-STICKY-SUB)               00017420
              MOVE TC-FIELD-FILE TO WS-FIELD-FILE                       00017430
              MOVE TC-FIELD-NUMBER TO WS-FIELD-NUMBER                   00017440
              MOVE WS-LAST-FIELD-ID TO TC-STICKY-FIELD-ID               00017450
              MOVE TUTOR-FD-POSITION-FROM TO TC-STICKY-POSITION         00017460
              MOVE TUTOR-FD-FIELD-LENGTH  TO TC-STICKY-LENGTH           00017470
              MOVE TUTOR-FD-FIELD-TYPE    TO TC-STICKY-TYPE             00017480
              MOVE TC-STICKY-VALUE-TABLE (WS-STICKY-SUB) TO             00017490
                   TC-STICKY-VALUE.                                     00017500
                                                                        00017510
           GO TO 0900-RETURN-TO-CALLER.                                 00017520
                                                                        00017530
       0605-CHECK-NUMERIC.                                              00017540
           IF TC-STICKY-LINE (WS-STICKY-SUB) NOT NUMERIC                00017550
                MOVE ZERO TO TC-STICKY-LINE (WS-STICKY-SUB).            00017560
                                                                        00017570
       0610-FIND-LINE.                                                  00017580
                                                                        00017590
       0900-MOVE-IN-MESSAGE.                                            00017600
      *                                                                 00017610
      * THIS CODE WILL START AT THE BEGINNING OF ONE OF THE FOUR        00017620
      * MESSAGE AREAS ON THE SCREEN BUFFER IMAGE, THE MESSAGE NUMBER    00017630
      * SUFFIXED IN WS-MSGTEXT-SUB, AND WILL LOOK FOR THE FIRST NON     00017640
      * SPACE OR LOW-VALUE CHAR.  THIS WILL HOPEFULLY BE THE ATTRIBUTE  00017650
      * CHARACTER OF THE MESSAGE AND PRESUMABLY, THE MESSAGE WILL       00017660
      * FOLLOW IMMEDIATLY.  THE FIRST 5 CHARACTERS OF THAT MESSAGE      00017670
      * WILL THEN BE MOVED TO WS-MSG-WORK AND WS-MSG-WORK WILL THEN     00017680
      * BE MOVED TO WS-MESSAGE-AREA FOR TUTORIAL TO LOOK UP.            00017690
      *                                                                 00017700
           MOVE +0 TO WS-MSGPOS-SUB.                                    00017710
           PERFORM 0903-IDENTIFY-MSG-POSITIONS                          00017720
              VARYING WS-MSGBUFF-SUB FROM LS-BUFFLEN  BY -1             00017730
                 UNTIL WS-MSGPOS-SUB > 160.                             00017740
           MOVE 'N' TO FOUND-SW.                                        00017750
           PERFORM 0901-FIND-NON-SPACE                                  00017760
              VARYING WS-MSGBUFF-SUB FROM WS-MSG-POS( WS-MSGTEXT-SUB )  00017770
                 BY 1                                                   00017780
                 UNTIL FOUND-SW = 'Y'                                   00017790
                 OR WS-MSGBUFF-SUB > WS-MSG-POS( WS-MSGTEXT-SUB ) + 35. 00017800
           IF FOUND-SW = 'Y'                                            00017810
              MOVE +1 TO WS-MSGWORK-SUB                                 00017820
              PERFORM 0902-MOVE-IN-MSGNUM 5 TIMES                       00017830
              MOVE WS-MSG-WORK TO WS-MESSAGE-AREA                       00017840
           ELSE                                                         00017850
              MOVE SPACES TO WS-MESSAGE-AREA.                           00017860
                                                                        00017870
       0901-FIND-NON-SPACE.                                             00017880
           IF LS-BUFF-CHAR( WS-MSGBUFF-SUB ) = LOW-VALUES               00017890
           OR LS-BUFF-CHAR( WS-MSGBUFF-SUB ) = SPACES                   00017900
           OR LS-BUFF-CHAR( WS-MSGBUFF-SUB ) = WS-SET-ATTR-BYTE         00017910
               NEXT SENTENCE                                            00017920
           ELSE                                                         00017930
               MOVE 'Y' TO FOUND-SW.                                    00017940
                                                                        00017950
       0902-MOVE-IN-MSGNUM.                                             00017960
           MOVE LS-BUFF-CHAR( WS-MSGBUFF-SUB ) TO                       00017970
                WS-MSGWORK-CHAR( WS-MSGWORK-SUB ).                      00017980
           ADD +1 TO WS-MSGBUFF-SUB, WS-MSGWORK-SUB.                    00017990
                                                                        00018000
       0903-IDENTIFY-MSG-POSITIONS.                                     00018010
           IF LS-BUFF-CHAR( WS-MSGBUFF-SUB) NOT = WS-SET-ATTR-BYTE      00018020
              ADD +1 TO WS-MSGPOS-SUB.                                  00018030
           IF WS-MSGPOS-SUB = 41                                        00018040
              MOVE WS-MSGBUFF-SUB TO WS-MSG-POS( 4 ).                   00018050
           IF WS-MSGPOS-SUB = 80                                        00018060
              MOVE WS-MSGBUFF-SUB TO WS-MSG-POS( 3 ).                   00018070
           IF WS-MSGPOS-SUB = 121                                       00018080
              MOVE WS-MSGBUFF-SUB TO WS-MSG-POS( 2 ).                   00018090
           IF WS-MSGPOS-SUB = 160                                       00018100
              MOVE WS-MSGBUFF-SUB TO WS-MSG-POS( 1 ).                   00018110
                                                                        00018120
       0500-SEND-MAP.                                                   00018130
      *---------------------------------------------------------------* 00018140
      * THIS IS THE ROUTINE TO SEND THE MAP NO MATTER WHICH OPTION    * 00018150
      * IS BEING DISPLAYED                                            * 00018160
      *---------------------------------------------------------------* 00018170
           EXEC CICS SEND MAP('TU001M1')                                00018180
                          ERASE                                         00018190
                          FREEKB                                        00018200
                          END-EXEC.                                     00018210
           IF  TC-CARRIER IS GREATER THAN SPACES                        00018220
      *        MOVE TC-CARRIER TO WS-COST-CARR-CODE                     00018230
      *    ELSE                                                         00018240
      *        MOVE SPACES     TO WS-COST-CARR-CODE.                    00018250
           PERFORM 0990-WRITE-COST-JOURNAL                              00018260
           EXEC CICS RETURN                                             00018270
                     TRANSID('TU03')                                    00018280
                     COMMAREA(LS-COMMAREA)                              00018290
                     END-EXEC.                                          00018300
           GOBACK.                                                      00018310
                                                                        00018320
       0900-RETURN-TO-CALLER.                                           00018330
                                                                        00018340
           IF TC-SCREEN-NAME = 'IS548M2' OR 'IS548M3'                   00018350
               COMPUTE TC-CURSOR-POSITION = TC-CURSOR-POSITION + 480.   00018360
                                                                        00018370
           EXEC CICS SEND FROM(LS-CA-BUFFSAVE)                          00018380
                          ERASE                                         00018390
                          CTLCHAR(WS-CTLCHAR)                           00018400
                          END-EXEC.                                     00018410
           EXEC CICS SEND CONTROL                                       00018420
                          CURSOR(TC-CURSOR-POSITION)                    00018430
                          END-EXEC.                                     00018440
                                                                        00018450
           IF LS-COMMLEN > 0                                            00018460
               EXEC CICS RETURN TRANSID(TC-TRANID)                      00018470
                                COMMAREA(LS-CA-COMMSAVE)                00018480
                                END-EXEC                                00018490
           ELSE                                                         00018500
               EXEC CICS RETURN TRANSID(TC-TRANID)                      00018510
                                END-EXEC.                               00018520
                                                                        00018530
           COPY CAPRCINC.                                               00018540
      *---------------------------------------------------------------* 00018550
      *    COPY TURBOCAL.                                             * 00018560
      *---------------------------------------------------------------* 00018570
