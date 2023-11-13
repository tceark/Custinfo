   CBL DATA(24)                                                         00000010
       IDENTIFICATION DIVISION.                                         00000020
       PROGRAM-ID. NA330B.                                              00000030
       DATE-WRITTEN. 03/22/88.                                          00000360
      *REMARKS. NA330B - ADDS CUST INFO TO DB2 TABLE              E     00000370
      *------------------------PROGRAM PURPOSE-------------------------*00000400
      *                                                                *00000410
      *  PROGRAM TITLE: NA330B                                         *00000420
      *  PROGRAM TEXT:                                                 *00000430
      *--------------------COMPILATION OPTIONS-------------------------*00000450
      *  COBOL II DB2 CICS                                             *00000440
      *----------------------------------------------------------------*00000850
      * -------------------------------------------------------------- *00001160
                                                                        00001680
       ENVIRONMENT DIVISION.                                            00001690
       DATA DIVISION.                                                   00001700
       WORKING-STORAGE SECTION.                                         00001710
                                                                        00003030
       01  WS-WORK-AREA.                                                00003040
      *                                                                 00003130
           05  WS-CUSTOMER-STATUS           PIC X     VALUE SPACES.     00003230
               88  INVALID-CUSTOMER-STATUS            VALUE 'D' 'C'.    00003240
           05  WS-SUBCRIPT COMP.                                        00003450
               10  ACTION-SUB               PIC S99.                    00003460
           05  WS-HOLD-MESSAGE              PIC X(5)   VALUE SPACES.    00003470
           05  WS-TRAN                      PIC X(6)   VALUE 'ADD'.     00003470
           05  WS-IDNTITY-TYPE              PIC X(8)   VALUE 'SPACES'.  00003470
           05  WS-N                         PIC X(1)   VALUE 'N'.       00003470
           05  WS-ERR-FIELD                 PIC X(40)  VALUE SPACES.    00003510
           05  WS-DATE-R                    PIC 9(6)   VALUE ZERO.      00003620
           05  WS-DATE.                                                 00003630
               10  WS-MM                    PIC 99     VALUE ZERO.      00003640
               10  WS-DD                    PIC 99     VALUE ZERO.      00003650
               10  WS-YY                    PIC 99     VALUE ZERO.      00003660
           05  WS-DATE-A REDEFINES WS-DATE.                             00003670
               10  WS-MM-A                  PIC XX.                     00003680
               10  WS-DD-A                  PIC XX.                     00003690
               10  WS-YY-A                  PIC XX.                     00003700
           05  WS-EFFECTIVE-DATE.                                       00003780
               10  EF-CC                    PIC XX.                     00003790
               10  EF-YY                    PIC XX.                     00003800
               10  FILLER                   PIC X      VALUE '-'.       00003810
               10  EF-MM                    PIC XX.                     00003820
               10  FILLER                   PIC X      VALUE '-'.       00003830
               10  EF-DD                    PIC XX.                     00003840
           05  HOLD-NEXT-ID                 PIC 9(8) VALUE ZERO.        00003950
           05  HOLD-NEXT-ID-R  REDEFINES  HOLD-NEXT-ID                  00003960
                                            PIC X(8).                   00003970
           05  WS-IDNTITY-NUM                PIC X(8).                  00004220
           05  WS-NUMERIC-CHECK.                                        00004270
               10 WS-NUMERIC-CHECK-BYTE OCCURS 20 TIMES                 00004280
                                            PIC X.                      00004290
           05  POSTAL-CODE                  PIC X(9).                   00004340
           05  COUNTRY-CODE                 PIC X(3).                   00004350
           05  IND-POSTAL-CODE              PIC S9(4) COMP.             00004360
           05  IND-COUNTRY-CODE             PIC S9(4) COMP.             00004370
           05  WS-NUMERIC-SW                PIC X.                      00004410
           05  WS-NUMERIC-TEST8  PIC X(8).                              00004500
           05  WS-TABLE-RETRY-CNT           PIC 9(2) COMP-3 VALUE ZERO. 00004560
           05  WS-SPACES-1                  PIC X(1)   VALUE SPACES.    00003510
           05  WS-SPACES-2                  PIC X(2)   VALUE SPACES.    00003510
           05  WS-SPACES-4                  PIC X(4)   VALUE SPACES.    00003510
           05  WS-SPACES-5                  PIC X(5)   VALUE SPACES.    00003510
                                                                        00005480
      *    COPY AGNTNCW3.                                               00006080
           COPY AGNTNV05.                                               00006090
      *    COPX AGNTNAME is not the base copybook, AGNTNV05 is.         00006100
           COPY CASNVW3.                                                00006110
           COPY NEXTCIM.                                                00006120
           COPY IDTYHIST.                                               00006120
           COPY IDTYINFO.                                               00006120
                                                                        00006130
           EXEC SQL                                                     00006140
              INCLUDE SQLCA                                             00006150
           END-EXEC.                                                    00006160
                                                                        00006180
       LINKAGE SECTION.                                                 00007340

       01 DFHCOMMAREA.
           COPY ADDCOMA.
                                                                        00006070
      ******************************************************************00007500
                                                                        00007510
       PROCEDURE DIVISION.                                              00007520
                                                                        00007530
       0010-BEGIN-PROGRAM.                                              00007540
           DISPLAY 'NA330B 0010-BEGIN-PROGRAM'.
           MOVE COM-ENTITY-TYPE    TO COM-ENTITY-LITERAL.
      *    MOVE COM-ENTITY-LITERAL TO COM-ENTITY-TYPE.
           DISPLAY 'EIBTRNID:' EIBTRNID.
           DISPLAY 'COM-IDNTITY:           '  COM-IDNTITY
           DISPLAY 'COM-LAST-NAME:         '  COM-LAST-NAME
           DISPLAY 'COM-FIRST-NAME:        '  COM-FIRST-NAME
           DISPLAY 'COM-MIDDLE-NAME:       '  COM-MIDDLE-NAME
           DISPLAY 'COM-PREFIX:            '  COM-PREFIX
           DISPLAY 'COM-SUFFIX1:           '  COM-SUFFIX1
           DISPLAY 'COM-SUFFIX2:           '  COM-SUFFIX2
           DISPLAY 'COM-COMPANY-IND:       '  COM-COMPANY-IND
           DISPLAY 'COM-COMPANY-IN-ADDRESS:'  COM-COMPANY-IN-ADDRESS
           DISPLAY 'COM-COMPANY-NAME:      '  COM-COMPANY-NAME
           DISPLAY 'COM-DISPLAY-NAME:      '  COM-DISPLAY-NAME
           DISPLAY 'COM-NICKNAME:          '  COM-NICKNAME
           DISPLAY 'COM-ADDRESS1:          '  COM-ADDRESS1
           DISPLAY 'COM-ADDRESS2:          '  COM-ADDRESS2
           DISPLAY 'COM-CITY:              '  COM-CITY
           DISPLAY 'COM-STATE:             '  COM-STATE
           DISPLAY 'COM-ZIP:               '  COM-ZIP
           DISPLAY 'COM-ZIP-PLUS4:         '  COM-ZIP-PLUS4
           DISPLAY 'COM-COUNTY-CODE:       '  COM-COUNTY-CODE
           DISPLAY 'COM-AREA-CODE:         '  COM-AREA-CODE
           DISPLAY 'COM-PHONE:             '  COM-PHONE
           DISPLAY 'COM-PHONE-EXTENSION:   '  COM-PHONE-EXTENSION
           DISPLAY 'COM-SSN:               '  COM-SSN
           DISPLAY 'COM-SEX:               '  COM-SEX
           DISPLAY 'COM-BIRTH-DATE:        '  COM-BIRTH-DATE
           DISPLAY 'COM-FINALST-REAS-CODE: '  COM-FINALST-REAS-CODE
           DISPLAY 'COM-FINALST-OVRD-IND:  '  COM-FINALST-OVRD-IND
           DISPLAY 'COM-DUP-ADDR-OVRD-IND: '  COM-DUP-ADDR-OVRD-IND
           DISPLAY 'COM-EFFECTIVE-DATE:    '  COM-EFFECTIVE-DATE
           DISPLAY 'COM-CHANGE-DATE:       '  COM-CHANGE-DATE
           DISPLAY 'COM-CHANGE-LOGON:      '  COM-CHANGE-LOGON
           DISPLAY 'COM-ENTITY-TYPE:       '  COM-ENTITY-TYPE
           DISPLAY 'COM-RECORD-STATUS:     '  COM-RECORD-STATUS
           DISPLAY 'COM-ALT-ADDRESS-IND:   '  COM-ALT-ADDRESS-IND
           DISPLAY 'COM-FUTURE-ADDRESS-IND:'  COM-FUTURE-ADDRESS-IND
           DISPLAY 'COM-RECORD-ORIGIN:     '  COM-RECORD-ORIGIN
           DISPLAY 'COM-COMBINED-STATUS:   '  COM-COMBINED-STATUS
           DISPLAY 'COM-SITE-CODE:         '  COM-SITE-CODE
           DISPLAY 'COM-NAME-KEY1:         '  COM-NAME-KEY1
           DISPLAY 'COM-NAME-KEY2:         '  COM-NAME-KEY2
           DISPLAY 'COM-NAME-KEY3:         '  COM-NAME-KEY3
           DISPLAY 'COM-ADDRESS-KEY1:      '  COM-ADDRESS-KEY1
           DISPLAY 'COM-ASSOCIATION1:      '  COM-ASSOCIATION1
           DISPLAY 'COM-ASSOCIATION2:      '  COM-ASSOCIATION2
           DISPLAY 'COM-ASSOCIATION3:      '  COM-ASSOCIATION3
           DISPLAY 'COM-ENTITY-LITERAL:    '  COM-ENTITY-LITERAL
           DISPLAY 'COM-CASE-SW:           '  COM-CASE-SW
           DISPLAY 'COM-FINALIST-SW:       '  COM-FINALIST-SW
           DISPLAY 'COM-FAX-AREA-CODE:     '  COM-FAX-AREA-CODE
           DISPLAY 'COM-FAX-PHONE:         '  COM-FAX-PHONE
           DISPLAY 'COM-EMAIL1:            '  COM-EMAIL1
           DISPLAY 'COM-POSTAL-CODE:       '  COM-POSTAL-CODE
           DISPLAY 'COM-COUNTRY-CODE:      '  COM-COUNTRY-CODE

           PERFORM 100-EDIT-CUST-INFO  THRU 100-EXIT.

           PERFORM 200-ADD-DB2-TABLE THRU 200-EXIT.

           DISPLAY 'NA330B 0010-END-PROGRAM'.
           EXEC CICS RETURN END-EXEC.

       0010-EXIT.
           EXIT.
                                                                        00007990
       100-EDIT-CUST-INFO.                                              00013800
           DISPLAY '100-EDIT-CUST-INFO'                                 00013810
           MOVE 'N' TO WS-EDIT-SW.                                      00013820
                                                                        00013830
      *---------------------------------------------------------------* 00013840
      * EDIT CUSTOMER TYPE FOR VALIDITY                               * 00013850
      *---------------------------------------------------------------* 00013860
           DISPLAY 'COM-ENTITY-LITERAL' COM-ENTITY-LITERAL              00013870
           INSPECT COM-ENTITY-LITERAL                                   00013880
               REPLACING ALL 'a' BY 'A'                                 00013890
                             'b' BY 'B'                                 00013900
                             'c' BY 'C'                                 00013910
                             'd' BY 'D'                                 00013920
                             'e' BY 'E'                                 00013930
                             'f' BY 'F'                                 00013940
                             'g' BY 'G'                                 00013950
                             'h' BY 'H'                                 00013960
                             'i' BY 'I'                                 00013970
                             'j' BY 'J'                                 00013980
                             'k' BY 'K'                                 00013990
                             'l' BY 'L'                                 00014000
                             'm' BY 'M'                                 00014010
                             'n' BY 'N'                                 00014020
                             'o' BY 'O'.                                00014030
                                                                        00014040
           INSPECT COM-ENTITY-LITERAL                                   00014050
               REPLACING ALL 'p' BY 'P'                                 00014060
                             'q' BY 'Q'                                 00014070
                             'r' BY 'R'                                 00014080
                             's' BY 'S'                                 00014090
                             't' BY 'T'                                 00014100
                             'u' BY 'U'                                 00014110
                             'v' BY 'V'                                 00014120
                             'w' BY 'W'                                 00014130
                             'x' BY 'X'                                 00014140
                             'y' BY 'Y'                                 00014150
                             'z' BY 'Z'.                                00014160

           DISPLAY '1'
           IF  COM-ENTITY-LITERAL NOT = 'CAM' AND 'AM' AND 'ML' AND     00014430
               'MAIL' AND 'BR' AND 'BROKER' AND 'LB' AND 'LIST BILL'    00014440
               AND 'BA' AND 'BILL ADDR'                                 00014450
               AND 'LD' AND 'LEAD'                                      00014460
               AND 'RP' AND 'NOI'                                       00014470
                   MOVE 'Y'      TO WS-EDIT-SW                          00014480
                   MOVE 'MAP-ENTITY-TYPE' TO WS-ERR-FIELD               00014490
                   MOVE 'NA004'  TO WS-HOLD-MESSAGE                     00014510
                   PERFORM 160-SLIDE-ERROR-MESSAGES THRU 160-EXIT.      00014520
           DISPLAY '2'
                                                                        00014530
      *---------------------------------------------------------------* 00014540
      * EDIT CUSTOMER STATUS FOR VALIDITY                             * 00014550
      *---------------------------------------------------------------* 00014560
              IF COM-ENTITY-TYPE = 'BR' OR 'ML'                         00014680
                 MOVE COM-RECORD-STATUS  TO WS-CUSTOMER-STATUS          00014690
                 IF  INVALID-CUSTOMER-STATUS                            00014700
                     MOVE 'Y'      TO WS-EDIT-SW                        00014710
                     MOVE 'MAP-CUST-STATUS' TO WS-ERR-FIELD             00014490
                     MOVE 'NA005'  TO WS-HOLD-MESSAGE                   00014740
                     PERFORM 160-SLIDE-ERROR-MESSAGES THRU 160-EXIT     00014750
                 END-IF                                                 00014760
              END-IF                                                    00014770
           DISPLAY '3'
                                                                        00014790
      *---------------------------------------------------------------* 00014800
      * EDIT CUSTOMER SSN STATUS FOR VALIDITY                         * 00014810
      *---------------------------------------------------------------* 00014820
           IF COM-SSN NOT = SPACES AND COM-SSN NOT = LOW-VALUES         00014830
               IF COM-SSN NOT NUMERIC                                   00014840
                   MOVE 'Y'      TO WS-EDIT-SW                          00014850
                   MOVE 'MAP-SSN' TO WS-ERR-FIELD                       00014490
                   MOVE 'NA031'  TO WS-HOLD-MESSAGE                     00014880
                   PERFORM 160-SLIDE-ERROR-MESSAGES THRU 160-EXIT.      00014890
           DISPLAY '4'
                                                                        00014900
      *---------------------------------------------------------------* 00014910
      * EDIT CUSTOMER PHONE NUMBER FOR VALIDITY                       * 00014920
      *---------------------------------------------------------------* 00014930
           IF COM-AREA-CODE NOT = SPACES AND                            00014940
               COM-AREA-CODE NOT = LOW-VALUES                           00014950
               IF COM-AREA-CODE NOT NUMERIC                             00014960
                   MOVE 'Y'      TO WS-EDIT-SW                          00014970
                   MOVE 'MAP-AREA-CODE' to WS-ERR-FIELD                 00014490
                   MOVE 'NA032'  TO WS-HOLD-MESSAGE                     00015000
                   PERFORM 160-SLIDE-ERROR-MESSAGES THRU 160-EXIT.      00015010
                                                                        00015020
           IF COM-PHONE NOT = SPACES AND                                00015030
               COM-PHONE NOT = LOW-VALUES                               00015040
               IF COM-PHONE NOT NUMERIC                                 00015050
                   MOVE 'Y'      TO WS-EDIT-SW                          00015060
                   MOVE 'MAP-PHONE3' TO WS-ERR-FIELD                    00014490
                   MOVE 'NA033'  TO WS-HOLD-MESSAGE                     00015100
                   PERFORM 160-SLIDE-ERROR-MESSAGES THRU 160-EXIT.      00015110
                                                                        00015120
           IF COM-PHONE-EXTENSION NOT = SPACES AND                      00015130
               COM-PHONE-EXTENSION NOT = LOW-VALUES                     00015140
               IF COM-PHONE-EXTENSION NOT NUMERIC                       00015150
                   MOVE 'Y'      TO WS-EDIT-SW                          00015160
                   MOVE 'MAP-PHONE-EXT' TO WS-ERR-FIELD                 00014490
                   MOVE 'NA034'  TO WS-HOLD-MESSAGE                     00015190
                   PERFORM 160-SLIDE-ERROR-MESSAGES THRU 160-EXIT.      00015200
                                                                        00015210
      *---------------------------------------------------------------* 00015220
      * EDIT CUSTOMER FAX PHONE NUMBER FOR VALIDITY                   * 00015230
      *---------------------------------------------------------------* 00015240
           IF COM-FAX-AREA-CODE NOT = SPACES AND                        00015250
               COM-FAX-AREA-CODE NOT = LOW-VALUES                       00015260
               IF COM-FAX-AREA-CODE NOT NUMERIC                         00015270
                   MOVE 'Y'      TO WS-EDIT-SW                          00015280
                   MOVE 'MAP-FAX-AREA-CD' TO WS-ERR-FIELD               00014490
                   MOVE 'NA186'  TO WS-HOLD-MESSAGE                     00015310
                   PERFORM 160-SLIDE-ERROR-MESSAGES THRU 160-EXIT.      00015320
                                                                        00015330
           IF COM-FAX-PHONE NOT = SPACES AND                            00015340
               COM-FAX-PHONE NOT = LOW-VALUES                           00015350
               IF COM-FAX-PHONE NOT NUMERIC                             00015360
                   MOVE 'Y'      TO WS-EDIT-SW                          00015370
                   MOVE 'MAP-FAX-PHONE3' TO WS-ERR-FIELD                00014490
                   MOVE 'NA187'  TO WS-HOLD-MESSAGE                     00015410
                   PERFORM 160-SLIDE-ERROR-MESSAGES THRU 160-EXIT.      00015420
                                                                        00015690
           IF  COM-COMPANY-IN-ADDRESS NOT = 'Y' AND                     00015510
               COM-COMPANY-IN-ADDRESS NOT = 'N'                         00015520
               MOVE 'Y'      TO WS-EDIT-SW                              00015530
               MOVE 'MAP-COMP-IN-ADD' TO WS-ERR-FIELD                   00014490
               MOVE 'NA035'  TO WS-HOLD-MESSAGE                         00015560
               PERFORM 160-SLIDE-ERROR-MESSAGES THRU 160-EXIT.          00015570
                                                                        00015580
      *---------------------------------------------------------------* 00015590
      * EDIT ADDRESS OVERRIDE INDICATOR FOR VALIDITY                  * 00015600
      *---------------------------------------------------------------* 00015610
           IF COM-FINALST-OVRD-IND NOT = 'N' AND                        00015620
              COM-FINALST-OVRD-IND NOT = 'Y'                            00015630
                   MOVE 'Y'      TO WS-EDIT-SW                          00015640
                   MOVE 'MAP-ADDR-OVRIDE' TO WS-ERR-FIELD               00014490
                   MOVE 'NA036'  TO WS-HOLD-MESSAGE                     00015670
                   PERFORM 160-SLIDE-ERROR-MESSAGES THRU 160-EXIT.      00015680
                                                                        00015690
      *---------------------------------------------------------------* 00015700
      * EDIT ADDRESS FIELDS WITH FINALST FOR VALIDITY                 * 00015710
      *---------------------------------------------------------------* 00015720
                                                                        00015730
           IF COM-ZIP NOT = SPACES  AND                                 00015740
              COM-ZIP NOT NUMERIC                                       00015750
              MOVE 'Y'      TO WS-EDIT-SW                               00015370
              MOVE 'MAP-ZIP' TO WS-ERR-FIELD                            00014490
              MOVE 'NA194'  TO WS-HOLD-MESSAGE                          00015760
              MOVE 'N' TO COM-FINALIST-SW                               00015770
              PERFORM 160-SLIDE-ERROR-MESSAGES THRU 160-EXIT            00015780
           END-IF                                                       00015790
           IF COM-ZIP EQUAL SPACES OR ZEROS                             00015800
              AND COM-FINALST-OVRD-IND = 'Y'                            00015810
              MOVE 'Y'      TO WS-EDIT-SW                               00015370
              MOVE 'MAP-ZIP' TO WS-ERR-FIELD                            00014490
              MOVE 'NA194'  TO WS-HOLD-MESSAGE                          00015820
              MOVE 'N' TO COM-FINALIST-SW                               00015830
              PERFORM 160-SLIDE-ERROR-MESSAGES THRU 160-EXIT            00015840
           END-IF                                                       00015850
                                                                        00015860
      *---------------------------------------------------------------* 00016110
      * EDIT ADDRESS FIELDS FOR SPACES                                * 00016120
      *---------------------------------------------------------------* 00016130
           IF COM-ADDRESS1 = SPACES AND COM-ADDRESS2 = SPACES           00016140
                   MOVE 'Y'      TO WS-EDIT-SW                          00016150
                   MOVE 'MAP-ADDRESS1' TO WS-ERR-FIELD                  00014490
                   MOVE 'NA182'  TO WS-HOLD-MESSAGE                     00016180
                   PERFORM 160-SLIDE-ERROR-MESSAGES THRU 160-EXIT.      00016190
           DISPLAY '5'
                                                                        00016200
      *---------------------------------------------------------------* 00016210
      * EDIT CITY FIELD FOR SPACES                                    * 00016220
      *---------------------------------------------------------------* 00016230
           IF COM-CITY = SPACES                                         00016240
                   MOVE 'Y'      TO WS-EDIT-SW                          00016250
                   MOVE 'MAP-CITY' TO WS-ERR-FIELD                      00014490
                   MOVE 'NA183'  TO WS-HOLD-MESSAGE                     00016280
                   PERFORM 160-SLIDE-ERROR-MESSAGES THRU 160-EXIT.      00016290
                                                                        00016300
           DISPLAY '5A'
      *---------------------------------------------------------------* 00016310
      * EDIT STATE FIELD FOR SPACES                                   * 00016320
      *---------------------------------------------------------------* 00016330
           IF COM-STATE = SPACES                                        00016340
                   MOVE 'Y'      TO WS-EDIT-SW                          00016350
                   MOVE 'MAP-STATE' TO WS-ERR-FIELD                     00014490
                   MOVE 'NA184'  TO WS-HOLD-MESSAGE                     00016380
                   PERFORM 160-SLIDE-ERROR-MESSAGES THRU 160-EXIT.      00016390
           DISPLAY '5B'
                                                                        00016400
           IF COM-LAST-NAME = SPACES AND COM-FIRST-NAME = SPACES AND    00017210
              COM-MIDDLE-NAME = SPACES AND COM-COMPANY-NAME = SPACES    00017220
                 MOVE 'Y'     TO WS-EDIT-SW                             00017230
                 MOVE 'MAP-FIRST-NAME' TO WS-ERR-FIELD                  00014490
                 MOVE 'NA027' TO WS-HOLD-MESSAGE                        00017360
                 PERFORM 160-SLIDE-ERROR-MESSAGES THRU 160-EXIT.        00017370
                                                                        00017380
           DISPLAY '5F'
      *---------------------------------------------------------------* 00017390
      * EDIT LAST NAME SPACES WHEN FIRST AND MIDDLE ENTERED           * 00017400
      *---------------------------------------------------------------* 00017410
                                                                        00017420
           IF COM-FIRST-NAME NOT = SPACES AND                           00017430
               COM-MIDDLE-NAME NOT = SPACES AND COM-LAST-NAME = SPACES  00017440
               OR COM-FIRST-NAME NOT = SPACES AND COM-LAST-NAME = SPACES00017450
               OR COM-MIDDLE-NAME NOT = SPACES AND COM-LAST-NAME = SPACE00017460
                   MOVE 'Y'      TO WS-EDIT-SW                          00017470
                   MOVE 'MAP-LAST-NAME' TO WS-ERR-FIELD                 00014490
                   MOVE 'NA161'  TO WS-HOLD-MESSAGE                     00017480
                   PERFORM 160-SLIDE-ERROR-MESSAGES THRU 160-EXIT.      00017510
                                                                        00017520
           DISPLAY '5G'
      *---------------------------------------------------------------* 00017530
      * EDIT FIRST NAME WHEN LAST NAME ENTERED                        * 00017540
      *---------------------------------------------------------------* 00017550
                                                                        00017560
           IF COM-LAST-NAME NOT = SPACES AND COM-FIRST-NAME = SPACE     00017570
                   MOVE 'Y'      TO WS-EDIT-SW                          00017580
                   MOVE 'MAP-FIRST-NAME' TO WS-ERR-FIELD                00017590
                   MOVE 'NA161'  TO WS-HOLD-MESSAGE                     00017610
                   PERFORM 160-SLIDE-ERROR-MESSAGES THRU 160-EXIT.      00017620
                                                                        00017630
      *---------------------------------------------------------------* 00017640
      * DECIDE IF RECORD BELONGS TO A COMPANY OR AGENT                * 00017650
      *---------------------------------------------------------------* 00017660
           DISPLAY '6'
           IF (COM-LAST-NAME = SPACES AND COM-FIRST-NAME = SPACES AND   00017680
               COM-MIDDLE-NAME = SPACES) OR                             00017690
MANJU        (COM-COMPANY-NAME NOT = SPACES)                            00017710
                   MOVE 'Y' TO COM-COMPANY-IND                          00017720
                   DISPLAY 'COM-COMPANY-IND' COM-COMPANY-IND
                   UNSTRING COM-COMPANY-NAME                            00017730
                       DELIMITED BY ALL ' ' OR ','                      00017740
                           INTO COM-NAME-KEY1                           00017750
                                COM-NAME-KEY2                           00017760
                                COM-NAME-KEY3                           00017770
           ELSE                                                         00017780
              MOVE 'N' TO COM-COMPANY-IND                               00017790
              MOVE COM-LAST-NAME   TO COM-NAME-KEY1                     00017800
              MOVE COM-FIRST-NAME  TO COM-NAME-KEY2                     00017810
              MOVE COM-MIDDLE-NAME TO COM-NAME-KEY3.                    00017820
                                                                        00017830
      *---------------------------------------------------------------* 00017940
      * EDIT CUSTOMER SEX STATUS FOR VALIDITY                         * 00017950
      *---------------------------------------------------------------* 00017960
           DISPLAY 'COM-COMPANY-IND '   COM-COMPANY-IND                 00017970
           IF COM-COMPANY-IND NOT = 'Y'                                 00017980
               IF COM-SEX NOT = 'M' AND COM-SEX NOT = 'F'               00017990
                   MOVE 'Y'      TO WS-EDIT-SW                          00018000
                   MOVE 'MAP-SEX' TO WS-ERR-FIELD                       00017590
                   MOVE 'NA154'  TO WS-HOLD-MESSAGE                     00018030
                   PERFORM 160-SLIDE-ERROR-MESSAGES THRU 160-EXIT.      00018040
                                                                        00018050
           IF COM-COMPANY-IND = 'Y'                                     00018060
               MOVE SPACE TO COM-SEX.                                   00018070
                                                                        00018080
      *---------------------------------------------------------------* 00018090
      * RECORD ORIGIN IS AN ONLINE ADD = 'O'                          * 00018100
      *---------------------------------------------------------------* 00018110
                                                                        00018120
           MOVE 'O' TO COM-RECORD-ORIGIN.                               00018130
                                                                        00018140
      *---------------------------------------------------------------* 00018150
      * EDIT LAST-NAME FOR NUMERICS                                   * 00018160
      *---------------------------------------------------------------* 00018170
                                                                        00018180
           DISPLAY '7'                                                  00017970
           MOVE COM-LAST-NAME TO WS-NUMERIC-CHECK.                      00018190
           PERFORM 150-NUMERIC-CHECK THRU 150-EXIT                      00018200
               VARYING ACTION-SUB FROM 1 BY 1                           00018210
                   UNTIL ACTION-SUB > 20.                               00018220
                                                                        00018230
           DISPLAY '8'                                                  00017970
           IF WS-NUMERIC-SW = 'Y'                                       00018240
                   MOVE 'Y'      TO WS-EDIT-SW                          00018250
                   MOVE 'MAP-LAST-NAME' TO WS-ERR-FIELD                 00017590
                   MOVE 'NA022'  TO WS-HOLD-MESSAGE                     00018280
                   PERFORM 160-SLIDE-ERROR-MESSAGES THRU 160-EXIT.      00018290
                                                                        00018300
      *---------------------------------------------------------------* 00018310
      * EDIT FIRST NAME FOR NUMERICS                                  * 00018320
      *---------------------------------------------------------------* 00018330
                                                                        00018340
           DISPLAY '9'                                                  00017970
           MOVE 'N'            TO WS-NUMERIC-SW.                        00018350
           MOVE COM-FIRST-NAME TO WS-NUMERIC-CHECK.                     00018360
           PERFORM 150-NUMERIC-CHECK THRU 150-EXIT                      00018370
               VARYING ACTION-SUB FROM 1 BY 1                           00018380
                   UNTIL ACTION-SUB > 15.                               00018390
                                                                        00018400
           DISPLAY '10'                                                 00017970
           IF WS-NUMERIC-SW = 'Y'                                       00018410
                   MOVE 'Y'      TO WS-EDIT-SW                          00018420
                   MOVE 'MAP-FIRST-NAME' TO WS-ERR-FIELD                00017590
                   MOVE 'NA040'  TO WS-HOLD-MESSAGE                     00018450
                   PERFORM 160-SLIDE-ERROR-MESSAGES THRU 160-EXIT.      00018460
                                                                        00018470
      *---------------------------------------------------------------* 00018480
      * EDIT MIDDLE NAME FOR NUMERICS                                 * 00018490
      *---------------------------------------------------------------* 00018500
                                                                        00018510
           MOVE 'N'             TO WS-NUMERIC-SW.                       00018520
           MOVE COM-MIDDLE-NAME TO WS-NUMERIC-CHECK.                    00018530
           PERFORM 150-NUMERIC-CHECK THRU 150-EXIT                      00018540
               VARYING ACTION-SUB FROM 1 BY 1                           00018550
                   UNTIL ACTION-SUB > 10.                               00018560
                                                                        00018570
           DISPLAY '11'                                                 00017970
           IF WS-NUMERIC-SW = 'Y'                                       00018580
                   MOVE 'Y'      TO WS-EDIT-SW                          00018590
                   MOVE 'MAP-MIDDLE-NAME' TO WS-ERR-FIELD               00017590
                   MOVE 'NA041'  TO WS-HOLD-MESSAGE                     00018620
                   PERFORM 160-SLIDE-ERROR-MESSAGES THRU 160-EXIT.      00018630
                                                                        00018640
                                                                        00018650
      *---------------------------------------------------------------* 00018840
      * EDIT COUNTRY CODE FOR VALIDITY                                * 00018850
      *---------------------------------------------------------------* 00018860
           DISPLAY '12'                                                 00017970
           DISPLAY 'COM-ZIP:' COM-ZIP
           IF COM-ZIP = '00100'                                         00018870
              EVALUATE TRUE                                             00018880
                 WHEN COM-COUNTRY-CODE = SPACES                         00018890
                   MOVE 'Y'      TO WS-EDIT-SW                          00018900
                   MOVE 'MAP-COUNTRY-CD' TO WS-ERR-FIELD
                   MOVE 'CM427'  TO WS-HOLD-MESSAGE                     00018930
                   PERFORM 160-SLIDE-ERROR-MESSAGES THRU 160-EXIT       00018940
                 WHEN COM-COUNTRY-CODE = 'US'                           00018950
                   MOVE 'Y'      TO WS-EDIT-SW                          00018960
                   MOVE 'MAP-COUNTRY-CD' TO WS-ERR-FIELD
                   MOVE 'CM428'  TO WS-HOLD-MESSAGE                     00018990
                   PERFORM 160-SLIDE-ERROR-MESSAGES THRU 160-EXIT       00019000
              END-EVALUATE                                              00019170
           ELSE                                                         00019180
              IF COM-ZIP NOT EQUAL '00100'                              00019190
                 EVALUATE TRUE                                          00019200
                    WHEN COM-COUNTRY-CODE = SPACES                      00019210
                       CONTINUE                                         00019220
                    WHEN COM-COUNTRY-CODE NOT EQUAL SPACES              00019230
                       MOVE 'Y'      TO WS-EDIT-SW                      00019240
                       MOVE 'MAP-COUNTRY-CD' TO WS-ERR-FIELD
                       MOVE 'CM428'  TO WS-HOLD-MESSAGE                 00019270
                       PERFORM 160-SLIDE-ERROR-MESSAGES THRU 160-EXIT   00019280
                    END-EVALUATE                                        00019290
              END-IF                                                    00019300
           END-IF.                                                      00019310
                                                                        00019320
           DISPLAY '14'                                                 00017970
                                                                        00019330
      *---------------------------------------------------------------* 00019340
      * EDIT POSTAL CODE FOR VALIDITY                                 * 00019350
      *---------------------------------------------------------------* 00019360
           IF COM-ZIP = '00100'                                         00019370
             IF COM-COUNTRY-CODE = 'CA'                                 00019380
               EVALUATE TRUE ALSO TRUE                                  00019390
                 WHEN COM-STATE = 'AB' ALSO COM-POSTAL-CODE (1:1) = 'T' 00019400
                   CONTINUE                                             00019410
                 WHEN COM-STATE = 'BC' ALSO COM-POSTAL-CODE (1:1) = 'V' 00019420
                   CONTINUE                                             00019430
                 WHEN COM-STATE = 'LB' ALSO COM-POSTAL-CODE (1:1) = 'A' 00019440
                   CONTINUE                                             00019450
                 WHEN COM-STATE = 'MB' ALSO COM-POSTAL-CODE (1:1) = 'R' 00019460
                   CONTINUE                                             00019470
                 WHEN COM-STATE = 'NB' ALSO COM-POSTAL-CODE (1:1) = 'E' 00019480
                   CONTINUE                                             00019490
                 WHEN COM-STATE = 'NF' ALSO COM-POSTAL-CODE (1:1) = 'A' 00019500
                   CONTINUE                                             00019510
                 WHEN COM-STATE = 'NA' ALSO COM-POSTAL-CODE (1:1) = 'X' 00019520
                   CONTINUE                                             00019530
                 WHEN COM-STATE = 'NS' ALSO COM-POSTAL-CODE (1:1) = 'B' 00019540
                   CONTINUE                                             00019550
                 WHEN COM-STATE = 'ON' ALSO COM-POSTAL-CODE (1:1) = 'L' 00019560
                 WHEN COM-STATE = 'ON' ALSO COM-POSTAL-CODE (1:1) = 'N' 00019570
                 WHEN COM-STATE = 'ON' ALSO COM-POSTAL-CODE (1:1) = 'K' 00019580
                 WHEN COM-STATE = 'ON' ALSO COM-POSTAL-CODE (1:1) = 'M' 00019590
                 WHEN COM-STATE = 'ON' ALSO COM-POSTAL-CODE (1:1) = 'P' 00019600
                   CONTINUE                                             00019610
                 WHEN COM-STATE = 'PE' ALSO COM-POSTAL-CODE (1:1) = 'C' 00019620
                   CONTINUE                                             00019630
                 WHEN COM-STATE = 'PQ' ALSO COM-POSTAL-CODE (1:1) = 'H' 00019640
                 WHEN COM-STATE = 'PQ' ALSO COM-POSTAL-CODE (1:1) = 'J' 00019650
                 WHEN COM-STATE = 'PQ' ALSO COM-POSTAL-CODE (1:1) = 'G' 00019660
                   CONTINUE                                             00019670
                 WHEN COM-STATE = 'SK' ALSO COM-POSTAL-CODE (1:1) = 'S' 00019680
                   CONTINUE                                             00019690
                 WHEN COM-STATE = 'YT' ALSO COM-POSTAL-CODE (1:1) = 'Y' 00019700
                   CONTINUE                                             00019710
                 WHEN OTHER                                             00019720
                     MOVE 'Y'      TO WS-EDIT-SW                        00019730
                     MOVE 'MAP-POSTAL-CD' TO WS-ERR-FIELD
                     MOVE 'CM120'  TO WS-HOLD-MESSAGE                   00019760
                     PERFORM 160-SLIDE-ERROR-MESSAGES THRU 160-EXIT     00019770
                 END-EVALUATE                                           00019780
             END-IF                                                     00019790
           ELSE                                                         00019800
             IF COM-POSTAL-CODE = SPACES                                00019810
                CONTINUE                                                00019820
             ELSE                                                       00019830
                MOVE 'Y'      TO WS-EDIT-SW                             00019840
                MOVE 'MAP-POSTAL-CD' TO WS-ERR-FIELD
                MOVE 'CM120'  TO WS-HOLD-MESSAGE                        00019870
                PERFORM 160-SLIDE-ERROR-MESSAGES THRU 160-EXIT          00019880
             END-IF                                                     00019890
           END-IF.                                                      00019900
                                                                        00019910
           DISPLAY '15'                                                 00017970
      *---------------------------------------------------------------* 00019920
      * EDIT STATE CODE FOR VALIDITY                                  * 00019930
      *---------------------------------------------------------------* 00019940
           IF COM-STATE = SPACES OR                                     00019950
              COM-STATE = 'XX'                                          00019960
              EVALUATE TRUE                                             00019970
                 WHEN COM-COUNTRY-CODE = SPACES                         00019980
                 WHEN COM-COUNTRY-CODE = LOW-VALUES                     00019990
                 WHEN COM-COUNTRY-CODE = 'CA'                           00020000
                   MOVE 'Y'      TO WS-EDIT-SW                          00020010
                   MOVE 'MAP-STATE'  TO WS-ERR-FIELD
                   MOVE 'CM016'  TO WS-HOLD-MESSAGE                     00020040
                   PERFORM 160-SLIDE-ERROR-MESSAGES THRU 160-EXIT       00020050
              END-EVALUATE                                              00020060
           END-IF.                                                      00020120
                                                                        00021630
       100-EXIT.                                                        00013800
           EXIT.

       150-NUMERIC-CHECK.                                               00022730
                                                                        00022740
           IF WS-NUMERIC-CHECK-BYTE (ACTION-SUB) NUMERIC                00022750
               MOVE 'Y' TO WS-NUMERIC-SW.                               00022760
                                                                        00022770
       150-EXIT.
           EXIT.

       160-SLIDE-ERROR-MESSAGES.                                        00022780
                                                                        00022790
           IF WS-MESSAGE-NUMBER1 = SPACES OR LOW-VALUES                 00022800
               MOVE WS-HOLD-MESSAGE TO WS-MESSAGE-NUMBER1               00022810
               MOVE WS-ERR-FIELD    TO WS-ERR-FIELD1
           ELSE                                                         00022820
           IF WS-MESSAGE-NUMBER2 = SPACES OR LOW-VALUES                 00022830
               MOVE WS-HOLD-MESSAGE TO WS-MESSAGE-NUMBER2               00022840
               MOVE WS-ERR-FIELD    TO WS-ERR-FIELD2
           ELSE                                                         00022850
           IF WS-MESSAGE-NUMBER3 = SPACES OR LOW-VALUES                 00022860
               MOVE WS-HOLD-MESSAGE TO WS-MESSAGE-NUMBER3               00022870
               MOVE WS-ERR-FIELD    TO WS-ERR-FIELD3
           ELSE                                                         00022880
           IF WS-MESSAGE-NUMBER4 = SPACES OR LOW-VALUES                 00022890
               MOVE WS-HOLD-MESSAGE TO WS-MESSAGE-NUMBER4               00022900
               MOVE WS-ERR-FIELD    TO WS-ERR-FIELD4.

       160-EXIT.
           EXIT.
                                                                        00022910
       200-ADD-DB2-TABLE.
           DISPLAY 'WS-EDIT-SW:' WS-EDIT-SW
           DISPLAY 'COM-ENTITY-LITERAL:' COM-ENTITY-LITERAL

           MOVE 'N'    TO WS-SQL-ERROR.

           IF WS-EDIT-SW NOT = 'Y'                                      00021640
              DISPLAY 'DB2 ADD START'
              IF COM-ENTITY-LITERAL = 'AM' OR 'CAM'                     00021650
                  OR 'LB' OR 'LIST BILL'                                00021660
                  OR 'BA' OR 'BILL ADDR'                                00021670
                  OR 'LD' OR 'LEAD'                                     00021680
                  DISPLAY 'DB2 ADD START CASENAME'
                  PERFORM 400-ADD-CASENAME-ROW  THRU 400-EXIT           00021690
              ELSE                                                      00021700
                 IF COM-ENTITY-LITERAL = 'BR' OR 'ML' OR 'RP' OR        00021710
                                         'BROKER' OR 'MAIL' OR 'NOI'    00021720
                    DISPLAY 'DB2 ADD START AGNTNAME'
                    PERFORM 300-ADD-AGNTNAME-ROW  THRU 300-EXIT         00021730
                 END-IF
              END-IF
           END-IF.
                                                                        00021740
       200-EXIT.                                                        00013800
           EXIT.

       300-ADD-AGNTNAME-ROW.                                            00024480
                                                                        00024490
           DISPLAY '300-ADD-AGNTNAME-ROW:'
           MOVE ZEROS TO WS-TABLE-RETRY-CNT
           EXEC SQL
               LOCK TABLE NEXT_IDNTITY IN EXCLUSIVE MODE
           END-EXEC
           IF SQLCODE = (-904 OR -911 OR -913) AND
               WS-TABLE-RETRY-CNT < 10
              ADD 1 TO WS-TABLE-RETRY-CNT
              GO TO 300-ADD-AGNTNAME-ROW
           END-IF
           DISPLAY 'SQLCODE' SQLCODE
           IF SQLCODE NOT = ZERO
              MOVE 'Y'           TO WS-SQL-ERROR
              MOVE 'IDNTITY TABLE LOCK ERROR' TO WS-SQL-ERROR-MSG
              MOVE 'DB007'       TO WS-MESSAGE-NUMBER1
              MOVE 'MAP-CIM'     TO WS-ERR-FIELD1
              MOVE SQLCODE       TO WS-SQLCODE
              GO TO 300-EXIT
           END-IF
           EXEC SQL
               SELECT NEXT_IDNTITY_NUM
                 INTO :HOLD-NEXT-ID-R
               FROM NEXT_IDNTITY
           END-EXEC
           IF SQLCODE NOT = ZERO
              MOVE 'Y'           TO WS-SQL-ERROR
              MOVE 'IDNTITY TABLE SELECT ERROR' TO WS-SQL-ERROR-MSG
              MOVE 'DBERR'       TO WS-MESSAGE-NUMBER1
              MOVE SPACES        TO WS-ERR-FIELD1
              MOVE SQLCODE       TO WS-SQLCODE
              GO TO 300-EXIT
           END-IF
           ADD 1 TO HOLD-NEXT-ID
           MOVE HOLD-NEXT-ID-R    TO WS-IDNTITY-NUM
                                                                        00024790
           PERFORM  500-MOVE-COMMAREA-TO-TABLE THRU 500-EXIT.           00026160
                                                                        00026170
      *---------------------------------------------------------------* 00026180
      *    NOTE YOU MAY HAVE TO ADD OTHER ENTITY TYPES TO THE FOLLOWIN* 00026190
      *    IF STATEMENT. THIS ALLOWS THE PROGRAM TO TRANFER TO THE    * 00026200
      *    BROKER ADD PROGRAM AND IF THE BROKER ADD IS CANCELLED THE  * 00026210
      *    TYPE REMAINS AS A MAIL AND IS NOT UPDATED IN THE SYSTEM.   * 00026220
      *    FUTURE TYPES THAT MAY NEED TO BE ADDED ARE CAMS, CASES AND * 00026230
      *    MAGICS.                                                    * 00026240
      *---------------------------------------------------------------* 00026250
                                                                        00026260
           IF COM-ENTITY-TYPE = 'BR'                                    00026270
               MOVE 'ML'  TO ENTITY-TYPE.                               00026280
           IF COM-ENTITY-TYPE = 'RP'                                    00026290
               MOVE 'RP'  TO ENTITY-TYPE.                               00026300
           MOVE WS-IDNTITY-NUM TO COM-IDNTITY.                          00026310
           MOVE WS-IDNTITY-NUM TO IDNTITY.                              00026320
           MOVE IDNTITY           TO PASS-WORD.                         00026330

      **ADD ENTRY TO IDNTITY_INFO TABLE                                 00026350
           MOVE 'AGNTNAME'           TO WS-IDNTITY-TYPE.
           PERFORM 425-ADD-IDNTITY-INFO    THRU 425-EXIT.
           IF SQLCODE NOT = ZERO                                        00026810
              GO TO 300-EXIT                                            00026840
           END-IF                                                       00026850

      *---------------------------------------------------------------* 00026360
      *                                                               * 00026370
      *    WHEN EXECUTING THE ADD FUNCTION CHANGE THE ENTITY TYPE     * 00026380
      *    TO MAIL. THEN WHEN THE SYSTEM SUCH AS BROKER COMPLETES     * 00026390
      *    ITS PORTION OF THE ADD THE 2BYTES ENTITY TYPE WILL BE      * 00026400
      *    UPDATED TO REFLECT THE PARTICULAR ADD THAT WAS EXECUTED    * 00026410
      *    (BROKER, CASE, MAGIC). IF THE CUSTOMER DOES NOT COMPLETE   * 00026420
      *    THE ADD THE MAILING TYPE REMAINS.                          * 00026430
      *---------------------------------------------------------------* 00026440
                                                                        00026450
           EXEC SQL                                                     00026460
              INSERT INTO  AGNTNAME                                     00026470
                (IDNTITY, LAST_NAME, FIRST_NAME, MIDDLE_NAME,           00026480
                 PREFIX, SUFFIX1, SUFFIX2,                              00026490
                 COMPANY_IND, COMPANY_IN_ADDRESS,                       00026500
                 COMPANY_NAME, DISPLAY_NAME, NICKNAME,                  00026510
                 ADDRESS1, ADDRESS2,                                    00026520
                 CITY, STATE, ZIP, ZIP_PLUS4, COUNTY_CODE, AREA_CODE,   00026530
                 PHONE,  PHONE_EXTENSION,  SSN, SEX, BIRTH_DATE,        00026540
                 FINALST_REAS_CODE, FINALST_OVRD_IND, DUP_ADDR_OVRD_IND,00026550
                 EFFECTIVE_DATE, CHANGE_DATE, CHANGE_LOGON, ENTITY_TYPE,00026560
                 RECORD_STATUS, ALT_ADDRESS_IND, FUTURE_ADDRESS_IND,    00026570
                 RECORD_ORIGIN, COMBINED_STATUS, SITE_CODE, NAME_KEY1,  00026580
                 NAME_KEY2, NAME_KEY3, ADDRESS_KEY1,                    00026590
                 ASSOCIATION1, ASSOCIATION2, ASSOCIATION3,              00026600
                 FAX_AREA_CODE, FAX_PHONE, PASSWORD)                    00026610
           VALUES (:IDNTITY, :LAST-NAME, :FIRST-NAME, :MIDDLE-NAME,     00026620
             :PREFIX, :SUFFIX1, :SUFFIX2,                               00026630
             :COMPANY-IND, :COMPANY-IN-ADDRESS,                         00026640
             :COMPANY-NAME, :DISPLAY-NAME, :NICKNAME,                   00026650
             :ADDRESS1, :ADDRESS2,                                      00026660
             :CITY, :STATE, :ZIP, :ZIP-PLUS4, :COUNTY-CODE, :AREA-CODE, 00026670
             :PHONE, :PHONE-EXTENSION, :SSN-NUM, :SEX,                  00026680
             :BIRTH-DATE :IND-BIRTH-DATE,                               00026690
             :FINALST-REAS-CODE, :FINALST-OVRD-IND, :DUP-ADDR-OVRD-IND, 00026700
             :EFFECTIVE-DATE, CURRENT TIMESTAMP,                        00026710
             :CHANGE-LOGON, :ENTITY-TYPE,                               00026720
             :RECORD-STATUS, :ALT-ADDRESS-IND, :FUTURE-ADDRESS-IND,     00026730
             :RECORD-ORIGIN, :COMBINED-STATUS, :SITE-CODE, :NAME-KEY1,  00026740
             :NAME-KEY2, :NAME-KEY3, :ADDRESS-KEY1,                     00026750
             :ASSOCIATION1, :ASSOCIATION2, :ASSOCIATION3,               00026760
             :FAX-AREA-CODE, :FAX-PHONE,                                00026770
             :PASS-WORD)                                                00026780
           END-EXEC.                                                    00026790
           DISPLAY 'SQLCODE:' SQLCODE
           IF SQLCODE NOT = ZERO                                        00026810
              MOVE 'Y'           TO WS-SQL-ERROR
              MOVE 'AGNT TABLE INSERT ERROR' TO WS-SQL-ERROR-MSG
              MOVE 'DBERR'       TO WS-MESSAGE-NUMBER1                  00024610
              MOVE SPACES        TO WS-ERR-FIELD1
              MOVE SQLCODE       TO WS-SQLCODE                          00024630
              GO TO 300-EXIT                                            00026840
           ELSE                                                         00026850
              MOVE 'NA153'           TO WS-MESSAGE-NUMBER1.             00026860

           PERFORM 450-ADD-IDNTITY-HISTORY THRU 450-EXIT.
                                                                        00026870
           IF SQLCODE = 0
               EXEC SQL                                                 00026890
                   UPDATE NEXT_IDNTITY                                  00026900
                      SET NEXT_IDNTITY_NUM = :HOLD-NEXT-ID-R            00026910
               END-EXEC                                                 00026920
               IF SQLCODE NOT = 0                                       00026930
                 MOVE 'Y'           TO WS-SQL-ERROR
                 MOVE 'IDENTITY TABLE UPDATE ERROR' TO  WS-SQL-ERROR-MSG
                 MOVE 'DBERR'       TO WS-MESSAGE-NUMBER1               00024610
                 MOVE SPACES        TO WS-ERR-FIELD1
                 MOVE SQLCODE       TO WS-SQLCODE                       00024630
                 GO TO 300-EXIT                                         00026840
               END-IF
           END-IF
                                                                        00026980
           DISPLAY 'END OD INSERT'
           DISPLAY 'IDNTITY:' IDNTITY.

       300-EXIT.                                                        00027590
           EXIT.
                                                                        00027600
       400-ADD-CASENAME-ROW.                                            00027610

           DISPLAY '400-ADD-CASENAME-ROW:'
                                                                        00027620
           EXEC SQL                                                     00027630
               LOCK TABLE NEXT_IDNTITY IN EXCLUSIVE MODE                00027640
           END-EXEC.                                                    00027650
           IF SQLCODE = (-904 OR -911 OR -913) AND                      00027660
               WS-TABLE-RETRY-CNT < 10                                  00027670
              ADD 1 TO WS-TABLE-RETRY-CNT                               00027680
              GO TO 400-ADD-CASENAME-ROW                                00027690
           END-IF.                                                      00027700
           IF SQLCODE NOT = ZERO                                        00027710
              MOVE 'Y'           TO WS-SQL-ERROR
              MOVE 'IDNTITY TABLE LOCK ERROR' TO WS-SQL-ERROR-MSG
              MOVE 'MAP-CIM'     TO WS-ERR-FIELD1                       00024620
              MOVE SQLCODE       TO WS-SQLCODE                          00024630
              MOVE 'DB008'       TO WS-MESSAGE-NUMBER1                  00027720
              GO TO 400-EXIT
           END-IF.                                                      00027780
           EXEC SQL                                                     00027790
               SELECT NEXT_IDNTITY_NUM                                  00027800
                 INTO :HOLD-NEXT-ID-R                                   00027810
               FROM NEXT_IDNTITY                                        00027820
           END-EXEC.                                                    00027830
           IF SQLCODE NOT = ZERO
              MOVE 'Y'           TO WS-SQL-ERROR
              MOVE 'IDNTITY TABLE SELECT ERROR' TO WS-SQL-ERROR-MSG
              MOVE 'DBERR'       TO WS-MESSAGE-NUMBER1
              MOVE SPACES        TO WS-ERR-FIELD1
              MOVE SQLCODE       TO WS-SQLCODE
              GO TO 400-EXIT
           END-IF
           ADD 1 TO HOLD-NEXT-ID.                                       00027840
           MOVE HOLD-NEXT-ID-R    TO WS-IDNTITY-NUM.                    00027850
                                                                        00028610
           PERFORM 500-MOVE-COMMAREA-TO-TABLE THRU 500-EXIT.            00028620
                                                                        00028630
      *---------------------------------------------------------------* 00028640
      *    NOTE YOU MAY HAVE TO ADD OTHER ENTITY TYPES TO THE FOLLOWIN* 00028650
      *    IF STATEMENT. THIS ALLOWS THE PROGRAM TO TRANFER TO THE    * 00028660
      *    BROKER ADD PROGRAM AND IF THE BROKER ADD IS CANCELLED THE  * 00028670
      *    TYPE REMAINS AS A MAIL AND IS NOT UPDATED IN THE SYSTEM.   * 00028680
      *    FUTURE TYPES THAT MAY NEED TO BE ADDED ARE CAMS, CASES AND * 00028690
      *    MAGICS.                                                    * 00028700
      *---------------------------------------------------------------* 00028710
                                                                        00028720
           IF COM-ENTITY-TYPE = 'BR'                                    00028730
               MOVE 'ML'  TO ENTITY-TYPE.                               00028740
           MOVE WS-IDNTITY-NUM TO COM-IDNTITY.                          00028750
           MOVE WS-IDNTITY-NUM TO IDNTITY.                              00028760
           MOVE IDNTITY        TO PASS-WORD.                            00028770

           IF COM-ENTITY-TYPE NOT = 'BA'                                00028790
               MOVE 'W'           TO RECORD-STATUS.                     00028800
                                                                        00028810
           IF COM-ENTITY-TYPE = 'LD'                                    00028820
               MOVE 'A'           TO RECORD-STATUS.                     00028830
                                                                        00028840
      **ADD ENTRY TO IDNTITY_INFO TABLE                                 00026350
           MOVE 'CASENAME'           TO WS-IDNTITY-TYPE.
           PERFORM 425-ADD-IDNTITY-INFO    THRU 425-EXIT.
           IF SQLCODE NOT = ZERO                                        00026810
              GO TO 400-EXIT                                            00026840
           END-IF                                                       00026850

      *---------------------------------------------------------------* 00028850
      *                                                               * 00028860
      *    WHEN EXECUTING THE ADD FUNCTION CHANGE THE ENTITY TYPE     * 00028870
      *    TO MAIL. THEN WHEN THE SYSTEM SUCH AS BROKER COMPLETES     * 00028880
      *    ITS PORTION OF THE ADD THE 2BYTES ENTITY TYPE WILL BE      * 00028890
      *    UPDATED TO REFLECT THE PARTICULAR ADD THAT WAS EXECUTED    * 00028900
      *    (BROKER, CASE, MAGIC). IF THE CUSTOMER DOES NOT COMPLETE   * 00028910
      *    THE ADD THE MAILING TYPE REMAINS.                          * 00028920
      *---------------------------------------------------------------* 00028930
                                                                        00028940
           EXEC SQL                                                     00028950
              INSERT INTO  CASENAME                                     00028960
                (IDNTITY, LAST_NAME, FIRST_NAME, MIDDLE_NAME,           00028970
                 PREFIX, SUFFIX1, SUFFIX2,                              00028980
                 COMPANY_IND, COMPANY_IN_ADDRESS,                       00028990
                 COMPANY_NAME, DISPLAY_NAME, NICKNAME,                  00029000
                 ADDRESS1, ADDRESS2,                                    00029010
                 CITY, STATE, ZIP, ZIP_PLUS4, COUNTY_CODE, AREA_CODE,   00029020
                 PHONE,  PHONE_EXTENSION,  SSN, SEX, BIRTH_DATE,        00029030
                 FINALST_REAS_CODE, FINALST_OVRD_IND, DUP_ADDR_OVRD_IND,00029040
                 EFFECTIVE_DATE, CHANGE_DATE, CHANGE_LOGON, ENTITY_TYPE,00029050
                 RECORD_STATUS, ALT_ADDRESS_IND, FUTURE_ADDRESS_IND,    00029060
                 RECORD_ORIGIN, COMBINED_STATUS, SITE_CODE, NAME_KEY1,  00029070
                 NAME_KEY2, NAME_KEY3, ADDRESS_KEY1,                    00029080
                 ASSOCIATION1, ASSOCIATION2, ASSOCIATION3,              00029090
                 FAX_AREA_CODE, FAX_PHONE, EMAIL, PASSWORD,             00029100
                 COUNTRY_CODE, POSTAL_CODE,                             00029110
                 ORIGINAL_STATE, ORIGINAL_ZIP, ORIGINAL_ZIP_PLUS4,
                 EMAIL_STATUS)
      *          FAX_AREA_CODE, FAX_PHONE, PASSWORD)                    00029120
      *          FAX_AREA_CODE, FAX_PHONE)                              00029130
           VALUES (:IDNTITY, :LAST-NAME, :FIRST-NAME, :MIDDLE-NAME,     00029140
             :PREFIX, :SUFFIX1, :SUFFIX2,                               00029150
             :COMPANY-IND, :COMPANY-IN-ADDRESS,                         00029160
             :COMPANY-NAME, :DISPLAY-NAME, :NICKNAME,                   00029170
             :ADDRESS1, :ADDRESS2,                                      00029180
             :CITY, :STATE, :ZIP, :ZIP-PLUS4, :COUNTY-CODE, :AREA-CODE, 00029190
             :PHONE, :PHONE-EXTENSION, :SSN-NUM, :SEX,                  00029200
             :BIRTH-DATE :IND-BIRTH-DATE,                               00029210
             :FINALST-REAS-CODE, :FINALST-OVRD-IND, :DUP-ADDR-OVRD-IND, 00029220
             :EFFECTIVE-DATE, CURRENT TIMESTAMP,                        00029230
             :CHANGE-LOGON, :ENTITY-TYPE,                               00029240
             :RECORD-STATUS, :ALT-ADDRESS-IND, :FUTURE-ADDRESS-IND,     00029250
             :RECORD-ORIGIN, :COMBINED-STATUS, :SITE-CODE, :NAME-KEY1,  00029260
             :NAME-KEY2, :NAME-KEY3, :ADDRESS-KEY1,                     00029270
             :ASSOCIATION1, :ASSOCIATION2, :ASSOCIATION3,               00029280
             :FAX-AREA-CODE, :FAX-PHONE, :EMAIL1, :PASS-WORD,           00029290
             :COUNTRY-CODE, :POSTAL-CODE,                               00029300
             :WS-SPACES-2, :WS-SPACES-5, :WS-SPACES-4,
             :WS-SPACES-1)
      *      :FAX-AREA-CODE, :FAX-PHONE, :PASS-WORD)                    00029310
      *      :FAX-AREA-CODE, :FAX-PHONE)                                00029320
           END-EXEC.                                                    00029330
                                                                        00029340
           IF SQLCODE NOT = ZERO                                        00029350
                 MOVE 'Y'           TO WS-SQL-ERROR
                 MOVE 'CASE TABLE INSERT ERROR'  TO WS-SQL-ERROR-MSG
                 MOVE 'DBERR'       TO WS-MESSAGE-NUMBER1
                 MOVE SPACES        TO WS-ERR-FIELD1
                 MOVE SQLCODE       TO WS-SQLCODE
                 GO TO 400-EXIT
           ELSE                                                         00029390
              MOVE 'NA153'           TO WS-MESSAGE-NUMBER1.             00029400
                                                                        00029410
           PERFORM 450-ADD-IDNTITY-HISTORY THRU 450-EXIT.
                                                                        00026870
           IF SQLCODE = 0
              EXEC SQL                                                     00029
                  UPDATE NEXT_IDNTITY                                      00029
                     SET NEXT_IDNTITY_NUM = :HOLD-NEXT-ID-R                00029
              END-EXEC                                                     00029
              IF SQLCODE NOT = 0                                           00029
                MOVE 'Y'           TO WS-SQL-ERROR
                MOVE 'IDENTITY TABLE UPDATE ERROR' TO  WS-SQL-ERROR-MSG
                MOVE 'DBERR'       TO WS-MESSAGE-NUMBER1
                MOVE SPACES        TO WS-ERR-FIELD1
                MOVE SQLCODE       TO WS-SQLCODE
                GO TO 400-EXIT
              END-IF
           END-IF.
                                                                        00029480
           DISPLAY 'END OD INSERT'
           DISPLAY 'IDNTITY:' IDNTITY.
                                                                        00029480
       400-EXIT.                                                        00029720
           EXIT.

       425-ADD-IDNTITY-INFO.                                            00027610

           DISPLAY '425-ADD-IDNTITY-INFO'.
           DISPLAY 'EFFECTIVE DATE:' EFFECTIVE-DATE.
                                                                        00027620
           EXEC SQL                                                     00028950
              INSERT INTO  IDNTITY_INFO                                 00028960
                (IDNTITY,                                               00028970
                 IDNTITY_TYPE,                                          00028970
                 ADDRESS1,                                              00028970
                 ADDRESS2,                                              00028970
                 CITY,                                                  00028970
                 STATE,                                                 00028970
                 ZIP,                                                   00028970
                 ZIP_PLUS4,                                             00028970
                 EFFECTIVE_DATE,                                        00028970
                 CHANGE_DATE,                                           00028970
                 ENTITY_TYPE,                                           00028970
                 RECORD_STATUS)                                         00029140
           VALUES (:IDNTITY, :WS-IDNTITY-TYPE,                          00026620
             :ADDRESS1, :ADDRESS2,                                      00026660
             :CITY, :STATE, :ZIP, :ZIP-PLUS4,                           00026670
             :EFFECTIVE-DATE, CURRENT TIMESTAMP,                        00026710
             :ENTITY-TYPE,                                              00026720
             :RECORD-STATUS)                                            00026730
           END-EXEC.                                                    00029330
                                                                        00029340
           IF SQLCODE NOT = ZERO                                        00029350
                 DISPLAy 'SQLCODE:' SQLCODE
                 MOVE 'Y'           TO WS-SQL-ERROR
                 MOVE 'IDNTITY INFO INSERT ERROR'  TO WS-SQL-ERROR-MSG
                 MOVE 'DBERR'       TO WS-MESSAGE-NUMBER1
                 MOVE SPACES        TO WS-ERR-FIELD1
                 MOVE SQLCODE       TO WS-SQLCODE
           END-IF.                                                      00029410
                                                                        00029480
       425-EXIT.                                                        00029720
           EXIT.

       450-ADD-IDNTITY-HISTORY.                                         00027610

           DISPLAY '450-ADD-IDNTITY-HISTORY'.
                                                                        00027620
           EXEC SQL                                                     00028950
              INSERT INTO  IDNTITY_HISTORY                              00028960
                (IDNTITY, TRANSACTION, CHANGE_DATE, MONGODB_SYNC)       00028970
           VALUES (:IDNTITY, :WS-TRAN, CURRENT TIMESTAMP, :WS-N)        00029140
           END-EXEC.                                                    00029330
                                                                        00029340
           IF SQLCODE NOT = ZERO                                        00029350
                 MOVE 'Y'           TO WS-SQL-ERROR
                 MOVE 'IDNTITY HIST INSERT ERROR'  TO WS-SQL-ERROR-MSG
                 MOVE 'DBERR'       TO WS-MESSAGE-NUMBER1
                 MOVE SPACES        TO WS-ERR-FIELD1
                 MOVE SQLCODE       TO WS-SQLCODE
           END-IF.                                                      00029410
                                                                        00029480
       450-EXIT.                                                        00029720
           EXIT.

       500-MOVE-COMMAREA-TO-TABLE.                                      00029720
                                                                        00029730
           DISPLAY '500-MOVE-COMMAREA-TO-TABLE:'.
           MOVE COM-LAST-NAME          TO LAST-NAME.                    00029740
           MOVE COM-FIRST-NAME         TO FIRST-NAME.                   00029750
           MOVE COM-MIDDLE-NAME        TO MIDDLE-NAME.                  00029760
           MOVE COM-PREFIX             TO PREFIX.                       00029770
           MOVE COM-SUFFIX1            TO SUFFIX1.                      00029780
           MOVE COM-SUFFIX2            TO SUFFIX2.                      00029790
           MOVE COM-COMPANY-IND        TO COMPANY-IND.                  00029800
           MOVE COM-COMPANY-IN-ADDRESS TO COMPANY-IN-ADDRESS.           00029810
           MOVE COM-COMPANY-NAME       TO COMPANY-NAME.                 00029820
           MOVE COM-DISPLAY-NAME       TO DISPLAY-NAME.                 00029830
           MOVE COM-NICKNAME           TO NICKNAME.                     00029840
           MOVE COM-ADDRESS1           TO ADDRESS1.                     00029850
           MOVE COM-ADDRESS2           TO ADDRESS2.                     00029860
           MOVE COM-CITY               TO CITY.                         00029870
           MOVE COM-STATE              TO STATE.                        00029880
           MOVE COM-ZIP                TO ZIP.                          00029890
           MOVE COM-ZIP-PLUS4          TO ZIP-PLUS4.                    00029900
           MOVE COM-COUNTY-CODE        TO COUNTY-CODE.                  00029910
           MOVE COM-AREA-CODE          TO AREA-CODE.                    00029920
           MOVE COM-PHONE              TO PHONE.                        00029930
           MOVE COM-PHONE-EXTENSION    TO PHONE-EXTENSION.              00029940
           MOVE COM-FAX-AREA-CODE      TO FAX-AREA-CODE.                00029950
           MOVE COM-FAX-PHONE          TO FAX-PHONE.                    00029960
           MOVE COM-SSN                TO SSN-NUM.                      00029970
           MOVE COM-SEX                TO SEX.                          00029980
           MOVE COM-FINALST-REAS-CODE  TO FINALST-REAS-CODE.            00029990
           MOVE COM-FINALST-OVRD-IND   TO FINALST-OVRD-IND.             00030000
           MOVE COM-DUP-ADDR-OVRD-IND  TO DUP-ADDR-OVRD-IND.            00030010
           MOVE COM-CHANGE-LOGON       TO CHANGE-LOGON.                 00030020
           MOVE COM-ENTITY-TYPE        TO ENTITY-TYPE.                  00030030
           MOVE COM-RECORD-STATUS      TO RECORD-STATUS.                00030040
           MOVE COM-ALT-ADDRESS-IND    TO ALT-ADDRESS-IND.              00030050
           MOVE COM-FUTURE-ADDRESS-IND TO FUTURE-ADDRESS-IND.           00030060
           MOVE COM-RECORD-ORIGIN      TO RECORD-ORIGIN.                00030070
           MOVE COM-COMBINED-STATUS    TO COMBINED-STATUS.              00030080
           MOVE COM-SITE-CODE          TO SITE-CODE.                    00030090
           MOVE COM-NAME-KEY1          TO NAME-KEY1.                    00030100
           MOVE COM-NAME-KEY2          TO NAME-KEY2.                    00030110
           MOVE COM-NAME-KEY3          TO NAME-KEY3.                    00030120
           MOVE COM-ADDRESS-KEY1       TO ADDRESS-KEY1.                 00030130
           MOVE COM-ASSOCIATION1       TO ASSOCIATION1.                 00030140
           MOVE COM-ASSOCIATION2       TO ASSOCIATION2.                 00030150
           MOVE COM-ASSOCIATION3       TO ASSOCIATION3.                 00030160
           MOVE COM-EMAIL1             TO EMAIL1.                       00030170
           MOVE COM-POSTAL-CODE        TO POSTAL-CODE.                  00030180
           MOVE COM-COUNTRY-CODE       TO COUNTRY-CODE.                 00030190
           DISPLAY 'COM-EFFECTIVE-DATE2:' COM-EFFECTIVE-DATE
           MOVE COM-EFFECTIVE-DATE     TO WS-DATE-R.                    00030200
           MOVE WS-DATE-R              TO WS-DATE.                      00030210

           MOVE '20'                   TO EF-CC.                        00030360
           MOVE WS-YY-A                TO EF-YY.                        00030400
           MOVE WS-MM-A                TO EF-MM.                        00030410
           MOVE WS-DD-A                TO EF-DD.                        00030420
           MOVE WS-EFFECTIVE-DATE      TO EFFECTIVE-DATE.               00030430
           DISPLAY 'EFFECTIVE-DATE:' EFFECTIVE-DATE
                                                                        00030440
           MOVE -1 TO IND-BIRTH-DATE.                                   00030660
                                                                        00030670
       500-EXIT.
           EXIT.
