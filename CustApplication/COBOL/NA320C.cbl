   CBL DATA(24)                                                         00000010
       IDENTIFICATION DIVISION.                                         00000020
       PROGRAM-ID. NA320C.                                              00000030
       DATE-WRITTEN. 01/15/20.                                          00000360
      *REMARKS. NA320B - SUBMODULE CALLED FROM NA320A FOR DB2 PROCESSING00000370
      *------------------------PROGRAM PURPOSE-------------------------*00000400
      *                                                                *00000410
      *  PROGRAM TITLE: NA320B                                         *00000420
      *  PROGRAM TEXT:  INQUIRE MODULE                                 *00000430
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
           05  WS-HOLD-MESSAGE              PIC X(5)   VALUE SPACE.     00003470
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
MB1     01 CASE-FETCH-SW                PIC X.
MB1     01 ORIGINAL-STATE               PIC XX.
      * 01 WS-RECORD-NOTFND             PIC X.
      *    COPY AGNTNCW3.                                               00006080
           COPY AGNTNV05.                                               00006090
      *    COPX AGNTNAME is not the base copybook, AGNTNV05 is.         00006100
           COPY CASNVW3.                                                00006110
      *    COPY NEXTCIM.                                                00006120
                                                                        00006130
           EXEC SQL                                                     00006140
              INCLUDE SQLCA                                             00006150
           END-EXEC.                                                    00006160
                                                                        00006180
           EXEC SQL DECLARE AGNTCUR CURSOR FOR
11735A*        SELECT *
11735A         SELECT
                    IDNTITY,
                    LAST_NAME,
                    FIRST_NAME,
                    MIDDLE_NAME,
                    PREFIX,
                    SUFFIX1,
                    SUFFIX2,
                    COMPANY_IND,
                    COMPANY_IN_ADDRESS,
                    COMPANY_NAME,
                    DISPLAY_NAME,
                    NICKNAME,
                    ADDRESS1,
                    ADDRESS2,
                    CITY,
                    STATE,
289843              ZIP,
                    ZIP_PLUS4,
                    COUNTY_CODE,
                    AREA_CODE,
                    PHONE,
                    PHONE_EXTENSION,
                    SSN,
                    SEX,
                    BIRTH_DATE,
                    FINALST_REAS_CODE,
                    FINALST_OVRD_IND,
                    DUP_ADDR_OVRD_IND,
                    EFFECTIVE_DATE,
                    CHANGE_DATE,
                    CHANGE_LOGON,
                    ENTITY_TYPE,
                    RECORD_STATUS,
                    ALT_ADDRESS_IND,
                    FUTURE_ADDRESS_IND,
                    RECORD_ORIGIN,
                    COMBINED_STATUS,
                    SITE_CODE,
                    NAME_KEY1,
                    NAME_KEY2,
                    NAME_KEY3,
                    ADDRESS_KEY1,
                    ASSOCIATION1,
                    ASSOCIATION2,
                    ASSOCIATION3
900837*        FROM AGNTNV03
R00700         FROM AGNTNAME
               WHERE IDNTITY = :WS-CIM-NUMBER
           END-EXEC.

KPL        EXEC SQL DECLARE CASECUR CURSOR FOR
R00700*        SELECT *
900837*        FROM CASENV03
R00700         SELECT
R00700             IDNTITY, LAST_NAME, FIRST_NAME, MIDDLE_NAME,
R00700             PREFIX, SUFFIX1, SUFFIX2, COMPANY_IND,
R00700             COMPANY_IN_ADDRESS, COMPANY_NAME, DISPLAY_NAME,
R00700             NICKNAME, ADDRESS1, ADDRESS2, CITY, STATE, ZIP,
R00700             ZIP_PLUS4, COUNTY_CODE, AREA_CODE, PHONE,
R00700             PHONE_EXTENSION, SSN, SEX, BIRTH_DATE,
R00700             FINALST_REAS_CODE, FINALST_OVRD_IND,
R00700             DUP_ADDR_OVRD_IND, EFFECTIVE_DATE,
R00700             CHANGE_DATE, CHANGE_LOGON, ENTITY_TYPE,
R00700             RECORD_STATUS, ALT_ADDRESS_IND,
R00700             FUTURE_ADDRESS_IND, RECORD_ORIGIN,
R00700             COMBINED_STATUS, SITE_CODE, NAME_KEY1,
R00700             NAME_KEY2, NAME_KEY3, ADDRESS_KEY1,
R00700             ASSOCIATION1, ASSOCIATION2, ASSOCIATION3,
R00700***          FAX_AREA_CODE, FAX_PHONE, EMAIL,
R04023             FAX_AREA_CODE, FAX_PHONE, ORIGINAL_STATE, EMAIL,
R02539             COUNTRY_CODE, POSTAL_CODE
R00700         FROM CASENAME
               WHERE IDNTITY = :WS-CIM-NUMBER
           END-EXEC.

       LINKAGE SECTION.                                                 00007340

       01 DFHCOMMAREA.
           COPY INQCOMA.
                                                                        00006070
      ******************************************************************00007500
                                                                        00007510
       PROCEDURE DIVISION.                                              00007520

       0010-BEGIN-PROGRAM.                                              00007540
           DISPLAY 'NA320C 0010-BEGIN-PROGRAM'.
           DISPLAY 'EIBTRNID:' EIBTRNID.

           PERFORM 0250-PROCESS-DB2-REQUESTS THRU 250-EXIT.
MB1        MOVE DCLAGNTNAME TO LS-TABLE-DATA.
           MOVE COUNTRY-CODE TO LS-COUNTRY-CODE.
           MOVE POSTAL-CODE  TO LS-POSTAL-CODE.
           DISPLAY 'NA320C 0010-END-PROGRAM'.
           EXEC CICS RETURN END-EXEC.

       0010-EXIT.
           EXIT.
                                                                        00007990
       0250-PROCESS-DB2-REQUESTS.

           DISPLAY 'WS-CIM-NUMBER'         WS-CIM-NUMBER.
           EXEC SQL
               OPEN AGNTCUR
           END-EXEC.
           DISPLAY 'AGNT CUR OPEN CUR' SQLCODE.
           IF SQLCODE NOT = ZERO
            MOVE SQLCODE TO WS-SQLCODE
            MOVE 'AGNT CURSOR ERROR' TO WS-SQL-ERROR-MSG
            MOVE 'Y'  TO WS-SQL-ERROR
            GO TO 250-EXIT.

           EXEC SQL FETCH AGNTCUR
R01142         INTO :DCLAGNTNAME:AGNTNAME-INDICATORS
           END-EXEC.

           DISPLAY 'AGNT FETCH SQLCODE ' SQLCODE.

           IF SQLCODE = 100
               EXEC SQL OPEN CASECUR END-EXEC
               MOVE 'Y' TO CASE-FETCH-SW
KPL            EXEC SQL FETCH CASECUR
R00700*            INTO DCLAGNTNAME:AGNTNAME-INDICATORS
R00700             INTO  :IDNTITY, :LAST-NAME :IND-LAST-NAME,
C23311                   :FIRST-NAME :IND-FIRST-NAME,
C23311                   :MIDDLE-NAME :IND-MIDDLE-NAME,
R00700                   :PREFIX :IND-PREFIX, :SUFFIX1 :IND-SUFFIX1,
R00700                   :SUFFIX2 :IND-SUFFIX2,
R00700                   :COMPANY-IND, :COMPANY-IN-ADDRESS,
C23311                   :COMPANY-NAME :IND-COMPANY-NAME,
C23311                   :DISPLAY-NAME :IND-DISPLAY-NAME,
R00700                   :NICKNAME :IND-NICKNAME,
C23311                   :ADDRESS1, :ADDRESS2 :IND-ADDRESS2,
R00700                   :CITY, :STATE, :ZIP, :ZIP-PLUS4,
R00700                   :COUNTY-CODE, :AREA-CODE,
R00700                   :PHONE, :PHONE-EXTENSION, :SSN-NUM, :SEX,
R00700                   :BIRTH-DATE :IND-BIRTH-DATE,
R00700                   :FINALST-REAS-CODE, :FINALST-OVRD-IND,
R00700                   :DUP-ADDR-OVRD-IND,
R00700                   :EFFECTIVE-DATE, :CHANGE-DATE,
R00700                   :CHANGE-LOGON, :ENTITY-TYPE,
R00700                   :RECORD-STATUS, :ALT-ADDRESS-IND,
R00700                   :FUTURE-ADDRESS-IND,
R00700                   :RECORD-ORIGIN, :COMBINED-STATUS,
R00700                   :SITE-CODE, :NAME-KEY1,
R00700                   :NAME-KEY2, :NAME-KEY3, :ADDRESS-KEY1,
R00700                   :ASSOCIATION1 :IND-ASSOCIATION1,
R00700                   :ASSOCIATION2 :IND-ASSOCIATION2,
R00700                   :ASSOCIATION3 :IND-ASSOCIATION3,
R00700                   :FAX-AREA-CODE, :FAX-PHONE,
R04023                   :ORIGINAL-STATE,
R00700                   :EMAIL1 :IND-EMAIL1,
R02539                   :COUNTRY-CODE :IND-COUNTRY-CODE,
R02539                   :POSTAL-CODE :IND-POSTAL-CODE
               END-EXEC.
           DISPLAY 'CASE SQLCODE         ' SQLCODE
           DISPLAY 'WS-CIM-NUMBER'         WS-CIM-NUMBER
           IF  SQLCODE = 100
               MOVE 'Y'               TO WS-RECORD-NOTFND
             GO TO 250-EXIT
           ELSE
           IF  SQLCODE NOT = ZERO
           MOVE SQLCODE TO WS-SQLCODE
           MOVE 'CASE CURSOR ERROR' TO WS-SQL-ERROR-MSG
           MOVE 'Y'           TO WS-SQL-ERROR
           GO TO 250-EXIT
920403     END-IF.

           IF IND-LAST-NAME IS NEGATIVE
               MOVE SPACES TO LAST-NAME.
           IF IND-FIRST-NAME IS NEGATIVE
               MOVE SPACES TO FIRST-NAME.
           IF IND-MIDDLE-NAME IS NEGATIVE
               MOVE SPACES TO MIDDLE-NAME.
           IF IND-PREFIX IS NEGATIVE
               MOVE SPACES TO PREFIX.
           IF IND-SUFFIX1 IS NEGATIVE
               MOVE SPACES TO SUFFIX1.
           IF IND-SUFFIX2 IS NEGATIVE
               MOVE SPACES TO SUFFIX2.
           IF IND-COMPANY-NAME IS NEGATIVE
               MOVE SPACES TO COMPANY-NAME.
           IF IND-BIRTH-DATE IS NEGATIVE
               MOVE ZEROES TO BIRTH-DATE.
           IF IND-DISPLAY-NAME IS NEGATIVE
               MOVE SPACES TO DISPLAY-NAME.
           IF IND-NICKNAME IS NEGATIVE
               MOVE SPACES TO NICKNAME.
           IF IND-ADDRESS2 IS NEGATIVE
               MOVE SPACES TO ADDRESS2.
           IF IND-ASSOCIATION1 IS NEGATIVE
               MOVE SPACES TO ASSOCIATION1.
           IF IND-ASSOCIATION2 IS NEGATIVE
               MOVE SPACES TO ASSOCIATION2.
           IF IND-ASSOCIATION3 IS NEGATIVE
               MOVE SPACES TO ASSOCIATION3.
           IF IND-FAX-PHONE IS NEGATIVE
               MOVE SPACES TO FAX-PHONE.
           IF IND-EMAIL1 IS NEGATIVE
               MOVE SPACES TO EMAIL1.
R02539     IF IND-POSTAL-CODE IS NEGATIVE
R02539         MOVE SPACES TO POSTAL-CODE.
R02539     IF IND-COUNTRY-CODE IS NEGATIVE
R02539         MOVE SPACES TO COUNTRY-CODE.

           EXEC SQL
               CLOSE AGNTCUR
           END-EXEC.
           IF CASE-FETCH-SW = 'Y'
               EXEC SQL CLOSE CASECUR END-EXEC.
       250-EXIT.
           EXIT.
