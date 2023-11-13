      ******************************************************************
      * Licensed materials - Property of IBM                           *
      * 5724-T07(C) Copyright IBM Corp. 2018                           *
      * All rights reserved                                            *
      * US Government users restricted rights  -  Use, duplication or  *
      * disclosure restricted by GSA ADP schedule contract with IBM    *
      * Corp.                                                          *
      *                                                                *
      * IBM Developer for z/OS (IDz)                                   *
      * IBM z/OS Automated Unit Testing Framework (zUnit)              *
      * Enterprise COBOL zUnit Test Case Sample EPSCMORT.cbl           *
      *                                                                *
      * @since   14.1.5.0                                              *
      * @version 14.1.5.0                                              *
      ******************************************************************
       ID DIVISION.
       PROGRAM-ID. EPSCMORT.
      *    THIS DEMONSTRATES CICS           - EPSDEMOS 2018
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-FLEX-ES.
       OBJECT-COMPUTER. IBM-FLEX-ES.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
       01  W-FLAGS.
           10  W-SEND-FLAG                    PIC X.
               88  SEND-ERASE                   VALUE '1'.
               88  SEND-DATAONLY                VALUE '2'.
               88  SEND-MAPONLY                 VALUE '3'.
               88  SEND-DATAONLY-ALARM          VALUE '4'.
               88  SEND-ALL                     VALUE '5'.

       01 W-CONVERSIONS.
           05  W-PMT-CNVRT     PIC X(12).
           05  W-PMT-NUMBER
               REDEFINES W-PMT-CNVRT
                               PIC 9(10)V99.
           05  WS-FORMAT-NUMBER PIC Z,ZZZ,ZZ9.99.
           05  W-PRINC-CNVRT   PIC X(12).
           05  W-PRINC-NUMBER
               REDEFINES W-PRINC-CNVRT
                               PIC 9(10)V99.

       01 W-CALL-PROGRAM                      PIC X(8).
      *
       01 W-RETIREMENT-WA                     PIC 9(4).
       01 W-COMAREA-LENGTH                    PIC 9(4) COMP.

       01  WORK-VAR1.
          10 WORK-PRINCIPLE-DATA   PIC S9(9)V99 COMP.
          10 WORK-QUOTED-INTEREST-RATE
                                        PIC S9(2)v9(3) COMP.
       01 IBMREQD                           PIC X(1).
      *
       01  END-OF-TRANS-MSG                 PIC X(30)
             VALUE 'END OF TRANSACTION - THANK YOU'.
       01  COMPANY-NOT-FOUND-MSG            PIC X(30)
             VALUE 'NO COMPANY FOUND - THANK YOU  '.
       01  BLANK-MSG                        PIC X(1) VALUE ' '.
           COPY DFHAID.
           COPY EPSMORT.

       01  W-COMMUNICATION-AREA.
           COPY EPSMTCOM.

       COPY EPSNBRPM.

       LINKAGE SECTION.

       01 DFHCOMMAREA.
       COPY EPSMTCOM.

       PROCEDURE DIVISION USING DFHCOMMAREA.

       EPSCMORT-MAINLINE.
           IF EIBCALEN EQUAL ZERO THEN
             MOVE LENGTH OF W-COMMUNICATION-AREA to W-COMAREA-LENGTH
           ELSE
             MOVE LENGTH OF DFHCOMMAREA to W-COMAREA-LENGTH
             MOVE DFHCOMMAREA to W-COMMUNICATION-AREA
           END-IF.
           EVALUATE TRUE
               WHEN EIBCALEN = ZERO
      * First time in - Show Screen
                   MOVE LOW-VALUES TO EPMENUO
                   SET SEND-ERASE TO TRUE
                   PERFORM A300-SEND-MAP
                   MOVE '3' TO
                      PROCESS-INDICATOR OF W-COMMUNICATION-AREA
               WHEN EIBAID = DFHCLEAR
      * Process CLEAR key
                   MOVE LOW-VALUES TO EPMENUO
                   SET SEND-ERASE TO TRUE
                   PERFORM A300-SEND-MAP
                   MOVE '3' TO
                      PROCESS-INDICATOR OF W-COMMUNICATION-AREA
               WHEN EIBAID = DFHPF3 OR DFHPF12
      * Process END/RETURN keys
                   EXEC CICS
                      SEND TEXT FROM (END-OF-TRANS-MSG)
                      ERASE
                      FREEKB
                   END-EXEC
                   EXEC CICS
                        RETURN
                   END-EXEC
               WHEN EIBAID = DFHENTER
                   DISPLAY 'ENTER KEY'
      * Process ENTER Key
                   IF PROCESS-INDICATOR OF W-COMMUNICATION-AREA = '3'
                   DISPLAY 'A100-MAP'
                      PERFORM A100-PROCESS-MAP
                      MOVE '0' TO
                         PROCESS-INDICATOR OF W-COMMUNICATION-AREA
                   ELSE
                      MOVE LOW-VALUES TO EPMENUO
                      SET SEND-ERASE TO TRUE
                      PERFORM A300-SEND-MAP
                      MOVE '3' TO
                         PROCESS-INDICATOR OF W-COMMUNICATION-AREA
                   END-IF
               WHEN EIBAID = DFHPF9
      * Show not found message
                   EXEC CICS
                      SEND TEXT FROM (COMPANY-NOT-FOUND-MSG)
                      ERASE
                      FREEKB
                   END-EXEC
                   EXEC CICS
                        RETURN
                   END-EXEC
               WHEN OTHER
      * Process Data
                    IF PROCESS-INDICATOR OF W-COMMUNICATION-AREA = '3'
                      PERFORM A600-CALCULATE-MORTGAGE
                      EXEC CICS RETURN END-EXEC
                    END-IF
           END-EVALUATE
           EXEC CICS
               RETURN TRANSID('EPSP')
               COMMAREA(W-COMMUNICATION-AREA)
               LENGTH(W-COMAREA-LENGTH)
           END-EXEC.

       A100-PROCESS-MAP.
           PERFORM A400-RECEIVE-MAP.
           PERFORM A600-CALCULATE-MORTGAGE
           SET SEND-DATAONLY TO TRUE
           PERFORM A300-SEND-MAP
               .

       A300-SEND-MAP.
           EVALUATE TRUE
              WHEN SEND-MAPONLY
                   EXEC CICS
                     SEND MAP ('EPMENU')
                       MAPSET('EPSMORT')
                       MAPONLY
                       CURSOR
                   END-EXEC
              WHEN SEND-ERASE
                   EXEC CICS
                     SEND MAP ('EPMENU')
                         MAPSET('EPSMORT')
                         FROM(EPMENUO)
                         ERASE
                         CURSOR
                   END-EXEC
              WHEN SEND-DATAONLY
                   EXEC CICS
                     SEND MAP ('EPMENU')
                         MAPSET('EPSMORT')
                         FROM(EPMENUO)
                         DATAONLY
                         CURSOR
                   END-EXEC
              WHEN SEND-ALL
                   EXEC CICS
                     SEND MAP ('EPMENU')
                         MAPSET('EPSMORT')
                         FROM(EPMENUO)
                     END-EXEC.

       A400-RECEIVE-MAP.
           EXEC CICS
                RECEIVE MAP('EPMENU')
                   MAPSET('EPSMORT')
                   INTO (EPMENUI)
           END-EXEC.

           DISPLAY "EPLOANI:" EPLOANI
           DISPLAY "EPYEARSI:" EPYEARSI
           DISPLAY "EPRATEI:" EPRATEI

           MOVE EPLOANI        TO EPSPARM-VALIDATE-DATA.
           MOVE LENGTH OF EPLOANI
                               TO EPSPARM-MAX-LENGTH.
           CALL 'EPSNBRVL' USING EPS-NUMBER-VALIDATION.
           COMPUTE EPSPCOM-PRINCIPLE-DATA
                OF W-COMMUNICATION-AREA
                = EPSPARM-NUMBER + EPSPARM-DECIMAL.
           MOVE EPSPCOM-PRINCIPLE-DATA
                OF W-COMMUNICATION-AREA TO WORK-PRINCIPLE-DATA
           DISPLAY "EPSPCOM-PRINCIPLE-DATA:" WORK-PRINCIPLE-DATA

           MOVE EPYEARSI             TO EPSPARM-VALIDATE-DATA.
           MOVE LENGTH OF EPYEARSI   TO EPSPARM-MAX-LENGTH.
           CALL 'EPSNBRVL' USING EPS-NUMBER-VALIDATION.
           COMPUTE EPSPCOM-NUMBER-OF-YEARS
                OF W-COMMUNICATION-AREA
                = EPSPARM-NUMBER + EPSPARM-DECIMAL.

           MOVE EPRATEI              TO EPSPARM-VALIDATE-DATA.
           MOVE LENGTH OF EPRATEI    TO EPSPARM-MAX-LENGTH.
           CALL 'EPSNBRVL' USING EPS-NUMBER-VALIDATION.
           COMPUTE EPSPCOM-QUOTED-INTEREST-RATE
                OF W-COMMUNICATION-AREA
                = EPSPARM-NUMBER + EPSPARM-DECIMAL.
           MOVE EPSPCOM-QUOTED-INTEREST-RATE
                OF W-COMMUNICATION-AREA TO WORK-QUOTED-INTEREST-RATE
           DISPLAY "EPSPCOM-QUOTED-INTEREST-RATE:"
             WORK-QUOTED-INTEREST-RATE.

       A600-CALCULATE-MORTGAGE.
           DISPLAY 'CALC MORTGAE'.
           DISPLAY 'EPSPCOM-PRINCIPLE-DATA:' EPSPCOM-PRINCIPLE-DATA
                           OF W-COMMUNICATION-AREA.
           DISPLAY 'EPSPCOM-NUMBER-OF-YEARS:' EPSPCOM-NUMBER-OF-YEARS
                           OF W-COMMUNICATION-AREA.
           DISPLAY 'EPSPCOM-INT-RATE:' EPSPCOM-QUOTED-INTEREST-RATE
                           OF W-COMMUNICATION-AREA.
           MOVE 'Y' TO EPSPCOM-YEAR-MONTH-IND
                           OF W-COMMUNICATION-AREA.
           MOVE 'EPSCSMRT' TO W-CALL-PROGRAM
           EXEC CICS LINK PROGRAM( W-CALL-PROGRAM )
                          COMMAREA( W-COMMUNICATION-AREA )
           END-EXEC
           .
           DISPLAY 'EPSPCOM-RETURN-MONTH-PAYMENT:'
               EPSPCOM-RETURN-MONTH-PAYMENT oF W-COMMUNICATION-AREA.
           MOVE EPSPCOM-RETURN-MONTH-PAYMENT
                             OF W-COMMUNICATION-AREA
                             TO WS-FORMAT-NUMBER.

           MOVE WS-FORMAT-NUMBER
                             TO EPPAYMNTO.
           DISPLAY "EPPAYMNTO,WS-FORMAT-NUMBER:" EPPAYMNTO
           MOVE EPSPCOM-ERRMSG
                             OF W-COMMUNICATION-AREA
                             TO MSGERRO.
