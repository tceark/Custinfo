   CBL NUMPROC(MIG),FLAG(I,W),RENT
      ******************************************************************
      * Licensed materials - Property of IBM                           *
      * 5724-T07(C) Copyright IBM Corp. 2018                           *
      * All rights reserved                                            *
      * US Government users restricted rights  -  Use, duplication or  *
      * disclosure restricted by GSA ADP schedule contract with IBM    *
      * Corp.                                                          *
      *                                                                *
      * IBM Developer for z/OS (IDz)                              *
      * IBM z/OS Automated Unit Testing Framework (zUnit)              *
      * Enterprise COBOL zUnit Test Case Sample EPSCSMRT.cbl           *
      *                                                                *
      * @since   14.1.5.0                                              *
      * @version 14.1.5.0                                              *
      ******************************************************************
       ID DIVISION.
       PROGRAM-ID. EPSCSMRT.
      *    THIS IS A CALLED PROGRAM EXAMPLE FOR DEMONSTRATION
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. FLEX-ES.
       OBJECT-COMPUTER. FLEX-ES.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
       01  WS-CALLED-PROGRAM    PIC X(8).

       01  STATIC-CALLED-PROGRAMS.
           03 STATIC-CALLED-PROGRAM-TABLE.
              05 FILLER            PIC X(8) VALUE 'EPSMPMT'.
              05 FILLER            PIC X(8) VALUE 'NOT VLD'.
              05 FILLER            PIC X(8) VALUE ' '.
           03 CALLED-PROGRAM-TABLE
                        REDEFINES STATIC-CALLED-PROGRAM-TABLE
                        OCCURS 3 TIMES.
              05 CALLED-PROGRAM-NAME PIC X(8).

       COPY EPSPDATA.

       LINKAGE SECTION.
      *
       01 DFHCOMMAREA.
       COPY EPSMTCOM.

       PROCEDURE DIVISION USING DFHCOMMAREA.
      *
       A000-MAINLINE.
           DISPLAY 'IN EPSCSMRT'
           DISPLAY 'EPSPCOM-PRINCIPLE-DATA:' EPSPCOM-PRINCIPLE-DATA
           DISPLAY 'EPSPCOM-NUMBER-OF-YEARS:' EPSPCOM-NUMBER-OF-YEARS
           DISPLAY 'EPSPCOM-INT-RATE:' EPSPCOM-QUOTED-INTEREST-RATE
           MOVE EPSPCOM-PRINCIPLE-DATA  TO EPSPDATA-PRINCIPLE-DATA.
           MOVE EPSPCOM-NUMBER-OF-YEARS TO EPSPDATA-NUMBER-OF-YEARS.
           MOVE 'Y'                     TO EPSPDATA-YEAR-MONTH-IND.
           MOVE EPSPCOM-QUOTED-INTEREST-RATE
                                        TO
                                   EPSPDATA-QUOTED-INTEREST-RATE.
           DISPLAY 'EPSPDAT-PRINCIPLE-DATA:'  EPSPDATA-PRINCIPLE-DATA.
           DISPLAY 'EPSPDAT-NUMBER-OF-YEARS:' EPSPDATA-NUMBER-OF-YEARS.
           DISPLAY 'EPSPDAT-INT-RATE:' EPSPDATA-QUOTED-INTEREST-RATE.
           MOVE CALLED-PROGRAM-NAME(1)  TO WS-CALLED-PROGRAM.
           MOVE SPACES                  TO EPSPDATA-RETURN-ERROR.

           MOVE SPACES TO EPSPDATA-RETURN-ERROR.
           CALL 'EPSMPMT' USING EPSPDATA.
           DISPLAY 'EPSPDATA-RETURN-MONTH-PAYMENT:'
                    EPSPDATA-RETURN-MONTH-PAYMENT.
           MOVE EPSPDATA-RETURN-MONTH-PAYMENT
                                        TO
                                        EPSPCOM-RETURN-MONTH-PAYMENT.
           MOVE EPSPDATA-RETURN-ERROR   TO EPSPCOM-ERRMSG.
           IF EPSPDATA-RETURN-ERROR = SPACES
              MOVE ZERO TO EPSPCOM-PROGRAM-RETCODE
           ELSE
              MOVE 8 TO EPSPCOM-PROGRAM-RETCODE
           END-IF.
           GOBACK
           .
