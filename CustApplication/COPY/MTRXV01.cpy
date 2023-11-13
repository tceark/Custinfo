      ******************************************************************MTRXV01
      * DCLGEN FOR VIEW MTRXV01 BASED ON TABLE ECIMTRX.                *MTRXV01
      ******************************************************************MTRXV01
           EXEC SQL DECLARE MTRXV01 TABLE                               MTRXV01
           (AGNT#IDNTITY        CHAR(8)          NOT NULL,              MTRXV01
            SYSTEM_ID           CHAR(2)          NOT NULL,              MTRXV01
            ACTION_CODE         CHAR(5)          NOT NULL,              MTRXV01
            MEDIA_TYPE          CHAR(1)          NOT NULL,              MTRXV01
            EFFECTIVE_DATE      DATE             NOT NULL,              MTRXV01
            TERMINATION_DATE    DATE             NOT NULL               MTRXV01
             ) END-EXEC.                                                MTRXV01
      ******************************************************************MTRXV01
      * COBOL DECLARATION FOR TABLE MTRXV01                            *MTRXV01
      ******************************************************************MTRXV01
       01  DCLMTRXV01.                                                  MTRXV01
           10  AGNT-IDNTITY        PIC X(8).                            MTRXV01
           10  SYSTEM-ID           PIC X(2).                            MTRXV01
           10  ACTION-CODE         PIC X(5).                            MTRXV01
           10  MEDIA-TYPE          PIC X(1).                            MTRXV01
           10  EFFECTIVE-DATE      PIC X(10).                           MTRXV01
           10  TERMINATION-DATE    PIC X(10).                           MTRXV01
      *----------------------------------------------------------------*MTRXV01
      *    NULL INDICATOR ARRAY                                        *MTRXV01
      *----------------------------------------------------------------*MTRXV01
       01  MTRXV01-INDICATOR-AREA.                                      MTRXV01
           10  MTRXV01-INDICATORS OCCURS 06 TIMES PIC S9(04) COMP.      MTRXV01
       01  FILLER REDEFINES MTRXV01-INDICATOR-AREA.                     MTRXV01
           10  IND-AGNT-IDNTITY        PIC S9(4)   COMP.                MTRXV01
           10  IND-SYSTEM-ID           PIC S9(4)   COMP.                MTRXV01
           10  IND-ACTION-CODE         PIC S9(4)   COMP.                MTRXV01
           10  IND-MEDIA-TYPE          PIC S9(4)   COMP.                MTRXV01
           10  IND-EFFECTIVE-DATE      PIC S9(4)   COMP.                MTRXV01
           10  IND-TERMINATION-DATE    PIC S9(4)   COMP.                MTRXV01
      ******************************************************************MTRXV01
      * COBOL DECLARATION FOR TABLE MTRXV01  WITH FIELD PREFIX *        MTRXV01
      ******************************************************************MTRXV01
       01  ECI-MTRXV01.                                                 MTRXV01
           10  MTX-AGNT-IDNTITY        PIC X(8).                        MTRXV01
           10  MTX-SYSTEM-ID           PIC X(2).                        MTRXV01
           10  MTX-ACTION-CODE         PIC X(5).                        MTRXV01
           10  MTX-MEDIA-TYPE          PIC X(1).                        MTRXV01
           10  MTX-EFFECTIVE-DATE      PIC X(10).                       MTRXV01
           10  MTX-TERMINATION-DATE    PIC X(10).                       MTRXV01
