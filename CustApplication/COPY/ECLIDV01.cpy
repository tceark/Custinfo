      ******************************************************************
      * DCLGEN FOR VIEW ECLIDV01 BASED ON TABLE ECILOGON.              *
      ******************************************************************
           EXEC SQL DECLARE ECLIDV01 TABLE
           (AGNT#IDNTITY        CHAR(8)          NOT NULL,
            LOGONID             CHAR(8)          NOT NULL,
            STATUS              CHAR(1)          NOT NULL
             ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE ECLIDV0                            *
      ******************************************************************
       01  DCLECLIDV01.
           10  AGNT-IDNTITY        PIC X(8).
           10  LOGONID             PIC X(8).
           10  STATUS-CODE         PIC X(1).
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *    NULL INDICATOR ARRAY                                        *
      *----------------------------------------------------------------*
       01  ECLIDV01-INDICATOR-AREA.
           10  ECLIDV01-INDICATORS PIC S9(04) COMP OCCURS 03 TIMES.
       01  FILLER REDEFINES ECLIDV01-INDICATOR-AREA.
           10  IND-AGNT-IDNTITY        PIC S9(4)   COMP.
           10  IND-LOGONID             PIC S9(4)   COMP.
           10  IND-STATUS-CODE         PIC S9(4)   COMP.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE ECLIDV01  WITH FIELD PREFIX *
      ******************************************************************
       01  ECI-ECLIDV01.
          10  LID-AGNT-IDNTITY        PIC X(8).
          10  LID-LOGONID             PIC X(8).
          10  LID-STATUS-CODE         PIC X(1).
