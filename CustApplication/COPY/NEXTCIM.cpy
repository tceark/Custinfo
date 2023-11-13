      ******************************************************************
      * DCLGEN TABLE(NEXT_IDNTITY)                                     *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE NEXT_IDNTITY TABLE
           (NEXT_IDNTITY_NUM  CHAR(8)            NOT NULL,
            RCVD_DATE         DATE               NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE NEXT_IDNTITY                       *
      ******************************************************************
       01  DCLNEXTCIM.
           10 NEXT-IDNTITY-NUM            PIC X(8).
           10 RCVD-DATE               PIC X(10).
      ******************************************************************
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS  2      *
      ******************************************************************
       01  NEXT-ID-INDICATOR-ARRAY.
           05  NEXT-ID-INDICATOR OCCURS  2 TIMES PIC S9(4) COMP.
       01  FILLER REDEFINES NEXT-ID-INDICATOR-ARRAY.
           05  NEXT-ID-NUMBER-IND                PIC S9(4) COMP.
           05  RCVD-DATE-IND                     PIC S9(4) COMP.
