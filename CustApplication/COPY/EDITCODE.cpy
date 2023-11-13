      ******************************************************************
      * DCLGEN TABLE(EDITCODE)                                         *
      *        LIBRARY(IBMUSER.TEST.COPY(EDITCODE))                    *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE EDITCODE TABLE
           ( EDIT_CD                        CHAR(5) NOT NULL,
             EDIT_DESC                      CHAR(100) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE EDITCODE                           *
      ******************************************************************
       01  DCLEDITCODE.
           10 EDIT-CD              PIC X(5).
           10 EDIT-DESC            PIC X(100).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 2       *
      ******************************************************************
