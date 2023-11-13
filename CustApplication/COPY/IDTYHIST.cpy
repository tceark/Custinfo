      ******************************************************************
      * DCLGEN TABLE(IDNTITY_HISTORY)
      *        LIBRARY(IBMUSER.CICS.COPY(IDTYHIST))                    *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE IDNTITY_HISTORY TABLE
           ( IDNTITY                CHAR(8) NOT NULL,
             TRANSACTION            CHAR(6) NOT NULL,
             CHANGE_DATE            TIMESTAMP NOT NULL,
             MONGODB_SYNC           CHAR(1) NOT NULL
           ) END-EXEC.
