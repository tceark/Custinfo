      ******************************************************************
      * DCLGEN TABLE(IDNTITY_INFO)                                     *
      *        LIBRARY(IBMUSER.CICS.COPY(IDTYINFO))                    *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE IDNTITY_INFO TABLE
           ( IDNTITY                CHAR(8) NOT NULL,
             IDNTITY_TYPE           CHAR(8) NOT NULL,
             ADDRESS1               CHAR(30) NOT NULL,
             ADDRESS2               CHAR(30),
             CITY                   CHAR(30) NOT NULL,
             STATE                  CHAR(2) NOT NULL,
             ZIP                    CHAR(5) NOT NULL,
             ZIP_PLUS4              CHAR(4) NOT NULL,
             EFFECTIVE_DATE         DATE NOT NULL,
             CHANGE_DATE            TIMESTAMP NOT NULL,
             ENTITY_TYPE            CHAR(2) NOT NULL,
             RECORD_STATUS          CHAR(1) NOT NULL
           ) END-EXEC.
