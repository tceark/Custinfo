      ***************************************************************** TURBDESC
      * TURBDESC                                                        TURBDESC
      *                                                                 TURBDESC
      ******************************************************************TURBDESC
       01  TURBO-DESC-RECORD.                                           TURBDESC
           05  TURBO-DESC-KEY.                                          TURBDESC
               10 TURBO-DESC-TYPE            PIC X.                     TURBDESC
               10 TURBO-DESC-DDNAME.                                    TURBDESC
                  15 TURBO-DESC-FILLER       PIC X(4).                  TURBDESC
                  15 TURBO-DESC-TABLE        PIC 9(4).                  TURBDESC
               10 TURBO-DESC-FILE-PREFIX     PIC XX.                    TURBDESC
               10 TURBO-DESC-FIELD-NUMBER    PIC 9(5).                  TURBDESC
           05  TURBO-DESC-KEY-FLAG           PIC X.                     TURBDESC
           05  TURBO-DESC-REQUIRED-FLAG      PIC X.                     TURBDESC
           05  TURBO-DESC-FORMAT             PIC X.                     TURBDESC
           05  TURBO-DESC-FIELD-LENGTH       PIC S9(4) COMP.            TURBDESC
           05  TURBO-DESC-FIELD-DESCRIPTION  PIC X(35).                 TURBDESC
           05  TURBO-DESC-HEADING1           PIC X(10).                 TURBDESC
           05  TURBO-DESC-HEADING1-LEN       PIC 99.                    TURBDESC
           05  TURBO-DESC-HEADING2           PIC X(10).                 TURBDESC
           05  TURBO-DESC-HEADING2-LEN       PIC 99.                    TURBDESC
           05  TURBO-DESC-OFFSET             PIC 9(4).                  TURBDESC
           05  TURBO-DESC-FILL               PIC X(16).                 TURBDESC
