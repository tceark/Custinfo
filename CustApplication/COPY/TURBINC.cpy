      ******************************************************************TURBINC
      *                                                                 TURBINC
      *  WORKING STORAGE INCLUDE FIELDS NECESSARY FOR THE LINK TO       TURBINC
      *  PSIGSFO FOR THE TURBO GSF INTERFACE.                           TURBINC
      *                                                                 TURBINC
      ******************************************************************TURBINC
       01  TB-STUFF.                                                    TURBINC
           05  TB-SUB                PIC S9(4) COMP VALUE ZEROS.        TURBINC
           05  TB-SOURCE-LEN         PIC S9(4) COMP VALUE ZEROS.        TURBINC
           05  TB-SOURCE-PTR         PIC S9(4) COMP VALUE ZEROS.        TURBINC
           05  TB-TARGET-PTR         PIC S9(4) COMP VALUE ZEROS.        TURBINC
           05  TB-CNTR               PIC S9(4) COMP VALUE ZEROS.        TURBINC
           05  TB-SPACE-COUNT        PIC S9(4) COMP VALUE ZEROS.        TURBINC
           05  TB-ERROR-STATUS       PIC S9(8) COMP VALUE ZEROS.        TURBINC
           05  TB-RECORD-LEN         PIC S9(4) COMP VALUE +1720.        TURBINC
           05  TB-APPLID             PIC X(8)       VALUE SPACES.       TURBINC
           05  TB-DONE-SW            PIC X VALUE SPACE.                 TURBINC
               88  TASK-DONE         VALUE 'Y'.                         TURBINC
           05  TB-LAST-SPACE-SW      PIC X VALUE SPACE.                 TURBINC
               88  LAST-CHARACTER-WAS-SPACE VALUE 'Y'.                  TURBINC
           05  TB-SUFFIX             PIC X(12) VALUE ',E=(31,1, ) '.    TURBINC
           05  TB-HOLD-AREA.                                            TURBINC
               10  TB-FIRST-BYTE        PIC X VALUE SPACES.             TURBINC
               10  TB-HOLD-REMAINDER.                                   TURBINC
                   15  FILLER            PIC X(11) VALUE SPACES.        TURBINC
                   15  TB-HOLD-DD-NAME   PIC X(8) VALUE SPACES.         TURBINC
                   15  FILLER            PIC X(3) VALUE SPACES.         TURBINC
                   15  TB-HOLD-TABLE     PIC X(4) VALUE SPACES.         TURBINC
                   15  FILLER            PIC X(228) VALUE SPACES.       TURBINC
           05  TB-COPY-AREA           PIC X(255)  VALUE SPACES.         TURBINC
           05  TB-SAVE-AREA           PIC X(255)  VALUE SPACES.         TURBINC
                                                                        TURBINC
       77  DL-GSF-ROUTINE              PIC X(7)    VALUE 'PSIGSFC'.     TURBINC
                                                                        TURBINC
       01  WT-HOLDAREA.                                                 TURBINC
          02  WT-GROUP.                                                 TURBINC
           05  FILLER             PIC X(8).                             TURBINC
           05  WT-CONTROL         PIC X(8).                             TURBINC
           05  WT-NAME.                                                 TURBINC
               10  WT-NAME-PREF   PIC X(3).                             TURBINC
               10  FILLER         PIC X(5).                             TURBINC
           05  WT-REQUEST         PIC X.                                TURBINC
           05  FILLER             PIC X.                                TURBINC
           05  WT-RETURN          PIC S9(4) COMP.                       TURBINC
           05  WT-KEY-LENGTH      PIC S9(4) COMP.                       TURBINC
           05  WT-ENTRY-LENGTH    PIC S9(4) COMP.                       TURBINC
           05  WT-PARM-LENGTH     PIC S9(4) COMP.                       TURBINC
          02  WT-DATA-GROUP.                                            TURBINC
           05  WT-KEY.                                                  TURBINC
               10  FILLER OCCURS 1 TO 30 TIMES                          TURBINC
                      DEPENDING ON WT-KEY-LENGTH PIC X.                 TURBINC
           05  WT-DATA.                                                 TURBINC
               10  FILLER OCCURS 1 TO 1720 TIMES                        TURBINC
                      DEPENDING ON WT-ENTRY-LENGTH PIC X.               TURBINC
           05  WT-PARM.                                                 TURBINC
               10  PARM-CHAR OCCURS 1 TO 255 TIMES                      TURBINC
                      DEPENDING ON WT-PARM-LENGTH PIC X.                TURBINC
           05  WT-TERMINATOR            PIC X(8).                       TURBINC
                                                                        TURBINC
       01  TURBO-STUFF.                                                 TURBINC
           05  TURBO-PASS-LEN               PIC S9(4) COMP VALUE +0.    TURBINC
           05  TURBO-CNTL-AREA.                                         TURBINC
               10  TURBO-GROUP              PIC X(34) VALUE SPACES.     TURBINC
               10  TURBO-REMAINDER          PIC X(1100) VALUE SPACES.   TURBINC
