       01  TUTORIAL-COMM-AREA.                                          TUTORCOM
           05  TC-CURSOR-POSITION      PIC S9(4) COMP.                  TUTORCOM
           05  TC-SCREEN-NAME          PIC X(8).                        TUTORCOM
           05  TC-PAGE-NUMBER          PIC S9(3) COMP-3.                TUTORCOM
           05  TC-PROGRAM-NAME         PIC X(8).                        TUTORCOM
           05  TC-TRANSACTION-ID       PIC X(4).                        TUTORCOM
           05  TC-KEY-DATA.                                             TUTORCOM
               10 TC-CARRIER           PIC XX.                          TUTORCOM
               10 TC-MESSAGE PIC X(5) OCCURS 4 .                        TUTORCOM
               10 TC-FIELD-FILE        PIC XX.                          TUTORCOM
               10 TC-FIELD-NUMBER      PIC 9(5).                        TUTORCOM
               10 TC-LAST-KEY.                                          TUTORCOM
                   15 TC-LAST-TABLE    PIC 9(4).                        TUTORCOM
                   15 TC-LAST-REST     PIC X(16).                       TUTORCOM
               10 TC-PREV-KEY.                                          TUTORCOM
                   15 TC-PREV-TABLE    PIC 9(4).                        TUTORCOM
                   15 TC-PREV-REST     PIC X(16).                       TUTORCOM
           05  TC-COMMAND-LINE         PIC X VALUE 'N'.                 TUTORCOM
           05  TC-STICKY-AREA.                                          TUTORCOM
               10 TC-STICKY-FIELD-ID   PIC X(7).                        TUTORCOM
               10 TC-STICKY-POSITION   PIC 9(3).                        TUTORCOM
               10 TC-STICKY-LENGTH     PIC 9(3).                        TUTORCOM
               10 TC-STICKY-TYPE       PIC X.                           TUTORCOM
               10 TC-STICKY-VALUE      PIC X(30).                       TUTORCOM
           05  TC-STICKY-TABLE OCCURS 24.                               TUTORCOM
               10 TC-STICKY-LINE       PIC 9(2).                        TUTORCOM
               10 TC-STICKY-VALUE-TABLE  PIC X(30).                     TUTORCOM
