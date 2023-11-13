       01  WS-DEMOGRAPHIC-COMM-AREA.
           05  WS-DEMO-PROGRAM         PIC X(4) VALUE SPACES.
           05  WS-DEMO-RETURN-CODE     PIC S9(4) COMP VALUE +0.
           05  WS-DEMO-VERSION-NUM     PIC X(4) VALUE SPACES.
           05  WS-DEMO-LOGONID.
               10  WS-DEMO-LID-1-4.
                   15  WS-DEMO-LID-1-2 PIC X(2) VALUE SPACES.
                   15  WS-DEMO-LID-3-4 PIC X(2) VALUE SPACES.
               10  WS-DEMO-LID-5-8     PIC X(4) VALUE SPACES.
           05  WS-DEMO-USER-NAME       PIC X(20) VALUE SPACES.
           05  WS-DEMO-OPERID          PIC X(3) VALUE SPACES.
           05  WS-DEMO-ACCT-NUM.
               10  WS-DEMO-DIVISION    PIC X(02) VALUE SPACES.
               10  WS-DEMO-BRANCH      PIC X(02) VALUE SPACES.
               10  WS-DEMO-DEPARTMENT  PIC X(02) VALUE SPACES.
               10  WS-DEMO-SECTION     PIC X(02) VALUE SPACES.
           05  WS-DEMO-SITE-CODE       PIC XX VALUE SPACES.
               88  WS-DEMO-SITE-T                     VALUE '10'.
               88  WS-DEMO-SITE-D                     VALUE '11'.
               88  WS-DEMO-SITE-C                     VALUE '20'.
               88  WS-DEMO-SITE-NEW-H                 VALUE '30'.
               88  WS-DEMO-SITE-S                     VALUE '40'.
               88  WS-DEMO-SITE-W                     VALUE '50'.
               88  WS-DEMO-SITE-H                     VALUE '60'.
               88  WS-DEMO-SITE-P                     VALUE '61'.
               88  WS-DEMO-SITE-M                     VALUE '65'.
           05  WS-DEMO-TERMINAL-ID     PIC X(4) VALUE SPACES.
           05  WS-DEMO-ROUTE-CODE      PIC X(8) VALUE SPACES.
           05  WS-DEMO-GREGORIAN-DATE  PIC X(8) VALUE SPACES.
           05  WS-DEMO-CS-PRT-ID       PIC X(4) VALUE SPACES.
           05  WS-DEMO-MAGIC-PRT-ID    PIC X(4) VALUE SPACES.
           05  WS-DEMO-CICS-TEST-FLAG  PIC X VALUE SPACE.
           05  FILLER                  PIC X(8) VALUE SPACES.
           05  WS-DEMO-PROGRAM2        PIC X(4) VALUE SPACES.
