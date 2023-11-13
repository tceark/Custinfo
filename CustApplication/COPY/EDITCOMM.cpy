      *---------------------------------------------------------------* EDITCOMM
      *                                                               * EDITCOMM
      *              C O M M O N    E D I T   A R E A                 * EDITCOMM
      *                                                               * EDITCOMM
      *---------------------------------------------------------------* EDITCOMM
      *                                                                 EDITCOMM
       01  EDIT-AREA.                                                   EDITCOMM
           05  EDIT-FEEDBACK.                                           EDITCOMM
               10  ERROR-CODE               PIC X(05).                  EDITCOMM
               10  SEVERITY-CODE            PIC X(01).                  EDITCOMM
               10  TURBO-TABLE              PIC X(04).                  EDITCOMM
               10  VALUES-CHANGED-SW        PIC X(01).                  EDITCOMM
                   88   VALUES-HAVE-CHANGED VALUE 'C'.                  EDITCOMM
      *                                                                 EDITCOMM
           05  SUBSYSTEM-CODE               PIC X(02).                  EDITCOMM
           05  ACTION-CODE                  PIC X(05).                  EDITCOMM
           05  STATUS-CODE                  PIC X(02).                  EDITCOMM
               88   INIT-ISSUE              VALUE 'II'.                 EDITCOMM
               88   PRE-ISSUE               VALUE 'PI'.                 EDITCOMM
               88   AFTER-ISSUE             VALUE 'AI'.                 EDITCOMM
      *                                                                 EDITCOMM
           05  FILLER                       PIC X(20).                  EDITCOMM
