      *                                                                 IOAREA2K
      * THIS IS THE COMM AREA TO PASS TO IOMOD2K FOR THE NEW ONLINE     IOAREA2K
      * SYSTEM                                                          IOAREA2K
      *  INPUT (1) FUNCTION CODE                                        IOAREA2K
      *        (2) TERMINAL ID                                          IOAREA2K
      *        (3) RECORD ADDRESS (WHEN REWRITING OR ADDING RECORD      IOAREA2K
      *                                                                 IOAREA2K
      *  OUTPUT (1) ADDRESS OF RECORD WHEN READING                      IOAREA2K
      *         (2) RETURN CODE                                         IOAREA2K
      *                                                                 IOAREA2K
            05  IO-FUNCTION                  PIC X.                     IOAREA2K
                88 IO-GENERIC-UPDATE         VALUE 'G'.                 IOAREA2K
                88 IO-INQUIRY                VALUE 'I'.                 IOAREA2K
                88 IO-INQUIRY-FOR-UPDATE     VALUE 'U'.                 IOAREA2K
                88 IO-REWRITE-SPECIFIC       VALUE 'Q'.                 IOAREA2K
                88 IO-REWRITE                VALUE 'R'.                 IOAREA2K
                88 IO-REWRITE-NO-RELEASE     VALUE 'T'.                 IOAREA2K
                88 IO-RELEASE                VALUE 'L'.                 IOAREA2K
                88 IO-RELEASE-SPECIFIC       VALUE 'M'.                 IOAREA2K
                88 IO-DELETE                 VALUE 'D'.                 IOAREA2K
                88 IO-ADD                    VALUE 'A'.                 IOAREA2K
                88 IO-STARTBR                VALUE 'S'.                 IOAREA2K
                88 IO-READ-NEXT              VALUE 'N'.                 IOAREA2K
                88 IO-READ-PREV              VALUE 'P'.                 IOAREA2K
                88 IO-ENDBR                  VALUE 'E'.                 IOAREA2K
            05  IO-RETURN                    PIC X.                     IOAREA2K
                88 IO-GOOD                   VALUE ' '.                 IOAREA2K
                88 IO-NOT-FOUND              VALUE 'N'.                 IOAREA2K
                88 IO-OUT-FOR-UPDATE         VALUE 'U'.                 IOAREA2K
                88 IO-DUP-RECORD             VALUE 'D'.                 IOAREA2K
                88 IO-ERROR                  VALUE 'E'.                 IOAREA2K
                88 IO-ENDFILE                VALUE 'F'.                 IOAREA2K
                88 IO-NOTOPEN                VALUE 'O'.                 IOAREA2K
            05  IO-TERM                      PIC X(4).                  IOAREA2K
            05  IO-FILE-NAME                 PIC X(8).                  IOAREA2K
            05  IO-KEY.                                                 IOAREA2K
                10  IO-CASE                  PIC X(6).                  IOAREA2K
                10  IO-REST                  PIC X(34).                 IOAREA2K
            05  IO-ADDR                      PIC S9(8) COMP.            IOAREA2K
