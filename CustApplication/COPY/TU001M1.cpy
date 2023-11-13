       01   TU001M1I.                                                   TU001M1
           02 FILLER PIC X(12).                                         TU001M1
           02 MAP-TYPEL COMP PIC S9(4).                                 TU001M1
           02 MAP-TYPEF PIC X.                                          TU001M1
      *TU00000                                                          TU001M1
           02 MAP-TYPEI PIC X(10).                                      TU001M1
           02 MAP-SCREEN-NAMEL COMP PIC S9(4).                          TU001M1
           02 MAP-SCREEN-NAMEF PIC X.                                   TU001M1
      *TU00001                                                          TU001M1
           02 MAP-SCREEN-NAMEI PIC X(8).                                TU001M1
           02 MAP-PAGE-NUMBERL COMP PIC S9(4).                          TU001M1
           02 MAP-PAGE-NUMBERF PIC X.                                   TU001M1
      *TU00002                                                          TU001M1
           02 MAP-PAGE-NUMBERI PIC 99.                                  TU001M1
           02 MAP-TITLEL COMP PIC S9(4).                                TU001M1
           02 MAP-TITLEF PIC X.                                         TU001M1
      *TU00003                                                          TU001M1
           02 MAP-TITLEI PIC X(79).                                     TU001M1
           02 S5I OCCURS  20 TIMES.                                     TU001M1
             03 MAP-SCREEN-DESCL COMP PIC S9(4).                        TU001M1
             03 MAP-SCREEN-DESCF PIC X.                                 TU001M1
      *TU00004                                                          TU001M1
             03 MAP-SCREEN-DESCI PIC X(79).                             TU001M1
       01   TU001M1O REDEFINES TU001M1I.                                TU001M1
           02 FILLER PIC X(12).                                         TU001M1
           02 FILLER PIC X(2).                                          TU001M1
           02 MAP-TYPEA PIC X.                                          TU001M1
      *TU00000                                                          TU001M1
           02 MAP-TYPEO PIC X(10).                                      TU001M1
           02 FILLER PIC X(2).                                          TU001M1
           02 MAP-SCREEN-NAMEA PIC X.                                   TU001M1
      *TU00001                                                          TU001M1
           02 MAP-SCREEN-NAMEO PIC X(8).                                TU001M1
           02 FILLER PIC X(2).                                          TU001M1
           02 MAP-PAGE-NUMBERA PIC X.                                   TU001M1
      *TU00002                                                          TU001M1
           02 MAP-PAGE-NUMBERO PIC 99.                                  TU001M1
           02 FILLER PIC X(2).                                          TU001M1
           02 MAP-TITLEA PIC X.                                         TU001M1
      *TU00003                                                          TU001M1
           02 MAP-TITLEO PIC X(79).                                     TU001M1
           02 S5O OCCURS  20 TIMES.                                     TU001M1
             03 FILLER PIC X(2).                                        TU001M1
             03 MAP-SCREEN-DESCA PIC X.                                 TU001M1
      *TU00004                                                          TU001M1
             03 MAP-SCREEN-DESCO PIC X(79).                             TU001M1
