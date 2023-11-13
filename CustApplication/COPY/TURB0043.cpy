      *------------------------PROGRAM PURPOSE-------------------------*00000010
      *                                                                *00000020
      *     COPYBOOK TITLE: TURB0043                                   *00000030
      *     COPYBOOK TEXT:  ASSOCIATION CODES - EDIT CHECKING          *00000040
      *----------------------------------------------------------------*00000050
       01  WT-CNTL0043.                                                 00000210
           05  FILLER                       PIC X(8)  VALUE 'GSFRB.10'. 00000220
           05  WT-C0043-CONTROL             PIC X(8)  VALUE SPACE.      00000230
           05  WT-C0043-NAME                PIC X(8)  VALUE 'TURB0043'. 00000240
           05  WT-C0043-REQUEST             PIC X     VALUE 'S'.        00000250
           05  FILLER                       PIC X     VALUE SPACE.      00000260
           05  WT-C0043-RETURN              PIC S9(4) COMP VALUE +0.    00000270
           05  WT-C0043-KEY-LENGTH          PIC S9(4) COMP VALUE +5.    00000280
           05  WT-C0043-ENTRY-LENGTH        PIC S9(4) COMP VALUE +43.   00000290
           05  WT-C0043-PARM-LENGTH         PIC S9(4) COMP VALUE +80.   00000300
           05  WT-C0043-KEY.                                            00000310
               10  WT-C0043-ASSOC-CODE-KEY    PIC X(5).                 00000320
           05  WT-C0043-DATA.                                           00000330
               10  WT-C0043-SITE-CODE         PIC X(2).                 00000340
               10  WT-C0043-RECORD-DESC       PIC X(35).                00000350
               10  WT-C0043-ASSOC-TERM-DATE.                            00000360
                   15  WT-C0043-YY            PIC 99.                   00000370
                   15  WT-C0043-MM            PIC 99.                   00000380
                   15  WT-C0043-DD            PIC 99.                   00000390
           05  WT-C0043-PARM.                                           00000400
               10 FILLER                      PIC X(50) VALUE           00000410
      *                                                                 00000420
      * IMR CHANGE BEGIN                                                00000430
      *                                                                 00000440
      *        ' CONTROL DD=IS600V02,M=0043,K=(5,5),D=(41,43),S=B,'.    00000450
      *        10 FILLER                      PIC X(30) VALUE           00000460
      *        'E=(31,1, )'.                                            00000470
               ' CONTROL DD=IS600V02,M=0043,K=(5,5),D=(51,43),S=B,'.    00000480
               10 FILLER                      PIC X(30) VALUE           00000490
               'E=(41,1, )'.                                            00000500
      *                                                                 00000510
      * IMR CHANGE END                                                  00000520
      *                                                                 00000530
           05  FILLER                        PIC X(8) VALUE 'GSFRB.10'. 00000540
