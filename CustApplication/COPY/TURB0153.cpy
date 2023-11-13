      *------------------------PROGRAM PURPOSE-------------------------*00000010
      *                                                                *00000020
      *     COPYBOOK TITLE: TURB0153                                   *00000030
      *     COPYBOOK TEXT:  AGENCY CODES                               *00000040
      *----------------------------------------------------------------*00000050
       01  WT-CNTL0153.                                                 00000270
           05  FILLER                       PIC X(8)  VALUE 'GSFRB.10'. 00000280
           05  WT-C0153-CONTROL             PIC X(8)  VALUE SPACE.      00000290
           05  WT-C0153-NAME                PIC X(8)  VALUE 'TBV50153'. 00000300
           05  WT-C0153-REQUEST             PIC X     VALUE 'S'.        00000310
           05  FILLER                       PIC X     VALUE SPACE.      00000320
           05  WT-C0153-RETURN              PIC S9(4) COMP VALUE +0.    00000330
           05  WT-C0153-KEY-LENGTH          PIC S9(4) COMP VALUE +7.    00000340
           05  WT-C0153-ENTRY-LENGTH        PIC S9(4) COMP VALUE +120.  00000350
           05  WT-C0153-PARM-LENGTH         PIC S9(4) COMP VALUE +80.   00000360
           05  WT-C0153-KEY.                                            00000370
               10  WT-C0153-CARRIER-CODE      PIC X(2).                 00000380
               10  WT-C0153-AGENCY-CODE       PIC X(3).                 00000390
               10  WT-C0153-AGENCY-SEQ        PIC X(2).                 00000400
           05  WT-C0153-DATA.                                           00000410
               10  WT-C0153-AGENCY-NAME       PIC X(30).                00000420
               10  WT-C0153-STATE-CODE        PIC X(2).                 00000430
               10  WT-C0153-AGCY-CITY         PIC X(15).                00000440
               10  WT-C0153-AGCY-ADD1         PIC X(22).                00000450
               10  WT-C0153-AGCY-ADD2         PIC X(22).                00000460
               10  WT-C0153-AGCY-ZIP          PIC 9(09).                00000470
               10  WT-C0153-AGCY-PHONE        PIC 9(10).                00000480
               10  WT-C0153-AGCY-TYPE         PIC X(02).                00000490
               10  WT-C0153-ANNUALIZED-CODE   PIC X(01).                00000500
               10  WT-C0153-GROUP-SPECIALIST  PIC X(01).                00000510
               10  WT-C0153-SPEC-EFF-DATE     PIC 9(06).                00000520
           05  WT-C0153-PARM                  PIC X(80) VALUE           00000530
      *                                                                 00000540
      * IMR CHANGE BEGIN                                                00000550
      *                                                                 00000560
      *             ' CONTROL DD=IS600V02,M=0153,K=(5,7),D=(41,120),S=B,00000570
      *-           'E=(31,1, )'.                                        00000580
                    ' CONTROL DD=IS600V02,M=0153,K=(5,7),D=(51,120),S=B,00000590
      -            'E=(41,1, )'.                                        00000600
      *                                                                 00000610
      * IMR CHANGE END                                                  00000620
      *                                                                 00000630
           05  FILLER                        PIC X(8) VALUE 'GSFRB.10'. 00000640
