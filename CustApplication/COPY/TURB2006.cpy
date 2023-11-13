       01  WT-CNTL2006.                                                 00000100
           05  FILLER                   PIC X(8) VALUE 'GSFRB.10'.      00000110
           05  WT-C2006-CONTROL         PIC X(8) VALUE SPACE.           00000120
           05  WT-C2006-NAME            PIC X(8) VALUE 'TURB2006'.      00000130
           05  WT-C2006-REQUEST         PIC X    VALUE 'S'.             00000140
           05  FILLER                   PIC X    VALUE SPACE.           00000150
           05  WT-C2006-RETURN          PIC S9(4) COMP VALUE +0.        00000160
           05  WT-C2006-KEY-LENGTH      PIC S9(4) COMP VALUE +2.        00000170
           05  WT-C2006-ENTRY-LENGTH    PIC S9(4) COMP VALUE +10.       00000180
           05  WT-C2006-PARM-LENGTH     PIC S9(4) COMP VALUE +80.       00000190
           05  WT-C2006-KEY.                                            00000200
               10 WT-C2006-STATUS-CD    PIC XX.                         00000210
           05  WT-C2006-DATA.                                           00000220
               10 WT-C2006-STAT-DESC    PIC X(10).                      00000230
           05  WT-C2006-PARM            PIC X(80) VALUE                 00000240
      *                                                                 00000250
      * IMR CHANGE BEGIN.                                               00000260
      *                                                                 00000270
      *              ' CONTROL DD=IS600V02,M=2006,K=(5,2),D=(41,10),S=B,00000280
      *-          'E=(31,1, )'.                                         00000290
                     ' CONTROL DD=IS600V02,M=2006,K=(5,2),D=(51,10),S=B,00000300
      -           'E=(41,1, )'.                                         00000310
      *                                                                 00000320
      * IMR CHANGE END.                                                 00000330
      *                                                                 00000340
           05  FILLER                   PIC X(8) VALUE 'GSFRB.10'.      00000350
