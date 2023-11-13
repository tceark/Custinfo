       01  WT-CNTL9999.                                                 00000100
           05  FILLER                   PIC X(8) VALUE 'GSFRB.10'.      00000110
           05  WT-C9999-CONTROL         PIC X(8) VALUE SPACE.           00000120
           05  WT-C9999-NAME            PIC X(8) VALUE 'TURB9999'.      00000130
           05  WT-C9999-REQUEST         PIC X    VALUE 'S'.             00000140
           05  FILLER                   PIC X    VALUE SPACE.           00000150
           05  WT-C9999-RETURN          PIC S9(4) COMP VALUE +0.        00000160
           05  WT-C9999-KEY-LENGTH      PIC S9(4) COMP VALUE +5.        00000170
           05  WT-C9999-ENTRY-LENGTH    PIC S9(4) COMP VALUE +36.       00000180
           05  WT-C9999-PARM-LENGTH     PIC S9(4) COMP VALUE +80.       00000190
           05  WT-C9999-KEY.                                            00000200
               10 WT-C9999-ERROR-CODE   PIC X(5).                       00000210
           05  WT-C9999-DATA.                                           00000220
               10  WT-C9999-SEVERITY-CODE   PIC X.                      00000230
               10  WT-C9999-ERROR-MESSAGE   PIC X(35).                  00000240
           05  WT-C9999-PARM.                                           00000250
               10  FILLER               PIC X(50) VALUE                 00000260
      *                                                                 00000270
      * IMR CHANGE BEGIN                                                00000280
      *                                                                 00000290
      *       ' CONTROL DD=IS600V02,M=9999,K=(5,5),D=(41,36),S=B,'.     00000300
      *        10  FILLER               PIC X(30) VALUE                 00000310
      *       'E=(31,1, )'.                                             00000320
              ' CONTROL DD=IS600V02,M=9999,K=(5,5),D=(51,36),S=B,'.     00000330
               10  FILLER               PIC X(30) VALUE                 00000340
              'E=(41,1, )'.                                             00000350
      *                                                                 00000360
      * IMR CHANGE END                                                  00000370
      *                                                                 00000380
           05  FILLER                   PIC X(8) VALUE 'GSFRB.10'.      00000390
