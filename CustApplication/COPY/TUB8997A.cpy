       01  WT-CNTL8997A.                                                00000100
           05  FILLER                    PIC X(8) VALUE 'GSFRB.10'.     00000110
           05  WT-C8997A-CONTROL         PIC X(8) VALUE SPACE.          00000120
           05  WT-C8997A-NAME            PIC X(8) VALUE 'TUB8997A'.     00000130
           05  WT-C8997A-REQUEST         PIC X    VALUE 'S'.            00000140
           05  FILLER                    PIC X    VALUE SPACE.          00000150
           05  WT-C8997A-RETURN          PIC S9(4) COMP VALUE +0.       00000160
           05  WT-C8997A-KEY-LENGTH      PIC S9(4) COMP VALUE +10.      00000170
           05  WT-C8997A-ENTRY-LENGTH    PIC S9(4) COMP VALUE +4.       00000180
           05  WT-C8997A-PARM-LENGTH     PIC S9(4) COMP VALUE +80.      00000190
           05  WT-C8997A-KEY.                                           00000200
               10 WT-C8997A-SYSTEM-DESC  PIC X(10).                     00000210
           05  WT-C8997A-DATA.                                          00000220
               10 WT-C8997A-SYSTEM-CD    PIC X(4).                      00000230
           05  WT-C8997A-PARM              PIC X(80) VALUE              00000240
      *                                                                 00000250
      * IMR CHANGE BEGIN                                                00000260
      *                                                                 00000270
      *              ' CONTROL DD=IS600V02,M=8997,K=(51,10),D=(5,4),S=B,00000280
      *-       'E=(31,1, )'.                                            00000290
                     ' CONTROL DD=IS600V02,M=8997,K=(61,10),D=(5,4),S=B,00000300
      -        'E=(41,1, )'.                                            00000310
      *                                                                 00000320
      * IMR CHANGE END                                                  00000330
      *                                                                 00000340
           05  FILLER                   PIC X(8) VALUE 'GSFRB.10'.      00000350
