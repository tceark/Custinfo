       01  WT-CNTL8997.                                                 00000100
           05  FILLER                      PIC X(8) VALUE 'GSFRB.10'.   00000110
           05  WT-C8997-CONTROL            PIC X(8) VALUE SPACE.        00000120
           05  WT-C8997-NAME               PIC X(8) VALUE 'TURB8997'.   00000130
           05  WT-C8997-REQUEST            PIC X    VALUE 'S'.          00000140
           05  FILLER                      PIC X    VALUE SPACE.        00000150
           05  WT-C8997-RETURN             PIC S9(4) COMP VALUE +0.     00000160
           05  WT-C8997-KEY-LENGTH         PIC S9(4) COMP VALUE +4.     00000170
           05  WT-C8997-ENTRY-LENGTH       PIC S9(4) COMP VALUE +23.    00000180
           05  WT-C8997-PARM-LENGTH        PIC S9(4) COMP VALUE +80.    00000190
           05  WT-C8997-KEY.                                            00000200
               10 WT-C8997-SYSTEM-CD       PIC X(4).                    00000210
           05  WT-C8997-DATA.                                           00000220
               10 WT-C8997-SYS-TYPE        PIC X(10).                   00000230
               10 WT-C8997-ALTRN-KEY       PIC X(10).                   00000240
               10 WT-C8997-FILE-KEY-TYPE   PIC X.                       00000250
               10 WT-C8997-FILE-KEY-LENGTH PIC 99.                      00000260
           05  WT-C8997-PARM               PIC X(80) VALUE              00000270
      *                                                                 00000280
      * IMR CHANGE BEGIN                                                00000290
      *                                                                 00000300
      *              ' CONTROL DD=IS600V02,M=8997,K=(5,4),D=(41,23),S=B,00000310
      *-       'E=(31,1, )'.                                            00000320
                     ' CONTROL DD=IS600V02,M=8997,K=(5,4),D=(51,23),S=B,00000330
      -        'E=(41,1, )'.                                            00000340
      *                                                                 00000350
      * IMR CHANGE END                                                  00000360
      *                                                                 00000370
           05  FILLER                   PIC X(8) VALUE 'GSFRB.10'.      00000380
