       01  WT-CNTL8991.                                                 00000150
           05  FILLER                   PIC X(8) VALUE 'GSFRB.10'.      00000160
           05  WT-C8991-CONTROL         PIC X(8) VALUE SPACE.           00000170
           05  WT-C8991-NAME            PIC X(8) VALUE 'TURB8991'.      00000180
           05  WT-C8991-REQUEST         PIC X    VALUE 'S'.             00000190
           05  FILLER                   PIC X    VALUE SPACE.           00000200
           05  WT-C8991-RETURN              COMP PIC S9(4) VALUE +0.    00000210
           05  WT-C8991-KEY-LENGTH          COMP PIC S9(4) VALUE +10.   00000220
           05  WT-C8991-ENTRY-LENGTH        COMP PIC S9(4) VALUE +56.   00000230
           05  WT-C8991-PARM-LENGTH         COMP PIC S9(4) VALUE +80.   00000240
           05  WT-C8991-KEY.                                            00000250
               10 WT-C8991-SYSTEM-CD             PIC X(04).             00000260
               10 WT-C8991-ACTION-CD             PIC X(6).              00000270
           05  WT-C8991-DATA.                                           00000280
               10 WT-C8991-DESCRIPTION           PIC X(30).             00000290
               10 WT-C8991-DISP-ON-MAIN          PIC X(1).              00000300
               10 WT-C8991-DISP-ON-SUBS          PIC X(1).              00000310
               10 WT-C8991-SUBSYS-ENTRY-PROG     PIC X(8).              00000320
               10 WT-C8991-SUBSYS-ENTRY-TRAN     PIC X(4).              00000330
               10 WT-C8991-SUBSYS-NEXT-PROG      PIC X(8).              00000340
               10 WT-C8991-SUBSYS-NEXT-TRAN      PIC X(4).              00000350
           05  WT-C8991-PARM.                                           00000360
               10  FILLER                        PIC X(51) VALUE        00000370
      *                                                                 00000380
      * IMR CHANGE BEGIN                                                00000390
      *                                                                 00000400
      *    ' CONTROL DD=IS600V02,M=8991,K=(5,10),D=(41,56),S=B,'.       00000410
      *        10  FILLER                        PIC X(29) VALUE        00000420
      *           'E=(31,1, )'.                                         00000430
           ' CONTROL DD=IS600V02,M=8991,K=(5,10),D=(51,56),S=B,'.       00000440
               10  FILLER                        PIC X(29) VALUE        00000450
                  'E=(41,1, )'.                                         00000460
      *                                                                 00000470
      * IMR CHANGE END                                                  00000480
      *                                                                 00000490
           05  FILLER                   PIC X(8) VALUE 'GSFRB.10'.      00000500
