       01  WT-CNTL0525.                                                 00000010
      *------------------------PROGRAM PURPOSE-------------------------*00000020
      *                                                                *00000030
      *     COPYBOOK TITLE: CNTL0525                                   *00000040
      *     COPYBOOK TEXT: TABLE COPYBOOK FOR COUNTRY DESCRIPTION      *00000050
      *                                                                *00000060
      *     THIS TABLE CONTAINS COUNTRY CODE AND COUNTRY DESCRIPTION   *00000070
      *     THE KEY OF THE RECORD IS COUNTRY CODE(2 BYTES).            *00000080
      *                                                                *00000090
      *-------------------MODULES THAT USE THIS TABLE------------------*00000100
           05  FILLER                  PIC X(8) VALUE 'GSFRB.10'.       00000280
           05  WT-C0525-CONTROL         PIC X(8) VALUE SPACE.           00000290
           05  WT-C0525-NAME            PIC X(8) VALUE 'TURB0525'.      00000300
           05  WT-C0525-REQUEST         PIC X    VALUE 'S'.             00000310
           05  FILLER                   PIC X    VALUE SPACE.           00000320
           05  WT-C0525-RETURN          PIC S9(4) COMP VALUE +0.        00000330
           05  WT-C0525-KEY-LENGTH      PIC S9(4) COMP VALUE +2.        00000340
           05  WT-C0525-ENTRY-LENGTH    PIC S9(4) COMP VALUE +30.       00000350
           05  WT-C0525-PARM-LENGTH     PIC S9(4) COMP VALUE +80.       00000360
           05  WT-C0525-KEY.                                            00000370
               10 WT-C0525-COUNTRY      PIC X(2).                       00000380
           05  WT-C0525-DATA.                                           00000390
               10  WT-C0525-COUNTRY-DESC    PIC X(30) VALUE SPACE.      00000400
           05  WT-C0525-PARM.                                           00000410
               10  FILLER               PIC X(50) VALUE                 00000420
      *                                                                 00000430
      * IMR CHANGE BEGIN.                                               00000440
      *                                                                 00000450
      *        ' CONTROL DD=IS600V02,M=0525,K=(5,2),D=(41,30),S=B,'.    00000460
      *        10  FILLER               PIC X(30) VALUE                 00000470
      *        'E=(31,1, )'.                                            00000480
               ' CONTROL DD=IS600V02,M=0525,K=(5,2),D=(51,30),S=B,'.    00000490
               10  FILLER               PIC X(30) VALUE                 00000500
               'E=(41,1, )'.                                            00000510
      *                                                                 00000520
      * IMR CHANGE END.                                                 00000530
      *                                                                 00000540
           05  FILLER                  PIC X(8) VALUE 'GSFRB.10'.       00000550
