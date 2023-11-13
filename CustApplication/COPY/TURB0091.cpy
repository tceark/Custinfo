       01  WT-CNTL0091.                                                 00000450
           05  FILLER                   PIC X(8) VALUE 'GSFRB.10'.      00000460
           05  WT-C0091-CONTROL         PIC X(8) VALUE SPACE.           00000470
           05  WT-C0091-NAME            PIC X(8) VALUE 'TBV50091'.      00000480
           05  WT-C0091-REQUEST         PIC X    VALUE 'S'.             00000490
           05  FILLER                   PIC X    VALUE SPACE.           00000500
           05  WT-C0091-RETURN          PIC S9(4) COMP VALUE +0.        00000510
           05  WT-C0091-KEY-LENGTH      PIC S9(4) COMP VALUE +2.        00000520
           05  WT-C0091-ENTRY-LENGTH    PIC S9(4) COMP VALUE +88.       00000530
           05  WT-C0091-PARM-LENGTH     PIC S9(4) COMP VALUE +80.       00000540
           05  WT-C0091-KEY.                                            00000550
               10  WT-C0091-STATE            PIC X(2).                  00000560
           05  WT-C0091-DATA.                                           00000570
               10  WT-C0091-AGT-LIC-LIAB-FLG PIC X     VALUE SPACE.     00000580
               10  WT-C0091-STATE-NAME       PIC X(20) VALUE SPACE.     00000590
               10  WT-C0091-DNR-STATE        PIC X     VALUE SPACE.     00000600
               10  WT-C0091-ZIP-CODE-MIN     PIC 999.                   00000610
               10  WT-C0091-ZIP-CODE-MAX     PIC 999.                   00000620
               10  WT-C0091-ZIP-CODE-EXCEPTION1 PIC 999.                00000630
               10  WT-C0091-ZIP-CODE-EXCEPTION2 PIC 999.                00000640
               10  WT-C0091-AREA-CODE OCCURS 10 PIC 999.                00000650
               10  WT-C0091-TAX-DATE         PIC 9(6).                  00000660
               10  WT-C0091-TAX-PERCENT      PIC V9(6).                 00000670
               10  WT-C0091-UNISEX-IND       PIC X.                     00000680
               10  WT-C0091-LIC-RENEWAL-PERIOD PIC 99.                  00000690
               10  WT-C0091-LIC-RENEWAL-MMDD PIC 9(4).                  00000700
               10  WT-C0091-ECI-IND          PIC X.                     00000710
               10  WT-C0091-NUM-STATE-CODE   PIC 99.                    00000720
               10  WT-C0091-MKT-SITE         PIC XX.                    00000730
           05  WT-C0091-PARM.                                           00000740
               10  FILLER               PIC X(50) VALUE                 00000750
Y2KIMR*                                                                 00000760
Y2KIMR* IMR CHANGE BEGIN                                                00000770
Y2KIMR*                                                                 00000780
Y2KIMR*        ' CONTROL DD=IS600V02,M=0091,K=(5,2),D=(41,88),S=B,'.    00000790
Y2KIMR*        10  FILLER               PIC X(30) VALUE                 00000800
Y2KIMR*        'E=(31,1, )'.                                            00000810
Y2KIMR         ' CONTROL DD=IS600V02,M=0091,K=(5,2),D=(51,88),S=B,'.    00000820
Y2KIMR         10  FILLER               PIC X(30) VALUE                 00000830
Y2KIMR         'E=(41,1, )'.                                            00000840
Y2KIMR*                                                                 00000850
Y2KIMR* IMR CHANGE END                                                  00000860
Y2KIMR*                                                                 00000870
           05  FILLER                  PIC X(8) VALUE 'GSFRB.10'.       00000880
