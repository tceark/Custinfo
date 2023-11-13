           05  INPUT-DATA.                                              00000130
               10  INPUT-JULIAN-DATE.                                   00000140
                   15  INPUT-JULIAN-YY      PIC 99  VALUE ZERO.         00000150
      *************    88 INPUT-JULIAN-YY-VALID     VALUE 01 THRU 99.   00000160
                       88 INPUT-JULIAN-YY-VALID     VALUE 00 THRU 99.   00000170
                   15  INPUT-JULIAN-DDD     PIC 999 VALUE ZERO.         00000180
                       88 INPUT-JULIAN-DDD-VALID    VALUE 001 THRU 366. 00000190
               10  INPUT-GREGORIAN-DATE.                                00000200
                   15  INPUT-GREGORIAN-MM   PIC 99  VALUE ZERO.         00000210
                       88 INPUT-GREGORIAN-MM-VALID  VALUE 01 THRU 12.   00000220
                   15  INPUT-GREGORIAN-DD   PIC 99  VALUE ZERO.         00000230
                       88 INPUT-GREGORIAN-DD-VALID  VALUE 01 THRU 31.   00000240
                   15  INPUT-GREGORIAN-YY   PIC 99  VALUE ZERO.         00000250
                       88 INPUT-GREGORIAN-YY-VALID  VALUE 00 THRU 99.   00000260
               10  INPUT-GREGORIAN-DATE-2.                              00000270
                   15  INPUT-GREGORIAN-MM-2 PIC 99   VALUE ZERO.        00000280
                       88 INPUT-GREGORIAN-MM-2-VALID VALUE 01 THRU 12.  00000290
                   15  INPUT-GREGORIAN-DD-2  PIC 99  VALUE ZERO.        00000300
                       88 INPUT-GREGORIAN-DD-2-VALID VALUE 01 THRU 31.  00000310
                   15  INPUT-GREGORIAN-YY-2  PIC 99  VALUE ZERO.        00000320
                       88 INPUT-GREGORIAN-YY-2-VALID VALUE 00 THRU 99.  00000330
               10  INPUT-RELATIVE-DATE      PIC 9(5) VALUE ZERO.        00000340
                   88 INPUT-RELATIVE-VALID           VALUE 1 THRU 54422.00000350
      *******      88 INPUT-RELATIVE-VALID           VALUE 1 THRU 36159.00000360
               10  INPUT-OPERAND-1          PIC 999 VALUE ZERO.         00000370
               10  INPUT-OPERAND-2          PIC 999 VALUE ZERO.         00000380
               10  INPUT-OPERAND-3          PIC 999 VALUE ZERO.         00000390
               10  INPUT-DATE-FORMAT-CODE   PIC X   VALUE SPACES.       00000400
                   88  VALID-DATE-FORMAT    VALUE 'J' 'G' '2' 'R'.      00000410
                   88  INPUT-DATE-JULIAN            VALUE 'J'.          00000420
                   88  INPUT-DATE-GREGORIAN         VALUE 'G'.          00000430
                   88  INPUT-DATE-2GREGORIANS       VALUE '2'.          00000440
                   88  INPUT-DATE-RELATIVE          VALUE 'R'.          00000450
               10  OPERATION-TO-PERFORM     PIC XX  VALUE SPACES.       00000460
                   88  VALID-OPERATION      VALUE 'VJ' 'VG' 'VR' 'VE'   00000470
                                                  'VW' 'VD' 'VA' 'VS'   00000480
                                                  'FJ' 'FG' 'FR' 'FE'   00000490
                                                  'FW' 'FD' 'FA' 'FS'.  00000500
                   88  VALIDATE-AND-RETURN-JULIAN          VALUE 'VJ'.  00000510
                   88  VALIDATE-AND-RETURN-GREGORIAN       VALUE 'VG'.  00000520
                   88  VALIDATE-AND-RETURN-RELATIVE        VALUE 'VR'.  00000530
                   88  VALIDATE-AND-RETURN-EXPANDED        VALUE 'VE'.  00000540
                   88  VALIDATE-AND-RETURN-WEEKDAY         VALUE 'VW'.  00000550
                   88  VALIDATE-AND-RETURN-DAYS-DIFF       VALUE 'VD'.  00000560
                   88  VALIDATE-AND-ADD-DAYS               VALUE 'VA'.  00000570
                   88  VALIDATE-AND-SUBTRACT-DAYS          VALUE 'VS'.  00000580
                   88  VALIDATE-AND-RETURN-JULIAN-4        VALUE 'FJ'.  00000590
                   88  VALIDATE-AND-RETURN-GREG-4          VALUE 'FG'.  00000600
                   88  VALIDATE-AND-RETURN-RELATIVE-4      VALUE 'FR'.  00000610
                   88  VALIDATE-AND-RETURN-EXPANDED-4      VALUE 'FE'.  00000620
                   88  VALIDATE-AND-RETURN-WEEKDAY-4       VALUE 'FW'.  00000630
                   88  VALIDATE-AND-RETURN-DAYS-DIF-4      VALUE 'FD'.  00000640
                   88  VALIDATE-AND-ADD-DAYS-4             VALUE 'FA'.  00000650
                   88  VALIDATE-AND-SUBTRACT-DAYS-4        VALUE 'FS'.  00000660
      *                                                                 00000670
               10  FILLER                   PIC X(16) VALUE SPACES.     00000680
      *                                                                 00000690
           05  OUTPUT-DATA.                                             00000700
               10  OUT-JULIAN-DATE.                                     00000710
                   15  OUT-JULIAN-YY        PIC 99  VALUE ZERO.         00000720
                   15  OUT-JULIAN-DDD       PIC 999 VALUE ZERO.         00000730
               10  OUT-GREGORIAN-DATE.                                  00000740
                   15  OUT-GREGORIAN-MM     PIC 99  VALUE ZERO.         00000750
                   15  OUT-GREGORIAN-DD     PIC 99  VALUE ZERO.         00000760
                   15  OUT-GREGORIAN-YY     PIC 99  VALUE ZERO.         00000770
               10  OUT-RELATIVE-DATE        PIC 9(5)  VALUE ZERO.       00000780
               10  OUT-EXPANDED-DATE.                                   00000790
                   15  OUT-WEEKDAY-NAME     PIC X(9).                   00000800
                   15  OUT-FILLER-1         PIC XX.                     00000810
                   15  OUT-MONTH-NAME       PIC XXX.                    00000820
                   15  OUT-FILLER-2         PIC XX.                     00000830
                   15  OUT-DATE-NUM         PIC XX.                     00000840
                   15  OUT-FILLER-3.                                    00000850
                     17 OUT-FILLER-31       PIC X(2).                   00000860
                     17 OUT-FILLER-32       PIC X(2).                   00000870
                   15  OUT-YEAR-NUM         PIC XX.                     00000880
               10  OUT-WEEKDAY-CODE         PIC 9   VALUE ZEROS.        00000890
               10  OUT-LAPSED-DAYS          PIC 9(5) VALUE ZEROS.       00000900
               10  MODULE-ERROR-CODE        PIC X   VALUE SPACES.       00000910
                   88  ERRORS                       VALUE 'D' 'O'.      00000920
                   88  NO-ERROR                     VALUE ' '.          00000930
                   88  INPUT-DATE-ERROR             VALUE 'D'.          00000940
                   88  OTHER-ERROR                  VALUE 'O'.          00000950
               10  MODULE-ERROR-MSG         PIC X(15) VALUE SPACES.     00000960
                                                                        00000970
               10  FILLER                   PIC X(18) VALUE SPACES.     00000980
      *                                                                 00000990
           05  X-DATE-WORK-TABLE.                                       00001000
               08  X-MO-TBL1-ALL.                                       00001010
                   10  FILLER  PIC X(17)  VALUE 'JAN31000SUNDAY   '.    00001020
                   10  FILLER  PIC X(17)  VALUE 'FEB28031MONDAY   '.    00001030
                   10  FILLER  PIC X(17)  VALUE 'MAR31059TUESDAY  '.    00001040
                   10  FILLER  PIC X(17)  VALUE 'APR30090WEDNESDAY'.    00001050
                   10  FILLER  PIC X(17)  VALUE 'MAY31120THURSDAY '.    00001060
                   10  FILLER  PIC X(17)  VALUE 'JUN30151FRIDAY   '.    00001070
                   10  FILLER  PIC X(17)  VALUE 'JUL31181SATURDAY '.    00001080
                   10  FILLER  PIC X(17)  VALUE 'AUG31212         '.    00001090
                   10  FILLER  PIC X(17)  VALUE 'SEP30243         '.    00001100
                   10  FILLER  PIC X(17)  VALUE 'OCT31273         '.    00001110
                   10  FILLER  PIC X(17)  VALUE 'NOV30304         '.    00001120
                   10  FILLER  PIC X(17)  VALUE 'DEC31334         '.    00001130
               08  X-MO-TBL  REDEFINES  X-MO-TBL1-ALL.                  00001140
                   09  X-MO-TBL1-OCC            OCCURS 12 TIMES.        00001150
                       10  X-MO-TBL1            PIC X(03).              00001160
                       10  X-MO-TBL2            PIC 9(02).              00001170
                       10  X-MO-TBL3            PIC 9(03).              00001180
                       10  X-MO-TBL4            PIC X(09).              00001190
      *                                                                 00001200
           05  DATE-WORK-AREA1.                                         00001210
               08  DATE-FIELDS.                                         00001220
                   10  GDATE1.                                          00001230
                       15  GDATE1-MO     PIC 9(02)     VALUE ZEROS.     00001240
                       15  GDATE1-DY     PIC 9(02)     VALUE ZEROS.     00001250
                       15  GDATE1-YR     PIC 9(02)     VALUE ZEROS.     00001260
                   10  GDATE1-X  REDEFINES  GDATE1.                     00001270
                       15  GDATE1-N      PIC 9(06).                     00001280
                                                                        00001290
                   10  GDATE2.                                          00001300
                       15  GDATE2-MO     PIC 9(02)     VALUE ZEROS.     00001310
                       15  GDATE2-DY     PIC 9(02)     VALUE ZEROS.     00001320
                       15  GDATE2-YR     PIC 9(02)     VALUE ZEROS.     00001330
                   10  GDATE2-X  REDEFINES  GDATE2.                     00001340
                       15  GDATE2-N      PIC 9(06).                     00001350
                                                                        00001360
                   10  JDATE3.                                          00001370
                       15  JDATE3-YR     PIC 9(02)     VALUE ZEROS.     00001380
                       15  JDATE3-DY     PIC 9(03)     VALUE ZEROS.     00001390
                   10  JDATE3-X  REDEFINES  JDATE3.                     00001400
                       15  JDATE3-N      PIC 9(05).                     00001410
                                                                        00001420
                   10  RDATE4            PIC 9(05)     VALUE ZEROS.     00001430
                                                                        00001440
                   10  DAYS              PIC S9(05)    VALUE +0 COMP-3. 00001450
                   10  WEEKDAY-CODE      PIC 9(01)     VALUE ZEROS.     00001460
                   10  WEEKDAY           PIC X(09)     VALUE SPACES.    00001470
                   10  JULIAN-DATE       PIC 9(5)      VALUE ZEROS.     00001480
                   10  HOLD-JULIAN-DATE  PIC 9(5)      VALUE ZEROS.     00001490
      *                                                                 00001500
      *                                                                 00001510
           05  DATE-WORK-AREA2.                                         00001520
               08  XX-DATE-FIELDS.                                      00001530
                   10  XXDATES-1.                                       00001540
                       15  XXDATES-1-MO    PIC 9(02).                   00001550
                       15  XXDATES-1-DY    PIC 9(02).                   00001560
                       15  XXDATES-1-YR    PIC 9(02).                   00001570
                   10  XXDATES-1-X  REDEFINES  XXDATES-1.               00001580
                       15  XXDATES-1-N     PIC 9(06).                   00001590
                                                                        00001600
                   10  XXDATES-2.                                       00001610
                       15  XXDATES-2-MO    PIC 9(02).                   00001620
                       15  XXDATES-2-DY    PIC 9(02).                   00001630
                       15  XXDATES-2-YR    PIC 9(02).                   00001640
                   10  XXDATES-2-X  REDEFINES  XXDATES-2.               00001650
                       15  XXDATES-2-N     PIC 9(06).                   00001660
                                                                        00001670
                   10  XXDATES-3.                                       00001680
                       15  XXDATES-3-YR    PIC 9(02).                   00001690
                       15  XXDATES-3-DY    PIC 9(03).                   00001700
                   10  XXDATES-3-X  REDEFINES  XXDATES-3.               00001710
                       15  XXDATES-3-N     PIC 9(05).                   00001720
                                                                        00001730
                   10  XXDATES-4           PIC 9(05).                   00001740
                                                                        00001750
                   10  XXDATES-SUB1      PIC S9(05)    COMP-3.          00001760
                   10  XXDATES-DAYS-1    PIC S9(05)    COMP-3.          00001770
                   10  XXDATES-DAYS-2    PIC S9(05)    COMP-3.          00001780
                   10  XXDATES-DAYS-3    PIC S9(05)    COMP-3.          00001790
                   10  XXDATES-DAYS-4    PIC S9(05)    COMP-3.          00001800
                   10  XXDATES-DAYS-5    PIC S9(05)    COMP-3.          00001810
                   10  XXDATES-WORK-1    PIC S9(05)    COMP-3.          00001820
                   10  XXDATES-WORK-2    PIC S9(05)    COMP-3.          00001830
                   10  XXDATES-WORK-3    PIC S9(05)    VALUE +0.        00001840
                       88  WORK-RELATIVE-VALID    VALUE +1 THRU +54422. 00001850
      **********       88  WORK-RELATIVE-VALID    VALUE +1 THRU +36159. 00001860
                   10  XXDATES-DAY-NAME  PIC X(09).                     00001870
      *                                                                 00001880
           05  DATE-WORK-AREA3.                                         00001890
               08  XXDATES-6.                                           00001900
                   10  XXDATES-6-MODY          PIC 9(04).               00001910
                       88  VALID-GREG-MMDD     VALUE 0101 THRU 0131     00001920
                                                     0201 THRU 0228     00001930
                                                     0301 THRU 0331     00001940
                                                     0401 THRU 0430     00001950
                                                     0501 THRU 0531     00001960
                                                     0601 THRU 0630     00001970
                                                     0701 THRU 0731     00001980
                                                     0801 THRU 0831     00001990
                                                     0901 THRU 0930     00002000
                                                     1001 THRU 1031     00002010
                                                     1101 THRU 1130     00002020
                                                     1201 THRU 1231.    00002030
                   10  XXDATES-6-YR            PIC 9(02).               00002040
      *                                                                 00002050
           05  INPUT-DATA-Y2K.                                          00002060
              10   INPUT-4-JULIAN-DATE .                                00002070
                   15 INPUT-4-JULIAN-CC      PIC 99   VALUE ZERO  .     00002080
                       88  INPUT-4-JULIAN-CC-VALID   VALUE 19 THRU 20 . 00002090
                   15 INPUT-4-JULIAN-YR      PIC 99   VALUE ZERO  .     00002100
                       88  INPUT-4-JULIAN-YR-VALID   VALUE 00 THRU 99 . 00002110
                   15 INPUT-4-JULIAN-DY      PIC 999  VALUE ZERO  .     00002120
                       88  INPUT-4-JULIAN-DY-VALID   VALUE 1 THRU 366 . 00002130
                                                                        00002140
               10  INPUT-4-GREG-DATE .                                  00002150
                   15 INPUT-4-GREG-MO      PIC 99   VALUE ZERO  .       00002160
                       88  INPUT-4-GREG-MO-VALID   VALUE 1 THRU 12 .    00002170
                   15 INPUT-4-GREG-DY      PIC 99  VALUE ZERO  .        00002180
                       88  INPUT-4-GREG-DY-VALID   VALUE 1 THRU 31 .    00002190
                   15 INPUT-4-GREG-CC      PIC 99   VALUE ZERO  .       00002200
                       88  INPUT-4-GREG-CC-VALID   VALUE 19 THRU 20 .   00002210
                   15 INPUT-4-GREG-YR      PIC 99   VALUE ZERO  .       00002220
                       88  INPUT-4-GREG-YR-VALID   VALUE 00 THRU 99 .   00002230
                                                                        00002240
               10  INPUT-4-GREG-DATE-2.                                 00002250
                   15 INPUT-4-GREG-MO-2    PIC 99   VALUE ZERO  .       00002260
                       88  INPUT-4-GREG-MO-VALID-2   VALUE 1 THRU 12 .  00002270
                   15 INPUT-4-GREG-DY-2    PIC 99  VALUE ZERO  .        00002280
                       88  INPUT-4-GREG-DY-VALID-2   VALUE 1 THRU 31 .  00002290
                   15 INPUT-4-GREG-CC-2    PIC 99   VALUE ZERO  .       00002300
                       88  INPUT-4-GREG-CC-VALID-2   VALUE 19 THRU 20 . 00002310
                   15 INPUT-4-GREG-YR-2    PIC 99   VALUE ZERO  .       00002320
                       88  INPUT-4-GREG-YR-VALID-2   VALUE 00 THRU 99 . 00002330
                                                                        00002340
           05  OUTPUT-DATA-Y2K.                                         00002350
               10 OUT-4-JULIAN-DATE.                                    00002360
                  15 OUT-4-JULIAN-CC      PIC 99  VALUE ZERO .          00002370
                  15 OUT-4-JULIAN-YR      PIC 99  VALUE ZERO .          00002380
                  15 OUT-4-JULIAN-DY      PIC 999 VALUE ZERO .          00002390
               10 OUT-4-GREG-DATE.                                      00002400
                  15 OUT-4-GREG-MO      PIC 99  VALUE ZERO .            00002410
                  15 OUT-4-GREG-DY      PIC 99  VALUE ZERO .            00002420
                  15 OUT-4-GREG-CC      PIC 99  VALUE ZERO .            00002430
                  15 OUT-4-GREG-YR      PIC 99  VALUE ZERO .            00002440
                                                                        00002450
           05  WORK-AREA-4.                                             00002460
                   10 JULIAN-DATE-4       PIC 9(07) VALUE ZERO .        00002470
                   10 HOLD-JULIAN-DATE-4  PIC 9(07) VALUE ZERO .        00002480
                                                                        00002490
                   10  XXDATES-CENTURY-1 PIC  9(02)    VALUE ZEROS.     00002500
                   10  XXDATES-CENTURY-2 PIC  9(02)    VALUE ZEROS.     00002510
                   10  XXDATES-7.                                       00002520
                      15  XXDATES-7-MODY          PIC 9(04).            00002530
                      15  XXDATES-7-CC            PIC 9(02).            00002540
                      15  XXDATES-7-YR            PIC 9(02).            00002550
      **** 05  FILLER          PIC X(67)          VALUE SPACES.         00002560
           05  FILLER          PIC X(03)          VALUE SPACES.         00002570
      *----------------------------------------------------------------*00002580
      *--------------------END OF DTEPARM2 COPYBOOK--------------------*00002590
