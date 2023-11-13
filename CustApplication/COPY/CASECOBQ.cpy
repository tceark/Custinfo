
IDENT      02  CASE-REC.                                                00002180
00005          05  CASE-ACTIVE-CODES.                                   00002190
00010              10  CASE-ACTIVE-CODE1            PIC X(01).          00002200
00015              10  CASE-ACTIVE-CODE2            PIC X(01).          00002210
00020          05  CASE-ACTIVE-CODE                                     00002220
                                REDEFINES CASE-ACTIVE-CODES             00002230
                                                    PIC X(02).          00002240
                   88  CASE-ACTIVE                VALUE 'CM'.           00002250
                   88  CASE-BELOW-MINIMUM-EMPS    VALUE ' 1' '1P'.      00002260
                   88  CASE-NSF                   VALUE ' 2'.           00002270
                   88  CASE-CANCELED-BY-REQUEST   VALUE ' 3' '33'.      00002280
                   88  CASE-NONPAY-MIDMONTH       VALUE ' 4' '4P' '48'. 00002290
                   88  CASE-OTHER-TERMINATED      VALUE                 00002300
                                             '05' ' L' ' A' ' 5' '  '.  00002310
                   88  CASE-CHANGED-CARRIER       VALUE ' 6' '66'.      00002320
                   88  CASE-PSI-TERMINATED        VALUE ' 7'.           00002330
                   88  CASE-CALCULATED-NONPAY     VALUE                 00002340
                                                  ' 8' '8P' '83' '88'.  00002350
                   88  CASE-NEVER-INFORCE         VALUE ' 9'.           00002360
R01717             88  CASE-ISSUE-ERROR           VALUE ' E'.           00002370
R01717             88  CASE-FREE-LOOK             VALUE ' F'.           00002380
                   88  CASE-GRACE-PERIOD          VALUE ' P'.           00002390
R01873             88  CASE-PLATYPUS-LISTBILL     VALUE ' G'.           00002400
                   88  CASE-DUPLICATE             VALUE ' D'.           00002410
                   88  CASE-TELEPHONE-REQUEST     VALUE ' T'.           00002420
930285             88  CASE-RECISION              VALUE ' R'.           00002430
930616             88  CASE-OUT-OF-STATE          VALUE ' S'.           00002440
R04004             88  CASE-COND-ISSUE-TERM       VALUE ' C'.           00002450
R11712             88  CASE-ONE-GROUP-CONVERSION  VALUE ' Q'.           00002451
417584             88  CASE-AIG-REMEDIATION       VALUE ' M'.           00002452
544567             88  CASE-AIG-TAFT-AXA          VALUE 'AX'.           00002453
544567             88  CASE-AIG-TAFT-SYMETRA      VALUE 'SY'.           00002454
00025          05  CASE-MKT-CODE                    PIC X(01).          00002460
                   88  CASE-OK-MKTS               VALUE '1' '2' '3' '4' 00002470
                                                        '5' '6' '7' '8' 00002480
                                                        'F' 'G' 'J' 'K' 00002490
                                                        'L' 'M' 'N' 'P' 00002500
                                                        'Q' 'R' 'T' 'C' 00002510
                                                        'Y' 'O'.        00002520
                   88  CASE-SOUTHEAST-MKTS        VALUE '1'.            00002530
                   88  CASE-SOUTHWEST-MKTS        VALUE '2'.            00002540
                   88  CASE-MIDWEST-MKTS          VALUE '3'.            00002550
                   88  CASE-MIDATLANTIC-MKTS      VALUE '4'.            00002560
                   88  CASE-NORTHEAST-MKTS        VALUE '5'.            00002570
                   88  CASE-PLAINS-MKTS           VALUE '6'.            00002580
                   88  CASE-PACIFIC-MKTS          VALUE '7'.            00002590
                   88  CASE-NORTHWEST-MKTS        VALUE '8'.            00002600
                   88  CASE-CASCO-MKTS            VALUE 'C'.            00002610
                   88  CASE-DADE-MONROE-MKTS      VALUE 'F'.            00002620
                   88  CASE-BROWARD-PALMBEACH-MKTS                      00002630
                                                  VALUE 'G'.            00002640
                   88  CASE-DB-MKTS               VALUE 'H' 'J' 'K'     00002650
                                                        'L' 'M'.        00002660
                   88  CASE-DW-MKTS               VALUE 'M'.            00002670
                   88  CASE-NARC-MKTS             VALUE 'N'.            00002680
                   88  CASE-LIFE-GEORGIA-MKTS     VALUE 'O'.            00002690
                   88  CASE-MGT-MKTS              VALUE 'P'.            00002700
                   88  CASE-SM-MCHSP-MKTS         VALUE 'Q'.            00002710
                   88  CASE-PROV-MUTUAL-MKTS      VALUE 'R'.            00002720
                   88  CASE-IDS-MKTS              VALUE 'S'.            00002730
                   88  CASE-ID-HL-MKTS            VALUE 'T'.            00002740
                   88  CASE-GENAM-MKTS            VALUE 'U'.            00002750
                   88  CASE-LOYALTY-LIFE-MKTS     VALUE 'Y'.            00002760
00030          05  CASE-PREV-COV-IND                PIC X(01).          00002770
                   88  CASE-PREVIOUSLY-COVERED    VALUE 'Y'.            00002780
                   88  CASE-NOT-PREVIOUSLY-COVERED                      00002790
                                                  VALUE 'N' ' '.        00002800
00035          05  CASE-BILL-REINSTATE              PIC 9(04).          00002810
               05  FILLER REDEFINES CASE-BILL-REINSTATE.                00002820
00040              10  CASE-BILL-REINSTATE-MM       PIC 99.             00002830
00045              10  CASE-BILL-REINSTATE-YY       PIC 99.             00002840
00050          05  CASE-HANDLE-BILL-IND             PIC X.              00002850
00055          05  CASE-CASE-NAME                   PIC X(22).          00002860
00060          05  CASE-USER-HANDLE                 PIC X(01).          00002870
00061          05  CASE-CHECK-DIGIT                 PIC X(01).          00002880
00065          05  CASE-OVER20-EMPS-IND             PIC X(01).          00002890
00070          05  CASE-TRUST-CODE                  PIC X(02).          00002900
00075          05  CASE-CERTIFICATE-NUM             PIC 9(04).          00002910
00080          05  CASE-INCEPTION-DATE              PIC 9(06).          00002920
               05  FILLER REDEFINES CASE-INCEPTION-DATE.                00002930
00085              10  CASE-INCEPTION-MM            PIC 99.             00002940
                       88  CASE-INCEPTION-MM-OK   VALUE 01 THRU 12.     00002950
00095              10  CASE-INCEPTION-DD            PIC 99.             00002960
                       88  CASE-INCEPTION-DD-OK   VALUE 01 THRU 31.     00002970
00100              10  CASE-INCEPTION-YY            PIC 99.             00002980
00105          05  CASE-ELIGIBLE-DATE               PIC 9(04).          00002990
               05  FILLER REDEFINES CASE-ELIGIBLE-DATE.                 00003000
00110              10  CASE-ELIGIBLE-MM             PIC 99.             00003010
                       88  CASE-ELIGIBLE-MM-OK    VALUE 01 THRU 12.     00003020
00120              10  CASE-ELIGIBLE-YY             PIC 99.             00003030
00125          05  CASE-TERM-DATE                   PIC 9(04).          00003040
               05  FILLER REDEFINES CASE-TERM-DATE.                     00003050
00130              10  CASE-TERM-MM                 PIC 99.             00003060
                       88  CASE-TERM-MM-OK        VALUE 01 THRU 12.     00003070
00140              10  CASE-TERM-YY                 PIC 99.             00003080
00145          05  CASE-ALPHA-KEY                   PIC X(05).          00003090
00150          05  CASE-ADRESS-AREA.                                    00003100
00155              10  CASE-ADDRESS-1               PIC X(22).          00003110
00160              10  CASE-ADDRESS-2               PIC X(22).          00003120
00165              10  CASE-CITY-AND-STATE          PIC X(17).          00003130
00170              10  CASE-ZIP-CODE                PIC 9(05).          00003140
00175              10  CASE-STATE-CODE              PIC X(02).          00003150
                       88  CASE-VALID-STATE-CODE  VALUE                 00003160
                                        'AK' 'AL' 'AR' 'AZ' 'CA' 'CO'   00003170
                                        'CT' 'DE' 'FL' 'GA' 'HI' 'IA'   00003180
                                        'ID' 'IL' 'IN' 'KS' 'KY' 'LA'   00003190
                                        'MA' 'MD' 'ME' 'MI' 'MN' 'MO'   00003200
                                        'MS' 'MT' 'NC' 'ND' 'NE' 'NH'   00003210
                                        'NJ' 'NM' 'NY' 'NV' 'OH' 'OK'   00003220
                                        'OR' 'PA' 'RI' 'SC' 'SD' 'TN'   00003230
                                        'TX' 'UT' 'VA' 'VT' 'WA' 'WI'   00003240
                                        'WV' 'WY' 'DC'.                 00003250
00180          05  CASE-PHONE-NUM                   PIC 9(10).          00003260
00185          05  CASE-NUM-OF-EMPLOYEES            PIC S9(05) COMP-3.  00003270
00190          05  CASE-WAIT-CODE                   PIC X(01).          00003280
                   88  CASE-WAITING               VALUE 'M' 'R'.        00003290
                   88  CASE-NOT-WAITING           VALUE 'N' ' '.        00003300
00195          05  CASE-WAIT-MONTHS                 PIC X(01).          00003310
00200          05  CASE-LIFE-VOLUME                 PIC S9(07) COMP-3.  00003320
00205          05  CASE-MM-DATE                     PIC 9(06).          00003330
00210          05  CASE-MM-DATE-X REDEFINES                             00003340
                                CASE-MM-DATE        PIC X(6).           00003350
00215          05  CASE-REENTRY-DATE                PIC 9(06).          00003360
               05  FILLER REDEFINES CASE-REENTRY-DATE.                  00003370
00220              10  CASE-REENTRY-MM              PIC 99.             00003380
00225              10  CASE-REENTRY-DD              PIC 99.             00003390
00230              10  CASE-REENTRY-YY              PIC 99.             00003400
00235          05  CASE-NSF-CHECKS-COUNT            PIC S9.             00003410
00236          05  CASE-MR-REIN-TERM-DATE           PIC 9(04).          00003420
               05  FILLER REDEFINES CASE-MR-REIN-TERM-DATE.             00003430
00237              10  CASE-MR-REIN-TERM-MM         PIC 9(02).          00003440
00238              10  CASE-MR-REIN-TERM-YY         PIC 9(02).          00003450
00239          05  CASE-MR-REIN-TERM-CODE           PIC X(01).          00003460
00254          05  CASE-OVER50-EMPS-IND             PIC X(01).          00003470
01850          05  CASE-BILL-SORT-ORDER             PIC X(01).          00003480
00260          05  CASE-PREMIUM-COLLECTED-YTD       PIC S9(08) COMP.    00003490
00265          05  CASE-PREMIUM-COLLECTED-ITD       PIC S9(08) COMP.    00003500
00270          05  CASE-CLAIMS-PAID-YTD             PIC S9(08) COMP.    00003510
00275          05  CASE-CLAIMS-PAID-ITD             PIC S9(08) COMP.    00003520
00280          05  CASE-CLASS-RESTRICT-CODE         PIC X(01).          00003530
                   88  CASE-RESTRICTED-CLASS      VALUE 'M' 'R' 'Y'.    00003540
                   88  CASE-NOT-RESTRICTED-CLASS  VALUE 'N' ' '.        00003550
00281          05  CASE-LTD-PLAN                    PIC X(2).           00003560
00282          05  CASE-LTD-EFF-DATE                PIC 9(4).           00003570
               05  FILLER REDEFINES CASE-LTD-EFF-DATE.                  00003580
                 10  CASE-LTD-EFF-MM                PIC 99.             00003590
                 10  CASE-LTD-EFF-YY                PIC 99.             00003600
00283          05  CASE-LTD-TERM-DATE               PIC 9(4).           00003610
               05  FILLER REDEFINES CASE-LTD-TERM-DATE.                 00003620
                 10  CASE-LTD-TERM-MM               PIC 99.             00003630
                 10  CASE-LTD-TERM-YY               PIC 99.             00003640
00285          05  CASE-SUB-STD-FACTOR              PIC S9V99 COMP-3.   00003650
00287          05  CASE-MICKEYM-AGENT-CODE          PIC X.              00003660
00290          05  CASE-LIFE-VOLUME-ACCUM           PIC S9(07) COMP-3.  00003670
00295          05  CASE-LIFE-NUM-OF-EMPS            PIC S9(03) COMP-3.  00003680
00300          05  CASE-LIFE-OPTION                 PIC X(01).          00003690
00301          05  CASE-LIFE-EXP-ADJ                PIC S9V9(4) COMP-3. 00003700
00303          05  CASE-WI-EXP-ADJ                  PIC S9V9(4) COMP-3. 00003710
00305          05  CASE-UNCOLL-EMP-ADD-CHARGE       PIC S999V99 COMP-3. 00003720
01640          05  CASE-NEW-SITE-EFF-DATE           PIC 9(4).           00003730
01645          05  CASE-BILL-SITE-CODE              PIC XX.             00003740
01650          05  CASE-SUPP-ACC-PLAN               PIC X.              00003750
01655          05  CASE-SUPP-ACC-EFF-DATE           PIC 9(4).           00003760
               05  FILLER REDEFINES CASE-SUPP-ACC-EFF-DATE.             00003770
                 10  CASE-SUPP-ACC-EFF-MM           PIC 99.             00003780
                 10  CASE-SUPP-ACC-EFF-YY           PIC 99.             00003790
01660          05  CASE-SUPP-ACC-TERM-DATE          PIC 9(4).           00003800
               05  FILLER REDEFINES CASE-SUPP-ACC-TERM-DATE.            00003810
                 10  CASE-SUPP-ACC-TERM-MM          PIC 99.             00003820
                 10  CASE-SUPP-ACC-TERM-YY          PIC 99.             00003830
00355          05  CASE-MONTHS-BELOW-MIN            PIC 9(01).          00003840
00360          05  CASE-CASH-THRU-DATE              PIC 9(06).          00003850
               05  FILLER REDEFINES CASE-CASH-THRU-DATE.                00003860
00365              10  CASE-CASH-THRU-MM            PIC 99.             00003870
                       88  CASE-CASH-THRU-MM-OK   VALUE 01 THRU 12.     00003880
00375              10  CASE-CASH-THRU-DD            PIC 99.             00003890
                       88  CASE-CASH-THRU-DD-OK   VALUE 01 THRU 31.     00003900
00380              10  CASE-CASH-THRU-YY            PIC 99.             00003910
00385          05  CASE-LATE-CHARGE                 PIC X(01).          00003920
00390          05  CASE-LATE-FEE-NUM REDEFINES CASE-LATE-CHARGE         00003930
                                                    PIC 9.              00003940
00395          05  CASE-GTR-3-CODE                  PIC X(01).          00003950
00400          05  CASE-BELOW-MINIMUM REDEFINES CASE-GTR-3-CODE         00003960
                                                    PIC X(01).          00003970
00405          05  CASE-LATE-FEE-CODE               PIC X(01).          00003980
01893          05  CASE-PRODUCT-NUMBER              PIC X(04).          00003990
00420          05  CASE-UNDERWRITTEN-IND            PIC X(01).          00004000
                   88  CASE-UNDERWRITTEN          VALUE 'Y'.            00004010
                   88  CASE-NOT-UNDERWRITTEN      VALUE 'N' ' '.        00004020
00425          05  CASE-DATE-OFF                    PIC 9(04).          00004030
00435          05  CASE-DATEOFF-EXTR   REDEFINES CASE-DATE-OFF.         00004040
00440              10  CASE-DOFFE-MM                PIC 99.             00004050
                       88  CASE-DOFFE-MM-OK       VALUE 01 THRU 12.     00004060
00445              10  CASE-DOFFE-YY                PIC 99.             00004070
00450          05  CASE-RATE-INCREASE.                                  00004080
                   10  CASE-RATE-INCREASE-MM        PIC 9(2).           00004090
                   10  CASE-RATE-INCREASE-YY        PIC 9(2).           00004100
00455          05  CASE-COUNTY-CODE                 PIC X(03).          00004110
00456          05  CASE-NSF-RETURN-CNT              PIC S9.             00004120
00475          05  CASE-LTD-TRUST-CODE              PIC X(02).          00004130
01894          05  CASE-UW-OR-IND                   PIC X(01).          00004140
01895          05  CASE-UW-OR-DATE                  PIC 9(06).          00004150
               05  FILLER REDEFINES CASE-UW-OR-DATE.                    00004160
                   10  CASE-UW-OR-YY                PIC 9(02).          00004170
                   10  CASE-UW-OR-MM                PIC 9(02).          00004180
                   10  CASE-UW-OR-DD                PIC 9(02).          00004190
01896          05  CASE-CS-FEE-COLL-THRU-DATE       PIC 9(04).          00004200
               05  FILLER REDEFINES CASE-CS-FEE-COLL-THRU-DATE.         00004210
01897              10 CASE-CS-FEE-THRU-YY           PIC 99.             00004220
01898              10 CASE-CS-FEE-THRU-MM           PIC 99.             00004230
               05  CASE-CODE3-PLAN-EXT              PIC X(01).          00004240
               05  CASE-CODE6-PLAN-EXT              PIC X(01).          00004250
01900          05  CASE-LTD-NUM-EMPS                PIC S9(3) COMP-3.   00004260
00500          05  CASE-SELECTED-ROOM-CHARGES       PIC S9(3) COMP-3.   00004270
00505          05  CASE-ANNUAL-COMM                 PIC X(01).          00004280
                   88  CASE-ANNUAL-COMMISSION     VALUE 'A' 'Y'.        00004290
                   88  CASE-NO-ANNUAL-COMMISSION  VALUE 'N' ' '.        00004300
00510          05  CASE-INFORCE-AGING-CODE          PIC X(01).          00004310
00511          05  CASE-PREMIUM-REFUND-IND          PIC X(01).          00004320
00520          05  CASE-S-I-C                       PIC X(04).          00004330
               05  CASE-PART-DATE.                                      00004340
01901              10 CASE-PART-DATE-MM             PIC 9(02).          00004350
01902              10 CASE-PART-DATE-YY             PIC 9(02).          00004360
R02701         05  CASE-DENTAL-NUM-OF-EMPS          PIC S9(5) COMP-3.   00004370
R02701         05  FILLER REDEFINES CASE-DENTAL-NUM-OF-EMPS.            00004380
R02701             10  CASE-BR-PREV-NUM-OF-EMPS     PIC S9(5) COMP-3.   00004390
00535          05  CASE-ACCT-RECONCILED             PIC X(01).          00004400
                   88  CASE-ACCT-RECON            VALUE 'Y' '1'.        00004410
00540          05  CASE-STAT-DATE                   PIC 9(04).          00004420
               05  FILLER REDEFINES CASE-STAT-DATE.                     00004430
00541              10  CASE-STAT-MM                 PIC 9(02).          00004440
00542              10  CASE-STAT-YY                 PIC 9(02).          00004450
00545          05  CASE-GENL-GRP-INFO               PIC X(08).          00004460
00550          05  CASE-PRIMARY-BROKER.                                 00004470
00555              10  CASE-BK1-SSN                 PIC X(09).          00004480
00560              10  CASE-BK1-COMM-CK-TYPE        PIC X(01).          00004490
                       88  CASE-BK1-COMM-CK       VALUE 'R' 'O'.        00004500
                       88  CASE-BK1-NO-COMM-CK    VALUE 'N' ' '.        00004510
00565              10  CASE-BK1-LICENSE-IND REDEFINES                   00004520
                       CASE-BK1-COMM-CK-TYPE        PIC X(01).          00004530
00570              10  CASE-BK1-BKR-TYPE            PIC X(01).          00004540
                       88  CASE-BK1-TYPES         VALUE '1' '3' 'W' 'A'.00004550
                       88  CASE-BK1-REGULAR       VALUE '1'.            00004560
                       88  CASE-BK1-OVERRIDE      VALUE '3'.            00004570
                       88  CASE-BK1-COMMUNICATION VALUE 'W'.            00004580
                       88  CASE-BK1-ASSIGNEE      VALUE 'A'.            00004590
00575              10  CASE-BK1-A-PERCENT           PIC 9(04) COMP.     00004600
00580              10  CASE-BK1-B-PERCENT           PIC 9(04) COMP.     00004610
00585              10  CASE-BK1-C-PERCENT           PIC 9(04) COMP.     00004620
00590          05  CASE-BROKER-2.                                       00004630
00595              10  CASE-BK2-SSN                 PIC X(09).          00004640
00600              10  CASE-BK2-COMM-CK-TYPE        PIC X(01).          00004650
                       88  CASE-BK2-COMM-CK       VALUE 'R' 'O'.        00004660
                       88  CASE-BK2-NO-COMM-CK    VALUE 'N' ' '.        00004670
00605              10  CASE-BK2-LICENSE-IND REDEFINES                   00004680
                       CASE-BK2-COMM-CK-TYPE        PIC X(01).          00004690
00610              10  CASE-BK2-BKR-TYPE            PIC X(01).          00004700
                       88  CASE-BK2-TYPES         VALUE '1' '3' 'W' 'A'.00004710
                       88  CASE-BK2-REGULAR       VALUE '1'.            00004720
                       88  CASE-BK2-OVERRIDE      VALUE '3'.            00004730
                       88  CASE-BK2-COMMUNICATION VALUE 'W'.            00004740
                       88  CASE-BK2-ASSIGNEE      VALUE 'A'.            00004750
00615              10  CASE-BK2-A-PERCENT           PIC 9(04) COMP.     00004760
00620              10  CASE-BK2-B-PERCENT           PIC 9(04) COMP.     00004770
00625              10  CASE-BK2-C-PERCENT           PIC 9(04) COMP.     00004780
00630          05  CASE-BROKER-3.                                       00004790
00635              10  CASE-BK3-SSN                 PIC X(09).          00004800
00640              10  CASE-BK3-COMM-CK-TYPE        PIC X(01).          00004810
                       88  CASE-BK3-COMM-CK       VALUE 'R' 'O'.        00004820
                       88  CASE-BK3-NO-COMM-CK    VALUE 'N' ' '.        00004830
00645              10  CASE-BK3-LICENSE-IND REDEFINES                   00004840
                       CASE-BK3-COMM-CK-TYPE        PIC X(01).          00004850
00650              10  CASE-BK3-BKR-TYPE            PIC X(01).          00004860
                       88  CASE-BK3-TYPES         VALUE '1' '3' 'W' 'A'.00004870
                       88  CASE-BK3-REGULAR       VALUE '1'.            00004880
                       88  CASE-BK3-OVERRIDE      VALUE '3'.            00004890
                       88  CASE-BK3-COMMUNICATION VALUE 'W'.            00004900
                       88  CASE-BK3-ASSIGNEE      VALUE 'A'.            00004910
00655              10  CASE-BK3-A-PERCENT           PIC 9(04) COMP.     00004920
00660              10  CASE-BK3-B-PERCENT           PIC 9(04) COMP.     00004930
00665              10  CASE-BK3-C-PERCENT           PIC 9(04) COMP.     00004940
00670          05  CASE-BROKER-4.                                       00004950
00675              10  CASE-BK4-SSN                 PIC X(09).          00004960
00680              10  CASE-BK4-COMM-CK-TYPE        PIC X(01).          00004970
                       88  CASE-BK4-COMM-CK       VALUE 'R' 'O'.        00004980
                       88  CASE-BK4-NO-COMM-CK    VALUE 'N' ' '.        00004990
00685              10  CASE-BK4-LICENSE-IND REDEFINES                   00005000
                       CASE-BK4-COMM-CK-TYPE        PIC X(01).          00005010
00690              10  CASE-BK4-BKR-TYPE            PIC X(01).          00005020
                       88  CASE-BK4-TYPES         VALUE '1' '3' 'W' 'A'.00005030
                       88  CASE-BK4-REGULAR       VALUE '1'.            00005040
                       88  CASE-BK4-OVERRIDE      VALUE '3'.            00005050
                       88  CASE-BK4-COMMUNICATION VALUE 'W'.            00005060
                       88  CASE-BK4-ASSIGNEE      VALUE 'A'.            00005070
00695              10  CASE-BK4-A-PERCENT           PIC 9(04) COMP.     00005080
00700              10  CASE-BK4-B-PERCENT           PIC 9(04) COMP.     00005090
00705              10  CASE-BK4-C-PERCENT           PIC 9(04) COMP.     00005100
00710          05  CASE-BROKER-5.                                       00005110
00715              10  CASE-BK5-SSN                 PIC X(09).          00005120
00720              10  CASE-BK5-COMM-CK-TYPE        PIC X(01).          00005130
                       88  CASE-BK5-COMM-CK       VALUE 'R' 'O'.        00005140
                       88  CASE-BK5-NO-COMM-CK    VALUE 'N' ' '.        00005150
00725              10  CASE-BK5-LICENSE-IND REDEFINES                   00005160
                       CASE-BK5-COMM-CK-TYPE        PIC X(01).          00005170
00730              10  CASE-BK5-BKR-TYPE            PIC X(01).          00005180
                       88  CASE-BK5-TYPES         VALUE '1' '3' 'W' 'A'.00005190
                       88  CASE-BK5-REGULAR       VALUE '1'.            00005200
                       88  CASE-BK5-OVERRIDE      VALUE '3'.            00005210
                       88  CASE-BK5-COMMUNICATION VALUE 'W'.            00005220
                       88  CASE-BK5-ASSIGNEE      VALUE 'A'.            00005230
00735              10  CASE-BK5-A-PERCENT           PIC 9(04) COMP.     00005240
00740              10  CASE-BK5-B-PERCENT           PIC 9(04) COMP.     00005250
00745              10  CASE-BK5-C-PERCENT           PIC 9(04) COMP.     00005260
00792          05  CASE-MED-EXP-ADJ                 PIC S9V9(4) COMP-3. 00005270
00782          05  CASE-DENT-EXP-ADJ                PIC S9V9(4) COMP-3. 00005280
00747          05  CASE-24HR-COV-PLAN               PIC X(2).           00005290
00748          05  CASE-24HR-COV-EFF-DATE           PIC 9(4).           00005300
               05  FILLER REDEFINES CASE-24HR-COV-EFF-DATE.             00005310
                 10  CASE-24HR-COV-EFF-MM           PIC 99.             00005320
                 10  CASE-24HR-COV-EFF-YY           PIC 99.             00005330
00749          05  CASE-24HR-COV-TERM-DATE          PIC 9(4).           00005340
               05  FILLER REDEFINES CASE-24HR-COV-TERM-DATE.            00005350
                 10  CASE-24HR-COV-TERM-MM          PIC 99.             00005360
                 10  CASE-24HR-COV-TERM-YY          PIC 99.             00005370
00775          05  CASE-REINSTATE-FLAG              PIC X(01).          00005380
00780          05  CASE-PREVIOUS-CARRIER.                               00005390
00785              10  CASE-PREV-CARRIER            PIC X(02).          00005400
00790              10  CASE-PREV-POLICY             PIC X(01).          00005410
00795              10  CASE-PREV-TERM               PIC 9(04).          00005420
                   10  FILLER REDEFINES CASE-PREV-TERM.                 00005430
00800                  15  CASE-PREV-TERM-MM        PIC 9(02).          00005440
                           88  CASE-PREV-TERM-MM-OK VALUE 01 THRU 12.   00005450
00810                  15  CASE-PREV-TERM-YY        PIC 9(02).          00005460
00815              10  CASE-PREV-INCEPTION          PIC 9(04).          00005470
                   10  FILLER REDEFINES CASE-PREV-INCEPTION.            00005480
00820                  15  CASE-PREV-INCEPTION-MM   PIC 9(02).          00005490
                           88  CASE-PREV-INCEP-MM-OK VALUE 01 THRU 12.  00005500
00830                  15  CASE-PREV-INCEPTION-YY   PIC 9(02).          00005510
00836          05  CASE-REINST-PREV-TERM            PIC 9(04).          00005520
00835          05  CASE-SELECTED-NUMBER-OF-DAYS     PIC S9(3) COMP-3.   00005530
00850          05  CASE-BILLING-MONTH               PIC 9(02).          00005540
00855          05  CASE-BILLING-MONTH-X                                 00005550
                                    REDEFINES CASE-BILLING-MONTH PIC XX.00005560
00860          05  CASE-HEALTH-AREA.                                    00005570
00865              10  CASE-HLTH-CARRIER            PIC X(02).          00005580
04004                  88  CASE-HLTH-CARRIER-FREQ VALUE 'AV' 'HU' 'MU'. 00005590
00870              10  CASE-BROK-CHNG-LET-REQ       PIC X(01).          00005600
00875              10  CASE-HLTH-MATERNITY          PIC X(01).          00005610
      *            10  FILLER                       PIC X(01).        * 00005620
      *            10  FILLER                       PIC X(01).        * 00005630
                   10  CASE-TERM-DD                 PIC 99.             00005640
00890              10  CASE-HLTH-OPTION             PIC X(01).          00005650
                       88  CASE-HLTH-OPTN         VALUE '0' '1' 'P'.    00005660
                       88  CASE-DBSN-PCS-OPTN     VALUE 'P'.            00005670
00895              10  CASE-HLTH-EFF-DATE           PIC 9(04).          00005680
                   10  FILLER REDEFINES CASE-HLTH-EFF-DATE.             00005690
00905                  15  CASE-HLTH-EFF-MM         PIC 99.             00005700
                           88  CASE-HLTH-EFF-MM-OK   VALUE 01 THRU 12.  00005710
00915                  15  CASE-HLTH-EFF-YY         PIC 99.             00005720
00920              10  CASE-HLTH-TERM               PIC 9(04).          00005730
                   10  FILLER REDEFINES CASE-HLTH-TERM.                 00005740
                       15  CASE-HLTH-TERM-MM        PIC 99.             00005750
                       15  CASE-HLTH-TERM-YY        PIC 99.             00005760
00925          05  CASE-LIFE-AREA.                                      00005770
00930              10  CASE-LIFE-PLAN               PIC X(01).          00005780
00935              10  CASE-LIFE-EFF-DATE           PIC 9(04).          00005790
                   10  FILLER REDEFINES CASE-LIFE-EFF-DATE.             00005800
00945                  15  CASE-LIFE-EFF-MM         PIC 99.             00005810
                           88  CASE-LIFE-EFF-MM-OK   VALUE 01 THRU 12.  00005820
00955                  15  CASE-LIFE-EFF-YY         PIC 99.             00005830
00960              10  CASE-LIFE-TERM               PIC 9(04).          00005840
                   10  FILLER REDEFINES CASE-LIFE-TERM.                 00005850
                       15  CASE-LIFE-TERM-MM        PIC 99.             00005860
                       15  CASE-LIFE-TERM-YY        PIC 99.             00005870
00965          05  CASE-CODE-3-AREA.                                    00005880
00970              10  CASE-CODE3-COVERAGE          PIC X(02).          00005890
00975              10  CASE-CODE3-PLAN              PIC X(01).          00005900
00980              10  CASE-CODE3-EFF-DATE          PIC 9(04).          00005910
                   10  FILLER REDEFINES CASE-CODE3-EFF-DATE.            00005920
00990                  15  CASE-CODE3-EFF-MM        PIC 99.             00005930
                           88  CASE-CODE3-EFF-MM-OK  VALUE 01 THRU 12.  00005940
01000                  15  CASE-CODE3-EFF-YY        PIC 99.             00005950
01005              10  CASE-CODE3-TERM              PIC 9(04).          00005960
                   10  FILLER REDEFINES CASE-CODE3-TERM.                00005970
                       15  CASE-CODE3-TERM-MM       PIC 99.             00005980
                       15  CASE-CODE3-TERM-YY       PIC 99.             00005990
01015          05  CASE-CODE-4-AREA.                                    00006000
01020              10  CASE-CODE4-COVERAGE          PIC X(02).          00006010
01025              10  CASE-CODE4-PLAN              PIC X(01).          00006020
01030              10  CASE-CODE4-EFF-DATE          PIC 9(04).          00006030
                   10  FILLER REDEFINES CASE-CODE4-EFF-DATE.            00006040
01035                  15  CASE-CODE4-EFF-MM        PIC 99.             00006050
                           88  CASE-CODE4-EFF-MM-OK VALUE 01 THRU 12.   00006060
01045                  15  CASE-CODE4-EFF-YY        PIC 99.             00006070
01050              10  CASE-CODE4-TERM              PIC 9(04).          00006080
                   10  FILLER REDEFINES CASE-CODE4-TERM.                00006090
                       15  CASE-CODE4-TERM-MM       PIC 99.             00006100
                       15  CASE-CODE4-TERM-YY       PIC 99.             00006110
01055          05  CASE-CODE-5-AREA.                                    00006120
01060              10  CASE-CODE5-COVERAGE          PIC X(02).          00006130
01065              10  CASE-CODE5-PLAN              PIC X(01).          00006140
01070              10  CASE-CODE5-EFF-DATE          PIC 9(04).          00006150
                   10  FILLER REDEFINES CASE-CODE5-EFF-DATE.            00006160
01075                  15  CASE-CODE5-EFF-MM        PIC 99.             00006170
                           88  CASE-CODE5-EFF-MM-OK VALUE 01 THRU 12.   00006180
01085                  15  CASE-CODE5-EFF-YY        PIC 99.             00006190
01090              10  CASE-CODE5-TERM              PIC 9(04).          00006200
                   10  FILLER REDEFINES CASE-CODE5-TERM.                00006210
                       15  CASE-CODE5-TERM-MM       PIC 99.             00006220
                       15  CASE-CODE5-TERM-YY       PIC 99.             00006230
01095          05  CASE-CODE-6-AREA.                                    00006240
01100              10  CASE-CODE6-COVERAGE          PIC X(02).          00006250
01105              10  CASE-CODE6-PLAN              PIC X(01).          00006260
01110              10  CASE-CODE6-EFF-DATE          PIC 9(04).          00006270
                   10  FILLER REDEFINES CASE-CODE6-EFF-DATE.            00006280
01115                  15  CASE-CODE6-EFF-MM        PIC 99.             00006290
                           88  CASE-CODE6-EFF-MM-OK VALUE 01 THRU 12.   00006300
01125                  15  CASE-CODE6-EFF-YY        PIC 99.             00006310
01130              10  CASE-CODE6-TERM              PIC 9(04).          00006320
                   10  FILLER REDEFINES CASE-CODE6-TERM.                00006330
                       15  CASE-CODE6-TERM-MM       PIC 99.             00006340
                       15  CASE-CODE6-TERM-YY       PIC 99.             00006350
01135          05  CASE-GENERAL-INFORMATION         PIC X(15).          00006360
01195          05  CASE-CHANGE-AREA.                                    00006370
01200              10  CASE-INITALS                 PIC X(02).          00006380
01205              10  CASE-CHANGE-DATE             PIC 9(06).          00006390
                   10  FILLER REDEFINES CASE-CHANGE-DATE.               00006400
01215                  15  CASE-CHANGE-MM           PIC 9(02).          00006410
                           88  CASE-CHANGE-MM-OK     VALUE 01 THRU 12.  00006420
01225                  15  CASE-CHANGE-DD           PIC 9(02).          00006430
                           88  CASE-CHANGE-DD-OK     VALUE 01 THRU 31.  00006440
01230                  15  CASE-CHANGE-YY           PIC 9(02).          00006450
01775          05  CASE-EASYFLEX-AREA.                                  00006460
01780              10  CASE-EZFLX-PLAN              PIC X(01).          00006470
01785              10  CASE-EZFLX-EFF-DATE          PIC 9(04).          00006480
                   10  FILLER REDEFINES CASE-EZFLX-EFF-DATE.            00006490
01790                  15  CASE-EZFLX-EFF-MM        PIC 99.             00006500
                           88  CASE-EZFLX-EFF-MM-OK  VALUE 01 THRU 12.  00006510
01795                  15  CASE-EZFLX-EFF-YY        PIC 99.             00006520
01800              10  CASE-EZFLX-TERM              PIC 9(04).          00006530
                   10  FILLER REDEFINES CASE-EZFLX-TERM.                00006540
01805                  15  CASE-EZFLX-TERM-MM       PIC 99.             00006550
                           88  CASE-EZFLX-TERM-MM-OK  VALUE 01 THRU 12. 00006560
01810                  15  CASE-EZFLX-TERM-YY       PIC 99.             00006570
01820              10  CASE-EASYFLEX-3              PIC X.              00006580
01830              10  CASE-EZFLX-FILLER            PIC X(05).          00006590
01300          05  CASE-ADM-FEE-COLL-THRU-DATE      PIC 9(04).          00006600
               05  FILLER REDEFINES CASE-ADM-FEE-COLL-THRU-DATE.        00006610
01305              10 CASE-ADM-FEE-THRU-YY          PIC 99.             00006620
01310              10 CASE-ADM-FEE-THRU-MM          PIC 99.             00006630
01315          05  CASE-JOINDER-COLL-THRU-DATE      PIC 9(04).          00006640
               05  FILLER REDEFINES CASE-JOINDER-COLL-THRU-DATE.        00006650
01320              10 CASE-JOINDER-THRU-YY          PIC 99.             00006660
01325              10 CASE-JOINDER-THRU-MM          PIC 99.             00006670
01330          05  CASE-MATERNITY-EFF-DATE          PIC 9(04).          00006680
               05  FILLER REDEFINES CASE-MATERNITY-EFF-DATE.            00006690
01335              10  CASE-MATERNITY-EFF-MM        PIC 99.             00006700
                       88  CASE-MATERNITY-EFF-MM-OK   VALUE 01 THRU 12. 00006710
01340              10  CASE-MATERNITY-EFF-YY        PIC 99.             00006720
01345          05  CASE-MATERNITY-TERM              PIC 9(04).          00006730
               05  FILLER REDEFINES CASE-MATERNITY-TERM.                00006740
                   10  CASE-MATERNITY-TERM-MM       PIC 99.             00006750
                   10  CASE-MATERNITY-TERM-YY       PIC 99.             00006760
01350          05  CASE-EMPS-AT-INCEPTION           PIC S9(05) COMP-3.  00006770
01355          05  CASE-2PLUS-AREA.                                     00006780
01360              10  CASE-2PLUS-CARRIER           PIC XX.             00006790
01365**********    10  CASE-2PLUS-CASE-NUM          PIC 9(06).          00006800
01365              10  CASE-2PLUS-CASE-NUM          PIC X(06).          00006810
01370          05  CASE-ALPHA10-KEY-GROUP.                              00006820
01375              10  CASE-ALPHA10-KEY             PIC X(10).          00006830
01380**********    10  CASE-ALPHA-CASE-NUM          PIC 9(6).           00006840
01380              10  CASE-ALPHA-CASE-NUM          PIC X(6).           00006850
01385          05  CASE-NEW-POLICY-GROUP.                               00006860
01390              10  CASE-HLTH-POLICY.                                00006870
                       15  CASE-HLTH-POLICY-BYTE1   PIC X.              00006880
                       15  CASE-HLTH-POLICY-BYTE2   PIC X.              00006890
01395              10  CASE-HLTH-AREA               PIC X(02).          00006900
01400              10  CASE-HLTH-LEVEL              PIC X(02).          00006910
01903          05  CASE-CEDED-IND                   PIC X(01).          00006920
01405          05  CASE-OLD-LATE-CHARGE             PIC X(01).          00006930
01406          05  CASE-OLD-LATE-FEE-NUM REDEFINES CASE-OLD-LATE-CHARGE 00006940
                                                    PIC 9.              00006950
01410          05  CASE-NEXT-HLTH-LEVEL             PIC X(02).          00006960
01415          05  CASE-RECOMPOSITE-IND             PIC X(01).          00006970
                   88  CASE-SAYS-RECOMPOSITE-RATES   VALUE 'Y'.         00006980
01420          05  CASE-MISC-CHARGES-AREA.                              00006990
01425              10 CASE-MISC-CHARGES1            PIC S9(5)V99 COMP-3.00007000
01430              10 CASE-MISC-CHARGES2            PIC S9(5)V99 COMP-3.00007010
01435              10 CASE-MISC-CHARGES3            PIC S9(5)V99 COMP-3.00007020
01440              10 CASE-MISC-CHARGES4            PIC S9(5)V99 COMP-3.00007030
01445              10 CASE-MISC-CHARGES5            PIC S9(5)V99 COMP-3.00007040
01446          05  CASE-COMPOSITE-SWITCH            PIC X.              00007050
01447          05  CASE-MAGIC-PROPOSAL-NUM          PIC 9(7).           00007060
00245          05  CASE-SINGLE-COUNT                PIC S9(05) COMP-3.  00007070
00250          05  CASE-FAMILY-COUNT                PIC S9(05) COMP-3.  00007080
00410          05  CASE-NUM-OF-ADDITIONS            PIC S9(05) COMP-3.  00007090
00415          05  CASE-NUM-OF-TERMINATIONS         PIC S9(05) COMP-3.  00007100
00417          05  CASE-BILL-FREQUENCY              PIC S9(03) COMP-3.  00007110
00419          05  CASE-LAST-BILL-DATE              PIC 9(06).          00007120
               05  FILLER REDEFINES CASE-LAST-BILL-DATE.                00007130
00421              10  CASE-LAST-BILL-YY            PIC 99.             00007140
00423              10  CASE-LAST-BILL-MM            PIC 99.             00007150
00427              10  CASE-LAST-BILL-DD            PIC 99.             00007160
00429          05  CASE-NEXT-BILL-DATE              PIC 9(06).          00007170
               05  FILLER REDEFINES CASE-NEXT-BILL-DATE.                00007180
00431              10  CASE-NEXT-BILL-YY            PIC 99.             00007190
00433              10  CASE-NEXT-BILL-MM            PIC 99.             00007200
00437              10  CASE-NEXT-BILL-DD            PIC 99.             00007210
00240          05  CASE-PCS-COUNT                   PIC S9(5) COMP-3.   00007220
01665          05  CASE-HLTH-NUM-OF-EMPS            PIC S9(5) COMP-3.   00007230
01670          05  CASE-BANK-ACCT-NUMBER            PIC 9(10).          00007240
01675          05  CASE-BANK-ID-CODE                PIC XX.             00007250
                   88  WELLS-FARGO                  VALUE 'WF'.         00007260
01680          05  CASE-EFT-FLAG                    PIC X.              00007270
                   88  CASE-PAYS-BY-EFT             VALUE 'Y'.          00007280
                   88  CASE-ARREARS                 VALUE 'A'.          00007290
                   88  CASE-HOLD                    VALUE 'H'.          00007300
                   88  CASE-DRAFT                   VALUE 'D'.          00007310
      ***                                                               00007320
01700 *        05  CASE-ASSOCIATION-AREA.                               00007330
01705 *            10  CASE-ASSOC-FEE-CODE             PIC X.           00007340
01710 *            10  CASE-ASSOC-FEE-EFF-DATE         PIC 9(04).       00007350
      *            10  FILLER REDEFINES CASE-ASSOC-FEE-EFF-DATE.        00007360
01715 *                15  CASE-ASSOC-FEE-EFF-MM       PIC 99.          00007370
01720 *                15  CASE-ASSOC-FEE-EFF-YY       PIC 99.          00007380
01725 *            10  CASE-ASSOC-FEE-TERM-DATE        PIC 9(04).       00007390
      *            10  FILLER REDEFINES CASE-ASSOC-FEE-TERM-DATE.       00007400
01730 *                15  CASE-ASSOC-FEE-TERM-MM      PIC 99.          00007410
01735 *                15  CASE-ASSOC-FEE-TERM-YY      PIC 99.          00007420
01740 *            10  CASE-ASSOC-FEE-PAID-THRU-DATE   PIC 9(04).       00007430
      *            10  FILLER REDEFINES CASE-ASSOC-FEE-PAID-THRU-DATE.  00007440
01745 *                15  CASE-ASSOC-FEE-PAID-THRU-YY PIC 99.          00007450
01750 *                15  CASE-ASSOC-FEE-PAID-THRU-MM PIC 99.          00007460
      ***                                                               00007470
      *  THE ABOVE 05 LEVEL HAS BEEN CHANGED TO THE FOLLOWING 05 LEVEL  00007480
      *  TO ACCOMODATE THE REQUIRED CHANGES FOR SECTION 125             00007490
      ***                                                               00007500
               05  CASE-SECTION-125-AREA.                               00007510
01904              10  CASE-SEC125-FEE-CODE            PIC X.           00007520
01905              10  CASE-SEC125-FEE-EFF-DATE        PIC 9(04).       00007530
                   10  FILLER REDEFINES CASE-SEC125-FEE-EFF-DATE.       00007540
                       15  CASE-SEC125-FEE-EFF-MM      PIC 99.          00007550
                       15  CASE-SEC125-FEE-EFF-YY      PIC 99.          00007560
01906              10  CASE-SEC125-FEE-TERM-DATE       PIC 9(04).       00007570
                   10  FILLER REDEFINES CASE-SEC125-FEE-TERM-DATE.      00007580
                       15  CASE-SEC125-FEE-TERM-MM     PIC 99.          00007590
                       15  CASE-SEC125-FEE-TERM-YY     PIC 99.          00007600
01907              10  CASE-SEC125-FEE-PAID-THRU-DATE  PIC 9(04).       00007610
                   10  FILLER REDEFINES CASE-SEC125-FEE-PAID-THRU-DATE. 00007620
                       15  CASE-SEC125-FEE-PAID-THRU-YY PIC 99.         00007630
                       15  CASE-SEC125-FEE-PAID-THRU-MM PIC 99.         00007640
01755          05  CASE-TIER-NUMBER                 PIC 9.              00007650
01760          05  CASE-ZIP-PLUS-4                  PIC 9(4).           00007660
01765          05  CASE-HLTH-OPT-EFF-DATE           PIC 9(4).           00007670
01765          05  FILLER REDEFINES CASE-HLTH-OPT-EFF-DATE.             00007680
01765              10  CASE-HLTH-OPT-EFF-MM         PIC 9(2).           00007690
01765              10  CASE-HLTH-OPT-EFF-YY         PIC 9(2).           00007700
01770          05  CASE-HLTH-OPT-TERM-DATE          PIC 9(4).           00007710
01770          05  FILLER REDEFINES CASE-HLTH-OPT-TERM-DATE.            00007720
01765              10  CASE-HLTH-OPT-TERM-MM        PIC 9(2).           00007730
01765              10  CASE-HLTH-OPT-TERM-YY        PIC 9(2).           00007740
01771**********05  FILLER                           PIC X(9).           00007750
01913          05  CASE-LAPSE-RULE                  PIC X(3).           00007760
               05  CASE-PAYMENT-OPTION              PIC X.              00007770
               05  FILLER                           PIC X(5).           00007780
01450          05  CASE-UNCOLL-HCC-CHARGES          PIC S9(7)V99 COMP-3.00007790
01860          05  CASE-RATING-REFERENCE-DATE.                          00007800
                   10  CASE-RATING-REFERENCE-YY     PIC X(2).           00007810
                   10  CASE-RATING-REFERENCE-MM     PIC X(2).           00007820
                   10  CASE-RATING-REFERENCE-DD     PIC X(2).           00007830
01878          05  CASE-HOW-BILLED                  PIC X(02).          00007840
                   88  CASE-DIRECT-BILL             VALUE '01'.         00007850
                   88  CASE-EFT                     VALUE '02'.         00007860
                   88  CASE-USE-ALT-BILL-ADDR       VALUE '03'.         00007870
                   88  CASE-CREDIT-CARD-CHARGE      VALUE '04'.         00007880
                   88  CASE-LIST-BILL               VALUE '05'.         00007890
                   88  CASE-CREDIT-CARD-LIST-BILL   VALUE '06'.         00007900
                   88  CASE-LIST-BILL-EFT           VALUE '07'.         00007910
                   88  CASE-PLATYPUS-DIRECT-BILL    VALUE 'P5'.         00007920
                   88  CASE-PLATYPUS-CREDIT-CARD    VALUE 'P6'.         00007930
                   88  CASE-PLATYPUS-EFT            VALUE 'P7'.         00007940
01875          05  CASE-CREDIT-CARD-CODE            PIC X(01).          00007950
                   88  CASE-AMERICAN-EXPRESS        VALUE '1'.          00007960
                   88  CASE-MASTERCARD              VALUE '2'.          00007970
                   88  CASE-VISA                    VALUE '3'.          00007980
01877          05  CASE-CREDIT-CARD-NUMBER          PIC X(20).          00007990
01876          05  CASE-CREDIT-CARD-EXP-DATE.                           00008000
                   10  CASE-CREDIT-CARD-EXP-YY      PIC 9(02).          00008010
                   10  CASE-CREDIT-CARD-EXP-MM      PIC 9(02).          00008020
01891*******   05  CASE-PREV-CASE-NUM               PIC 9(06).          00008030
01891          05  CASE-PREV-CASE-NUM               PIC X(06).          00008040
01892          05  CASE-PRODUCT-LINE                PIC X(03).          00008050
01580          05  CASE-SITE-CODE                   PIC X(2).           00008060
01581          05  CASE-AREA-CHANGE                 PIC XX.             00008070
01582          05  CASE-CARRIER-ALTERNATE-KEY.                          00008080
01583              10  CASE-CARRIER-ALT-KEY         PIC XX.             00008090
01584*******       10  CASE-CARRALT-CASE-NUM        PIC 9(06).          00008100
01584              10  CASE-CARRALT-CASE-NUM        PIC X(06).          00008110
01590          05  CASE-CLMS-PAID-THRU-DATE         PIC 9(06).          00008120
               05  FILLER REDEFINES CASE-CLMS-PAID-THRU-DATE.           00008130
01595              10  CASE-CLMS-PAID-DATE-MM       PIC 99.             00008140
                       88  CASE-CLMS-PAID-MM-OK      VALUE 01 THRU 12.  00008150
01605              10  CASE-CLMS-PAID-DATE-DD       PIC 99.             00008160
                       88  CASE-CLMS-PAID-DD-OK      VALUE 01 THRU 31.  00008170
01610              10  CASE-CLMS-PAID-DATE-YY       PIC 99.             00008180
01865          05  CASE-NSF-INCR-CHECKS-COUNT       PIC S9.             00008190
01855          05  CASE-DUNS-NUM                    PIC S9(09)  COMP-3. 00008200
01630          05  CASE-KEY.                                            00008210
01635              10  CASE-CASE-NUM                PIC X(6).           00008220
