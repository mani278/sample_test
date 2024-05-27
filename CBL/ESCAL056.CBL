000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID. ESCAL056.
000300*AUTHORS.    CMS.
000400*       EFFECTIVE APRIL 1, 2005
000500******************************************************************
000600* 4/06/05 - Allow provider type '05' for pediatric hosp          *
000700*         - To be effective with the next release                *
000800*         - Changed in 0100-INITIAL-ROUTINE with provider        *
000900*           type '40'                                            *
001000* 1/01/06 - NEW CBSA TABLE FOR CY2006                            *
001100*         - UPDATE RATES WITH 1.6% INCREASE                      *
001200* 1/18/07 - THIS PROGRAM WAS ENHANCED IN ORDER TO VERIFY MORE    *
001300*           INFORMATION (VIA THE MASTER DRIVER).  VERY CLOSE     *
001400*           ATTENTION HAS BEEN PAID TO ENSURE THAT ALL VARIABLES *
001500*           THAT CHANGE FROM YEAR TO YEAR CONTINUE TO HAVE THE   *
001600*           SAME VALUES AS IN THE ORIGINAL VERSION.  NO CHANGES  *
001700*           WERE MADE IN THE CORE LOGIC OF THE PROCEDURE DIVISION*
001800*           WHERE THE ACTUAL CALCULATIONS OCCUR.  THOROUGH       *
001900*           TESTING USING THE ORIGINAL SET OF TEST CASES WAS MADE*
002000*           AND THERE ARE ABSOLUTELY NO DIFFERENCES IN CALCULATED*
002100*           AMOUNTS.  THE ORIGINAL CODE THAT WAS SENT OUT TO THE *
002200*           FISCAL INTERMEDIARIES IN 2006 REMAINS INTACT UNDER   *
002300*           THE MU00 ACCOUNT.                                    *
002400* 1/18/07 - MSA-CBSA BLEND PERCENT SET AT 100% MSA 0% CBSA       *
002500*         - ADDITIONAL VARIABLES WERE CREATED IN ORDER TO MAKE   *
002600*           CHANGING VALUES EASIER (IN WORKING STORAGE RATHER    *
002700*           THAN IN THE PROCEDURE DIVISION)                      *
002800*         - THIS PROGRAM NOW REFLECTS ENHANCEMENTS MADE SO THAT  *
002900*           TESTING OF THE CODE DOES NOT REQUIRE COMMENTING      *
003000*           IN/OUT LINES OF CODE.  IN ADDITION CALCULATED        *
003100*           VARIABLES ARE NOW PASSED BACK WHEN TEST CASES ARE    *
003200*           ENCOUNTERED IN ORDER FOR THE MASTER DRIVER TO PRINT  *
003300*           MORE INFORMATION ABOUT WHAT WENT ON IN THIS PROGRAM  *
003400******************************************************************
003500 DATE-COMPILED.
003600 ENVIRONMENT DIVISION.
003700 CONFIGURATION SECTION.
003800 SOURCE-COMPUTER.            IBM-Z990.
003900 OBJECT-COMPUTER.            ITTY-BITTY-MACHINE-CORPORATION.
004000 INPUT-OUTPUT  SECTION.
004100 FILE-CONTROL.
004200
004300 DATA DIVISION.
004400 FILE SECTION.
004500
004600 WORKING-STORAGE SECTION.
004700 01  W-STORAGE-REF                  PIC X(46)  VALUE
004800     'ESCAL056      - W O R K I N G   S T O R A G E'.
004900
005000 01  CAL-VERSION                    PIC X(05)  VALUE 'C05.6'.
005100
005200 01  DISPLAY-LINE-MEASUREMENT.
005300     05  FILLER                     PIC X(50) VALUE
005400         '....:...10....:...20....:...30....:...40....:...50'.
005500     05  FILLER                     PIC X(50) VALUE
005600         '....:...60....:...70....:...80....:...90....:..100'.
005700     05  FILLER                     PIC X(20) VALUE
005800         '....:..110....:..120'.
005900
006000 01  PRINT-LINE-MEASUREMENT.
006100     05  FILLER                     PIC X(51) VALUE
006200         'X....:...10....:...20....:...30....:...40....:...50'.
006300     05  FILLER                     PIC X(50) VALUE
006400         '....:...60....:...70....:...80....:...90....:..100'.
006500     05  FILLER                     PIC X(32) VALUE
006600         '....:..110....:..120....:..130..'.
006700
006800 01  HOLD-PPS-COMPONENTS.
006900     05  H-PYMT-AMT                 PIC 9(07)V9(02).
007000     05  H-WAGE-ADJ-PYMT-AMT        PIC 9(07)V9(02).
007100     05  H-FIXED-LOSS-AMT           PIC 9(07)V9(02).
007200     05  H-ESRD-FAC-RATE            PIC 9(07)V9(02).
007300     05  H-PATIENT-AGE              PIC 9(03).
007400     05  H-AGE-FACTOR               PIC 9(01)V9(03).
007500     05  H-BSA-FACTOR               PIC 9(01)V9(04).
007600     05  H-BMI-FACTOR               PIC 9(01)V9(04).
007700     05  H-BSA                      PIC 9(03)V9(04).
007800     05  H-BMI                      PIC 9(03)V9(04).
007900     05  H-DRUG-ADDON               PIC 9(01)V9(04).
008000
008100*   THE FOLLOWING THREE VARIABLES WILL CHANGE FROM YEAR TO YEAR
008200 01  DRUG-ADDON                     PIC 9(01)V9(04) VALUE 1.0870.
008300
008400*   THE NEXT TWO PERCENTAGES MUST ADD UP TO 1 (I.E. 100%)
008500*   THEY WILL CONTINUE TO CHANGE UNTIL 2009 WHEN CBSA WILL BE 1.00
008600 01  MSA-BLEND-PCT                  PIC 9(01)V9(02) VALUE 1.00.
008700 01  CBSA-BLEND-PCT                 PIC 9(01)V9(02) VALUE 0.00.
008800
008900*  CONSTANTS AREA
009000*   THE NEXT TWO PERCENTAGES MUST ADD UP TO 1 (I.E. 100%)
009100 01  NAT-LABOR-PCT                  PIC 9(01)V9(05) VALUE 0.53711.
009200 01  NAT-NONLABOR-PCT               PIC 9(01)V9(05) VALUE 0.46289.
009300
009400 01  HEMO-PERI-CCPD-AMT             PIC 9(02)       VALUE 20.
009500 01  CAPD-AMT                       PIC 9(02)       VALUE 12.
009600 01  CAPD-OR-CCPD-FACTOR            PIC 9(01)V9(06) VALUE
009700                                                       0.428571.
009800
009900*  THE FOLLOWING NUMBER THAT IS LOADED INTO THE PAYMENT EQUATION
010000*  IS MEANT TO BUDGET NEUTRALIZE CHANGES IN THE CASE MIX INDEX
010100*  AND   --DOES NOT CHANGE--
010200 01  CASE-MIX-BDGT-NEUT-FACTOR      PIC 9(01)V9(04) VALUE 0.9116.
010300
010400
010500******************************************************************
010600*                                                                *
010700*   ******** POSSIBLE RETURN CODES FROM THIS PROGRAM ********    *
010800*                                                                *
010900*    ****  PPS-RTC 00-49 = BILL PAYMENT INFORMATION CODES        *
011000*                                                                *
011100*            00 = ESRD PPS PAYMENT CALCULATED                    *
011200*                                                                *
011300*    ****  PPS-RTC 50-99 = WHY THE BILL WAS NOT PAID             *
011400*                                                                *
011500*            52 = PROVIDER TYPE NOT = '40' OR '41'               *
011600*            53 = SPECIAL PAYMENT INDICATOR NOT = '1' OR BLANK   *
011700*            54 = DATE OF BIRTH  NOT NUMERIC OR = ZERO           *
011800*            55 = PATIENT WEIGHT NOT NUMERIC OR = ZERO           *
011900*            56 = PATIENT HEIGHT NOT NUMERIC OR = ZERO           *
012000*            57 = REVENUE CENTER CODE NOT IN RANGE               *
012100*            58 = CONDITION CODE NOT = '73' OR '74' OR BLANK     *
012200*            71 = EXCEEDS MAXIMUM HEIGHT ALLOWANCE               *
012300*            72 = EXCEEDS MAXIMUM WEIGHT ALLOWANCE               *
012400******************************************************************
012500/
012600 LINKAGE SECTION.
012700 COPY BILLCPY.
012800*COPY "BILLCPY.CPY".
012900/
013000 COPY WAGECPY.
013100*COPY "WAGECPY.CPY".
013200/
013300 PROCEDURE DIVISION  USING BILL-NEW-DATA
013400                           PPS-DATA-ALL
013500                           WAGE-NEW-RATE-RECORD.
013600
013700******************************************************************
013800* THERE ARE VARIOUS WAYS TO COMPUTE A FINAL DOLLAR AMOUNT.  THE  *
013900* METHOD USED IN THIS PROGRAM IS TO USE ROUNDED INTERMEDIATE     *
014000* VARIABLES.  THIS WAS DONE TO SIMPLIFY THE CALCULATIONS SO THAT *
014100* WHEN SOMETHING GOES AWRY, ONE IS NOT LEFT WONDERING WHERE IN   *
014200* A VAST COMPUTE STATEMENT, THINGS HAVE GONE AWRY.  THE METHOD   *
014300* UTILIZED HERE HAS BEEN APPROVED BY WIL GEHNE AND JOEY BRYSON   *
014400* BOTH OF WHOM WORK IN THE DIVISION OF INSTITUTIONAL CLAIMS      *
014500* PROCESSING (DICP).                                             *
014600*                                                                *
014700*                                                                *
014800*    PROCESSING:                                                 *
014900*        A. WILL PROCESS CLAIMS BASED ON AGE/HEIGHT/WEIGHT       *
015000*        B. INITIALIZE ESCAL HOLD VARIABLES.                     *
015100*        C. EDIT THE DATA PASSED FROM THE CLAIM BEFORE           *
015200*           ATTEMPTING TO CALCULATE PPS. IF THIS CLAIM           *
015300*           CANNOT BE PROCESSED, SET A RETURN CODE AND           *
015400*           GOBACK.                                              *
015500*        D. ASSEMBLE PRICING COMPONENTS.                         *
015600*        E. CALCULATE THE PRICE.                                 *
015700******************************************************************
015800
015900 0000-MAINLINE-CONTROL.
016000
016100     PERFORM 0100-INITIAL-ROUTINE.
016200
016300     IF PPS-RTC = 00
016400        PERFORM 1000-EDIT-THE-BILL-INFO
016500     END-IF.
016600
016700     IF PPS-RTC = 00
016800        PERFORM 2000-ASSEMBLE-PPS-VARIABLES
016900        PERFORM 3000-CALC-PAYMENT
017000     END-IF.
017100
017200     PERFORM 9000-MOVE-RESULTS.
017300
017400     GOBACK.
017500/
017600 0100-INITIAL-ROUTINE.
017700
017800******************************************************************
017900**   NEW PAYMENT RATES FOR NEW LEGISLATION                      **
018000******************************************************************
018100     INITIALIZE PPS-DATA-ALL.
018200     INITIALIZE BILL-DATA-TEST.
018300     INITIALIZE HOLD-PPS-COMPONENTS.
018400     MOVE ZEROS                        TO PPS-RTC.
018500     MOVE CAL-VERSION                  TO PPS-CALC-VERS-CD.
018600
018700* PROVIDER TYPE '40' IS A HOSPITAL BASED ESRD FACILITY
018800     IF P-PROV-TYPE = '40'
018900        MOVE ZERO                      TO PPS-NAT-LABOR-PCT
019000        MOVE ZERO                      TO PPS-NAT-NONLABOR-PCT
019100        MOVE W-NEW-RATE1-RECORD        TO PPS-WAGE-ADJ-RATE
019200        MOVE W-NEW-RATE1-RECORD        TO MSA-WAGE-AMT
019300     ELSE
019400* PROVIDER TYPE '41' IS AN INDEPENDENT ESRD FACILITY
019500        IF P-PROV-TYPE = '41'
019600           MOVE ZERO                   TO PPS-NAT-LABOR-PCT
019700           MOVE ZERO                   TO PPS-NAT-NONLABOR-PCT
019800           MOVE W-NEW-RATE2-RECORD     TO PPS-WAGE-ADJ-RATE
019900           MOVE W-NEW-RATE2-RECORD     TO MSA-WAGE-AMT
020000        ELSE
020100           MOVE 52                     TO PPS-RTC
020200           MOVE ZERO                   TO PPS-WAGE-ADJ-RATE
020300        END-IF
020400     END-IF.
020500
020600     MOVE PPS-WAGE-ADJ-RATE            TO MSA-ADJ-YEAR-AMT.
020700
020800******************************************************************
020900**  NEW DRUG ADD-ON FOR NEW LEGISLATION                         **
021000******************************************************************
021100
021200     MOVE CASE-MIX-BDGT-NEUT-FACTOR    TO PPS-BDGT-NEUT-RATE.
021300     MOVE DRUG-ADDON                   TO H-DRUG-ADDON.
021400/
021500******************************************************************
021600***  BILL DATA EDITS IF ANY FAIL SET PPS-RTC                   ***
021700***  AND DO NOT ATTEMPT TO PRICE.                              ***
021800******************************************************************
021900 1000-EDIT-THE-BILL-INFO.
022000
022100     IF PPS-RTC = 00
022200        IF P-SPEC-PYMT-IND NOT = '1' AND ' '
022300           MOVE 53                     TO PPS-RTC
022400        END-IF
022500     END-IF.
022600
022700     IF PPS-RTC = 00
022800        IF (B-DOB-DATE = ZERO) OR (B-DOB-DATE NOT NUMERIC)
022900           MOVE 54                     TO PPS-RTC
023000        END-IF
023100     END-IF.
023200
023300     IF PPS-RTC = 00
023400        IF (B-PATIENT-WGT = 0) OR (B-PATIENT-WGT NOT NUMERIC)
023500           MOVE 55                     TO PPS-RTC
023600        END-IF
023700     END-IF.
023800
023900     IF PPS-RTC = 00
024000        IF (B-PATIENT-HGT = 0) OR (B-PATIENT-HGT NOT NUMERIC)
024100           MOVE 56                     TO PPS-RTC
024200        END-IF
024300     END-IF.
024400
024500     IF PPS-RTC = 00
024600        IF B-REV-CODE  = '0821' OR '0831' OR '0841' OR '0851'
024700                                OR '0880' OR '0881'
024800           NEXT SENTENCE
024900        ELSE
025000           MOVE 57                     TO PPS-RTC
025100        END-IF
025200     END-IF.
025300
025400     IF PPS-RTC = 00
025500        IF B-COND-CODE NOT = '73' AND '74' AND '  '
025600           MOVE 58                     TO PPS-RTC
025700        END-IF
025800     END-IF.
025900
026000     IF PPS-RTC = 00
026100        IF B-PATIENT-HGT > 300.00
026200           MOVE 71                     TO PPS-RTC
026300        END-IF
026400     END-IF.
026500
026600     IF PPS-RTC = 00
026700        IF B-PATIENT-WGT > 500.00
026800           MOVE 72                     TO PPS-RTC
026900        END-IF
027000     END-IF.
027100
027200     IF PPS-RTC = 00
027300        PERFORM 1200-CALC-AGE
027400     END-IF.
027500
027600
027700 1200-CALC-AGE.
027800******************************************************************
027900***  CALCULATE PATIENT AGE                                     ***
028000******************************************************************
028100
028200     COMPUTE H-PATIENT-AGE = B-THRU-CCYY - B-DOB-CCYY.
028300
028400     IF B-DOB-MM > B-THRU-MM
028500        COMPUTE H-PATIENT-AGE = H-PATIENT-AGE - 1
028600     END-IF.
028700
028800******************************************************************
028900***  SET AGE ADJUSTMENT FACTOR                                 ***
029000******************************************************************
029100
029200     IF H-PATIENT-AGE < 18
029300        MOVE 1.620                     TO H-AGE-FACTOR
029400     ELSE
029500        IF H-PATIENT-AGE > 17 AND H-PATIENT-AGE < 45
029600           MOVE 1.223                  TO H-AGE-FACTOR
029700        ELSE
029800           IF H-PATIENT-AGE > 44 AND H-PATIENT-AGE < 60
029900              MOVE 1.055               TO H-AGE-FACTOR
030000           ELSE
030100              IF H-PATIENT-AGE > 59 AND H-PATIENT-AGE < 70
030200                 MOVE 1.000            TO H-AGE-FACTOR
030300              ELSE
030400                 IF H-PATIENT-AGE > 69 AND H-PATIENT-AGE < 80
030500                    MOVE 1.094         TO H-AGE-FACTOR
030600                 ELSE
030700                    IF H-PATIENT-AGE > 79
030800                       MOVE 1.174      TO H-AGE-FACTOR
030900                    END-IF
031000                 END-IF
031100              END-IF
031200           END-IF
031300        END-IF
031400     END-IF.
