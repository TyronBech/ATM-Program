      ******************************************************************
      * Author: ENCINA, JOHN REIGN M.
      * Date: 11-11-2021
      * Purpose: SEQ-MAIN-MENU
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SEQ-MAIN-MENU.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
      * MAIN-MENU VARIABLES
       01 MAIN-CHOICE PIC 99.
       01 MAIN-PICK PIC -Z9.
       01 ENT PIC X.

      * SDPQ VARIABLES
       01 Num1 PIC 9(3).
       01 Num2 PIC 9(3).
       01 Num1-I PIC -Z(2)9.
       01 Num2-I PIC -Z(2)9.
       01 Total PIC 9(3).
       01 TOTALR PIC -ZZ9.
       01 Sample PIC 9(2).

      * CIRCLE VARIABLE
       01 RADIUS PIC 9(3)V99.
       01 RADIUS-I PIC -Z(2)9V99.
       01 AREA-C PIC 9(6)V99.
       01 CIRCUM PIC 9(6)V99.

       01 AREA-C-R PIC -Z(5)9.99.
       01 CIRCUM-R PIC -Z(5)9.99.

      * SWAP VARIABLE
       01 SNUM1 PIC 9(2).
       01 SNUM2 PIC 9(2).
       01 SNUM1-I PIC -Z9.
       01 SNUM2-I PIC -Z9.
       01 SNUM1-R PIC -Z9.
       01 SNUM2-R PIC -Z9.
       01 STEMP-VAR PIC 9(2).

      * TEMP VARIABLE
       01 CEL PIC S9(4)V9(2).
       01 CEL-I PIC -Z(3)V9(2).
       01 FAH PIC S9(4)V9(2).
       01 FAHR PIC -ZZZ9.9(2).

      * TOTAL-SALES VARIABLE
        01 FN PIC A(10).
       01 MN PIC A(10).
       01 LN PIC A(10).
       01 POS PIC A(10).
       01 IDNUM PIC -Z(9)9.

       01 NO-OF-HOURS PIC 9(3).
       01 RATE-PER-HOUR PIC 9(5).
       01 DEDUCTION PIC 9(5).
       01 NO-OF-HOURS-I PIC -Z(2)9.
       01 RATE-PER-HOUR-I PIC -Z(4)9.
       01 DEDUCTION-I PIC -Z(4)9.

       01 GROSS-PAY PIC 9(10).
       01 GROSS-PAY-REPLACER PIC -ZZZ,ZZZ,ZZ9.
       01 NET-PAY PIC 9(10).
       01 NET-PAY-REPLACER PIC -ZZZ,ZZZ,ZZ9.

      * AVE-SEM VARIABLES
       01 Prelim PIC 999.
       01 Midterm PIC 999.
       01 Finals PIC 999.
       01 Prelim-I PIC -ZZ9.
       01 Midterm-I PIC -ZZ9.
       01 Finals-I PIC -ZZ9.
       01 Average PIC 999V99.
       01 AverageR PIC -ZZ9.99.

      * RECTANGLE VARIABLE
       01 PERIMETER PIC 9(5)V99.
       01 AREA-RECTANGLE PIC 9(5)V99.
       01 PERIMETER-R PIC -Z(4)9.99.
       01 AREA-RECTANGLE-R PIC -Z(4)9.99.
       01 SIDE-A PIC 9(4)V99.
       01 SIDE-B PIC 9(4)V99.
       01 SIDE-A-I PIC -Z(3)V99.
       01 SIDE-B-I PIC -Z(3)V99.

      * CHANGE-CALCULATOR VARIABLE
       01 orderNUM PIC 99.
       01 amountORDER PIC 999V99.
       01 orderNUM-I PIC -Z9.
       01 amountORDER-I PIC -ZZ9V99.
       01 totalORDER PIC 999V99.
       01 totalORDER-R PIC -ZZ9.99.
       01 amountTENDER PIC 9999.
       01 amountTENDER-I PIC -ZZZ9.
       01 change PIC 999V99.
       01 change-R PIC -ZZ9.99.

      * QUADRATIC VARIABLE
       01 A PIC S9(2)V99.
       01 B PIC S9(2)V99.
       01 C PIC S9(2)V99.
       01 A-I PIC XXX.
       01 B-I PIC XXX.
       01 C-I PIC XXX.
       01 DISCRIM PIC S9(3)V9(2).
       01 X1 PIC S9(3)V9(2).
       01 X2 PIC S9(3)V9(2).
       01 tempVAR1 PIC S9(3)V9(2).
       01 tempVAR2 PIC S9(3)V9(2).
       01 X1-R PIC -ZZ9.9(2).
       01 X2-R PIC -ZZ9.9(2).
       01 tempVAR1-R PIC -ZZ9.9(2).
       01 tempVAR2-R PIC -ZZ9.9(2).
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM UNTIL MAIN-CHOICE = 11
            DISPLAY " " ERASE SCREEN
            DISPLAY "-- SEQUENCE MAIN MENU --" AT 0428 BLINK
            DISPLAY "1 - HELLO WORLD" AT 0618
            DISPLAY "2 - SDPQ" AT 0718
            DISPLAY "3 - AREA|CIRCUMFERENCE OF A CIRCLE" AT 0818
            DISPLAY "4 - SWAP NUMBERS" AT 0918
            DISPLAY "5 - CELSIUS TO FAHRENHEIT" AT 1018
            DISPLAY "6 - EMPLOYEE TOTAL SALES" AT 1118
            DISPLAY "7 - AVERAGE OF PRELIM/MIDTERM/FINALS" AT 1218
            DISPLAY "8 - PERIMETER OF A RECTANGLE" AT 1318
            DISPLAY "9 - CHANGE COMPUTER" AT 1418
            DISPLAY "10 - QUADRATIC EQUATION CALCULATOR" AT 1518
            DISPLAY "11 - EXIT" AT 1618

            DISPLAY "ENTER CHOICE:" AT 1818
            ACCEPT MAIN-PICK AT 1832

            MOVE MAIN-PICK TO MAIN-CHOICE
            EVALUATE MAIN-CHOICE
               WHEN 1
                   PERFORM HELLO-WORLD
                   PERFORM ENTER
               WHEN 2
                   PERFORM SDPQ
                   PERFORM ENTER
               WHEN 3
                   PERFORM CIRCLE
                   PERFORM ENTER
               WHEN 4
                   PERFORM SWAP
                   PERFORM ENTER
               WHEN 5
                   PERFORM TEMP
                   PERFORM ENTER
               WHEN 6
                   PERFORM TOTAL-SALES
                   PERFORM ENTER
               WHEN 7
                   PERFORM AVE-SEM
                   PERFORM ENTER
               WHEN 8
                   PERFORM RECTANGLE
                   PERFORM ENTER
               WHEN 9
                   PERFORM CHANGE-CALCULATOR
                   PERFORM ENTER
               WHEN 10
                   PERFORM QUADRATIC
                   PERFORM ENTER
            END-EVALUATE
            END-PERFORM
            STOP RUN.

       ENTER.
           DISPLAY "Press ""Enter"" to continue" AT 3001
           ACCEPT ENT AT 3026.

       HELLO-WORLD.
           DISPLAY " " ERASE SCREEN
           DISPLAY "HELLO, WORLD!" AT 1534.

       SDPQ.
           DISPLAY " " ERASE SCREEN
           DISPLAY "SUM, DIFFERENCE, PRODUCT, & QUOTIENT" AT 0422

           DISPLAY "ENTER THE FIRST NUMBER: " AT 0620
           ACCEPT Num1-I AT 0644
           DISPLAY "ENTER THE SECOND NUMBER: " AT 0720
           ACCEPT Num2-I AT 0745

           MOVE Num2-I TO Num2
           MOVE Num1-I TO Num1

           DISPLAY "USING COMPUTE EXPRESSION" AT 0920
           COMPUTE Total = Num1 + Num2
           MOVE Total TO TOTALR
           DISPLAY "SUM: " AT 1021 TOTALR AT 1027

           COMPUTE Total = Num1 - Num2
           MOVE Total TO TOTALR
           DISPLAY "DIFF: " AT 1121 TOTALR AT 1127

           COMPUTE Total = Num1 * Num2
           MOVE Total TO TOTALR
           DISPLAY "PRO: " AT 1221 TOTALR AT 1227

           COMPUTE Total = Num1 / Num2
           MOVE Total TO TOTALR
           DISPLAY "QUO: " AT 1321 TOTALR AT 1327

           DISPLAY "USING ADD/SUBTRACT/MULTIPLY/DIVIDE EXPRESSION"
           AT 1520
           ADD Num1, Num2 GIVING Total
           MOVE Total TO TOTALR
           DISPLAY "SUM: " AT 1621 TOTALR AT 1627

           SUBTRACT Num1 FROM Num2 GIVING Total
           MOVE Total TO TOTALR
           DISPLAY "DIFF: " AT 1721 TOTALR AT 1727

           MULTIPLY Num1 BY Num2 GIVING Total
           MOVE Total TO TOTALR
           DISPLAY "PRO: " AT 1821 TOTALR AT 1827

           DIVIDE Num2 INTO Num1 GIVING Total
           MOVE Total TO TOTALR
           DISPLAY "QUO: " AT 1921 TOTALR AT 1927.

       CIRCLE.
           DISPLAY " " ERASE SCREEN
           DISPLAY "AREA AND CIRCUMFERENCE OF CIRCLE" AT 0424.

           DISPLAY "ENTER RADIUS: " AT 0618
           ACCEPT RADIUS-I AT 0632
           MOVE RADIUS-I TO RADIUS

           COMPUTE AREA-C = 3.14 * RADIUS * RADIUS
           COMPUTE CIRCUM = 2 * 3.14 * RADIUS

           MOVE AREA-C TO AREA-C-R
           MOVE CIRCUM TO CIRCUM-R

           DISPLAY "AREA: " AT 0818 AREA-C-R AT 0825
           DISPLAY "CIRCUMFERENCE: " AT 0918 CIRCUM-R AT 0932.

       SWAP.
           DISPLAY " " ERASE SCREEN
           DISPLAY "SWAPPING NUMBERS" AT 0432
            DISPLAY "ENTER NUM1: " AT 0610
            ACCEPT SNUM1-I AT 0623
            DISPLAY "ENTER NUM2: " AT 0710
            ACCEPT SNUM2-I AT 0723

            MOVE SNUM1-I TO SNUM1
            MOVE SNUM2-I TO SNUM2

            MOVE SNUM2 TO STEMP-VAR
            MOVE SNUM1 TO SNUM2
            MOVE STEMP-VAR TO SNUM1

            MOVE SNUM1 TO SNUM1-R
            MOVE SNUM2 TO SNUM2-R

            DISPLAY "NUM1: " AT 0910 SNUM1-R AT 0916
            DISPLAY "NUM2: " AT 1010 SNUM2-R AT 1016.

       TEMP.
           DISPLAY " " ERASE SCREEN
           DISPLAY "CELSIUS TO FAHRENHEIT" AT 0429
            DISPLAY "ENTER CELCIUS: " AT 0620
            ACCEPT CEL-I AT 0636
            MOVE CEL-I TO CEL

            COMPUTE FAH= (CEL * 9) / 5 + 32;
            MOVE FAH TO FAHR.

            DISPLAY "FAHRENHEIT: " AT 0820 FAHR AT 0833.

       TOTAL-SALES.
           DISPLAY " " ERASE SCREEN
           DISPLAY "EMPLOYEE TOTAL SALES" AT 0430
            DISPLAY "ENTER FIRST NAME: " AT 0510
            ACCEPT FN AT 0529
            DISPLAY "ENTER MIDDLE NAME: " AT 0610
            ACCEPT MN AT 0630
            DISPLAY "ENTER LAST NAME: " AT 0710
            ACCEPT LN AT 0728
            DISPLAY "ENTER POSITION: " AT 0810
            ACCEPT POS AT 0827
            DISPLAY "ENTER ID NUMBER: " AT 0910
            ACCEPT IDNUM AT 0928
            DISPLAY "ENTER NO. OF HOURS: " AT 1010
            ACCEPT NO-OF-HOURS-I AT 1031
            DISPLAY "ENTER RATE/HOUR: " AT 1110
            ACCEPT RATE-PER-HOUR-I AT 1128
            DISPLAY "ENTER DEDUCTION: " AT 1210
            ACCEPT DEDUCTION-I AT 1228

            MOVE NO-OF-HOURS-I TO NO-OF-HOURS
            MOVE RATE-PER-HOUR-I TO RATE-PER-HOUR
            MOVE DEDUCTION-I TO DEDUCTION

            COMPUTE  GROSS-PAY = NO-OF-HOURS * RATE-PER-HOUR
            COMPUTE NET-PAY = GROSS-PAY - DEDUCTION

            MOVE GROSS-PAY TO GROSS-PAY-REPLACER
            MOVE NET-PAY TO NET-PAY-REPLACER

            DISPLAY "GROSS PAY: Php " AT 1410 GROSS-PAY-REPLACER AT 1425
            DISPLAY "NET PAY: Php " AT 1510 NET-PAY-REPLACER AT 1525.

       AVE-SEM.
           DISPLAY " " ERASE SCREEN
           DISPLAY "AVERAGE OF PRELIM/MIDTERM/FINALS" AT 0224

           DISPLAY "ENTER PRELIM: " AT 0410
           ACCEPT Prelim-I AT 0425
           DISPLAY "ENTER MIDTERM: " AT 0510
           ACCEPT Midterm-I AT 0526
           DISPLAY "ENTER FINALS: " AT 0610
           ACCEPT Finals-I AT 0625

           MOVE Prelim-I TO Prelim
           MOVE Midterm-I TO Midterm
           MOVE Finals-I TO Finals

           COMPUTE Average = (Prelim + Midterm + Finals) / 3
           MOVE AVERAGE TO AverageR

           DISPLAY "AVERAGE: " AT 0810 AverageR AT 0820.

       RECTANGLE.
           DISPLAY " " ERASE SCREEN
            DISPLAY "PERIMETER AND AREA OF A RECTANGLE" AT 0223

            DISPLAY "ENTER SIDE-A: " AT 0410
            ACCEPT SIDE-A-I AT 0425
            DISPLAY "ENTER SIDE-B: " AT 0510
            ACCEPT SIDE-B-I AT 0525

            MOVE SIDE-A-I TO SIDE-A
            MOVE SIDE-B-I TO SIDE-B

            COMPUTE PERIMETER = (SIDE-A*2) + (SIDE-B*2)
            COMPUTE AREA-RECTANGLE = SIDE-A*SIDE-B

            MOVE PERIMETER TO PERIMETER-R
            MOVE AREA-RECTANGLE TO AREA-RECTANGLE-R

            DISPLAY "AREA: " AT 0710 AREA-RECTANGLE-R AT 0717
            DISPLAY "PERIMETER: " AT 0810 PERIMETER-R AT 0822.

       CHANGE-CALCULATOR.
           DISPLAY " " ERASE SCREEN
           DISPLAY "CHANGE CALCULATOR" AT 0431
           DISPLAY "NO. OF ORDER: " AT 0610
           ACCEPT orderNUM-I AT 0625
           DISPLAY "AMOUNT OF ORDER: " AT 0710
           ACCEPT amountORDER-I AT 0728

           MOVE orderNUM-I TO orderNUM
           MOVE amountORDER-I TO amountORDER

           COMPUTE totalORDER = orderNUM*amountORDER.
           MOVE totalORDER TO totalORDER-R

           DISPLAY "TOTAL: " AT 0910 totalORDER-R AT 0918

           DISPLAY "AMOUNT TENDER: " AT 1010
           ACCEPT amountTENDER-I AT 1026

           MOVE amountTENDER-I TO amountTENDER

           COMPUTE change = amountTENDER - totalORDER.
           MOVE change TO change-R

           DISPLAY "CHANGE: Php" AT 1210 change-R AT 1223.

       QUADRATIC.
           DISPLAY " " ERASE SCREEN
           DISPLAY "ROOTS OF QUADRATIC EQUATION" AT 0227

           DISPLAY "ENTER A: " AT 0410
           ACCEPT A-I AT 0420
           DISPLAY "ENTER B: " AT 0510
           ACCEPT B-I AT 0520
           DISPLAY "ENTER C: " AT 0610
           ACCEPT C-I AT 0620

           MOVE A-I TO A
           MOVE B-I TO B
           MOVE C-I TO C

           COMPUTE DISCRIM = (B*B) - (4*A*C)
           COMPUTE X1 = (-B + FUNCTION SQRT(DISCRIM)) / (2*A)
           COMPUTE X2 = (-B - FUNCTION SQRT(DISCRIM)) / (2*A)


           MOVE X1 TO X1-R
           MOVE X2 TO X2-R

           IF DISCRIM IS NEGATIVE THEN
           DISPLAY "FIRST SOLUTION" AT 0815
           DISPLAY "X1 = " AT 0910 X1-R AT 0916
           DISPLAY "X2 = " AT 1010 X2-R AT 1016
           END-IF

           COMPUTE tempVAR1 = -B / (2*A)
           COMPUTE tempVAR2 = FUNCTION sqrt(-DISCRIM) / (2*A)

           MOVE tempVAR1 TO tempVAR1-R
           MOVE tempVAR2 TO tempVAR2-R

           DISPLAY "SECOND SOLUTION" AT 1215
           DISPLAY "X1 = " AT 1310 tempVAR1-R AT 1316
           " +" AT 1323 tempVAR2-R AT 1326 "i" AT 1333
           DISPLAY "X1 = " AT 1410 tempVAR1-R AT 1416
           " -" AT 1423 tempVAR2-R AT 1426 "i" AT 1433.

       END PROGRAM SEQ-MAIN-MENU.
