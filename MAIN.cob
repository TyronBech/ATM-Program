       IDENTIFICATION DIVISION.
       PROGRAM-ID. ATM-MACHINE.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
      *MENU USER CHOICE
       01 WS-CHOICE PIC 9 VALUE 0.
      *QUIT VARIABLE
       01 WS-QUIT PIC X VALUE SPACE.
      *PROFILE VARIABLES
       01 WS-USER-PROFILE.
           02 WS-USER-NAME PIC X(20) VALUE SPACES.
           02 WS-USER-BIRTHDAY.
               03 MM PIC 99 VALUE ZEROES.
               03 DD PIC 99 VALUE ZEROES.
               03 YYYY PIC 9999 VALUE ZEROES.
           02 WS-USER-AGE PIC 99 VALUE ZEROES.
           02 WS-USER-BALANCE PIC -ZZZ,ZZZ,ZZZ,ZZ9.
           02 WS-USERNAME PIC X(20) VALUE SPACES.
           02 WS-PASSWORD PIC X(20) VALUE SPACES.
       PROCEDURE DIVISION.
      *******************************************************
      *    THE LENGTH OF THE CONSOLE HORIZONTALLY IS 120    *
      *******************************************************
       MAIN.
           PERFORM ATM UNTIL WS-CHOICE IS EQUAL TO 5.
           DISPLAY " " ERASE SCREEN.
           STOP RUN.
      *PAUSE SECTION
       P-PAUSE.
           DISPLAY "PRESS ANY KEY TO CONTINUE..." AT 1646.
           ACCEPT WS-QUIT AT 1674.
           EXIT.
      *MAIN PAGE OF ATM
       ATM.
           DISPLAY " " ERASE SCREEN.
           DISPLAY "ATM CONSOLE PROGRAM" AT 0650.
      *    DISPLAY " ".
           DISPLAY "1 - BALANCE" AT 0752.
           DISPLAY "2 - DEPOSIT" AT 0852.
           DISPLAY "3 - WITHDRAW" AT 0952.
           DISPLAY "4 - PROFILE" AT 1052.
           DISPLAY "5 - EXIT" AT 1152.
           DISPLAY "ENTER YOUR CHOICE:" AT 1252.
           ACCEPT WS-CHOICE AT 1271.
           EVALUATE WS-CHOICE
               WHEN 1 PERFORM P-BALANCE
               WHEN 2 PERFORM P-DEPOSIT
               WHEN 3 PERFORM P-WITHDRAW
               WHEN 4 PERFORM P-PROFILE
           END-EVALUATE.
           PERFORM P-PAUSE.
           EXIT.

      *BALANCE SECTION
       P-BALANCE.
           DISPLAY " " ERASE SCREEN.
           DISPLAY "BALANCE SECTION" AT 0652.
           EXIT.

      *DEPOSIT SECTION
       P-DEPOSIT.
           DISPLAY " " ERASE SCREEN.
           DISPLAY "DEPOSIT SECTION" AT 0652.
           EXIT.

      *WITHDRAW SECTION
       P-WITHDRAW.
           DISPLAY " " ERASE SCREEN.
           DISPLAY "WITHDRAW SECTION" AT 0651.
           EXIT.

      *PROFILE SECTION
       P-PROFILE.
           DISPLAY " " ERASE SCREEN.
           DISPLAY "PROFILE SECTION" AT 0652.
           EXIT.
       END PROGRAM ATM-MACHINE.
