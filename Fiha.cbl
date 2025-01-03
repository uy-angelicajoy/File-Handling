       IDENTIFICATION DIVISION.
       PROGRAM-ID. MENUFILEHANDLING.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STUDENT-FILE ASSIGN TO "student_prof.TXT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD STUDENT-FILE.
       01 STUDENT-RECORD.
           05 STUDENT-DATA      PIC X(80).

       WORKING-STORAGE SECTION.
       01 WS-MENU-CHOICE        PIC 9.
       01 WS-CONSENT            PIC X.
       01 WS-STUDENT-NUMBER     PIC X(20).
       01 WS-STUDENT-NAME       PIC X(40).
       01 WS-STUDENT-PROGRAM    PIC X(5).
       01 WS-YEAR-LEVEL         PIC 9.
       01 WS-FINAL-GRADE        PIC X(5). 
       01 WS-TEMP-LINE          PIC X(80).
       01 WS-OUTPUT-LINE        PIC X(80).
       01 WS-FINAL-GRADE-STRING PIC X(5).
       01 EOF-FLAG              PIC X VALUE "N".

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM DISPLAY-MENU
           PERFORM UNTIL WS-MENU-CHOICE = 4
               EVALUATE WS-MENU-CHOICE
                   WHEN 1
                       PERFORM GET-CONSENT
                   WHEN 2
                       PERFORM CREATE-STUDENT-PROFILE
                   WHEN 3
                       PERFORM DISPLAY-STUDENT-INFORMATION
                   WHEN 4
                       DISPLAY "Exiting program..."
                       STOP RUN
                   WHEN OTHER
                       DISPLAY "Invalid choice. Please try again."
               END-EVALUATE
               PERFORM DISPLAY-MENU
           END-PERFORM.
           STOP RUN.

       DISPLAY-MENU.
           DISPLAY "=================================="
           DISPLAY "             MENU               "
           DISPLAY "=================================="
           DISPLAY "1. Consent to share information"
           DISPLAY "2. Create Student Profile"
           DISPLAY "3. Display Student Information"
           DISPLAY "4. Exit"
           DISPLAY "Enter your choice: ".
           ACCEPT WS-MENU-CHOICE.

       GET-CONSENT.
           DISPLAY "Do you agree to share your information? (Y/N): ".
           ACCEPT WS-CONSENT.
           IF WS-CONSENT = "Y" OR WS-CONSENT = "y"
               DISPLAY "You agreed to share your information."
           ELSE
               DISPLAY "You did not agree to share your information."
           END-IF.

       CREATE-STUDENT-PROFILE.
           DISPLAY "Student Number: ".
           ACCEPT WS-STUDENT-NUMBER.
           DISPLAY "Student Name: ".
           ACCEPT WS-STUDENT-NAME.
           DISPLAY "Program: ".
           ACCEPT WS-STUDENT-PROGRAM.
           DISPLAY "Year Level: ".
           ACCEPT WS-YEAR-LEVEL.
           DISPLAY "Final Grade: ".
           ACCEPT WS-FINAL-GRADE. 

           OPEN OUTPUT STUDENT-FILE.

           MOVE SPACES TO WS-OUTPUT-LINE.

           STRING "Student Number: " DELIMITED BY SIZE
                  WS-STUDENT-NUMBER DELIMITED BY SIZE
                  INTO WS-OUTPUT-LINE
           WRITE STUDENT-RECORD FROM WS-OUTPUT-LINE.

           MOVE SPACES TO WS-OUTPUT-LINE.

           STRING "Student Name: " DELIMITED BY SIZE
                  WS-STUDENT-NAME DELIMITED BY SIZE
                  INTO WS-OUTPUT-LINE
           WRITE STUDENT-RECORD FROM WS-OUTPUT-LINE.

           MOVE SPACES TO WS-OUTPUT-LINE.

           STRING "Program: " DELIMITED BY SIZE
                  WS-STUDENT-PROGRAM DELIMITED BY SIZE
                  INTO WS-OUTPUT-LINE
           WRITE STUDENT-RECORD FROM WS-OUTPUT-LINE.

           MOVE SPACES TO WS-OUTPUT-LINE.

           STRING "Year Level: " DELIMITED BY SIZE
                  WS-YEAR-LEVEL DELIMITED BY SIZE
                  INTO WS-OUTPUT-LINE
           WRITE STUDENT-RECORD FROM WS-OUTPUT-LINE.

           MOVE SPACES TO WS-OUTPUT-LINE.

           STRING "Final Grade: " DELIMITED BY SIZE
                  WS-FINAL-GRADE DELIMITED BY SIZE  
                  INTO WS-OUTPUT-LINE
           WRITE STUDENT-RECORD FROM WS-OUTPUT-LINE.

           CLOSE STUDENT-FILE.

           DISPLAY "Student profile saved successfully.".

       DISPLAY-STUDENT-INFORMATION.
           OPEN INPUT STUDENT-FILE
           PERFORM UNTIL EOF-FLAG = "Y"
               READ STUDENT-FILE INTO WS-TEMP-LINE
                   AT END
                       MOVE "Y" TO EOF-FLAG
                   NOT AT END
                       DISPLAY WS-TEMP-LINE
               END-READ
           END-PERFORM
           CLOSE STUDENT-FILE
           MOVE "N" TO EOF-FLAG.
