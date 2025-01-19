      ******************************************************************
      * Author: JAMES MICHAEL
      * Date: 1/18/25
      * Purpose: PROJECT [INDEXED FILE HANDLING]
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. INDEX-FILEHANDLING.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STUDENT-FILE ASSIGN TO "STUDENTFILE.DAT"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS STUD-ID
               FILE STATUS IS FILESTATUS.

           SELECT CSV-FILE ASSIGN TO "STUDENTFILE.CSV"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD STUDENT-FILE.
       01 STUDENT-RECORD.
           05 STUD-ID          PIC X(5).
           05 STUD-NAME        PIC X(20).
           05 STUD-PROGRAM     PIC X(10).
       
       FD CSV-FILE.
       01 CSV-RECORD           PIC X(40).

       WORKING-STORAGE SECTION.
           01 FILESTATUS       PIC X(2).
           01 WS-OPTION        PIC 9.
           01 WS-EndOfFile     PIC X VALUE 'N'.
           01 WS-WAITFORINPUT           PIC X.

       PROCEDURE DIVISION.
           PERFORM MAIN-PROCEDURE.
           STOP RUN.

       MAIN-PROCEDURE.
            PERFORM UNTIL WS-EndOfFile = 'Y'
               CALL "SYSTEM" USING "CLS" *> this is for clearing the entirety of the screen #mejares
               DISPLAY "      <PUP-T STUDENT DATABASE >"
               DISPLAY "------------------------------------"
               DISPLAY "1. CREATE STUDENT PROFILE"
               DISPLAY "2. SEARCH STUDENT"
               DISPLAY "3. EXPORT TO READABLE FILE/CSV"
               DISPLAY "4. EXIT"
               DISPLAY "------------------------------------"
               DISPLAY "Please enter your desired option: " NO ADVANCING.
               ACCEPT WS-OPTION
              
               EVALUATE WS-OPTION
                   WHEN 1 PERFORM ADD-STUDENT
                   WHEN 2 PERFORM SEARCH-STUDENT
                   WHEN 3 PERFORM EXPORT-TO-CSV
                   WHEN 4 MOVE 'Y' TO WS-EndOfFile
                   WHEN OTHER DISPLAY "INVALID OPTION"
           END-PERFORM.

       ADD-STUDENT.
           OPEN I-O STUDENT-FILE.
           IF FILESTATUS = "35"
               OPEN OUTPUT STUDENT-FILE
               CLOSE STUDENT-FILE
               OPEN I-O STUDENT-FILE
           END-IF.
           DISPLAY " "
           DISPLAY "Enter Student ID: " NO ADVANCING.
           ACCEPT STUD-ID.
           DISPLAY "Enter Student Name: " NO ADVANCING.
           ACCEPT STUD-NAME.
           DISPLAY "Enter Program: " NO ADVANCING.
           ACCEPT STUD-PROGRAM.

           WRITE STUDENT-RECORD.
           IF FILESTATUS NOT = "00"
               DISPLAY " "
               DISPLAY "------------------------------------"
               DISPLAY "  Error in Writing Student Profile!"
               DISPLAY "------------------------------------"
               DISPLAY " "
               ACCEPT WS-WAITFORINPUT
           ELSE
               DISPLAY " "
               DISPLAY "------------------------------------"
               DISPLAY "      Student Profile created!"
               DISPLAY "------------------------------------"
               DISPLAY " "
               ACCEPT WS-WAITFORINPUT
           END-IF.
           CLOSE STUDENT-FILE.

       SEARCH-STUDENT.
           OPEN I-O STUDENT-FILE.
           DISPLAY " "
           DISPLAY "Enter Student ID to Search: " NO ADVANCING.
           ACCEPT STUD-ID.
           READ STUDENT-FILE KEY IS STUD-ID
               INVALID KEY 
                   DISPLAY " "
                   DISPLAY "------------------------------------"
                   DISPLAY "   Student not found/Registered!"
                   DISPLAY "------------------------------------"
                   DISPLAY " "
                   ACCEPT WS-WAITFORINPUT
               NOT INVALID KEY 
                   DISPLAY " "
                   DISPLAY "------------------------------------"
                   DISPLAY "Student Found: " STUD-NAME.
                   DISPLAY "STUDENT ID: " STUD-ID.
                   DISPLAY "STUD PROGRAM: " STUD-PROGRAM.
                   DISPLAY "------------------------------------"
                   DISPLAY " "
                   ACCEPT WS-WAITFORINPUT
           CLOSE STUDENT-FILE.
           IF FILESTATUS NOT = "00"
               DISPLAY "Error in Reading Student Record!"
               ACCEPT WS-WAITFORINPUT
           END-IF.

       EXPORT-TO-CSV.
           OPEN OUTPUT CSV-FILE.
           OPEN I-O STUDENT-FILE.
           DISPLAY " "
           DISPLAY "Exporting Student Data to CSV File..."

           PERFORM UNTIL FILESTATUS = "10"
               READ STUDENT-FILE
                   AT END
                       MOVE "10" TO FILESTATUS
                       EXIT PERFORM
                   NOT AT END
                       MOVE STUD-ID TO CSV-RECORD
                       MOVE STUD-NAME TO CSV-RECORD(6:20)
                       MOVE STUD-PROGRAM TO CSV-RECORD(26:10)
                       WRITE CSV-RECORD
               END-READ
           END-PERFORM.

           CLOSE STUDENT-FILE.
           CLOSE CSV-FILE.
           DISPLAY "Data exported to CSV Successfully!"
           ACCEPT WS-WAITFORINPUT.
