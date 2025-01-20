      ******************************************************************
      * Author: TECHNEWJEANS
      * Date: 1/18/25
      * Purpose: PROJECT [INDEXED FILE HANDLING]
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. INDEX-FILEHANDLING.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STUDENT-FILE ASSIGN TO "STUDENTFILE.DAT" *> this is for the file name #mejares
               ORGANIZATION IS INDEXED *> this is for the organization of the file #mejares
               ACCESS MODE IS DYNAMIC *> this is for the access mode of the file #mejares
               RECORD KEY IS STUD-ID *> this is for the record key of the file #mejares
               FILE STATUS IS FILESTATUS. *> this is for the file status #mejares

           SELECT CSV-FILE ASSIGN TO "STUDENTFILE.CSV" *> this is for the file name #mejares
               ORGANIZATION IS LINE SEQUENTIAL. *> this is for the organization of the file #mejares

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
           PERFORM MAIN-MENU.
           STOP RUN.

       MAIN-MENU.
            PERFORM UNTIL WS-EndOfFile = 'Y'
               CALL "SYSTEM" USING "CLS" *> this is for clearing the entirety of the screen #mejares
               DISPLAY "      <PUP-T STUDENT DATABASE>"
               DISPLAY "------------------------------------"
               DISPLAY "1. CREATE STUDENT PROFILE"
               DISPLAY "2. SEARCH STUDENT"
               DISPLAY "3. EXPORT TO READABLE FILE/CSV"
               DISPLAY "4. DISPLAY ALL STUDENTS"
               DISPLAY "5. EXIT"
               DISPLAY "------------------------------------"
               DISPLAY "Please enter your desired option: " NO ADVANCING.
               ACCEPT WS-OPTION
              
               EVALUATE WS-OPTION
                   WHEN 1 PERFORM CREATE-STUDENT-PROFILE
                   WHEN 2 PERFORM SEARCH-STUDENT
                   WHEN 3 PERFORM EXPORT-TO-CSV
                   WHEN 4 PERFORM DISPLAY-ALL-STUDENTS
                   WHEN 5
                       DISPLAY "TERMINATING PROGRAM..."
                       CLOSE STUDENT-FILE *> CLOSE THE FILE BEFORE EXITING #mejares 
                       MOVE 'Y' TO WS-EndOfFile *> this is for exiting the program #mejares
                   WHEN OTHER DISPLAY "INVALID OPTION"
           END-PERFORM.

       CREATE-STUDENT-PROFILE.
           OPEN I-O STUDENT-FILE. *> this is for opening the file #mejares
           IF FILESTATUS = "35" *> this is for checking if the file exists filestatus 35 means file not found #mejares
               OPEN OUTPUT STUDENT-FILE *> this is for creating the file if it does not exist #mejares
               CLOSE STUDENT-FILE *> this is for closing the file #mejares
               OPEN I-O STUDENT-FILE *> this is for opening the file #mejares
           END-IF.

           DISPLAY " "
           DISPLAY "Enter Student ID: " NO ADVANCING.
           ACCEPT STUD-ID.
           READ STUDENT-FILE *> error handler for duplicate student id #mejares
               INVALID KEY 
                   CONTINUE 
               NOT INVALID 
                   DISPLAY "Student ID Number already exists!"
                   CLOSE STUDENT-FILE
                   ACCEPT WS-WAITFORINPUT
                   EXIT PARAGRAPH
           END-READ
           DISPLAY "Enter Student Name: " NO ADVANCING.
           ACCEPT STUD-NAME.
           DISPLAY "Enter Program: " NO ADVANCING.
           ACCEPT STUD-PROGRAM.

           WRITE STUDENT-RECORD.
           IF FILESTATUS NOT = "00" *> error handler for writing student profile/file status 00 means succesful file #mejares
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
           CLOSE STUDENT-FILE. *> this is for closing the file #mejares

       SEARCH-STUDENT.
           OPEN I-O STUDENT-FILE. *> this is for opening the file #mejares
           DISPLAY " "
           DISPLAY "Enter Student ID to Search: " NO ADVANCING.
           ACCEPT STUD-ID.
           READ STUDENT-FILE KEY IS STUD-ID *> this is for reading the student file #mejares
               INVALID KEY *> error handler for student not found #mejares
                   DISPLAY " "
                   DISPLAY "------------------------------------"
                   DISPLAY "   Student not found/Registered!"
                   DISPLAY "------------------------------------"
                   DISPLAY " "
                   ACCEPT WS-WAITFORINPUT
                   EXIT PARAGRAPH
               NOT INVALID KEY *> this is for displaying the student profile #mejares
                   DISPLAY " "
                   DISPLAY "------------------------------------"
                   DISPLAY "Student Found: " STUD-NAME.
                   DISPLAY "STUDENT ID: " STUD-ID.
                   DISPLAY "STUD PROGRAM: " STUD-PROGRAM.
                   DISPLAY "------------------------------------"
                   DISPLAY " "
                   ACCEPT WS-WAITFORINPUT
           CLOSE STUDENT-FILE.
           IF FILESTATUS NOT = "00" *> error handler for reading student profile/file status 00 means succesful file #mejares
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
       DISPLAY-ALL-STUDENTS.
           OPEN INPUT STUDENT-FILE
           IF FILESTATUS = "35" *> this is for checking if the file exists/filestatus 35 means file not found #mejares 
               DISPLAY " "
               DISPLAY "------------------------------------"
               DISPLAY "           No data found."
               DISPLAY "------------------------------------"
               EXIT PARAGRAPH
           END-IF.
           
           DISPLAY " "
           DISPLAY "------------------------------------"
           DISPLAY "       ALL STUDENTS DATABASE"
           DISPLAY "------------------------------------"
           PERFORM UNTIL FILESTATUS = "10"
               READ STUDENT-FILE
                   AT END
                       MOVE "10" TO FILESTATUS
                   NOT AT END
                       DISPLAY "Student ID: " STUD-ID
                       DISPLAY "Student Name  : " STUD-NAME
                       DISPLAY "Program      : " STUD-PROGRAM
                       DISPLAY "=================================="
               END-READ
           END-PERFORM

           CLOSE STUDENT-FILE.
           ACCEPT WS-WAITFORINPUT.
