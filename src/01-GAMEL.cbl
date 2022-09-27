       IDENTIFICATION DIVISION.
       PROGRAM-ID. 01-GAMEL.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.     
       SPECIAL-NAMES.             
       DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  CT-CONSTANTS.
           05  CT-TURNS         PIC 9(08)    VALUE 500.
           05  CT-FILLER-LINE   PIC X(100)    VALUE ALL '-'.
           05  CT-CANVAS-WIDTH  PIC 9(03)    VALUE 100.
           05  CT-CANVAS-HEIGHT PIC 9(03)    VALUE 50.
           05  CT-CHANCE-OF-INITIAL-CELL
                                PIC 9V999    VALUE 0,666.
           05  CT-LIVING-CELL   PIC X        VALUE '@'.             
           05  CT-EMPTY-CELL    PIC X        VALUE '.'.

       01  WS-VECTOR            OCCURS 100 TIMES.
           05  WS-LINE          PIC X(100)   VALUE SPACES.
           05  WS-ARRAY         OCCURS 100 TIMES.
               10  WS-CELL      PIC 9        VALUE ZEROES.
           05  WS-ARRAY2. 
               10  WS-NEIGHBORS PIC 9        VALUE ZEROES 
                                OCCURS 100 TIMES. 

       01  WS-VARIABLES.
           05  WS-X            PIC 9(03)    VALUE ZEROES.
           05  WS-Y            PIC 9(03)    VALUE ZEROES.
           05  WS-X-AUX        PIC 9(03)    VALUE ZEROES.
           05  WS-Y-AUX        PIC 9(03)    VALUE ZEROES.

       01  WS-FECHA            PIC 9(14)    VALUE ZEROES.

       PROCEDURE DIVISION.
       MAIN-PROGRAM.

           PERFORM 1000-INIT  
           THRU  F-1000-INIT.

           PERFORM 2000-PROCESS
           THRU  F-2000-PROCESS
           CT-TURNS TIMES.

           PERFORM 9999-FINAL   
           THRU  F-9999-FINAL.

       F-MAIN-PROGRAM. GOBACK.

       1000-INIT.

           MOVE FUNCTION CURRENT-DATE (1:14) TO WS-FECHA.
           MOVE FUNCTION RANDOM(WS-FECHA) TO WS-FECHA.
           
           PERFORM 1500-CREATE-RANDOM-CELLS
           THRU  F-1500-CREATE-RANDOM-CELLS
           VARYING WS-X FROM 1 BY 1
           UNTIL   WS-X > CT-CANVAS-HEIGHT.

           PERFORM 3000-DISPLAY-SCREEN
           THRU  F-3000-DISPLAY-SCREEN
           VARYING WS-X FROM 1 BY 1 
           UNTIL   WS-X > CT-CANVAS-HEIGHT.

           DISPLAY     CT-FILLER-LINE.

       F-1000-INIT. EXIT.

       1500-CREATE-RANDOM-CELLS.

           PERFORM VARYING WS-Y FROM 1 BY 1 UNTIL WS-Y > CT-CANVAS-WIDTH
               IF FUNCTION RANDOM() < CT-CHANCE-OF-INITIAL-CELL
                   MOVE 1 TO WS-CELL(WS-X,WS-Y)
                   MOVE    CT-LIVING-CELL TO WS-LINE(WS-X)(WS-Y : 1)
               ELSE
                   MOVE 0 TO WS-CELL(WS-X,WS-Y)
                   MOVE CT-EMPTY-CELL TO WS-LINE(WS-X)(WS-Y : 1)
               END-IF
           END-PERFORM. 

       F-1500-CREATE-RANDOM-CELLS. EXIT.

       2000-PROCESS.

           PERFORM 4000-CHECK-NEIGHBOR-CELLS
           THRU  F-4000-CHECK-NEIGHBOR-CELLS
           VARYING WS-X FROM 1 BY 1 
           UNTIL   WS-X > CT-CANVAS-HEIGHT.

           PERFORM 3500-OVERWRITE-CELL
           THRU  F-3500-OVERWRITE-CELL
           VARYING WS-X FROM 1 BY 1 
           UNTIL   WS-X > CT-CANVAS-HEIGHT.

           PERFORM 3000-DISPLAY-SCREEN
           THRU  F-3000-DISPLAY-SCREEN
           VARYING WS-X FROM 1 BY 1
           UNTIL   WS-X > CT-CANVAS-HEIGHT.

           DISPLAY CT-FILLER-LINE.

       F-2000-PROCESS. EXIT.
       
       3000-DISPLAY-SCREEN.

           DISPLAY WS-LINE(WS-X).
           INITIALIZE WS-ARRAY2(WS-X).

       F-3000-DISPLAY-SCREEN. EXIT.

       3500-OVERWRITE-CELL.

           PERFORM 6000-PREPARE-LINE-FOR-DISPLAY
           THRU  F-6000-PREPARE-LINE-FOR-DISPLAY
           VARYING WS-Y FROM 1 BY 1
           UNTIL   WS-Y > CT-CANVAS-WIDTH.

       F-3500-OVERWRITE-CELL. EXIT.

       4000-CHECK-NEIGHBOR-CELLS.

           PERFORM 5000-CHECK-CORNER-CASES
           THRU  F-5000-CHECK-CORNER-CASES
           VARYING WS-Y FROM 1 BY 1
           UNTIL   WS-Y > CT-CANVAS-WIDTH.

       F-4000-CHECK-NEIGHBOR-CELLS. EXIT.

       5000-CHECK-CORNER-CASES.

           EVALUATE WS-X
             WHEN 1

               PERFORM 5100-UPPER-LINE
               THRU  F-5100-UPPER-LINE
             
             WHEN CT-CANVAS-HEIGHT

               PERFORM 5200-LOWER-LINE
               THRU  F-5200-LOWER-LINE 

             WHEN OTHER

               PERFORM 5300-MIDDLE-LINES
               THRU  F-5300-MIDDLE-LINES

           END-EVALUATE.

           IF  WS-CELL(WS-X,WS-Y) = 1
               AND (WS-NEIGHBORS(WS-X,WS-Y) < 2 
                   OR WS-NEIGHBORS(WS-X,WS-Y) > 3)
                   MOVE 0 TO WS-CELL(WS-X,WS-Y) 
           ELSE IF WS-CELL(WS-X,WS-Y) = 0 
                AND WS-NEIGHBORS(WS-X,WS-Y) = 3
                   MOVE 1 TO WS-CELL(WS-X,WS-Y)
           END-IF.

       F-5000-CHECK-CORNER-CASES. EXIT.

       5100-UPPER-LINE.
       
           EVALUATE WS-Y
             WHEN 1

               PERFORM VARYING   WS-X-AUX 
               FROM 2 BY 1 UNTIL WS-X-AUX > 3

                   PERFORM 5300-ADD-NEIGHBOR
                   THRU  F-5300-ADD-NEIGHBOR
                   VARYING WS-Y-AUX FROM 2 BY 1 
                   UNTIL   WS-Y-AUX > 3

               END-PERFORM  

             WHEN CT-CANVAS-WIDTH

               PERFORM VARYING   WS-X-AUX 
               FROM 2 BY 1 UNTIL WS-X-AUX > 3

                   PERFORM 5300-ADD-NEIGHBOR
                   THRU  F-5300-ADD-NEIGHBOR
                   VARYING WS-Y-AUX FROM 1 BY 1 
                   UNTIL   WS-Y-AUX > 2

               END-PERFORM

             WHEN OTHER

               PERFORM VARYING   WS-X-AUX 
               FROM 2 BY 1 UNTIL WS-X-AUX > 3

                   PERFORM 5300-ADD-NEIGHBOR
                   THRU  F-5300-ADD-NEIGHBOR
                   VARYING WS-Y-AUX FROM 1 BY 1 
                   UNTIL   WS-Y-AUX > 3

               END-PERFORM

           END-EVALUATE.

       F-5100-UPPER-LINE. EXIT.

       5200-LOWER-LINE.

           EVALUATE WS-Y
             WHEN 1

               PERFORM VARYING   WS-X-AUX 
               FROM 1 BY 1 UNTIL WS-X-AUX > 2

                   PERFORM 5300-ADD-NEIGHBOR
                   THRU  F-5300-ADD-NEIGHBOR
                   VARYING WS-Y-AUX FROM 2 BY 1 
                   UNTIL   WS-Y-AUX > 3

               END-PERFORM  

             WHEN CT-CANVAS-WIDTH

               PERFORM VARYING   WS-X-AUX 
               FROM 1 BY 1 UNTIL WS-X-AUX > 2

                   PERFORM 5300-ADD-NEIGHBOR
                   THRU  F-5300-ADD-NEIGHBOR
                   VARYING WS-Y-AUX FROM 1 BY 1 
                   UNTIL   WS-Y-AUX > 2

               END-PERFORM

             WHEN OTHER

               PERFORM VARYING   WS-X-AUX 
               FROM 1 BY 1 UNTIL WS-X-AUX > 2

                   PERFORM 5300-ADD-NEIGHBOR
                   THRU  F-5300-ADD-NEIGHBOR
                   VARYING WS-Y-AUX FROM 1 BY 1 
                   UNTIL   WS-Y-AUX > 3 

               END-PERFORM

           END-EVALUATE.

       F-5200-LOWER-LINE. EXIT.

       5300-MIDDLE-LINES.

               EVALUATE WS-Y
                 WHEN 1

                   PERFORM VARYING   WS-X-AUX 
                   FROM 1 BY 1 UNTIL WS-X-AUX > 3

                       PERFORM 5300-ADD-NEIGHBOR
                       THRU  F-5300-ADD-NEIGHBOR
                       VARYING WS-Y-AUX FROM 2 BY 1 
                       UNTIL   WS-Y-AUX > 3

                   END-PERFORM  

                 WHEN CT-CANVAS-WIDTH

                   PERFORM VARYING   WS-X-AUX 
                   FROM 1 BY 1 UNTIL WS-X-AUX > 3

                       PERFORM 5300-ADD-NEIGHBOR
                       THRU  F-5300-ADD-NEIGHBOR
                       VARYING WS-Y-AUX FROM 1 BY 1 
                       UNTIL   WS-Y-AUX > 2

                   END-PERFORM

                 WHEN OTHER

                   PERFORM VARYING   WS-X-AUX 
                   FROM 1 BY 1 UNTIL WS-X-AUX > 3

                       PERFORM 5300-ADD-NEIGHBOR
                       THRU  F-5300-ADD-NEIGHBOR
                       VARYING WS-Y-AUX FROM 1 BY 1 
                       UNTIL   WS-Y-AUX > 3 

                   END-PERFORM

               END-EVALUATE.

       F-5300-MIDDLE-LINES. EXIT.


       5300-ADD-NEIGHBOR.

           IF  NOT(WS-X-AUX = 2 AND WS-Y-AUX = 2) 
               AND WS-LINE(WS-X - 2 + WS-X-AUX)(WS-Y - 2 + WS-Y-AUX : 1)
                   = CT-LIVING-CELL
               ADD 1
               TO  WS-NEIGHBORS(WS-X,WS-Y)
           END-IF.

       F-5300-ADD-NEIGHBOR. EXIT.

       6000-PREPARE-LINE-FOR-DISPLAY.

           IF  WS-CELL(WS-X,WS-Y) = 1
               MOVE CT-LIVING-CELL TO WS-LINE(WS-X)(WS-Y : 1) 
           ELSE 
               MOVE CT-EMPTY-CELL TO WS-LINE(WS-X)(WS-Y : 1)
           END-IF.

       F-6000-PREPARE-LINE-FOR-DISPLAY. EXIT.

       9999-FINAL.

           CONTINUE.
       
       F-9999-FINAL. EXIT.
