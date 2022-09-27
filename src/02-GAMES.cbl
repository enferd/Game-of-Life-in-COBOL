       IDENTIFICATION DIVISION.
       PROGRAM-ID. 01-GAMEL.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.     
       SPECIAL-NAMES.             
       DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  CT-CONSTANTS.
           05  CT-TURNS         PIC 9(08)    VALUE 1.
           05  CT-FILLER-LINE   PIC X(100)   VALUE ALL '-'.
           05  CT-CANVAS-WIDTH  PIC 9(03)    VALUE 100.
           05  CT-CANVAS-HEIGHT PIC 9(03)    VALUE 40.
           05  CT-SCREEN-HEIGHT PIC 9(03)    VALUE 41.
      *    CT-SCREEN MUST ALWAYS BE ONE HIGHER THAN CT-CANVAS!!!
           05  CT-CHANCE-OF-INITIAL-CELL
                                PIC 9V999    VALUE 0,333.
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
           05  WS-COMMAND      PIC X        VALUE SPACES.
           05  WS-SEED         PIC X(80)    VALUE SPACES.

       01  WS-FECHA            PIC 9(18)    VALUE ZEROES.

       SCREEN SECTION.

       01  MAIN-MENU.
           05  VALUE "Conway's Game of Life - Programmed by enferd"
               BLANK SCREEN                 LINE 1 COL 2
               UNDERLINE.
           05  SC-COMMAND      PIC X         
               BLINK  TO WS-COMMAND    LINE CT-SCREEN-HEIGHT COL 99.
           05  VALUE '         PRESS ENTER TO START                    '
                                            LINE 3 COL 1.
           05  VALUE '         WRITE R TO RANDOMLY GENERATE THE FIELD  '
                                            LINE 5 COL 1.
           05  VALUE '         WRITE S AND WRITE A SEED TO GENERATE '
                                            LINE 7 COL 1.
           05  VALUE 'A SPECIFIC FIELD'     LINE 7 COL + 1.
           05  VALUE 'GRID HEIGHT (MAX 40): '      LINE 11 COL 2.
           05  SC-HEIGHT       PIC 9(03)    
               TO CT-CANVAS-HEIGHT          LINE 11 COL + 1.
           05  VALUE 'GRID WIDTH (MAX 100): '       LINE 13 COL 2.
           05  SC-WIDTH        PIC 9(03)    
               TO CT-CANVAS-WIDTH           LINE 13 COL + 1.
           05  VALUE 'SEED: '               LINE 20 COL 2.
           05  SC-SEED         PIC X(80)    TO WS-SEED
                                            LINE 20 COL + 1.

       01  CLEAR-SCREEN.
           05 VALUE SPACES BLANK SCREEN.

       01  GRID-SCREEN.
           05  SC-LINE         PIC X(100)    LINE WS-X COL 1
                               VALUE SPACES.
           05  SC-MESSAGE      PIC X(98) 
                                       LINE CT-SCREEN-HEIGHT COL 1.
           05  SC-COMMAND      PIC X        TO WS-COMMAND 
               BLINK                   LINE CT-SCREEN-HEIGHT COL 99.



       PROCEDURE DIVISION.
       MAIN-PROGRAM.

           DISPLAY MAIN-MENU.

           ACCEPT  MAIN-MENU.

           COMPUTE CT-SCREEN-HEIGHT = CT-CANVAS-HEIGHT + 1.

           PERFORM 1000-INIT  
           THRU  F-1000-INIT.

           PERFORM 2000-PROCESS
           THRU  F-2000-PROCESS
           UNTIL WS-COMMAND = 'X'.

           PERFORM 9999-FINAL   
           THRU  F-9999-FINAL.

       F-MAIN-PROGRAM. GOBACK.

       1000-INIT.

           MOVE FUNCTION CURRENT-DATE (1:18) TO WS-FECHA.
           MOVE FUNCTION RANDOM(WS-FECHA) TO WS-FECHA.
           
           EVALUATE WS-COMMAND
           
               WHEN 'R'

                   PERFORM 1500-CREATE-RANDOM-CELLS
                   THRU  F-1500-CREATE-RANDOM-CELLS
                   VARYING WS-X FROM 1 BY 1
                   UNTIL   WS-X > CT-CANVAS-HEIGHT

               WHEN 'S'

                   PERFORM 1750-CALCULATE-SEED
                   THRU  F-1750-CALCULATE-SEED
                   VARYING WS-X FROM 1 BY 1
                   UNTIL   WS-X > CT-CANVAS-HEIGHT
           
           END-EVALUATE.

           PERFORM 3100-CLEAR-SCREEN
           THRU    3100-CLEAR-SCREEN.

           MOVE 'R: Refresh; S: Switch display mode; C: Compute state'
                TO SC-MESSAGE.

           PERFORM 3000-DISPLAY-SCREEN
           THRU  F-3000-DISPLAY-SCREEN
           VARYING WS-X FROM 1 BY 1 
           UNTIL   WS-X > CT-CANVAS-HEIGHT. 
           MOVE SPACES TO SC-LINE.

       F-1000-INIT. EXIT.

       1500-CREATE-RANDOM-CELLS.

           PERFORM VARYING WS-Y FROM 1 BY 1 UNTIL WS-Y > CT-CANVAS-WIDTH
               IF FUNCTION RANDOM() < CT-CHANCE-OF-INITIAL-CELL
                   MOVE 1 TO WS-CELL(WS-X,WS-Y)
                   MOVE CT-LIVING-CELL TO WS-LINE(WS-X)(WS-Y : 1)
               ELSE
                   MOVE 0 TO WS-CELL(WS-X,WS-Y)
                   MOVE CT-EMPTY-CELL TO WS-LINE(WS-X)(WS-Y : 1)
               END-IF
           END-PERFORM. 

       F-1500-CREATE-RANDOM-CELLS. EXIT.

       1750-CALCULATE-SEED.

           CONTINUE.

       F-1750-CALCULATE-SEED. EXIT.

       2000-PROCESS.

           ACCEPT GRID-SCREEN.

           EVALUATE WS-COMMAND

               WHEN 'C'

                   PERFORM 4000-CHECK-NEIGHBOR-CELLS
                   THRU  F-4000-CHECK-NEIGHBOR-CELLS
                   VARYING WS-X FROM 1 BY 1 
                   UNTIL   WS-X > CT-CANVAS-HEIGHT

                   PERFORM 3500-OVERWRITE-CELL
                   THRU  F-3500-OVERWRITE-CELL
                   VARYING WS-X FROM 1 BY 1 
                   UNTIL   WS-X > CT-CANVAS-HEIGHT

      *            PERFORM 3100-CLEAR-SCREEN
      *            THRU    3100-CLEAR-SCREEN

                   MOVE 'Calculating next status' TO SC-MESSAGE

                   PERFORM 3000-DISPLAY-SCREEN
                   THRU  F-3000-DISPLAY-SCREEN
                   VARYING WS-X FROM 1 BY 1
                   UNTIL   WS-X > CT-CANVAS-HEIGHT
                   MOVE SPACES TO SC-LINE

               WHEN 'R'

                   PERFORM 3100-CLEAR-SCREEN
                   THRU    3100-CLEAR-SCREEN

                   MOVE 'Refreshing the screen' TO SC-MESSAGE

                   PERFORM 3000-DISPLAY-SCREEN
                   THRU  F-3000-DISPLAY-SCREEN
                   VARYING WS-X FROM 1 BY 1
                   UNTIL   WS-X > CT-CANVAS-HEIGHT
                   MOVE SPACES TO SC-LINE

               WHEN 'S'
                  
                   PERFORM 3600-SWITCH-CELL
                   THRU  F-3600-SWITCH-CELL
                   VARYING WS-X FROM 1 BY 1 
                   UNTIL   WS-X > CT-CANVAS-HEIGHT

                   PERFORM 3100-CLEAR-SCREEN
                   THRU    3100-CLEAR-SCREEN

                   PERFORM 3000-DISPLAY-SCREEN
                   THRU  F-3000-DISPLAY-SCREEN
                   VARYING WS-X FROM 1 BY 1
                   UNTIL   WS-X > CT-CANVAS-HEIGHT
                   MOVE SPACES TO SC-LINE
                   
           END-EVALUATE.

       F-2000-PROCESS. EXIT.
       
       3000-DISPLAY-SCREEN.

           MOVE WS-LINE(WS-X) TO SC-LINE.
           DISPLAY GRID-SCREEN.

       F-3000-DISPLAY-SCREEN. EXIT.

       3100-CLEAR-SCREEN.

           DISPLAY CLEAR-SCREEN.

       F-3100-CLEAR-SCREEN. EXIT.

       3500-OVERWRITE-CELL.

           PERFORM 6000-PREPARE-LINE-FOR-CELLS
           THRU  F-6000-PREPARE-LINE-FOR-CELLS
           VARYING WS-Y FROM 1 BY 1
           UNTIL   WS-Y > CT-CANVAS-WIDTH.

       F-3500-OVERWRITE-CELL. EXIT.

       3600-SWITCH-CELL.

           IF WS-LINE(WS-X)(1 : 1) = '@' OR
              WS-LINE(WS-X)(1 : 1) = '.'

               PERFORM 6000-PREPARE-LINE-FOR-DISPLAY
               THRU  F-6000-PREPARE-LINE-FOR-DISPLAY
               VARYING WS-Y FROM 1 BY 1
               UNTIL   WS-Y > CT-CANVAS-WIDTH

               MOVE 'Displaying previous neighbors' TO SC-MESSAGE

           ELSE

               PERFORM 6000-PREPARE-LINE-FOR-CELLS
               THRU  F-6000-PREPARE-LINE-FOR-CELLS
               VARYING WS-Y FROM 1 BY 1
               UNTIL   WS-Y > CT-CANVAS-WIDTH

               MOVE 'Displaying cells' TO SC-MESSAGE

           END-IF.

       F-3600-SWITCH-CELL. EXIT.

       4000-CHECK-NEIGHBOR-CELLS.

           INITIALIZE WS-ARRAY2(WS-X).

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

           MOVE WS-NEIGHBORS(WS-X,WS-Y) TO WS-LINE(WS-X)(WS-Y : 1).

       F-6000-PREPARE-LINE-FOR-DISPLAY. EXIT.

       6000-PREPARE-LINE-FOR-CELLS.

           IF  WS-CELL(WS-X,WS-Y) = 1
               MOVE CT-LIVING-CELL TO WS-LINE(WS-X)(WS-Y : 1) 
           ELSE 
               MOVE CT-EMPTY-CELL TO WS-LINE(WS-X)(WS-Y : 1)
           END-IF.

       F-6000-PREPARE-LINE-FOR-CELLS. EXIT.

       9999-FINAL.

           CONTINUE.
       
       F-9999-FINAL. EXIT.
