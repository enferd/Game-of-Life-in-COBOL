SRC := src

INPUT_FILE_NAME_DISPLAY := 01-GAMEL.cbl
INPUT_FILE_NAME_SCREEN  := 02-GAMES.cbl
OUTPUT_FILE_NAME := Game-Of-Life

DISPLAY_FILE := Output.txt

COMPILER := cobc

COMP_FLAGS := -x -Wall -Wcolumn-overflow -std=default -O

clean: 
	@echo Cleaning the executable...
	@rm   $(OUTPUT_FILE_NAME)
	@echo Finished!

run_program:
	@echo Deleting previous run...
	@rm   $(DISPLAY_FILE)
	@echo Printing displays to $(DISPLAY_FILE)...
	@./$(OUTPUT_FILE_NAME) >> $(DISPLAY_FILE)
	@echo Finished!

install_display:
	@echo Compiling Conway\'s Game of Life \(Display edition\)...
	@$(COMPILER) $(SRC)/$(INPUT_FILE_NAME_DISPLAY) $(COMP_FLAGS) -o $(OUTPUT_FILE_NAME)  

install_screen:
	@echo Compiling Conway\'s Game of Life \(Screen edition\)...
	@$(COMPILER) $(SRC)/$(INPUT_FILE_NAME_SCREEN) $(COMP_FLAGS) -o $(OUTPUT_FILE_NAME)  

