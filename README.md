# Conway's Game of Life

Hello! This is a rather simple implementation of Conway's Game of Life, written in GNU/COBOL, this was just for the lolz and not a real big project so if you are starting maybe you can use it as a small guide for writing COBOL programs, but don't take me as a genius as this thing still has a lot of stuff to do.

I wrote it in two "flavors":

- Display Edition: This edition is a simple, print to paper, state by state program that, as implied, just does the equivalent of C as a bunch of printfs that are separated by a line, this edition is Compatible with Enterprise COBOL standards and thus it can be played in an original Z/OS Mainframe, as long as your boss doesn't see you, and the Mainframe is competent enough to run like, 5 perform varyings per state (for non-COBOL-programmers, a PERFORM VARYING is just a for loop with a lot of struggling involved).

- Screen Edition: Following the GNU/COBOL standard, but probably also accessible for those running other standards (MicroFocus for example), this edition is a more fleshed out version that, as the name says, uses the SCREEN SECTION for programming various screens, configurable menus, and other stuff I want to implement.

## Installation

To install and run the program in your personal computer:

- Install cobc from whatever package manager you use or the SourceForge page. Or if you use another compiler, then make sure you note it in the CC flag of the MakeFile.
- Run `make install_{version}`, the version being "display" for the display edition, or "screen" for the screen edition.
- There you go! Now you can run the file in your terminal screen, but if you want to run the display edition, I'd recommend you use the `make run_program` command so that the displays get sent to a text file and you don't have to scroll through every state.

## Configuration

The simulation is easily customizable from the constants at the start of the WORKING STORAGE SECTION, basically change any of the values and you can see how it affects the program with a new run!

If you are running the Screen Edition, you'll see it also has a starting menu that allows you to set height and width, so you don't need to configure anything for that.

## To-do

As I said, it still needs a lot of working, I don't know if I'll keep writing code for it, but if anyone wants to add stuff I'll be glad to check it out and add you to the repo!

For the list of things missing:

- Re-factoring the Display version using ACCEPTs as to be able to configure the Height, Width, and Spawn chance, as well as other stuff like Cell characters (empty and alive).
- Further code the main menu of the screen version to add more configuration (Spawn chance, characters, etc.).
- Add a mode for the screen edition for cycles to occur every x miliseconds, this can be made with a perform varying that calls the CURRENT-DATE function, and compares it to an original time set before the perform.
- Think of a way to run a simulation with pre-selected cells, there's a hard-coded way to do this, but I'd like to find a way that's neither a long seed, nor a whole screen dedicated to punching characters.
- Similarly to the last one, I'd like a way to set default values to make running the program a bit faster. 
- This one is way out of the ball park and has a near 0% chance of happening, but I'd also like to add a CICS version that can run like the screen edition, but for CICS, thus making it Enterprise-compliant.
