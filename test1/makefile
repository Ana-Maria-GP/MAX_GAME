# Makefile for Haskell program

# Compiler
GHC = ghc

# Source file
SOURCE = main.hs

# Output executable
OUTPUT = game

# Rule to build the executable
$(OUTPUT): $(SOURCE)
	$(GHC) -o $(OUTPUT) $(SOURCE)

# Rule to clean up generated files
clean:
	rm -f $(OUTPUT) *.o *.hi

# Default rule
all: $(OUTPUT)

# Rule to run the program
run: $(OUTPUT)
	./$(OUTPUT)