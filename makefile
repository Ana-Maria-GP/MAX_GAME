all: max

max: main.hs max_game.hs
    ghc -o max --make main.hs

clean:
    rm -f max *.o *.hi

.PHONY: all clean
