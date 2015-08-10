.PHONY: solution renderer all clean

solution:
	(cd player; $(MAKE); cp play_icfp2015 ..)

renderer:
	(cd renderer; $(MAKE))
	
all: solution renderer

clean:
	rm play_icfp2015