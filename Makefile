.PHONY: solution renderer all clean submission

solution:
	(cd player; $(MAKE); cp play_icfp2015 ..)

renderer:
	(cd renderer; $(MAKE))
	
all: solution renderer

submission:
	git archive master | gzip > submission.tar.gz

clean:
	rm -f play_icfp2015
	rm -f submission.tar.gz

