

FILES=e.[[:digit:]].[[:digit:]][[:digit:]].rkt

TIME:=$(shell sed '' log/time)

TEST = 'time -o log/time -f%e raco test PROG \
	1> log/PROG.log \
	2> log/PROG.err; \
	if [ -s log/PROG.err ]; \
	then \
		echo -n PROG -- ERROR\\t\\t; \
		 \
	else \
		rm log/PROG.err; \
		echo -n PROG -- OK\\t\\t; \
	fi; \
	sed "/^[0-9]/p;d" log/time;'

all:
	@mkdir -p log
	@rm -f log/*
	@find ${FILES} -print0 | \
	xargs --null -I PROG sh -c ${TEST}
	@rm -f log/time


