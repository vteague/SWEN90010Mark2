FILES= list.adb

all: code.sum

code.sum: list/listaverage.siv
	pogs

#measures/limit.siv is just one file touched for sparksimp
list/listaverage.siv: spark.rep
	cp listaverage.rlu list/
	sparksimp -victor

spark.rep: *.ads *.adb
	spark -i=project.idx -vcg -config_file=spark.cfg $(FILES)

clean:
	rm -f *.ali *.o *.lst *.sli

CLEAN: clean
	rm -rf list/
	rm -f code.sum spark.rep
