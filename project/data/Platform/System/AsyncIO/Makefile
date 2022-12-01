CC=gcc
OBJS:=cAsyncIO.o queue.o hashtable.o

all: $(OBJS)

install: $(OBJS)
	mkdir -p ../../Clean\ System\ Files/ 
	mv -v $(OBJS) ../../Clean\ System\ Files/
	rm Clean.h cAsyncIO.h cAsyncIO.c hashtable.h hashtable.c queue.h queue.c

