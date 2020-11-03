
NAME = FuSe
VERSION = 0.7
BASE = $(NAME)-$(VERSION)

NULL =

DIRS = src decoder examples docs

all:
	for i in $(DIRS); do make -C $$i; done

dist: $(BASE).tar.gz

$(BASE).tar.gz: $(ARCHIVE)
	rm -rf $(BASE)
	mkdir $(BASE)
	cp --parents $(ARCHIVE) $(BASE)
	tar cvfz $@ $(BASE)
	rm -rf $(BASE)

clean:
	for i in $(DIRS); do make -C $$i clean; done
	rm -rf $(BASE) $(BASE).tar.gz
