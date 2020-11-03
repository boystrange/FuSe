
NULL =
SOURCES = \
  ../src/Session.mli \
  ../src/Service.mli \
  $(NULL)

DEST = padovani@pianeta.di.unito.it:public_html/Software/fs/

SYNC_FILES = $(wildcard *.html) $(wildcard *.css)

all:
	ocamldoc -I ../src -html -charset utf8 -rectypes $(SOURCES)

sync: $(SYNC_FILES)
	scp $^ $(DEST)

clean:
	rm -f $(SYNC_FILES)

.PHONY: clean html

