## Makefile based on one from Rattle v2.1

# TARGETS
#
# check:	Ask R to check that the package looks okay
# local:	Install into the local machine's R library
# build:	Generate package tar.gz archive
# zip:		Generate package zip archive

PACKAGE=playwith
SRC=$(PACKAGE)/R
NAMESPACE=$(PACKAGE)/NAMESPACE
DESCRIPTION=$(PACKAGE)/DESCRIPTION
DESCRIPTIN=DESCRIPTION.in

# Canonical version information from source file
MAJOR=$(shell grep -E "^MAJOR" $(SRC)/$(PACKAGE).R | cut -d\" -f 2)
MINOR=$(shell grep -E "^MINOR" $(SRC)/$(PACKAGE).R | cut -d\" -f 2)
REVISION=$(shell C:/"Program Files"/TortoiseSVN/bin/SubWCRev.exe . | grep committed | cut -d" " -f 5)
#$(shell svn info | egrep 'Revision:' |  cut -d" " -f 2)
VERSION=$(MAJOR).$(MINOR).$(REVISION)

DATE=$(shell c:/Rtools/bin/date +%F)

default: build

revision:
	perl -pi~ -e "s|Revision: \d*|Revision: $(REVISION)|" $(SRC)/$(PACKAGE).R

check: build
	R CMD check --no-install $(PACKAGE)

build: $(PACKAGE)_$(VERSION).tar.gz

$(PACKAGE)_$(VERSION).tar.gz: revision $(SRC)
	perl -p -e "s|^Version: .*$$|Version: $(VERSION)|" < $(DESCRIPTIN) |\
	perl -p -e "s|^Date: .*$$|Date: $(DATE)|" > $(DESCRIPTION)
	R CMD build $(PACKAGE)

zip: $(PACKAGE)_$(VERSION).zip

$(PACKAGE)_$(VERSION).zip: build
	R CMD build --binary --docs="normal" $(PACKAGE)

zipunix: local
	(cd /usr/local/lib/R/site-library; zip -r9 - $(PACKAGE)) >| \
	$(PACKAGE)_$(VERSION).zip

local: $(PACKAGE)_$(VERSION).tar.gz
	R CMD INSTALL $^

clean:
	rm -f $(PACKAGE)_*.tar.gz $(PACKAGE)_*.zip
	rm -f $(DESCRIPTION)

