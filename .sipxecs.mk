# See ./src/OpenACD.app.src for matching version number. Please keep in sync, but not strictly nec.
OpenACD_VER = 0.9.5

OpenACD_REV := $(shell cd $(SRC)/OpenACD; $(SRC)/config/revision-gen $(OpenACD_VER))
OpenACD_SRPM = openacd-$(OpenACD_VER)-$(OpenACD_REV).src.rpm
OpenACD_TAR = OpenACD/openacd-$(OpenACD_VER).tar.gz
OpenACD_SRPM_DEFS = --define "buildno $(OpenACD_REV)"
OpenACD_RPM_DEFS = --define="buildno $(OpenACD_REV)"

OpenACD.autoreconf OpenACD.configure :;

OpenACD.dist :
	test -d OpenACD || mkdir -p OpenACD
	cd $(SRC)/OpenACD; \
	  git archive --format tar --prefix OpenACD/ HEAD > $(abspath $(OpenACD_TAR:.tar.gz=.tar))
	make -C $(SRC)/OpenACD deps
	# tar up the source in the git submodules
	tar -C $(SRC) -rf $(abspath $(OpenACD_TAR:.tar.gz=.tar)) \
	  OpenACD/deps \
	  OpenACD/priv/www/contrib/dojo/dojo \
	  OpenACD/priv/www/contrib/dojo/dojox \
	  OpenACD/priv/www/contrib/dojo/dijit
	cat $(OpenACD_TAR:.tar.gz=.tar) | gzip > $(OpenACD_TAR)
