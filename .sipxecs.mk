# See ./src/OpenACD.app.src for matching version number. Please keep in sync, but not strictly nec.
OpenACD_VER = 0.9.5

OpenACD_REV = $(shell cd $(SRC)/$(PROJ); ../config/revision-gen $(OpenACD_VER))
OpenACD_SRPM = openacd-$(OpenACD_VER)-$(OpenACD_REV).src.rpm
OpenACD_TAR = OpenACD/OpenACD.tar.gz
OpenACD_SRPM_DEFS = --define "buildno $(OpenACD_REV)"
OpenACD_RPM_DEFS = --define="buildno $(OpenACD_REV)"

OpenACD.autoreconf OpenACD.configure :;

OpenACD.dist :
	test -d OpenACD || mkdir -p OpenACD
	make -C $(SRC)/OpenACD dist DESTDIR=$(abspath .)/OpenACD/
