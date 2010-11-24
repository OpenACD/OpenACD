SPEC_FILE = @PACKAGE@.spec

# Override default tarball creation, need to support paths > 99 chars
# change tar options: "chof" to "chf", which implies the tarball is not
# compatible with legacy systems.  See 'man tar' for more info
am__tar = ${AMTAR} chf - "$$tardir"

LOCALIZE = \
	-e 's/@PACKAGE\@/@PACKAGE@/g' \
	-e 's/@PACKAGE_VERSION\@/@PACKAGE_VERSION@/g'

CONTRIB_DEPS = \
	gen_server_mock \
	errd \
	gen_leader_revival \
	gen_server_mock \
	mochiweb \
	erlmongo \
	gen_smtp

WWW_CONTRIB = \
	dojo \
	dojox \
	dijit

CONTRIB_DEP_DIRS = \
	$(foreach D,$(CONTRIB_DEPS),$(srcdir)/contrib/$(D)/src)

WWW_CONTRIB_DEP_DIRS = \
	$(foreach D,$(WWW_CONTRIB_DEPS),$(srcdir)/priv/www/contrib/dojo/$(D))

EXTRA_DIST = \
	$(SPEC_FILE) \
	boot.rb \
	configure \
	Makefile.in \
	ebin \
	contrib \
	include \
	LICENSE \
	misc \
	Rakefile \
	README.markdown \
	src \
	success_message \
	tests \
	VERSION \
	priv

all compile :
	rake

install :
	@echo "No install target, copy this dir to where you want OpenACD to run"

.PHONY :
$(SPEC_FILE) :
	sed $(LOCALIZE) $(srcdir)/$@.in > $@

clean-local :
	rm $(SPECFILE)
	rake clean

# Download everything and put it in the tarball, so build server doesn't download anything
dist-hook : $(CONTRIB_DEP_DIRS) $(WWW_CONTRIB_DEP_DIRS)
	find $(distdir) -name '.git' -type d -exec echo rm -rf {} \;

$(CONTRIB_DEP_DIRS) $(WWW_CONTRIB_DEP_DIRS) :
	rake contrib
