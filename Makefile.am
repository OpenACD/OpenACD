LOCALIZE = \
	-e 's/@PACKAGE\@/@PACKAGE@/g' \
	-e 's/@PACKAGE_VERSION\@/@PACKAGE_VERSION@/g'

CONTRIB_DEPS = \
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
	openacd.spec \
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
	rake contrib
	rake

install :
	@echo "No install target, copy this dir to where you want OpenACD to run"

.PHONY : openacd.spec
openacd.spec :
	sed $(LOCALIZE) $(srcdir)/$@.in > $@

clean-local :
	rm openacd.spec
	rake clean

# Download everything and put it in the tarball, so build server doesn't download anything
dist-hook : $(CONTRIB_DEP_DIRS) $(WWW_CONTRIB_DEP_DIRS);

$(CONTRIB_DEP_DIRS) $(WWW_CONTRIB_DEP_DIRS) :
	cd $(srcdir); \
	  rake submodules
