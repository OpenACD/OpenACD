SPEC_FILE = @PACKAGE_NAME@.spec

LOCALIZE = \
	-e 's/@PACKAGE_NAME\@/@PACKAGE_NAME@/g' \
	-e 's/@PACKAGE_VERSION\@/@PACKAGE_VERSION@/g'

CONTRIB_DEPENDENCY = \
	errd \
	gen_leader_revival \
	gen_server_mock \
	gen_smtp

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
	www

all compile :
	rake

$(SPEC_FILE) : %: %.in
	sed $(LOCALIZE) $(srcdir)/$@.in > $@

clean-local :
	rm $(SPECFILE)
	rake clean

# Download everything and put it in the tarball, so build server doesn't have to download anything
dist-hook : $(foreach D,$(CONTRIB_DEPENDENCY),$(srcdir)/contrib/$(D)/src)
	find $(distdir) -name '.git' -type d -exec echo rm -rf {} \;

# Override default tarball creation, need to support paths > 99 chars
# change tar options: "chof" to "chf", which implies the tarball is not
# compatible with legacy systems.  See 'man tar' for more info
dist dist-all : distdir
        $(AMTAR) chf - $(distdir) | GZIP=$(GZIP_ENV) gzip -c >$(DIST_SRC)

#        $(am__remove_distdir)

$(foreach D,$(CONTRIB_DEPENDENCY),$(srcdir)/contrib/$(D)/src) :
	rake contrib
