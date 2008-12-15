# compile these directories ourselves
dirs=. session/lisp mailcrypt/lisp ruby-mode rdebug

# these dirs come with makefiles
makedirs=elib nxml jde/lisp cedet xslide color-theme

EMACS=emacs
src=$(foreach d,$(dirs),$(wildcard $(d)/*.el))
obj=$(patsubst %.el,%.elc,$(src))

# only compile these files inside python-mode
#src+=python-mode/python-mode.el

.PHONY:all
all: obj submakes

.PHONY:obj
obj: $(obj)

.PHONY:submakes
submakes: color-theme-autoloads
	for p in $(makedirs); do \
		$(MAKE) -C $$p ; \
	done

.PHONY:color-theme-autoloads
color-theme-autoloads:
	$(MAKE) -C color-theme autoloads

# obsolete, where did this come from?
#.el.elc:
#	$(EMACS) -batch $(EMACSFLAGS) -l $(srcdir)/load-path.hack \
#	  -f batch-byte-compile $<

%.elc : %.el
	$(EMACS) -batch --no-site-file $(foreach hack,$(wildcard $(dir $?)*.hack),-l $(hack)) \
	 -f batch-byte-compile $?
# windows emacs generates object files with execute bit set, let's remove it
ifeq ($(OSTYPE),cygwin)
	chmod -x $@
endif