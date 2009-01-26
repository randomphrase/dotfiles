# compile these directories ourselves
dirs=. session/lisp doxymacs

# these dirs come with makefiles
makedirs=elib nxml cedet color-theme

# these dirs have installable info files
infodirs=cedet/semantic/doc cedet/cogre cedet/common cedet/eieio cedet/srecode cedet/ede

EMACS=emacs
src=$(foreach d,$(dirs),$(wildcard $(d)/*.el))
obj=$(patsubst %.el,%.elc,$(src))
info=$(foreach d,$(infodirs),$(wildcard $(d)/*.info))

infodir=../info

.PHONY:all
all: obj submakes install-info

.PHONY:obj
obj: $(obj)

.PHONY:submakes
submakes: color-theme-autoloads cedet-makefiles
	for p in $(makedirs); do \
		$(MAKE) -C $$p ; \
	done

.PHONY:color-theme-autoloads
color-theme-autoloads:
	$(MAKE) -C color-theme autoloads

.PHONY:cedet-makefiles
cedet-makefiles:
	find cedet -name Makefile -exec touch \;

.PHONY:install-info
install-info: $(infodir)
# Do submake to force $(info) to be re-evaluated
	$(MAKE) $(infodir)/dir

$(infodir):
	mkdir -p $(infodir)

$(infodir)/dir: $(info)
	cp $? $(infodir)
	$(foreach i,$(notdir $?),install-info --info-dir=$(infodir) --dir-file=$@ $(infodir)/$(i) ;)

%.elc : %.el
	$(EMACS) -batch --no-site-file $(foreach hack,$(wildcard $(dir $?)*.hack),-l $(hack)) \
	 -f batch-byte-compile $?
# windows emacs generates object files with execute bit set, let's remove it
ifeq ($(OSTYPE),cygwin)
	chmod -x $@
endif
