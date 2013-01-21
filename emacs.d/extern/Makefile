# compile these directories ourselves
dirs=. session/lisp doxymacs

# these dirs come with makefiles
makedirs=elib cedet

EMACS=emacs
src=$(foreach d,$(dirs),$(wildcard $(d)/*.el))
obj=$(patsubst %.el,%.elc,$(src))
info=$(foreach d,$(infodirs),$(wildcard $(d)/*.info))

infodir=$(realpath ../info)

.PHONY:all
all: obj submakes install-info

.PHONY:obj
obj: $(obj)

.PHONY:submakes
submakes: 
	for p in $(makedirs); do \
		$(MAKE) -C $$p ; \
	done

.PHONY:install-info
install-info: $(infodir)
	for p in $(makedirs); do \
		echo "making in $$p" ; \
		$(MAKE) -C $$p infodir=$(infodir) INFO_DIR=$(infodir) INSTALL_INFO=install-info install-info ; \
	done

%.elc : %.el
	$(EMACS) -batch --no-site-file $(foreach hack,$(wildcard $(dir $?)*.hack),-l $(hack)) \
	 -f batch-byte-compile $?
# windows emacs generates object files with execute bit set, let's remove it
ifeq ($(OSTYPE),cygwin)
	chmod -x $@
endif

