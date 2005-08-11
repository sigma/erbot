.PHONY: all lisp contrib clean realclean distclean fullclean install dist
.PRECIOUS: %.elc

EMACS    = emacs
SITEFLAG = --no-site-file

# Xemacs users will probably want the following settings.
#EMACS    = xemacs
#SITEFLAG = -no-site-file

# Installation options
# PREFIX is only used here.
PREFIX   = /usr/local
ELISPDIR = $(PREFIX)/share/emacs/site-lisp/erbot

all: lisp contrib

lisp:
	@$(EMACS) -q $(SITEFLAG) -batch --debug-init \
		-l erball.el \
		-f erball-compile --compile-erbot

contrib:
	@(cd contrib && \
	   $(EMACS) -q $(SITEFLAG) -batch \
		-l ../erball.el \
		-f erball-compile \
		--paths-rel-to '../' --compile-erbot)

clean realclean distclean fullclean:
	-rm -f *.elc contrib/*.elc *~ contrib/*~

install:
	install -d $(ELISPDIR)
	install -m 0644 *.el *.elc $(ELISPDIR)
	install -d $(ELISPDIR)/contrib
	install -m 0644 contrib/*.el contrib/*.elc $(ELISPDIR)/contrib

dist: distclean
	(cd ..; tar cvzf ../erbot.tar.gz erbot)
