
SITE_LISP ?= ~/site-lisp

VPS_DIR ?= ~/.emacs.d/vps

VPS_TXT_FILES=$(wildcard $(VPS_DIR)/*.txt)

VPS_DB_FILES=$(patsubst %.txt,%.db,$(VPS_TXT_FILES))

help:
	@echo "git  - clone into ~/site-lisp (override with SITE_LISP)"
	@echo "db   - generate VPS inverse index files"

$(SITE_LISP):
	mkdir $@

git: $(SITE_LISP) $(SITE_LISP)/magit $(SITE_LISP)/helm
.PHONY: git

$(SITE_LISP)/helm:
	git clone git://github.com/emacs-helm/helm.git $@

$(SITE_LISP)/magit:
	git clone https://github.com/magit/magit.git $@

# https://github.com/magnars/hardcore-mode.el.git
# git://repo.or.cz/org-mode.git
# https://github.com/kiwanami/emacs-calfw.git 

.PHONY: db

db: $(VPS_DB_FILES)

%.db: %.txt
	@echo ---------------- $* ----------------
	perl -w ~/site-lisp/emacs-config/vj-make-index.pl $(notdir $*)
