
SITE_LISP ?= ~/site-lisp

help:
	@echo "git - clone into ~/site-lisp (override with SITE_LISP)"

$(SITE_LISP):
	mkdir $@

git: $(SITE_LISP) $(SITE_LISP)/magit $(SITE_LISP)/helm
.PHONY: git

$(SITE_LISP)/helm:
	git clone git://github.com/emacs-helm/helm.git $@

$(SITE_LISP)/magit:
	git clone https://github.com/magit/magit.git $@

$(SITE_LISP)/egit:
	git clone git://github.com/jimhourihan/egit.git $@

# https://github.com/magnars/hardcore-mode.el.git
# git://repo.or.cz/org-mode.git
# https://github.com/kiwanami/emacs-calfw.git 
