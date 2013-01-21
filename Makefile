help:
	@echo "devel targets: sdist, version, git-tag, git-push, upload"

README.html: README.md
	pandoc -s $< > $@

sdist: man README.html
	cabal sdist

man: man/cblrpm.1

man/cblrpm.1: man/cblrpm.1.md
	pandoc -s -t man $< > $@

upload:
	cabal upload

CABAL = cabal-rpm.cabal
VERSION := $(shell sed -ne 's/^[Vv]ersion:[[:space:]]*//p' $(CABAL))

version:
	echo $(VERSION)

git-tag:
	git tag $(VERSION)

git-push:
	git push
	git push --tags

copy:
	cp -p $(NAME)-$(VERSION).tar.gz ~/fedora/haskell/cabal-rpm/
