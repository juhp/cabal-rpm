help:
	@echo "devel targets: git-tag, sdist, upload, version, git-push, copy, publish"

README.html: README.md
	pandoc -s $< > $@

sdist: man
	./make-dist $(VERSION)

man: man/cabal-rpm.1

man/cabal-rpm.1: man/cabal-rpm.1.md
	pandoc -s -t man $< > $@

upload:
	cabal upload dist/$(NAME)-$(VERSION).tar.gz

NAME= cabal-rpm
VERSION := $(shell sed -ne 's/^[Vv]ersion:[[:space:]]*//p' $(NAME).cabal)

version:
	@echo $(VERSION)

git-tag:
	git tag $(VERSION)

git-push:
	git push
	git push --tags

copy:
	cp -p dist/$(NAME)-$(VERSION).tar.gz ~/fedora/haskell/cabal-rpm/

publish:
	cabal upload --publish dist/$(NAME)-$(VERSION).tar.gz
