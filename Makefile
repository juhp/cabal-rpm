help:
	@echo "devel targets: dist, copy, git-tag, git-push, upload"

README.html: README.md
	pandoc -s $< > $@

upload:
	cabal upload

git-tag:
	git tag $(VERSION)

git-push:
	git push
	git push --tags

copy:
	cp -p $(NAME)-$(VERSION).tar.gz ~/fedora/haskell/cabal-rpm/
