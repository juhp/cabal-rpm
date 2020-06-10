man: man/cabal-rpm.1

man/cabal-rpm.1: man/cabal-rpm.1.md
	pandoc -s -t man $< > $@

stack-all:
	stack --resolver nightly --stack-yaml stack-ghc810.yaml build
	@echo
	stack --resolver nightly build
	@echo
	stack --resolver lts build
	@echo
	stack --resolver lts-15 build
	@echo
	stack --resolver lts-14 build
	@echo
	stack --resolver lts-13 --stack-yaml stack-lts13.yaml build
	@echo
	stack --resolver lts-12 --stack-yaml stack-lts12.yaml build
	@echo
	stack --resolver lts-11 --stack-yaml stack-lts11.yaml build
	@echo
	stack --resolver lts-10 --stack-yaml stack-lts11.yaml build
	@echo
	stack --resolver lts-9 --stack-yaml stack-lts9.yaml build
	@echo
	stack --resolver lts-8 --stack-yaml stack-lts9.yaml build
