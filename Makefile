VERSION = $(shell cat package.yaml | grep '^version:' | grep --only-matching '[0-9]*\.[0-9]*\.[0-9]*')
GHC_VERSION = $(shell cat stack.yaml | grep '^compiler:' | grep --only-matching '[0-9]*\.[0-9]*\.[0-9]*')

# Use the fpm command (from Ruby gem fpm) to create a Debian package
dist/dietpdf_$(VERSION)_amd64.deb: .stack-work/dist/x86_64-linux/ghc-$(GHC_VERSION)/build/dietpdf/dietpdf Makefile
	rm -f $@
	fpm \
		-s dir \
		-t deb \
		--name dietpdf \
		--architecture amd64 \
		--version $(VERSION) \
		--description "Reduce PDF file size" \
		--license bsd3 \
		--url "https://github.com/zigazou/dietpdf-haskell" \
		--maintainer "Frédéric BISSON <zigazou@protonmail.com>" \
		--depends "ttfautohint" \
		--depends "libjpeg-turbo-progs" \
		--depends "grokj2k-tools" \
		--depends "imagemagick" \
		-p $@ \
		$<=/usr/local/bin/dietpdf
