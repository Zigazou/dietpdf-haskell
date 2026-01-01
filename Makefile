# Use the fpm command (from Ruby gem fpm) to create a Debian package
dist/dietpdf_1.1.0_amd64.deb: .stack-work/dist/x86_64-linux-tinfo6/ghc-9.8.4/build/dietpdf/dietpdf
	rm -f $@
	fpm \
		-s dir \
		-t deb \
		--name dietpdf \
		--architecture amd64 \
		--version 1.0.0 \
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
