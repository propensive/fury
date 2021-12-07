ire: niveau tmp/.publish scli
	./scli package -o ire --force --standalone -S 3.1.1-RC1-bin-SNAPSHOT -d org.typelevel:jawn-parser_3:1.2.0 -d org.typelevel:jawn-ast_3:1.2.0 -d com.vladsch.flexmark:flexmark:0.62.2 -d com.vladsch.flexmark:flexmark-ext-tables:0.62.2 -d com.vladsch.flexmark:flexmark-ext-typographic:0.62.2 -d javax.servlet:javax.servlet-api:3.0.1 -d net.java.dev.jna:jna:5.10.0 --progress --home . -language:experimental.erasedDefinitions -language:experimental.fewerBraces -language:experimental.saferExceptions -Wunused:all -O -deprecation -O -feature -O -new-syntax -Yrequire-targetName -Ysafe-init -Ycheck-all-patmat -Yexplicit-nulls src/core/*.scala niveau/mod/acyclicity/src/core/*.scala niveau/mod/adversaria/src/core/*.scala niveau/mod/caesura/src/core/*.scala niveau/mod/cataract/src/core/*.scala niveau/mod/clairvoyant/src/css/*.scala niveau/mod/clairvoyant/src/html/*.scala niveau/mod/clairvoyant/src/http/*.scala niveau/mod/contextual/src/core/*.scala niveau/mod/cosmopolite/src/core/*.scala niveau/mod/escapade/src/core/*.scala niveau/mod/escritoire/src/core/*.scala niveau/mod/eucalyptus/src/core/*.scala niveau/mod/euphemism/src/core/*.scala niveau/mod/exoskeleton/src/core/*.scala niveau/mod/gastronomy/src/core/*.scala niveau/mod/gesticulate/src/core/*.scala niveau/mod/gossamer/src/core/*.scala niveau/mod/guillotine/src/core/*.scala niveau/mod/harlequin/src/core/*.scala niveau/mod/honeycomb/src/core/*.scala niveau/mod/iridescence/src/core/*.scala niveau/mod/jovian/src/core/*.scala niveau/mod/kaleidoscope/src/core/*.scala niveau/mod/probably/src/core/*.scala niveau/mod/probably/src/cli/*.scala niveau/mod/probably/src/tolerance/*.scala niveau/mod/punctuation/src/core/*.scala niveau/mod/punctuation/src/ansi/*.scala niveau/mod/punctuation/src/html/*.scala niveau/mod/profanity/src/java/**/*.java niveau/mod/profanity/src/core/*.scala niveau/mod/rudiments/src/core/*.scala niveau/mod/scintillate/src/core/*.scala niveau/mod/scintillate/src/server/*.scala niveau/mod/scintillate/src/servlet/*.scala niveau/mod/slalom/src/core/*.scala niveau/mod/wisteria/src/core/*.scala niveau/mod/xylophone/src/core/*.scala

niveau:
	git clone git@github.com:propensive/niveau niveau --recursive

tmp:
	mkdir -p tmp

tmp/sbt.tgz: tmp
	curl -Lo tmp/sbt.tgz https://github.com/sbt/sbt/releases/download/v1.5.5/sbt-1.5.5.tgz

tmp/scli.gz: tmp
	curl -Lo tmp/scli.gz https://github.com/VirtusLab/scala-cli/releases/download/v0.0.9/scala-cli-x86_64-pc-linux-static.gz

scli: tmp/scli.gz
	gunzip -k tmp/scli.gz
	mv tmp/scli scli
	chmod +x scli

sbt/bin/sbt: tmp/sbt.tgz
	tar xvf tmp/sbt.tgz

scala:
	git clone git@github.com:dotty-staging/dotty --branch=fix-13691 scala

tmp/.publish: sbt scala
	cd scala && ../sbt/bin/sbt publishLocal
	touch tmp/.publish
