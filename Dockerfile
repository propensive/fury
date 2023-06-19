FROM openjdk:11
RUN apt update
RUN apt install -y jq
RUN mkdir /fury
RUN git clone https://github.com/lampepfl/dotty /fury/scala
RUN mkdir -p /fury/bin
RUN curl -Lo /fury/sbt.tgz https://github.com/sbt/sbt/releases/download/v1.7.1/sbt-1.7.1.tgz
RUN tar xvf /fury/sbt.tgz -C /fury
ENV PATH="/fury/sbt/bin:${PATH}"
ENV GITHUB_ACTIONS="true"
RUN /fury/scala/bin/scalac -version
RUN mkdir /fury/lib

RUN curl -Lo "/fury/jdk20.tar.gz" "https://api.adoptium.net/v3/binary/latest/20/ea/linux/x64/jre/hotspot/normal/eclipse"
RUN sh -c "tar xvf /fury/jdk20.tar.gz && mv /jdk-20* /fury/jdk"
ENV PATH="/fury/jdk/bin:$PATH"

RUN curl -Lo /fury/lib/jawn-parser.jar \
  https://repo1.maven.org/maven2/org/typelevel/jawn-parser_3/1.2.0/jawn-parser_3-1.2.0.jar

RUN curl -Lo /fury/lib/jawn-ast.jar \
  https://repo1.maven.org/maven2/org/typelevel/jawn-ast_3/1.2.0/jawn-ast_3-1.2.0.jar

RUN curl -Lo /fury/lib/jawn-util.jar \
  https://repo1.maven.org/maven2/org/typelevel/jawn-util_3/1.2.0/jawn-util_3-1.2.0.jar

RUN curl -Lo /fury/lib/servlet-api.jar \
  https://repo1.maven.org/maven2/javax/servlet/javax.servlet-api/3.0.1/javax.servlet-api-3.0.1.jar

RUN curl -Lo /fury/lib/jna.jar \
  https://repo1.maven.org/maven2/net/java/dev/jna/jna/5.3.1/jna-5.3.1.jar

RUN unzip -q -o -d /fury/bin /fury/lib/jawn-parser.jar
RUN unzip -q -o -d /fury/bin /fury/lib/jawn-ast.jar
RUN unzip -q -o -d /fury/bin /fury/lib/jawn-util.jar
RUN unzip -q -o -d /fury/bin /fury/lib/servlet-api.jar
RUN unzip -q -o -d /fury/bin /fury/lib/jna.jar
RUN unzip -q -o -d /fury/bin /fury/scala/dist/target/pack/lib/flexmark-0.42.12.jar
RUN unzip -q -o -d /fury/bin /fury/scala/dist/target/pack/lib/flexmark-util-0.42.12.jar
RUN unzip -q -o -d /fury/bin /fury/scala/dist/target/pack/lib/flexmark-formatter-0.42.12.jar
RUN unzip -q -o -d /fury/bin /fury/scala/dist/target/pack/lib/flexmark-ext-tables-0.42.12.jar
RUN rm -r /fury/bin/META-INF
ADD src /fury/src
ADD one.zip /fury/one.zip
RUN unzip -q /fury/one.zip -d /

RUN javac -classpath /fury/lib/jna.jar \
  -d /fury/bin \
  /one/mod/profanity/src/java/profanity/Termios.java

RUN cp -r /one/mod/gesticulate/res/gesticulate /fury/bin/
RUN cp -r /one/mod/oubliette/res/oubliette /fury/bin/

RUN java -version
ENV JAVA_HOME=/fury/jdk

RUN cd /fury && scala/bin/scalac \
  -classpath bin \
  -Xmax-inlines 64 \
  -J-Xss1536k \
  -J--enable-preview \
  -deprecation \
  -feature \
  -Wunused:all \
  -new-syntax \
  -Yrequire-targetName \
  -Ysafe-init \
  -Yexplicit-nulls \
  -Ycheck-all-patmat \
  -language:experimental.fewerBraces \
  -language:experimental.saferExceptions \
  -language:experimental.erasedDefinitions \
  -language:experimental.namedTypeArguments \
  -d bin \
  /one/mod/acyclicity/src/core/*.scala \
  /one/mod/adversaria/src/core/*.scala \
  /one/mod/caesura/src/core/*.scala \
  /one/mod/cardinality/src/core/*.scala \
  /one/mod/anticipation/src/css/*.scala \
  /one/mod/anticipation/src/file/*.scala \
  /one/mod/anticipation/src/html/*.scala \
  /one/mod/anticipation/src/http/*.scala \
  /one/mod/anticipation/src/time/*.scala \
  /one/mod/anticipation/src/uri/*.scala \
  /one/mod/contextual/src/core/*.scala \
  /one/mod/cosmopolite/src/core/*.scala \
  /one/mod/escapade/src/core/*.scala \
  /one/mod/escritoire/src/core/*.scala \
  /one/mod/eucalyptus/src/core/*.scala \
  /one/mod/gastronomy/src/core/*.scala \
  /one/mod/gesticulate/src/core/*.scala \
  /one/mod/gossamer/src/core/*.scala \
  /one/mod/imperial/src/core/*.scala \
  /one/mod/iridescence/src/core/*.scala \
  /one/mod/kaleidoscope/src/core/*.scala \
  /one/mod/probably/src/cli/*.scala \
  /one/mod/probably/src/core/*.scala \
  /one/mod/probably/src/tolerance/*.scala \
  /one/mod/profanity/src/core/*.scala \
  /one/mod/profanity/src/java/**/*.java \
  /one/mod/serpentine/src/core/*.scala \
  /one/mod/parasite/src/core/*.scala \
  /one/mod/tetromino/src/core/*.scala \
  /one/mod/turbulence/src/core/*.scala \
  /one/mod/wisteria/src/core/*.scala \
  /one/mod/rudiments/src/core/*.scala

RUN cd /fury && scala/bin/scalac \
  -classpath bin \
  -Xmax-inlines 64 \
  -J-Xss1536k \
  -J--enable-preview \
  -deprecation \
  -feature \
  -Wunused:all \
  -new-syntax \
  -Yrequire-targetName \
  -Ysafe-init \
  -Yexplicit-nulls \
  -Ycheck-all-patmat \
  -language:experimental.fewerBraces \
  -language:experimental.saferExceptions \
  -language:experimental.erasedDefinitions \
  -language:experimental.namedTypeArguments \
  -d bin \
  /one/mod/euphemism/src/core/*.scala \
  /one/mod/temporaneous/src/core/*.scala \
  /one/mod/merino/src/core/*.scala \
  /one/mod/exoskeleton/src/core/*.scala \
  /one/mod/xylophone/src/core/*.scala \
  /one/mod/harlequin/src/core/*.scala \
  /one/mod/punctuation/src/ansi/*.scala \
  /one/mod/joviality/src/core/*.scala \
  /one/mod/joviality/src/integration/*.scala \
  /one/mod/cataclysm/src/core/*.scala \
  /one/mod/cellulose/src/core/*.scala \
  /one/mod/quagmire/src/core/*.scala \
  /one/mod/oubliette/src/core/*.scala \
  /one/mod/polyvinyl/src/core/*.scala \
  /one/mod/punctuation/src/core/*.scala \
  /one/mod/telekinesis/src/client/*.scala \
  /one/mod/telekinesis/src/uri/*.scala \
  /one/mod/punctuation/src/html/*.scala \
  /one/mod/honeycomb/src/core/*.scala \
  /one/mod/scintillate/src/server/*.scala \
  /one/mod/scintillate/src/servlet/*.scala \
  /one/mod/guillotine/src/core/*.scala \
  /one/mod/surveillance/src/core/*.scala \
  /one/mod/tarantula/src/core/*.scala

RUN cd /fury && scala/bin/scalac \
  -classpath bin \
  -J--enable-preview \
  -Xmax-inlines 64 \
  -deprecation \
  -feature \
  -Wunused:all \
  -new-syntax \
  -Yrequire-targetName \
  -Ysafe-init \
  -Yexplicit-nulls \
  -Ycheck-all-patmat \
  -language:experimental.fewerBraces \
  -language:experimental.saferExceptions \
  -language:experimental.erasedDefinitions \
  -language:experimental.namedTypeArguments \
  -d bin \
  src/core/*.scala

RUN unzip -q -o -d /fury/bin /fury/scala/dist/target/pack/lib/tasty-core*.jar
RUN unzip -q -o -d /fury/bin /fury/scala/dist/target/pack/lib/compiler-interface*.jar
RUN unzip -q -o -d /fury/bin /fury/scala/dist/target/pack/lib/scala-library*.jar
RUN unzip -q -o -d /fury/bin /fury/scala/dist/target/pack/lib/scala3-compiler*.jar
RUN unzip -q -o -d /fury/bin /fury/scala/dist/target/pack/lib/scala3-library*.jar
RUN unzip -q -o -d /fury/bin /fury/scala/dist/target/pack/lib/scala3-staging*.jar
RUN unzip -q -o -d /fury/bin /fury/scala/dist/target/pack/lib/scala3-interfaces*.jar
RUN unzip -q -o -d /fury/bin /fury/scala/dist/target/pack/lib/scala3-tasty-inspector*.jar
RUN unzip -q -o -d /fury/bin /fury/scala/dist/target/pack/lib/scala-asm*.jar
RUN cp /one/mod/exoskeleton/res/exoskeleton/invoke /fury/bin/exoskeleton/invoke
ADD build.irk /fury/build.irk
RUN echo 'Manifest-Version: 1.0' > /fury/manifest
RUN echo -n 'Created-By: Fury ' >> /fury/manifest
RUN jq -r '.modules[0].version' /fury/build.irk >> /fury/manifest
RUN echo 'Implementation-Title: Fury' >> /fury/manifest
RUN echo -n 'Implementation-Version: ' >> /fury/manifest
RUN jq -r '.modules[0].version' /fury/build.irk >> /fury/manifest
RUN echo 'Main-Class: fury.Fury' >> /fury/manifest

RUN jar cmf /fury/manifest /fury/fury.jar  \
  -C /fury/bin NOTICE \
  -C /fury/bin compiler.properties \
  -C /fury/bin incrementalcompiler.version.properties \
  -C /fury/bin library.properties \
  -C /fury/bin scala-asm.properties \
  -C /fury/bin xsbti \
  -C /fury/bin scala \
  -C /fury/bin dotty \
  -C /fury/bin acyclicity \
  -C /fury/bin adversaria \
  -C /fury/bin anticipation \
  -C /fury/bin caesura \
  -C /fury/bin cardinality \
  -C /fury/bin cataclysm \
  -C /fury/bin com \
  -C /fury/bin contextual \
  -C /fury/bin cosmopolite \
  -C /fury/bin escapade \
  -C /fury/bin escritoire \
  -C /fury/bin eucalyptus \
  -C /fury/bin euphemism \
  -C /fury/bin exoskeleton \
  -C /fury/bin gastronomy \
  -C /fury/bin gesticulate \
  -C /fury/bin gossamer \
  -C /fury/bin guillotine \
  -C /fury/bin harlequin \
  -C /fury/bin honeycomb \
  -C /fury/bin iridescence \
  -C /fury/bin fury \
  -C /fury/bin imperial \
  -C /fury/bin javax \
  -C /fury/bin joviality \
  -C /fury/bin kaleidoscope \
  -C /fury/bin oubliette \
  -C /fury/bin polyvinyl \
  -C /fury/bin quagmire \
  -C /fury/bin cellulose \
  -C /fury/bin merino \
  -C /fury/bin org \
  -C /fury/bin parasite \
  -C /fury/bin probably \
  -C /fury/bin profanity \
  -C /fury/bin punctuation \
  -C /fury/bin rudiments \
  -C /fury/bin scintillate \
  -C /fury/bin serpentine \
  -C /fury/bin surveillance \
  -C /fury/bin tarantula \
  -C /fury/bin temporaneous \
  -C /fury/bin telekinesis \
  -C /fury/bin tetromino \
  -C /fury/bin turbulence \
  -C /fury/bin wisteria \
  -C /fury/bin xylophone

RUN cat /one/mod/exoskeleton/res/exoskeleton/invoke /fury/fury.jar > /fury/bootstrap
RUN chmod +x /fury/bootstrap
RUN rm /fury/fury.jar
RUN cd /fury && ./bootstrap
RUN mv /fury/fury /fury/fury-bootstrap
RUN rm -rf /root/.cache/fury
RUN md5sum /fury/fury-bootstrap | cut -d' ' -f1 > bootstrap.md5
RUN cd /fury && ./fury-bootstrap
RUN md5sum /fury/fury | cut -d' ' -f1 > fury.md5
RUN diff fury.md5 bootstrap.md5
