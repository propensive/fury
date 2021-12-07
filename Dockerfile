FROM ubuntu:22.04
RUN apt update
RUN apt install -y git make openjdk-17-jre-headless wget
RUN git clone https://github.com/propensive/ire
RUN cd ire && make
RUN chmod +x ire/ire
