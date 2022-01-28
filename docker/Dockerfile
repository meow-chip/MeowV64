# https://github.com/nightscape/scala-mill/blob/master/Dockerfile
FROM debian:bullseye
RUN sed -i 's/deb.debian.org/mirrors.tuna.tsinghua.edu.cn/g' /etc/apt/sources.list
RUN sed -i 's/security.debian.org/mirrors.tuna.tsinghua.edu.cn/g' /etc/apt/sources.list
RUN apt update
RUN apt install -y wget verilator iverilog curl
RUN wget --quiet https://github.com/riscv-collab/riscv-gnu-toolchain/releases/download/2022.01.17/riscv64-elf-ubuntu-20.04-nightly-2022.01.17-nightly.tar.gz -O toolchain.tgz
RUN tar -xzf toolchain.tgz
RUN cp -r riscv/* /usr/local
RUN sh -c "curl -L https://github.com/com-lihaoyi/mill/releases/download/0.9.12/0.9.12 > /usr/local/bin/mill && chmod +x /usr/local/bin/mill"
RUN apt update
RUN apt install -y openjdk-11-jdk
RUN touch build.sc
RUN mill -i resolve _
RUN rm build.sc
RUN apt install -y make bsdextrautils autoconf g++ build-essential
RUN apt install -y zlib1g-dev
