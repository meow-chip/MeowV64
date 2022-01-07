# https://github.com/nightscape/scala-mill/blob/master/Dockerfile
FROM debian:bullseye
RUN sed -i 's/deb.debian.org/mirrors.tuna.tsinghua.edu.cn/g' /etc/apt/sources.list
RUN sed -i 's/security.debian.org/mirrors.tuna.tsinghua.edu.cn/g' /etc/apt/sources.list
RUN apt update
RUN apt install -y wget verilator iverilog curl
RUN wget --quiet https://static.dev.sifive.com/dev-tools/riscv64-unknown-elf-gcc-8.3.0-2019.08.0-x86_64-linux-ubuntu14.tar.gz -O toolchain.tgz
RUN tar -xzf toolchain.tgz
RUN cp -r riscv64-unknown-elf-gcc-8.3.0-2019.08.0-x86_64-linux-ubuntu14/* /usr/local
RUN sh -c "curl -L https://github.com/com-lihaoyi/mill/releases/download/0.10.0-M3/0.10.0-M3 > /usr/local/bin/mill && chmod +x /usr/local/bin/mill"
RUN apt install -y openjdk-11-jdk
RUN touch build.sc
RUN mill -i resolve _
RUN rm build.sc
RUN apt install -y make bsdextrautils autoconf g++ build-essential
