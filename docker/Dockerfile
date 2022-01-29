# https://github.com/nightscape/scala-mill/blob/master/Dockerfile
FROM jiegec/riscv-toolchain
RUN sed -i 's/deb.debian.org/mirrors.tuna.tsinghua.edu.cn/g' /etc/apt/sources.list
RUN sed -i 's/security.debian.org/mirrors.tuna.tsinghua.edu.cn/g' /etc/apt/sources.list
RUN apt update
RUN apt install -y wget verilator iverilog curl
RUN sh -c "curl -L https://github.com/com-lihaoyi/mill/releases/download/0.9.12/0.9.12 > /usr/local/bin/mill && chmod +x /usr/local/bin/mill"
RUN apt update
RUN apt install -y openjdk-11-jdk
RUN touch build.sc
RUN mill -i resolve _
RUN rm build.sc
RUN apt install -y make bsdextrautils autoconf g++ build-essential
RUN apt install -y zlib1g-dev
