#!/bin/bash

# Install nginx
apt-get update -y
apt-get install -y nginx unzip

# Install ModSecurity
DEBIAN_FRONTEND=noninteractive apt-get install -y bison build-essential ca-certificates curl dh-autoreconf doxygen \
  flex gawk git iputils-ping libcurl4-gnutls-dev libexpat1-dev libgeoip-dev liblmdb-dev \
  libpcre3-dev libpcre++-dev libssl-dev libtool libxml2 libxml2-dev libyajl-dev locales \
  lua5.3-dev pkg-config wget zlib1g-dev libgd-dev autoconf

cd /opt
wget --load-cookies /tmp/cookies.txt "https://docs.google.com/uc?export=download&confirm=$(wget --quiet --save-cookies /tmp/cookies.txt --keep-session-cookies --no-check-certificate 'https://docs.google.com/uc?export=download&id=1nohrFNyilEvf6C8CD1p6LD0oi6Lzj1tL' -O- | sed -rn 's/.*confirm=([0-9A-Za-z_]+).*/\1\n/p')&id=1nohrFNyilEvf6C8CD1p6LD0oi6Lzj1tL" -O ModSecurity.zip && rm -rf /tmp/cookies.txt
unzip ModSecurity.zip && cd ModSecurity
git submodule init
git submodule update
./build.sh
./configure
make install

# Install ModSecurity-nginx connector
cd /opt && git clone --depth 1 https://github.com/SpiderLabs/ModSecurity-nginx.git