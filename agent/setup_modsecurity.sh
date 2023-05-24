#!/bin/bash

# Install nginx
apt-get update -y
apt-get install -y nginx

# Install ModSecurity
DEBIAN_FRONTEND=noninteractive apt-get install -y bison build-essential ca-certificates curl dh-autoreconf doxygen \
  flex gawk git iputils-ping libcurl4-gnutls-dev libexpat1-dev libgeoip-dev liblmdb-dev \
  libpcre3-dev libpcre++-dev libssl-dev libtool libxml2 libxml2-dev libyajl-dev locales \
  lua5.3-dev pkg-config wget zlib1g-dev libgd-dev autoconf

cd /opt && git clone https://github.com/SpiderLabs/ModSecurity
cd ModSecurity
git submodule init
git submodule update
./build.sh
./configure
make
make install

# Install ModSecurity-nginx connector
cd /opt && git clone --depth 1 https://github.com/SpiderLabs/ModSecurity-nginx.git