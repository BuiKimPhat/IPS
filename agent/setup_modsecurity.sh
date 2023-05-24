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

# Build modsecurity modules for nginx
wget http://nginx.org/download/nginx-1.18.0.tar.gz
tar -xvzf nginx-1.18.0.tar.gz
cd nginx-1.18.0
./configure --add-dynamic-module=../ModSecurity-nginx --with-compat
make modules
mkdir /etc/nginx/modules
cp objs/ngx_http_modsecurity_module.so /etc/nginx/modules

# Load modsecurity module in nginx
echo "load_module /etc/nginx/modules/ngx_http_modsecurity_module.so;" | tee -a /etc/nginx/nginx.conf

# Setting Up OWASP-CRS
git clone https://github.com/coreruleset/coreruleset /usr/local/modsecurity-crs
mv /usr/local/modsecurity-crs/crs-setup.conf.example /usr/local/modsecurity-crs/crs-setup.conf
mv /usr/local/modsecurity-crs/rules/REQUEST-900-EXCLUSION-RULES-BEFORE-CRS.conf.example /usr/local/modsecurity-crs/rules/REQUEST-900-EXCLUSION-RULES-BEFORE-CRS.conf

# Configuring Modsecurity
mkdir -p /etc/nginx/modsec
cp /opt/ModSecurity/unicode.mapping /etc/nginx/modsec
cp /opt/ModSecurity/modsecurity.conf-recommended /etc/nginx/modsec/modsecurity.conf
sed -i 's/SecRuleEngine DetectionOnly/SecRuleEngine On/g' /etc/nginx/modsec/modsecurity.conf
cat << EOF > /etc/nginx/modsec/main.conf
Include /etc/nginx/modsec/modsecurity.conf
Include /usr/local/modsecurity-crs/crs-setup.conf
Include /usr/local/modsecurity-crs/rules/*.conf
EOF