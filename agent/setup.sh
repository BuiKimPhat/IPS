#!/bin/sh
# For nginx 1.18.0 on Ubuntu 22.04
# Dependencies
apt-get update
DEBIAN_FRONTEND=noninteractive apt-get install -y build-essential libpcre3-dev libssl-dev zlib1g-dev

# Download nginx source
cd /opt && wget http://nginx.org/download/nginx-1.18.0.tar.gz
tar -xvzf nginx-1.18.0.tar.gz
cd nginx-1.18.0

# Install LuaJIT 2.1
wget https://github.com/openresty/luajit2/archive/refs/tags/v2.1-20230410.tar.gz
tar -xvzf v2.1-20230410.tar.gz
cd luajit2-2.1-20230410/
make
make install
export LUAJIT_LIB=/usr/local/lib
export LUAJIT_INC=/usr/local/include/luajit-2.1

# Download and build ngx_devel_kit, lua-nginx-module, lua-resty-core, lua-resty-lrucache as modules
cd ../
wget https://github.com/vision5/ngx_devel_kit/archive/refs/tags/v0.3.2.tar.gz
tar -xvzf v0.3.2.tar.gz
wget https://github.com/openresty/lua-nginx-module/archive/refs/tags/v0.10.24.tar.gz
tar -xvzf v0.10.24.tar.gz
wget https://github.com/openresty/lua-resty-core/archive/refs/tags/v0.1.26.tar.gz
tar -xvzf v0.1.26.tar.gz
wget https://github.com/openresty/lua-resty-lrucache/archive/refs/tags/v0.13.tar.gz
tar -xvzf v0.13.tar.gz

./configure --prefix=/usr/share/nginx \
            --with-ld-opt="-Wl,-rpath,/usr/local/lib" \
            --add-dynamic-module=./ngx_devel_kit-0.3.2 \
            --add-dynamic-module=./lua-nginx-module-0.10.24 \
            --with-compat

make modules
mkdir /etc/nginx/modules
cp objs/ndk_http_module.so /etc/nginx/modules
cp objs/ngx_http_lua_module.so /etc/nginx/modules

cd lua-resty-core-0.1.26
make install PREFIX=/usr/share/nginx
cd ../lua-resty-lrucache-0.13
make install PREFIX=/usr/share/nginx

# Load modules in nginx
sed -i 's/pid \/run\/nginx.pid;/pid \/run\/nginx.pid;\nload_module \/etc\/nginx\/modules\/ndk_http_module.so;\nload_module \/etc\/nginx\/modules\/ngx_http_lua_module.so;/' /etc/nginx/nginx.conf
sed -i 's/http {/http {\n\tlua_package_path "\/usr\/share\/nginx\/lib\/lua\/\?\.lua;;";/' /etc/nginx/nginx.conf