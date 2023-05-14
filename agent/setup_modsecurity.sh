#!/bin/bash

# Install dependencies
apt-get update
apt-get install -y nginx libmodsecurity3 libnginx-mod-http-modsecurity

# Copy ModSecurity configuration file to Nginx configuration directory
cp /etc/nginx/modsec/modsecurity.conf-recommended /etc/nginx/modsec/modsecurity.conf

# Configure ModSecurity rules
curl https://raw.githubusercontent.com/SpiderLabs/owasp-modsecurity-crs/v3.4/dev/crs-setup.conf.example > /etc/nginx/modsec/main.conf
curl https://github.com/SpiderLabs/owasp-modsecurity-crs/archive/v3.4.0.tar.gz | tar -zxv
mv owasp-modsecurity-crs-3.4.0 /etc/nginx/modsec/crs
cp /etc/nginx/modsec/crs/crs-setup.conf.example /etc/nginx/modsec/crs/crs-setup.conf

# Enable ModSecurity in NGINX
sed -i 's/# include \/etc\/nginx\/modsec\/modsecurity.conf;/include \/etc\/nginx\/modsec\/modsecurity.conf;/' /etc/nginx/nginx.conf
sed -i 's/# modsecurity_rules_file \/etc\/nginx\/modsec\/main.conf;/modsecurity_rules_file \/etc\/nginx\/modsec\/main.conf;/' /etc/nginx/modsec/modsecurity.conf

# Restart NGINX
systemctl restart nginx