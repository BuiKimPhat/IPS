# IPS

### Features
- [x] Monitor web servers/proxies (using NGINX) metrics

### To-do
- [ ] IPS with simple Top 10 OWASP
- [ ] Web UI for custom rules (iptables)
- [ ] Monitor logs

### Setup on **agent**
1. Run `sudo ./agent/setup.sh`
2. For code format, add the following code to your desired block (http, server, location,...), supposing that the output log path is `/var/log/nginx/access.log` 
```
log_format json_full escape=json
  '{'
    '"timestamp":"$time_local",'
    '"remote_addr":"$remote_addr",'
    '"remote_user":"$remote_user",'
    '"request":"$request",'
    '"status": "$status",'
    '"body_bytes_sent":"$body_bytes_sent",'
    '"request_time":"$request_time",'
    '"request_body":"$request_body",'
    '"req_header":"$req_header"'
  '}';

access_log /var/log/nginx/access.log json_full;
```
3. For Lua code to extract req/resp content, add the following code to your desired block (server, location, if, ...)
```
# Get response body
lua_need_request_body on;

set $resp_body "";
body_filter_by_lua '
    local resp_body = string.sub(ngx.arg[1], 1, 1000)
    ngx.ctx.buffered = (ngx.ctx.buffered or "") .. resp_body
    if ngx.arg[2] then
        ngx.var.resp_body = ngx.ctx.buffered
    end
';

# Get request header
set $req_header "";
header_filter_by_lua ' 
    local h = ngx.req.get_headers()
    for k, v in pairs(h) do
        ngx.var.req_header = ngx.var.req_header .. k.."="..v.."~"
    end
';
```
4. Restart nginx service `sudo service nginx restart`
