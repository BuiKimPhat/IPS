# IPS

### Features
- [x] Monitor web servers/proxies (using NGINX) metrics

### To-do
- [ ] IPS with simple Top 10 OWASP
- [ ] Web UI for custom rules (iptables)
- [ ] Monitor logs

### Setup on **agent**
1. Run `sudo ./agent/setup.sh`
2. For code format, add the following code to your desired block (http, server, location,...)
```
log_format json_combined escape=json
  '{'
    '"timestamp":"$time_local",'
    '"remote_addr":"$remote_addr",'
    '"remote_user":"$remote_user",'
    '"request":"$request",'
    '"status": "$status",'
    '"body_bytes_sent":"$body_bytes_sent",'
    '"request_time":"$request_time",'
    '"http_referrer":"$http_referer",'
    '"http_user_agent":"$http_user_agent"'
  '}';
log_format log_req_resp '$remote_addr - $remote_user [$time_local] '
                        '"$request" $status $body_bytes_sent ${request_time}ms '
                        '$request_body<>$req_header';

access_log  logs/access.log json_combined;
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
