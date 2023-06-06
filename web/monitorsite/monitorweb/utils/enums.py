from enum import Enum

class Operator(Enum):
    AND = "AND"
    OR = "OR"
 
class Request(Enum):
    url = "URL"
    status = "Status"
    bbs = "BBS"
    user = "User"
    req_time = "ReqTime"
    body = "Body"
    headers = "Headers"
    ip = "IP"
    timestamp = "timestamp"
    destination = "destination"