from django.urls import re_path

from . import consumers

websocket_urlpatterns = [
    re_path(r"ws/ips/(?P<agent_id>\w+)/$", consumers.IPSConsumer.as_asgi()),
]