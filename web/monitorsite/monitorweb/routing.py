from django.urls import re_path

from monitorweb.consumers import consumer_factory
from monitorweb.utils.status_check import AgentStatusUpdater

status_updater = AgentStatusUpdater(30)
thread = threading.Thread(target=status_updater.start)
thread.start()

websocket_urlpatterns = [
    re_path(r"ws/ips/(?P<agent_name>\w+)/$", consumers.IPSConsumer.as_asgi()),
]