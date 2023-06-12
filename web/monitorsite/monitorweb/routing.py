import threading

from django.urls import re_path
from . import consumers
from monitorweb.utils.status_check import AgentStatusUpdater

# Run Agent status updater
status_updater = AgentStatusUpdater()
thread = threading.Thread(target=status_updater.start)
thread.start()

websocket_urlpatterns = [
    re_path(r"ws/ips/(?P<agent_name>\w+)/$", consumers.IPSConsumer.as_asgi(), {'status_updater': status_updater}),
    re_path(r"ws/ips/notification/$", consumers.IPSConsumer.as_asgi()),
    re_path(r"ws/ips/statistics/$", consumers.IPSConsumer.as_asgi())
]