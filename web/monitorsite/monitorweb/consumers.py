import json

from channels.generic.websocket import AsyncWebsocketConsumer

class IPSConsumer(AsyncWebsocketConsumer):
    async def connect(self):
        self.agent_name = self.scope["url_route"]["kwargs"]["agent_name"]
        self.agent_group_name = "ipsgroup_%s" % self.agent_name

        # Join agent group
        await self.channel_layer.group_add(self.agent_group_name, self.channel_name)

        await self.accept()

    async def disconnect(self, close_code):
        # Leave agent group
        await self.channel_layer.group_discard(self.agent_group_name, self.channel_name)

    # Receive message from WebSocket
    async def receive(self, text_data):
        text_data_json = json.loads(text_data)
        data = text_data_json["data"]

        # Send message to agent group
        await self.channel_layer.group_send(
            self.agent_group_name, {"type": "chart_update", "data": data}
        )

    # Receive message from agent group
    async def agent_metrics(self, event):
        data = event["data"]

        # Send message to WebSocket
        await self.send(text_data=json.dumps({"data": data}))