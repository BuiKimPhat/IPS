import json
import time
from channels.generic.websocket import AsyncWebsocketConsumer
from .models import Agent


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
        try:
            text_data_json = json.loads(text_data)
            # print(text_data_json)

            # Register agent
            if text_data_json["type"] == "agent_register":
                agent_name = text_data_json["agent_name"]
                agent_ip = text_data_json["agent_ip"]
                agent_health = text_data_json["agent_health"]
                agent_status = "Registered" # Agent is registered but not yet received any metrics/health checks

                obj, created = await Agent.objects.aget_or_create(name=agent_name, ip=agent_ip, health=agent_health, status=agent_status)

                # Send message to agent group
                if created:
                    await self.channel_layer.group_send(
                        self.agent_group_name, {"type": "agent_register", "message": f"Agent {agent_name} has been registered successfully!"}
                    )
                else:
                    if agent_ip == obj.ip and agent_name != obj.name:
                        await self.channel_layer.group_send(
                            self.agent_group_name, {"type": "agent_register", "message": f"Agent {agent_name} was already registered with name '{obj.name}'."}
                        )
                    if agent_name == obj.name and agent_ip != obj.ip:
                        await self.channel_layer.group_send(
                            self.agent_group_name, {"type": "agent_register", "message": f"ERROR: Agent name '{agent_name}' was already registered. Please choose a different name for your agent."}
                        )
                    if agent_name == obj.name and agent_ip == obj.ip:
                        await self.channel_layer.group_send(
                            self.agent_group_name, {"type": "agent_register", "message": f"Successfully connected to WebSocket server."}
                        )


            # Metrics update
            if text_data_json["type"] == "metrics_update":
                cpu_percent = text_data_json["cpu_percent"]
                mem_percent = text_data_json["mem_percent"]
                disk_read = text_data_json["disk_read"]
                disk_write = text_data_json["disk_write"]
                net_out = text_data_json["net_out"]
                net_in = text_data_json["net_in"]
                timestamp = text_data_json["timestamp"]
                # Send message to agent group
                await self.channel_layer.group_send(
                    self.agent_group_name, 
                    {   
                        "type": "metrics_update", 
                        "cpu_percent": cpu_percent,
                        "mem_percent": mem_percent,
                        "disk_read": disk_read,
                        "disk_write": disk_write,
                        "net_out": net_out,
                        "net_in": net_in,
                        "timestamp": timestamp
                    }
                )                    

        except:
            print("Unexpected error! Last message: ", text_data)


    # Receive message from agent group
    async def agent_register(self, event):
        # Register agent
        message = event["message"]
        
        # Send message to WebSocket
        await self.send(text_data=json.dumps({"type":"agent_register","message": message}))

    async def metrics_update(self, event):
        # Real-time metrics
        
        cpu_percent = event["cpu_percent"]
        mem_percent = event["mem_percent"]
        disk_read = event["disk_read"]
        disk_write = event["disk_write"]
        net_out = event["net_out"]
        net_in = event["net_in"]
        timestamp = event["timestamp"]

        # Send message to WebSocket
        await self.send(text_data=json.dumps({   
                "type": "metrics_update",
                "cpu_percent": cpu_percent,
                "mem_percent": mem_percent,
                "disk_read": disk_read,
                "disk_write": disk_write,
                "net_out": net_out,
                "net_in": net_in,
                "timestamp": timestamp,
            }))

def consumer_factory(scope):
    agent_name = scope['url_route']['kwargs']['agent_name']
    status_checker = scope['status_checker']
    return MyConsumer.as_asgi()(scope, agent_name=agent_name, status_checker=status_checker)