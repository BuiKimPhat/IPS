import json
import time
from channels.generic.websocket import AsyncJsonWebsocketConsumer
from .models import Agent, Alert
from django.db.models import Q
from django.core.exceptions import ValidationError
import re
from monitorweb.utils.enums import Request
from monitorweb.utils.waf import WAF

waf = WAF()

class IPSConsumer(AsyncJsonWebsocketConsumer):
    async def connect(self):
        self.agent_name = self.scope["url_route"]["kwargs"]["agent_name"]
        self.agent_group_name = "ipsgroup_%s" % self.agent_name
        self.status_updater = self.scope["url_route"]["kwargs"]['status_updater']

        # Join agent group
        await self.channel_layer.group_add(self.agent_group_name, self.channel_name)
        await self.accept()

    async def disconnect(self, close_code):
        # Leave agent group
        await self.channel_layer.group_discard(self.agent_group_name, self.channel_name)

    # Receive message from WebSocket
    async def receive_json(self, text_data_json):
        try:
            # text_data_json = json.loads(text_data)
            print(text_data_json)

            # Register agent
            if text_data_json["type"] == "agent_register":
                agent_name = text_data_json["agent_name"]
                agent_ip = text_data_json["agent_ip"]
                agent_health = text_data_json["agent_health"]
                agent_status = "Registered" # Agent is registered but not yet received any metrics/health checks

                # Validate health check URL
                if agent_health != "None":
                    try:
                        self.validate(agent_health)
                    except ValidationError:
                        await self.channel_layer.group_send(
                            self.agent_group_name, {"type": "agent_register", "message": f"ERROR: Incorrect URL format for health check. Health check URL must be in a full URL form (example: https://www.example.com)."}
                        )

                try:
                    obj =  await Agent.objects.aget(Q(name=agent_name) | Q(ip=agent_ip))
                    created = True
                except Agent.DoesNotExist:
                    created = False

                # Send message to agent group
                if not created:
                    new_agent = Agent(name=agent_name, ip=agent_ip, health=agent_health, status=agent_status)
                    await new_agent.asave()
                    await self.channel_layer.group_send(
                        self.agent_group_name, {"type": "agent_register", "message": f"Agent {agent_name} has been registered successfully!"}
                    )
                else:
                    # Respond message
                    if agent_ip == obj.ip and agent_name != obj.name:
                        await self.channel_layer.group_send(
                            self.agent_group_name, {"type": "agent_register", "message": f"Agent {agent_name} was already registered with name '{obj.name}'. Messages will be sent to agent '{obj.name}'"}
                        )
                    if agent_name == obj.name and agent_ip != obj.ip:
                        await self.channel_layer.group_send(
                            self.agent_group_name, {"type": "agent_register", "message": f"ERROR: Agent name '{agent_name}' was already registered. Please choose a different name for your agent."}
                        )
                    if agent_name == obj.name and agent_ip == obj.ip:
                        # Update health check URL
                        obj.health = agent_health
                        await obj.asave()

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
                # Update last active time
                self.status_updater.update_last_activity_time(self.agent_name)

            # Log stream
            if text_data_json["type"] == "access_log":
                remote_addr = text_data_json["remote_addr"]
                timestamp = text_data_json["timestamp"]
                remote_user = text_data_json["remote_user"]
                request = text_data_json["request"]
                status = text_data_json["status"]
                body_bytes_sent = text_data_json["body_bytes_sent"]
                request_time = text_data_json["request_time"]
                request_body = text_data_json["request_body"]
                req_header = text_data_json["req_header"]

                # Send message to agent group
                await self.channel_layer.group_send(
                    self.agent_group_name, 
                    {   
                        "type": "access_log", 
                        'remote_addr': remote_addr,
                        'remote_user': remote_user,
                        'timestamp': timestamp,
                        'request': request,
                        'status': status,
                        'body_bytes_sent': body_bytes_sent,
                        'request_time': request_time,
                        'request_body': request_body,
                        'req_header': req_header,
                        'destination': self.agent_name
                    }
                )
                # Update last active time
                self.status_updater.update_last_activity_time(self.agent_name)

        except Exception as e:
            print("Unexpected error! ", e)


    # Receive message from agent group
    async def agent_register(self, event):
        # Register agent
        message = event["message"]
        
        # Send message to WebSocket
        await self.send_json({"type":"agent_register","message": message})

    async def access_log(self, event):
        # New access log on nginx agent
        request = {
            Request.ip : event["remote_addr"],
            Request.timestamp : event["timestamp"],
            Request.user : event["remote_user"],
            Request.url : event["request"],
            Request.status : event["status"],
            Request.bbs : event["body_bytes_sent"],
            Request.req_time : event["request_time"],
            Request.body : event["request_body"],
            Request.headers : event["req_header"],
            Request.destination : event["destination"]
        }

        # try:
        #     obj =  await Agent.objects.aget(name='agent1')
        #     created = True
        # except Agent.DoesNotExist:
        #     created = False


        # new_alert = Alert(agent=obj,message=message[:450], src='1.1.1.1', dst='1.1.1.2', dstp=80, protocol='TCP')
        # await new_alert.asave()

        # TODO: Implement log processing
        waf.detect_attack(request)
        
        # Send alert to WebSocket
        await self.send_json({"type":"attack_alert","message": f"Alert: "})

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
        await self.send_json({   
                "type": "metrics_update",
                "cpu_percent": cpu_percent,
                "mem_percent": mem_percent,
                "disk_read": disk_read,
                "disk_write": disk_write,
                "net_out": net_out,
                "net_in": net_in,
                "timestamp": timestamp,
            })

