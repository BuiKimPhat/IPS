import json
from channels.generic.websocket import AsyncJsonWebsocketConsumer
from .models import Agent, Alert, Rule
from django.db.models import Q, Count
from django.core.exceptions import ValidationError
import re
from monitorweb.utils.enums import Request
from monitorweb.utils.waf import WAF
from channels.layers import get_channel_layer
from django.core.validators import URLValidator
import asyncio
import datetime
import traceback

waf = WAF()

notification_group_name = "ips_notification"
statistics_group_name = "ips_statistics"

class IPSConsumer(AsyncJsonWebsocketConsumer):
    async def fetch_stats(self, update_interval=300, eval_range=300, max_agent=15): 
        # Statistics for dashboard page 
        try:
            while True:
                unprocessed = await Alert.objects.filter(is_processed=False).acount()
                agent_num = await Agent.objects.all().acount()
                rules_set = await Rule.objects.all().acount()
                endtime = datetime.datetime.now()
                starttime = endtime - datetime.timedelta(seconds=eval_range) 
                healthy = Agent.objects.filter(status='Healthy').values('name')
                alerts = Alert.objects.filter(Q(timestamp__range=(starttime,endtime)) & Q(agent__status='Healthy')).select_related('agent').values('agent__name').annotate(count=Count('id')).order_by('-agent__registered_at')
                alert_num = []
                async for agent in healthy:
                    isIn = False
                    async for alert in alerts:
                        if alert['agent__name'] == agent["name"]:
                            isIn = True
                            alert_num.append({"agent__name": agent["name"], "count": alert['count']})
                            break
                    if not isIn:
                        alert_num.append({"agent__name": agent["name"], "count": 0})

                
                await self.channel_layer.group_send(
                        self.group_name, 
                        {   
                            "type": "dashboard_update", 
                            "unprocessed": unprocessed,
                            "agent_num": agent_num,
                            "healthy": healthy.count(),
                            "rules_set": rules_set,
                            "alert_num": alert_num if len(alert_num) <= max_agent else alert_num[:max_agent],
                            "timestamp": endtime.strftime("%m-%d %H:%M:%S")
                        }
                    )
                await asyncio.sleep(update_interval)
        except asyncio.CancelledError:
            print("Statistics sending task cancelled.")
        except Exception:
            print(traceback.format_exc())

    async def connect(self):
        self.agent_name = self.scope["url_route"]["kwargs"]["agent_name"]
        if self.scope["path"] == "/ws/ips/notification/":
            self.group_name = notification_group_name
        elif self.scope["path"] == "/ws/ips/statistics/":
            self.group_name = statistics_group_name
            self.stat_task = asyncio.create_task(self.fetch_stats(update_interval=10,eval_range=10))
        else:
            self.group_name = "ipsgroup_%s" % self.agent_name
        self.status_updater = self.scope["url_route"]["kwargs"]['status_updater']
        self.validate = URLValidator()

        # Join group
        await self.channel_layer.group_add(self.group_name, self.channel_name)

        await self.accept()

    async def disconnect(self, close_code):
        # Leave group
        await self.channel_layer.group_discard(self.group_name, self.channel_name)
        if self.scope["path"] == "/ws/ips/statistics/":
            self.stat_task.cancel()

    # Receive message from WebSocket
    async def receive_json(self, text_data_json):
        try:
            # print(text_data_json)
            # Register agent
            if text_data_json["type"] == "agent_register":
                agent_name = text_data_json["agent_name"]
                agent_ip = text_data_json["agent_ip"]
                agent_health = text_data_json["agent_health"]
                agent_status = "Registered" # Agent is registered but not yet received any metrics/health checks

                # Validate health check URL
                if agent_health is not None and agent_health != "None" and agent_health != "":
                    try:
                        self.validate(agent_health)
                    except ValidationError:
                        await self.channel_layer.group_send(
                            self.group_name, {"type": "agent_register", "message": f"ERROR: Incorrect URL format for health check. Health check URL must be in a full URL form (example: https://www.example.com)."}
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
                        self.group_name, {"type": "agent_register", "message": f"Agent {agent_name} has been registered successfully!"}
                    )
                else:
                    # Respond message
                    if agent_ip == obj.ip and agent_name != obj.name:
                        await self.channel_layer.group_send(
                            self.group_name, {"type": "agent_register", "message": f"Agent {agent_name} was already registered with name '{obj.name}'. Messages will be sent to agent '{obj.name}'"}
                        )
                    if agent_name == obj.name and agent_ip != obj.ip:
                        await self.channel_layer.group_send(
                            self.group_name, {"type": "agent_register", "message": f"ERROR: Agent name '{agent_name}' was already registered. Please choose a different name for your agent."}
                        )
                    if agent_name == obj.name and agent_ip == obj.ip:
                        # Update health check URL
                        obj.health = agent_health
                        await obj.asave()

                        await self.channel_layer.group_send(
                            self.group_name, {"type": "agent_register", "message": f"Successfully connected to WebSocket server."}
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
                    self.group_name, 
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

            # Access Log stream
            if text_data_json["type"] == "access_log":
                req = {
                    Request.ip.value : text_data_json["remote_addr"],
                    Request.timestamp.value : text_data_json["timestamp"],
                    Request.user.value : text_data_json["remote_user"],
                    Request.url.value : text_data_json["request"],
                    Request.status.value : int(text_data_json["status"]),
                    Request.bbs.value : int(text_data_json["body_bytes_sent"]),
                    Request.req_time.value : float(text_data_json["request_time"]),
                    Request.body.value : text_data_json["request_body"],
                    Request.headers.value : text_data_json["req_header"],
                    Request.destination.value : self.agent_name
                }

                # WAF
                alerts = await waf.detect_attack(req)

                # Send message to noti group
                if len(alerts) > 0:
                    await self.channel_layer.group_send(
                        notification_group_name, 
                        {   
                            "type": "alert_attack", 
                            'alerts': alerts
                        }
                    )
                    await waf.send_alert_mail(alerts)
                # Update last active time
                self.status_updater.update_last_activity_time(self.agent_name)

            # Portscan log stream
            if text_data_json["type"] == "portscan_alert":
                message = text_data_json["message"]
                scanlogd_reg = "^(?P<timestamp>[a-zA-Z]+ \d+ \d{2}:\d{2}:\d{2}) (?P<hostname>.+) scanlogd: (?P<scrip>\d+\.\d+\.\d+\.\d+).* to (?P<dstip>\d+\.\d+\.\d+\.\d+) (?P<message>.*)$"
                obj = re.search(scanlogd_reg, message)
                req = {
                    Request.ip.value : obj.group(3),
                    Request.timestamp.value : obj.group(1),
                    Request.user.value : "",
                    Request.url.value : "",
                    Request.status.value : 0,
                    Request.bbs.value : 0,
                    Request.req_time.value : 0,
                    Request.body.value : "",
                    Request.headers.value : "",
                    Request.destination.value : self.agent_name
                }

                # Port scan detect
                alerts = await waf.detect_port_scan(req)

                # Send message to noti group
                if len(alerts) > 0:
                    await self.channel_layer.group_send(
                        notification_group_name, 
                        {   
                            "type": "alert_attack", 
                            'alerts': alerts
                        }
                    )
                    await waf.send_alert_mail(alerts)
                # Update last active time
                self.status_updater.update_last_activity_time(self.agent_name)

        except Exception:
            print(traceback.format_exc())

    # Handlers called with each connected client in the channel 
    # Received message from group
    async def agent_register(self, event):
        # Register agent
        message = event["message"]
        # Send message to WebSocket
        await self.send_json({"type":"agent_register","message": message})

    async def alert_attack(self, event):
        # New access log on nginx agent
        alerts = event['alerts']

        # Send alert to WebSocket
        await self.send_json({"type":"alert_attack", "alerts": alerts})

    async def iptables_rule(self, event):
        srcip = event["srcip"]
        protocol = event["protocol"] 
        chain = event["chain"]
        action = event["action"] 
        target = event["target"] 
        dport = event["dport"]

        await self.send_json({
            "type": "iptables_rule", 
            "srcip": srcip, 
            "protocol": protocol, 
            "chain": chain, 
            "action": action, 
            "target": target, 
            "dport": dport
        })

    async def dashboard_update(self, event):
        # Dashboard updates
        unprocessed = event["unprocessed"]
        agent_num = event["agent_num"]
        healthy = event["healthy"]
        rules_set = event["rules_set"]
        alert_num = event["alert_num"]
        timestamp = event["timestamp"]

        # Send message to WebSocket
        await self.send_json({   
            "type": "dashboard_update",
            "unprocessed": unprocessed,
            "agent_num": agent_num,
            "healthy": healthy,
            "rules_set": rules_set,
            "alert_num": alert_num,
            "timestamp": timestamp
        })

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

