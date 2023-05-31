import argparse
import asyncio
import websockets
import json
import psutil
import time
import subprocess
import re
import socket
import select

class IPSAgent:
    def __init__(self, server, name, interface, health):
        self.server = server
        self.name = name
        self.uri = self.server+self.name+"/"
        self.ip = ""
        self.health = health
        self.register_error = False

        for addr in psutil.net_if_addrs()[interface]:
            if addr.family == socket.AF_INET:
                self.ip = addr.address
                break
    async def connect_to_server(self):
        # Check agent_name
        if not re.search("^\w+$", self.name):
            self.error = True
            raise Exception("Invalid agent name! (a-Z, 0-9, _)")
        
        self.register_error = False
        async with websockets.connect(self.uri) as websocket:
            # Register the agent by sending a JSON-encoded message with its ID
            message = {
                'type': 'agent_register',
                'agent_name': self.name,
                'agent_ip': self.ip,
                'agent_health': self.health
            }
            await websocket.send(json.dumps(message))
            message_raw = await websocket.recv()
            message = json.loads(message_raw)["message"]
            if message[:5] == "ERROR":
                self.register_error = True
                raise Exception(message[6:])

            self.register_error = False
            print(message)

    async def send_metrics(self, metrics_interval):
        connected = False
        while not connected:
            try:
                async with websockets.connect(self.uri) as websocket:
                    print("Connection established. Sending messages...")
                    connected = True

                    # Metrics variables
                    last_sent_timestamp = 0
                    metrics_count = 0
                    cpu_percent = 0
                    mem_percent = 0
                    disk_read = 0
                    disk_write = 0
                    net_out = 0
                    net_in = 0
                    # Logs variables
                    last_line_num = 0

                    while True:
                        # Get CPU utilization and memory usage data
                        cpu_percent = cpu_percent + psutil.cpu_percent()
                        mem_percent = mem_percent + psutil.virtual_memory().percent
                        disk_read = disk_read + psutil.disk_io_counters().read_count
                        disk_write = disk_write + psutil.disk_io_counters().write_count
                        net_out = net_out + psutil.net_io_counters().bytes_sent
                        net_in = net_in + psutil.net_io_counters().bytes_recv
                        timestamp = int(time.time())
                        metrics_count = metrics_count + 1

                        # Send metrics updates to the server every metrics_interval
                        elapsed_seconds = timestamp - last_sent_timestamp
                        if elapsed_seconds >= metrics_interval:
                            # Construct a JSON message with the data
                            message = {
                                'type': 'metrics_update',  # Use a custom message type for metrics updates
                                'cpu_percent': cpu_percent / metrics_count,
                                'mem_percent': mem_percent / metrics_count,
                                'disk_read': disk_read / metrics_count,
                                'disk_write': disk_write / metrics_count,
                                'net_out': net_out / metrics_count,
                                'net_in': net_in / metrics_count,
                                'timestamp': timestamp,
                            }

                            # Send the message to the server
                            # print("Sending: ", message)
                            await websocket.send(json.dumps(message))

                            last_sent_timestamp = timestamp
                            metrics_count = 0
                            cpu_percent = 0
                            mem_percent = 0
                            disk_read = 0
                            disk_write = 0
                            net_out = 0
                            net_in = 0

                        # Check the ModSecurity audit log for new security events
                        with open('/var/log/modsec_audit.log') as f:
                            f.seek(last_line_num)
                            for line in f:
                                if 'ModSecurity: Access denied' in line:
                                    # Construct a JSON message for the attack
                                    attack_message = {
                                        'type': 'attack_alert',
                                        'message': line,
                                        'timestamp': timestamp
                                    }

                                    # Send the message to the server using the sendMessage function
                                    await websocket.send(json.dumps(attack_message))

                            last_line_num = f.tell()

                        # Sleep for 1 second before sending the next message
                        await asyncio.sleep(1)

            except Exception as e:
                connected = False
                print("Error: ", e)
                await asyncio.sleep(5) # 5 seconds delay before attempting to reconnect
                print("Reconnecting...")

    async def send_logs(self):
        connected = False
        while not connected:
            try:
                import time

                f = subprocess.Popen(['tail','-F',filename],\
                        stdout=subprocess.PIPE,stderr=subprocess.PIPE)
                p = select.poll()
                p.register(f.stdout)

                while True:
                    if p.poll(1):
                        print f.stdout.readline()
                    time.sleep(1)
                async with websockets.connect(self.uri) as websocket:
                    print("Connection established. Sending logs...")
                    connected = True

                    while True:
                        # Get CPU utilization and memory usage data
                        cpu_percent = cpu_percent + psutil.cpu_percent()
                        mem_percent = mem_percent + psutil.virtual_memory().percent
                        disk_read = disk_read + psutil.disk_io_counters().read_count
                        disk_write = disk_write + psutil.disk_io_counters().write_count
                        net_out = net_out + psutil.net_io_counters().bytes_sent
                        net_in = net_in + psutil.net_io_counters().bytes_recv

                        timestamp = int(time.time())
                        metrics_count = metrics_count + 1

                        # Send metrics updates to the server every metrics_interval
                        elapsed_seconds = timestamp - last_sent_timestamp
                        if elapsed_seconds >= metrics_interval:
                            # Construct a JSON message with the data
                            message = {
                                'type': 'metrics_update',  # Use a custom message type for metrics updates
                                'cpu_percent': cpu_percent / metrics_count,
                                'mem_percent': mem_percent / metrics_count,
                                'disk_read': disk_read / metrics_count,
                                'disk_write': disk_write / metrics_count,
                                'net_out': net_out / metrics_count,
                                'net_in': net_in / metrics_count,
                                'timestamp': timestamp,
                            }

                            # Send the message to the server
                            # print("Sending: ", message)
                            await websocket.send(json.dumps(message))

            except Exception as e:
                connected = False
                print("Error: ", e)
                await asyncio.sleep(5) # 5 seconds delay before attempting to reconnect
                print("Reconnecting...")

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Python agent for sending computer metrics and ModSecurity audit logs to a WebSocket server')
    parser.add_argument('-s','--server', required=False, type=str, help='WebSocket server URI', default='ws://10.0.101.69/ws/ips/')
    parser.add_argument('-c','--check-uri', required=False, type=str, help='Health check full URI (use this if you want to enable health check for the web server)', default='None')
    parser.add_argument('-n','--name', required=True, type=str, help='Agent name to register with the WebSocket server')
    parser.add_argument('-i','--interface', required=False, type=str, help='NIC chosen to register its IP to WebSocket server', default='eth0')
    parser.add_argument('-m', '--metrics-interval', required=False, type=int, help='Interval between metrics updates (second)', default=20)
    args = parser.parse_args()

    agent = IPSAgent(args.server, args.name, args.interface, args.check_uri)

    try:
        # Register the agent with the server
        asyncio.run(agent.connect_to_server())
        # Send metrics to the server
        asyncio.gather(agent.send_metrics(args.metrics_interval), agent.send_logs())
    except KeyboardInterrupt:
        print("Stopping agent...")