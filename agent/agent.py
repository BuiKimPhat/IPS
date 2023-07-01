import argparse
import asyncio
import websockets
import json
import psutil
import time
import subprocess
import socket
import re
import select

class IPSAgent:
    def __init__(self, server, name, interface, health, log_path, portscan_log):
        self.server = server
        self.name = name
        self.uri = f"ws://{self.server}/ws/ips/{self.name}/"
        self.ip = ""
        self.health = health
        self.interface = interface
        self.register_error = False
        self.log_path = log_path
        self.portscan_log = portscan_log

        for addr in psutil.net_if_addrs()[self.interface]:
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

    async def recv_mess(self):
        if self.register_error:
            raise Exception("Unidentified error during registration. Exiting...")
        connected = False

        while not connected:
            try:
                async with websockets.connect(self.uri) as websocket:
                    while True:
                        message_raw = await websocket.recv()
                        message = json.loads(message_raw)
                        if message["type"] == "iptables_rule":
                            if message['srcip'] is not None and message['srcip'] != "":
                                subprocess.run(["iptables", f"-{message['action']}", message['chain'], "-p", message['protocol'], "-s", message['srcip'], "-i", self.interface, "--dport", str(message['dport']), "-j", message['target']] + message["options"].split(" "))
                            else:
                                subprocess.run(["iptables", f"-{message['action']}", message['chain'], "-p", message['protocol'], "-i", self.interface, "--dport", str(message['dport']), "-j", message['target']] + message["options"].split(" "))

            except Exception as e:
                connected = False
                print("Error: ", e)
                await asyncio.sleep(3) # 3 seconds delay before attempting to reconnect
                print("Reconnecting...")


    async def send_message(self, metrics_interval):
        if self.register_error:
            raise Exception("Unidentified error during registration. Exiting...")
        connected = False
        # Access Logs variables
        last_line_num = 0
        # Portscan Logs variables
        portscan_last_line_num = 0

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
                            # print(message)
                            await websocket.send(json.dumps(message))

                            last_sent_timestamp = timestamp
                            metrics_count = 0
                            cpu_percent = 0
                            mem_percent = 0
                            disk_read = 0
                            disk_write = 0
                            net_out = 0
                            net_in = 0

                        # Check the log for new events
                        with open(self.log_path, 'r', errors='replace') as f:
                            if last_line_num == 0:
                                f.seek(last_line_num, 2)
                            else:
                                f.seek(last_line_num)
                            for line in f:
                                # Construct a JSON message for the attack
                                origin_log = json.loads(line)
                                log_message = {
                                    'type': 'access_log',
                                    'remote_addr': origin_log["remote_addr"],
                                    'remote_user':  origin_log['remote_user'],
                                    'timestamp': origin_log['timestamp'],
                                    'request': origin_log['request'],
                                    'status': origin_log['status'],
                                    'body_bytes_sent': origin_log['body_bytes_sent'],
                                    'request_time': origin_log['request_time'],
                                    'request_body': origin_log['request_body'],
                                    'req_header': origin_log['req_header'][:-1].split("~")
                                }

                                # print(log_message)
                                # Send the message to the server
                                await websocket.send(json.dumps(log_message))

                            last_line_num = f.tell()

                        # Check the log for new events
                        with open(self.portscan_log, 'r', errors='replace') as f:
                            if portscan_last_line_num == 0:
                                f.seek(portscan_last_line_num, 2)
                            else:
                                f.seek(portscan_last_line_num)
                            for line in f:
                                # Construct a JSON message for the attack
                                log_message = {
                                    'type': 'portscan_alert',
                                    'message': line,
                                }

                                # Send the message to the server
                                await websocket.send(json.dumps(log_message))

                            portscan_last_line_num = f.tell()

                        await asyncio.sleep(1)

            except Exception as e:
                connected = False
                print("Error: ", e)
                await asyncio.sleep(3) # 3 seconds delay before attempting to reconnect
                print("Reconnecting...")

async def main():
    parser = argparse.ArgumentParser(description='Python agent for sending computer metrics and ModSecurity audit logs to a WebSocket server')
    parser.add_argument('-s','--server', required=False, type=str, help='WebSocket server IP', default='10.0.101.69')
    parser.add_argument('-c','--check-uri', required=False, type=str, help='Health check full URI (use this if you want to enable health check for the web server)', default='None')
    parser.add_argument('-n','--name', required=True, type=str, help='Agent name to register with the WebSocket server')
    parser.add_argument('-i','--interface', required=False, type=str, help='NIC chosen to register its IP to WebSocket server', default='eth0')
    parser.add_argument('-m', '--metrics-interval', required=False, type=int, help='Interval between metrics updates (second)', default=20)
    parser.add_argument('-l','--log-path', required=False, type=str, help='Path to nginx access log file', default='/var/log/nginx/access.log')
    parser.add_argument('-sl','--scan-log-path', required=False, type=str, help='Path to scanlogd log file', default='/var/log/portscan.alert')

    args = parser.parse_args()

    agent = IPSAgent(args.server, args.name, args.interface, args.check_uri, args.log_path, args.scan_log_path)

    try:
        # Register the agent with the server
        await asyncio.create_task(agent.connect_to_server())

        # Send metrics to the server
        await asyncio.gather(agent.send_message(args.metrics_interval), agent.recv_mess())
    except KeyboardInterrupt:
        print("\nStopping agent...")
    except Exception as e:
        print(e)

if __name__ == '__main__':
    asyncio.run(main())