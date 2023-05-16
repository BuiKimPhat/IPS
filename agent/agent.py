import argparse
import asyncio
import websockets
import json
import psutil
import time
import subprocess

async def connect_to_server(websocket_uri, agent_name):
    async with websockets.connect(websocket_uri+agent_name+"/") as websocket:
        # Register the agent by sending a JSON-encoded message with its ID
        message = {
            'agent_name': agent_name
        }
        await websocket.send(json.dumps(message))
        print(f"Registered agent '{agent_name}' with server at {websocket_uri}")

async def setup_modsecurity():
    # Set up ModSecurity by running a shell script or executing API calls
    cmd = ['setup_modsecurity.sh']
    process = subprocess.Popen(cmd, stdout=subprocess.PIPE)
    output, error = process.communicate()
    print(output.decode('utf-8'))

async def send_metrics(websocket_uri):
    async with websockets.connect(websocket_uri) as websocket:
        # Initialize empty chart data and other variables
        max_data_points = 50  # Keep up to 50 data points
        chart_data = {
            'labels': [],
            'datasets': [{
                'label': 'CPU utilization',
                'data': [],
                'borderColor': 'rgb(75, 192, 192)',
                'tension': 0.1,
                'fill': False
            }, {
                'label': 'Memory usage',
                'data': [],
                'borderColor': 'rgb(255, 99, 132)',
                'tension': 0.1,
                'fill': False
            }]
        }

        # Create chart object
        chart_message = {
            'type': 'chart_update',
            'data': chart_data
        }
        await websocket.send(json.dumps(chart_message))

        # Keep track of the last line number read from the audit log file
        last_line_num = 0

        while True:
            try:
                # Get CPU utilization and memory usage data
                cpu_percent = psutil.cpu_percent()
                mem_percent = psutil.virtual_memory().percent

                # Add the data to the chart data arrays
                timestamp = int(time.time() * 1000)
                chart_data['labels'].append(timestamp)
                chart_data['datasets'][0]['data'].append(cpu_percent)
                chart_data['datasets'][1]['data'].append(mem_percent)

                # Limit the length of the data arrays
                if len(chart_data['labels']) > max_data_points:
                    chart_data['labels'] = chart_data['labels'][-max_data_points:]
                    chart_data['datasets'][0]['data'] = chart_data['datasets'][0]['data'][-max_data_points:]
                    chart_data['datasets'][1]['data'] = chart_data['datasets'][1]['data'][-max_data_points:]

                # Send chart updates to the server every 5 minutes
                elapsed_minutes = (chart_data['labels'][-1] - chart_data['labels'][0]) // (1000 * 60)
                if elapsed_minutes >= 5:
                    chart_message = {
                        'type': 'chart_update',
                        'data': chart_data
                    }
                    await websocket.send(json.dumps(chart_message))
                    chart_data['labels'] = [chart_data['labels'][-1]]
                    chart_data['datasets'][0]['data'] = [chart_data['datasets'][0]['data'][-1]]
                    chart_data['datasets'][1]['data'] = [chart_data['datasets'][1]['data'][-1]]

                # Construct a JSON message with the data
                message = {
                    'type': 'metrics_update',  # Use a custom message type for metrics updates
                    'cpu_percent': cpu_percent,
                    'mem_percent': mem_percent,
                    'timestamp': timestamp,
                }

                # Send the message to the server
                await websocket.send(json.dumps(message))

                # Check the ModSecurity audit log for new security events
                with open('/var/log/modsec_audit.log') as f:
                    f.seek(last_line_num)
                    for line in f:
                        if 'Message: Access denied' in line:
                            # Construct a JSON message for the attack
                            attack_message = {
                                'type': 'attack_detected',
                                'timestamp': timestamp
                            }

                            # Send the message to the server using the sendMessage function
                            sendMessage(attack_message)

                    last_line_num = f.tell()

            except Exception as e:
                print("Error:", e)

            # Sleep for 1 second before sending the next message
            await asyncio.sleep(1)

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Python agent for sending computer metrics and ModSecurity audit logs to a WebSocket server')
    parser.add_argument('-s','--server', required=False, type=str, help='WebSocket server URI', default='ws://10.0.101.69/ws/ips/')
    parser.add_argument('-n','--name', required=True, type=str, help='Agent name to register with the WebSocket server')
    parser.add_argument('--setup', action="store_true", help='Setup ModSecurity')
    args = parser.parse_args()

    # Register the agent with the server
    asyncio.run(connect_to_server(args.server, args.name))

    # Set up ModSecurity (if specified)
    if args.setup:
        setup_modsecurity()
        exit()

    # Send data to the server
    asyncio.run(send_data(args.server))