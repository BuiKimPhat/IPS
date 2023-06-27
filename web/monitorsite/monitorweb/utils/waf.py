from monitorweb.models import Rule, RuleComponent, Alert, Agent
import re
from monitorweb.utils.enums import Request, Operator
from asgiref.sync import sync_to_async
from django.db.models import Q
import smtplib
from email.mime.text import MIMEText
import yaml
import traceback
from django.contrib.auth.models import User
import time

class WAF:
    def __init__(self):
        self.isActive = True
        print("[INFO] WAF has been initiated.")

        # Get mail credentials
        self.config = {
            "mail_cred_path": '/home/ubuntu/IPS/web/monitorsite/monitorsite/.mail_credentials.yaml',
            "mail_interval": 180 # 3 minutes interval between mail sending
        }
        self.last_mail_sent = 0
        self.activate_mail()

    def deactivate(self):
        self.isActive = False
    def activate(self):
        self.isActive = True

    def set_mail_cred_path(self, path):
        self.config["mail_cred_path"] = path

    def activate_mail(self):
        try:
            cred = yaml.load(open(self.config["mail_cred_path"]), Loader=yaml.FullLoader)
            self.send_mail = True
            self.mail_config = {
                "email": cred['user']['email'],
                "pwd": cred['user']['password']
            }
        except FileNotFoundError:
            print("[WARNING] Mail credentials is not set, mailing alerts disabled. Please set mailing credentials in .mail_credentials.yaml file in monitorsite folder with the folowing form:")
            print("""
            user:
                email: ips_email@example.com
                password: ips_password
            """)
            self.send_mail = False
        except Exception:
            self.send_mail = False
            print(traceback.format_exc())

    def deactivate_mail(self): 
        self.send_mail = False

    async def create_alert(self, request, rule, message):
        if not self.isActive:
            return None
        try:
            agent = await Agent.objects.aget(name=request[Request.destination.value])
            # print(request, rule, message)
            if len(message) >= 200:
                message = message[:200]
            if len(request[Request.body.value]) >= 2000:
                request[Request.body.value] = request[Request.body.value][:2000]
            if len(request[Request.headers.value]) >= 1500:
                request[Request.headers.value] = request[Request.headers.value][:1500]
            new_alert = Alert(agent=agent, rule=rule, message=message, remote_user=request[Request.user.value], status=request[Request.status.value], body_bytes_sent=request[Request.bbs.value], request_time=request[Request.req_time.value], request=request[Request.url.value], body=request[Request.body.value], headers=request[Request.headers.value], remote_addr=request[Request.ip.value])
            await new_alert.asave()
            print(f"Alert: {new_alert.id}")
            return new_alert
        except Exception as e:
            print(f"Create alert error: {e}")

    async def send_alert_mail(self, alerts):
        if not self.send_mail:
            return None

        # Check time interval
        timestamp = time.time()
        if timestamp - self.last_mail_sent >= self.config["mail_interval"]:
            # Prepare recipients
            recipients = []
            async for user in User.objects.exclude(email__exact='').values("email"):
                recipients.append(user["email"])

            # Prepare subject and body
            subject = f"IPS rules triggered on agent {alerts[0]['agent_name']} - {', '.join([alert['rule_class'] for alert in alerts])}"
            body = f"""
            <html>
                <head>
                    <style>
                    table {{
                        font-family: Arial, Helvetica, sans-serif;
                        border-collapse: collapse;
                        width: 100%;
                    }}

                    table td, table th {{
                        border: 1px solid #ddd;
                        padding: 8px;
                    }}

                    table tr:nth-child(even){{background-color: #f2f2f2;}}

                    table tr:hover {{background-color: #ddd;}}

                    table th {{
                        padding-top: 12px;
                        padding-bottom: 12px;
                        text-align: left;
                        background-color: #04AA6D;
                        color: white;
                    }}
                    </style>
                </head>
                <body>
                    <h1><i><b>IPS</b> Intrusion alert</i></h1>
                    <h3>{subject}</h3>
                    <br/>
                    <table>
                        <thead>
                            <tr>
                                <th scope="col">Agent</th>
                                <th scope="col">Rule class</th>
                                <th scope="col">Rule triggered</th>
                                <th scope="col">Message</th>
                                <th scope="col">Action</th>
                            </tr>
                        </thead>
                        <tbody>
            """
            for alert in alerts:
                body += f"""
                    <tr style="cursor: pointer;">
                            <td>{alert['agent_name']}</td>
                            <td>{alert['rule_class']}</td>
                            <td>{alert['rule_name']}</td>
                            <td>{alert['message']}</td>
                            <td>{'Blocked' if alert['is_denied'] else 'Allowed'}</td>
                    </tr>

                """
            body += """
                        </tbody>
                    </table>
                </body>
            </html>
            """

            # Prepare, authenticate and send mail
            msg = MIMEText(body, 'html')
            msg['Subject'] = subject
            msg['From'] = self.mail_config["email"]
            msg['To'] = ', '.join(recipients)
            with smtplib.SMTP_SSL('smtp.gmail.com', 465) as smtp_server:
                try:
                    smtp_server.login(self.mail_config["email"], self.mail_config["pwd"])
                except smtplib.SMTPAuthenticationError:
                    self.activate_mail()
                smtp_server.sendmail(self.mail_config["email"], recipients, msg.as_string())

                # Set last_mail_sent timestamp
                self.last_mail_sent = time.time()
                print("Message sent!")

    async def detect_port_scan(self, log):
        if not self.isActive:
            return []
        print("Port scan detected")
        # Check if portscan rule exists
        try:
            # Get first rule to match
            rule = await Rule.objects.filter(Q(rule_class__icontains="portscan") | Q(rule_class__icontains="port scan") | Q(name__icontains="portscan") | Q(name__icontains="port scan")).afirst()
            portScanEnabled = True
        except Agent.DoesNotExist:
            portScanEnabled = False

        if portScanEnabled:
            message = "Port scan detected!"
            new_alert = await self.create_alert(log, rule, message)
            alerts = [{
                "id": new_alert.id, 
                "agent_name": new_alert.agent.name,
                "rule_name": rule.name, 
                "rule_class": rule.rule_class, 
                "message": new_alert.message,
                "is_denied": rule.is_denied
            }]

            # await self.send_alert_mail(alerts)
            return alerts
        else:
            return []

    async def detect_attack(self, request):
        if not self.isActive:
            return []

        print("Processing request...")
        # Check every rules
        alerts = []
        async for rule in Rule.objects.all():
            operator = rule.operator
            rule_components = RuleComponent.objects.filter(rule=rule)
            message = "Match payloads: "
            # Check every rule components
            # Operator "AND"
            if operator == Operator.AND.value:
                result = True
                async for component in rule_components:
                    try:
                        match = re.search(component.regex, str(request[component.filter_field]), re.IGNORECASE)
                        result = result and (match is not None) 
                        if match:
                            message += match.group() + ";"
                    except Exception as e:
                        print("Regex error: ", e, component.regex)

            # Operator "OR"
            if operator == Operator.OR.value:
                result = False
                async for component in rule_components:
                    try:
                        match = re.search(component.regex, str(request[component.filter_field]), re.IGNORECASE)
                        result = result or (match is not None) 
                        if match:
                            message += match.group() + ";"
                    except Exception as e:
                        print("Regex error: ", e, component.regex)

            if await rule_components.acount() == 0:
                result = False
            
            if result:
                new_alert = await self.create_alert(request, rule, message)
                alerts.append({
                    "id": new_alert.id, 
                    "agent_name": new_alert.agent.name,
                    "rule_name": rule.name, 
                    "rule_class": rule.rule_class, 
                    "message": new_alert.message,
                    "is_denied": rule.is_denied
                })

        # if len(alerts) > 0:
        #     await self.send_alert_mail(alerts)
        return alerts