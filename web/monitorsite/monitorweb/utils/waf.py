from monitorweb.models import Rule, RuleComponent, Alert, Agent
import re
from monitorweb.utils.enums import Request, Operator
from asgiref.sync import sync_to_async
from django.db.models import Q
class WAF:
    def __init__(self):
        self.isActive = True
        print("WAF has been initiated.")

    def deactivate(self):
        self.isActive = False
    def activate(self):
        self.isActive = True

    async def create_alert(self, request, rule, message):
        if not self.isActive:
            return None
        try:
            agent = await Agent.objects.aget(name=request[Request.destination.value])
            # print(request, rule, message)
            if len(message) >= 200:
                message = message[:200]
            new_alert = Alert(agent=agent, rule=rule, message=message, remote_user=request[Request.user.value], status=request[Request.status.value], body_bytes_sent=request[Request.bbs.value], request_time=request[Request.req_time.value], request=request[Request.url.value], body=request[Request.body.value], headers=request[Request.headers.value], remote_addr=request[Request.ip.value])
            await new_alert.asave()
            print(f"Alert: {new_alert.id}")
            return new_alert
        except Exception as e:
            print(f"Create alert error: {e}")

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
            return [{
                "id": new_alert.id, 
                "agent_name": new_alert.agent.name,
                "rule_name": rule.name, 
                "rule_class": rule.rule_class, 
                "message": new_alert.message,
                "is_denied": rule.is_denied
            }]
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
                    match = re.search(component.regex, str(request[component.filter_field]), re.IGNORECASE)
                    result = result and (match is not None) 
                    if match:
                        message += match.group() + ";"

            # Operator "OR"
            if operator == Operator.OR.value:
                result = False
                async for component in rule_components:
                    match = re.search(component.regex, str(request[component.filter_field]), re.IGNORECASE)
                    result = result or (match is not None) 
                    if match:
                        message += match.group() + ";"

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
        return alerts