from monitorweb.models import Rule, RuleComponent, Alert, Agent
import re
from monitorweb.utils.enums import Request, Operator
from asgiref.sync import sync_to_async
class WAF:
    def __init__(self):
        self.isActive = True
        print("WAF has been initiated.")

    def deactivate(self):
        self.isActive = False
    def activate(self):
        self.isActive = True

    def create_alert(self, request, rule, message):
        if not self.isActive:
            return None
        try:
            agent = Agent.objects.get(name=request[Request.destination.value])
            new_alert = Alert(agent=agent, rule=rule, message=message, remote_user=request[Request.user.value], status=request[Request.status.value], body_bytes_sent=request[Request.bbs.value], request_time=request[Request.req_time.value], request=request[Request.url.value], body=request[Request.body.value], headers=request[Request.headers.value], remote_addr=request[Request.ip.value])
            new_alert.save()
            print(f"Alert: {new_alert.id}")
            return new_alert
        except Exception as e:
            print(f"Error: {e}")

    @sync_to_async
    def detect_attack(self, request):
        if not self.isActive:
            return []

        print("Processing request...")
        # Check every rules
        alerts = []
        for rule in Rule.objects.all():
            operator = rule.operator
            rule_components = RuleComponent.objects.filter(rule=rule)
            message = "Match payloads: "
            # Check every rule components
            # Operator "AND"
            if operator == Operator.AND.value:
                result = True
                for component in rule_components:
                    match = re.search(component.regex, str(request[component.filter_field]), re.IGNORECASE)
                    result = result and (match is not None) 
                    if match:
                        message += match.group() + ";"
                    # TODO: Take deny actions with iptables

            # Operator "OR"
            if operator == Operator.OR.value:
                result = False
                for component in rule_components:
                    match = re.search(component.regex, str(request[component.filter_field]), re.IGNORECASE)
                    result = result or (match is not None) 
                    if match:
                        message += match.group() + ";"
            
            if result:
                new_alert = self.create_alert(request, rule, message)
                alerts.append({
                    "id": new_alert.id, 
                    "agent_name": new_alert.agent.name,
                    "rule_name": rule.name, 
                    "rule_class": rule.rule_class, 
                    "message": new_alert.message,
                    "is_denied": rule.is_denied
                })
        return alerts