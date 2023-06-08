from monitorweb.models import Rule, RuleComponent, Alert, Agent
import re
from monitorweb.utils.enums import Request, Operator
from asgiref.sync import sync_to_async
class WAF:
    def __init__(self):
        print("WAF has been initiated.")

    def create_alert(self, request, rule, message):
        try:
            agent = Agent.objects.get(name=request[Request.destination.value])
            new_alert = Alert(agent=agent, rule=rule, message=message, remote_user=request[Request.user.value], status=request[Request.status.value], body_bytes_sent=request[Request.bbs.value], request_time=request[Request.req_time.value], request=request[Request.url.value], body=request[Request.body.value], headers=request[Request.headers.value], remote_addr=request[Request.ip.value])
            new_alert.save()
            print(f"Alert: {new_alert.id}")
            return new_alert.id
        except Exception as e:
            print(f"Error: {e}")

    @sync_to_async
    def detect_attack(self, request):
        print("Processing...")
        # Check every rules
        rules_triggered = []
        for rule in Rule.objects.all():
            operator = rule.operator
            rule_components = RuleComponent.objects.filter(rule=rule)
            message = "Possible payloads: "
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
                    # print(component.regex, request[component.filter_field])
            # Operator "OR"
            if operator == Operator.OR.value:
                result = False
                for component in rule_components:
                    match = re.search(component.regex, str(request[component.filter_field]), re.IGNORECASE)
                    result = result or (match is not None) 
                    if match:
                        message += match.group() + ";"
                    # print(component.regex, request[component.filter_field])
            
            if result:
                new_alert_id = self.create_alert(request, rule, message)
                rules_triggered.append({"name": rule.name, "rule_class": rule.rule_class, "deny": rule.deny, "id": new_alert_id})
        return rules_triggered