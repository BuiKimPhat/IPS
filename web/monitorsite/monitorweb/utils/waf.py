from monitorweb.models import Rule, RuleComponent, Alert, Agent
import re
from monitorweb.utils.enums import Request, Operator
class WAF:
    rules = None
    def __init__(self):
        self.rules = Rule.objects.all()
    def get_rules(self):
        # Return the QuerySet of all rules
        return self.rules
    def import_rules(self, file_path):
        # TODO: import from JSON file
        with open(file_path) as f:
            pass
    async def alert(self, request, rule, message):
        try:
            agent = Agent.objects.aget(name=request[Request.destination])
            new_alert = Alert(agent=agent, rule=rule, message=message, remote_user=request[Request.user], status=request[Request.status], body_bytes_sent=request[Request.bss], request_time=request[Request.req_time], request=request.url, body=request.body, headers=request.headers, remote_addr=request.ip)
            await new_alert.asave()
        except Exception as e:
            print(f"Error: {e}")
    async def detect_attack(self, request):
        # Check every rules
        rules_triggered = []
        for rule in self.rules:
            operator = rule.operator
            rule_components = RuleComponent.objects.filter(rule=rule)
            message = "Possible payload: "
            # Check every rule components
            # Operator "AND"
            if operator == Operator.AND:
                result = True
                for component in rule_components:
                    match = re.search(component.regex, request[component.filter_field], re.IGNORECASE)
                    result = result and (match is not None) 
                    message += match.group() + ";"
                    # TODO: Take deny actions with iptables
            # Operator "OR"
            if operator == Operator.OR:
                result = False
                for component in rule_components:
                    match = re.search(component.regex, request[component.filter_field], re.IGNORECASE)
                    result = result or (match is not None) 
                    message += match.group() + ";"
            
            if result:
                rules_triggered.append(rule.name)
                is_attack = True

        return None if len(rules_triggered) == 0 else rules_triggered