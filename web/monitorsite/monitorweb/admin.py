from django.contrib import admin

from .models import Agent, Alert, Rule, RuleComponent, IptablesRule

admin.site.site_header = 'IPS Administration'

class AgentAdmin(admin.ModelAdmin):
    list_display = ["name", "ip","health", "status","registered_at"]
    list_filter = ["status"]
    search_fields = ["name", "ip"]

class AlertAdmin(admin.ModelAdmin):
    list_display = ["agent", "rule","remote_addr", "request", "status","timestamp"]
    list_filter = ["is_processed", "status"]
    search_fields = ["agent__name", "rule__name", "request"]

class RuleAdmin(admin.ModelAdmin):
    list_display = ["name", "rule_class","is_denied", "ref","operator"]
    list_filter = ["rule_class", "is_denied", "operator"]
    search_fields = ["name", "rule_class", "ref", "description"]

class RCAdmin(admin.ModelAdmin):
    list_display = ["rule", "filter_field"]
    list_filter = ["rule__operator", "filter_field"]
    search_fields = ["rule__name", "rule__rule_class", "regex"]

class IptRAdmin(admin.ModelAdmin):
    list_display = ["name", "agent","srcip", "protocol","chain", "target", "dport", "options"]
    list_filter = ["protocol", "chain", "target", "dport"]
    search_fields = ["name", "srcip", "options", "agent__name"]

admin.site.register(Agent, AgentAdmin)
admin.site.register(Alert, AlertAdmin)
admin.site.register(Rule, RuleAdmin)
admin.site.register(RuleComponent, RCAdmin)
admin.site.register(IptablesRule, IptRAdmin)