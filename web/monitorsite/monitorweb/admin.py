from django.contrib import admin

from .models import Agent, Alert, Rule, RuleComponent, IptablesRule

admin.site.site_header = 'IPS Administration'

admin.site.register(Agent)
admin.site.register(Alert)
admin.site.register(Rule)
admin.site.register(RuleComponent)
admin.site.register(IptablesRule)