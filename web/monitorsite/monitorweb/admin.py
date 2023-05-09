from django.contrib import admin

from .models import Agent, Alert

admin.site.site_header = 'IPS Administration'

admin.site.register(Agent)
admin.site.register(Alert)
