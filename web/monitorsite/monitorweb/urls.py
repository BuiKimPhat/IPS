from django.urls import path

from . import views

app_name = "monitorweb"
urlpatterns = [
    path("", views.index, name="index"),
    path("login/", views.login, name="login"),
    path("agents/", views.agents, name="agents"),
    path("agent/<int:agent_id>/", views.agent_detail, name="agent_detail"),
    path("alerts/", views.alerts, name="alerts"),
    path("logout/", views.logout, name="logout"),
    path("rules/", views.rules, name="rules"),
    path("rule/<int:rule_id>/", views.rule_detail, name="rule_detail"),
    path("iptables_rules/", views.iptables_rules, name="iptables_rules"),
    # APIs
    path("api/mark_alerts/", views.mark_alerts, name="mark_alerts"),
    path("api/mark_alert/<int:alert_id>/", views.mark_alert, name="mark_alert"),
    path("api/delete_all_alerts/", views.delete_all_alerts, name="delete_all_alerts")
    path("api/delete_iptables_rule/<int:rule_id>/", views.delete_iptables_rule, name="delete_iptables_rule"),
    path("api/import_rules/", views.import_rules, name="import_rules")
]