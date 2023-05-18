from django.urls import path

from . import views

app_name = "monitorweb"
urlpatterns = [
    path("", views.index, name="index"),
    path("login", views.login, name="login"),
    path("agents", views.agents, name="agents"),
    path("agent/<int:agent_id>/", views.agent_detail, name="agent_detail"),
    path("alerts", views.alerts, name="alerts"),
    path("logout", views.logout, name="logout")
]