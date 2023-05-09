from django.urls import path

from . import views

app_name = "monitorweb"
urlpatterns = [
    path("", views.index, name="index"),
    path("agents", views.agents, name="agents"),
    path("alerts", views.alerts, name="alerts"),
]