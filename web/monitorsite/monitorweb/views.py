from django.shortcuts import get_object_or_404, render, redirect
from django.contrib.auth.decorators import login_required
from django.contrib.auth import authenticate, logout as logout_func, login as login_func
from django.core.paginator import Paginator
from .models import Agent, Alert
from django.db.models import Q

def login(request):
    # Check if user is authenticated
    if request.user.is_authenticated:
        return redirect("monitorweb:index")
    # GET request to login page (no credentials)
    context = { "messages": None }
    if "username" not in request.POST or "password" not in request.POST:
        return render(request, "monitorweb/login.html", context)

    # POST request to login page (no credentials)
    username = request.POST["username"]
    password = request.POST["password"]
    user = authenticate(request, username=username, password=password)
    if user is not None:
        # Correct credentials
        login_func(request, user)
        if "next" in request.GET:
            return redirect(request.GET["next"])
        else:
            return redirect("monitorweb:index")
    else:
        # Incorrect credentials
        context["message"] = "Invalid credentials!"
        return render(request, "monitorweb/login.html", context)

def logout(request):
    logout_func(request)
    return redirect("monitorweb:login")

@login_required
def index(request):
    max_alert_noti = 15
    new_alerts = Alert.objects.filter(is_processed=False).order_by("-timestamp")
    new_alerts_count = new_alerts.count()
    if new_alerts_count > max_alert_noti:
        new_alerts = new_alerts[:max_alert_noti]
    context = {"page_header": "Dashboard", "new_alerts": new_alerts}
    return render(request, "monitorweb/index.html", context)    

@login_required
def agent_detail(request, agent_id):
    max_alert_noti = 15
    new_alerts = Alert.objects.filter(is_processed=False).order_by("-timestamp")
    new_alerts_count = new_alerts.count()
    if new_alerts_count > max_alert_noti:
        new_alerts = new_alerts[:max_alert_noti]
    agent = get_object_or_404(Agent, pk=agent_id)
    context = {"agent": agent, "page_header": f"Agent {agent.name}", "new_alerts": new_alerts}
    return render(request, "monitorweb/agent_detail.html", context)  

@login_required
def agents(request):
    max_alert_noti = 15
    max_agent_inpage = 40
    new_alerts = Alert.objects.filter(is_processed=False).order_by("-timestamp")
    new_alerts_count = new_alerts.count()
    if new_alerts_count > max_alert_noti:
        new_alerts = new_alerts[:max_alert_noti]
    search = request.GET.get("search")
    if search is None:
        agent_list = Agent.objects.order_by("-registered_at")
    else: 
        agent_list = Agent.objects.filter(Q(name__icontains=search) | Q(ip__contains=search) | Q(health__icontains=search)).order_by("-registered_at")

    paginator = Paginator(agent_list, max_agent_inpage)
    page_number = request.GET.get("page", 1)
    page_obj = paginator.get_page(page_number)
    return render(request, "monitorweb/agents.html", {"page_obj": page_obj, "search": search, "page_header": "Agents", "new_alerts": new_alerts})    

@login_required
def alerts(request):
    max_alert_noti = 15
    max_alert_inpage = 100
    new_alerts = Alert.objects.filter(is_processed=False).order_by("-timestamp")
    new_alerts_count = new_alerts.count()
    if new_alerts_count > max_alert_noti:
        new_alerts = new_alerts[:max_alert_noti]
    alert_list = Alert.objects.order_by("-timestamp")
    search = request.GET.get("search")
    if search is None:
        alert_list = Alert.objects.order_by("-timestamp")
    else: 
        alert_list = Alert.objects.filter(Q(message__icontains=search) | Q(rule__name__icontains=search) | Q(rule__class__icontains=search) | Q(agent__ip__contains=search) | Q(remote_addr__icontains=search) | Q(request__icontains=search) | Q(agent__name__icontains=search)).order_by("-timestamp")

    paginator = Paginator(alert_list, max_alert_inpage)

    page_number = request.GET.get("page", 1)
    page_obj = paginator.get_page(page_number)
    return render(request, "monitorweb/alerts.html", {"page_obj": page_obj, "search": search, "page_header": "Alerts", "new_alerts": new_alerts})    

# APIs
@login_required
def mark_alerts(request):
    try:
        Alert.objects.filter(is_processed=False).update(is_processed=True, processed_by=request.user)
    except Exception as e:
        print(e)
    return redirect("monitorweb:alerts")