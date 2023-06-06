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
    context = {}
    return render(request, "monitorweb/index.html", context)    

@login_required
def agent_detail(request, agent_id):
    agent = get_object_or_404(Agent, pk=agent_id)
    context = {"agent": agent}
    return render(request, "monitorweb/agent_detail.html", context)  

@login_required
def agents(request):
    search = request.GET.get("search")
    if search is None:
        agent_list = Agent.objects.order_by("-registered_at")
    else: 
        agent_list = Agent.objects.filter(Q(name__icontains=search) | Q(ip__contains=search) | Q(health__icontains=search)).order_by("-registered_at")

    paginator = Paginator(agent_list, 25)
    page_number = request.GET.get("page", 1)
    page_obj = paginator.get_page(page_number)
    return render(request, "monitorweb/agents.html", {"page_obj": page_obj, "search": search})    

@login_required
def alerts(request):
    alert_list = Alert.objects.order_by("-timestamp")
    search = request.GET.get("search")
    if search is None:
        alert_list = Alert.objects.order_by("-timestamp")
    else: 
        alert_list = Alert.objects.filter(Q(message__icontains=search) | Q(rule__name__icontains=search) | Q(rule__class__icontains=search) | Q(agent__ip__contains=search) | Q(remote_addr__icontains=search) | Q(request__icontains=search) | Q(agent__name__icontains=search)).order_by("-timestamp")

    paginator = Paginator(alert_list, 100)

    page_number = request.GET.get("page", 1)
    page_obj = paginator.get_page(page_number)
    return render(request, "monitorweb/alerts.html", {"page_obj": page_obj, "search": search})    