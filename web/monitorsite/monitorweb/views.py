from django.shortcuts import render, redirect
from django.contrib.auth.decorators import login_required
from django.contrib.auth import authenticate, logout as logout_func, login as login_func
from django.core.paginator import Paginator
from .models import Agent, Alert

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
def agents(request):
    agent_list = Agent.objects.order_by("-registered_at")
    paginator = Paginator(agent_list, 25)

    page_number = request.GET.get("page", 1)
    page_obj = paginator.get_page(page_number)
    return render(request, "monitorweb/agents.html", {"page_obj": page_obj})    

@login_required
def alerts(request):
    agent_list = Alert.objects.order_by("-created_at")
    paginator = Paginator(agent_list, 100)

    page_number = request.GET.get("page", 1)
    page_obj = paginator.get_page(page_number)
    return render(request, "monitorweb/alerts.html", {"page_obj": page_obj})    