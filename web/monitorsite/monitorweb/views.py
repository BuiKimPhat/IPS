from django.shortcuts import render
from django.contrib.auth.decorators import login_required
from django.contrib.auth import authenticate, login as login_func

def login(request):
    context = { "messages": None }
    if "username" not in request.POST or "password" not in request.POST:
        return render(request, "monitorweb/login.html", context)
    username = request.POST["username"]
    password = request.POST["password"]
    user = authenticate(request, username=username, password=password)

    if user is not None:
        login(request, user)
        next_view = request.GET["next"]
        if next_view is not None:
            redirect(next_view)
        else:
            redirect("index")
    else:
        context["message"] = "Invalid credentials!"
        return render(request, "monitorweb/login.html", context)

@login_required
def index(request):
    context = {}
    return render(request, "monitorweb/index.html", context)     

@login_required
def agents(request):
    context = {}
    return render(request, "monitorweb/agents.html", context)    

@login_required
def alerts(request):
    context = {}
    return render(request, "monitorweb/alerts.html", context)    