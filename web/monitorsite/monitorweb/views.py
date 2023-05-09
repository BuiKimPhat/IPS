from django.shortcuts import render

def index(request):
    context = {}
    return render(request, "monitorweb/index.html", context)    

def agents(request):
    context = {}
    return render(request, "monitorweb/agents.html", context)    

def alerts(request):
    context = {}
    return render(request, "monitorweb/alerts.html", context)    