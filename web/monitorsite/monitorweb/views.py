from django.shortcuts import render

def index(request):
    context = {}
    return render(request, "monitorweb/index.html", context)    