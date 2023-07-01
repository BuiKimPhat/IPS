from django.shortcuts import get_object_or_404, render, redirect
from django.contrib.auth.decorators import login_required
from django.contrib.auth import authenticate, logout as logout_func, login as login_func
from django.core.paginator import Paginator
from .models import Agent, Alert, IptablesRule, Rule, RuleComponent
from django.db.models import Q
import json

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
    # Notification
    max_alert_noti = 15
    new_alerts = Alert.objects.filter(is_processed=False).order_by("-timestamp")
    new_alerts_count = new_alerts.count()
    if new_alerts_count > max_alert_noti:
        new_alerts = new_alerts[:max_alert_noti]

    context = {"page_header": "Dashboard", "new_alerts": new_alerts}
    return render(request, "monitorweb/index.html", context)    

# Agents

@login_required
def agent_detail(request, agent_id):
    # Notification
    max_alert_noti = 15
    new_alerts = Alert.objects.filter(is_processed=False).order_by("-timestamp")
    new_alerts_count = new_alerts.count()
    if new_alerts_count > max_alert_noti:
        new_alerts = new_alerts[:max_alert_noti]
    # Get agent
    agent = get_object_or_404(Agent, pk=agent_id)

    context = {"agent": agent, "page_header": f"Agent {agent.name}", "new_alerts": new_alerts}
    return render(request, "monitorweb/agent_detail.html", context)  

@login_required
def agents(request):
    # Notification
    max_alert_noti = 15
    max_agent_inpage = 40
    new_alerts = Alert.objects.filter(is_processed=False).order_by("-timestamp")
    new_alerts_count = new_alerts.count()
    if new_alerts_count > max_alert_noti:
        new_alerts = new_alerts[:max_alert_noti]
    # List and search agents
    search = request.GET.get("search")
    if search is None:
        agent_list = Agent.objects.order_by("-registered_at")
    else: 
        agent_list = Agent.objects.filter(Q(name__icontains=search) | Q(ip__contains=search) | Q(health__icontains=search)).order_by("-registered_at")

    paginator = Paginator(agent_list, max_agent_inpage)
    page_number = request.GET.get("page", 1)
    page_obj = paginator.get_page(page_number)
    return render(request, "monitorweb/agents.html", {"page_obj": page_obj, "search": search, "page_header": "Agents", "new_alerts": new_alerts})    

# Alerts

@login_required
def alerts(request):
    # Notification
    max_alert_noti = 15
    max_alert_inpage = 100
    new_alerts = Alert.objects.filter(is_processed=False).order_by("-timestamp")
    new_alerts_count = new_alerts.count()
    if new_alerts_count > max_alert_noti:
        new_alerts = new_alerts[:max_alert_noti]
    # List and search alerts
    search = request.GET.get("search")
    if search is None:
        alert_list = Alert.objects.order_by("-timestamp")
    else: 
        alert_list = Alert.objects.filter(Q(message__icontains=search) | Q(rule__name__icontains=search) | Q(rule__rule_class__icontains=search) | Q(agent__ip__contains=search) | Q(remote_addr__icontains=search) | Q(request__icontains=search) | Q(agent__name__icontains=search)).order_by("-timestamp")

    paginator = Paginator(alert_list, max_alert_inpage)
    page_number = request.GET.get("page", 1)
    page_obj = paginator.get_page(page_number)
    return render(request, "monitorweb/alerts.html", {"page_obj": page_obj, "search": search, "page_header": "Alerts", "new_alerts": new_alerts})    

@login_required
def alert_detail(request, alert_id):
    # Notification
    max_alert_noti = 15
    new_alerts = Alert.objects.filter(is_processed=False).order_by("-timestamp")
    new_alerts_count = new_alerts.count()
    if new_alerts_count > max_alert_noti:
        new_alerts = new_alerts[:max_alert_noti]
    # Get alert
    alert = get_object_or_404(Alert, pk=alert_id)

    context = {"alert": alert, "page_header": f"Alert #{alert.id}", "new_alerts": new_alerts}
    return render(request, "monitorweb/alert_detail.html", context)  

# Rules

@login_required
def rules(request):
    # Notification
    max_alert_noti = 15
    max_rule_inpage = 100
    new_alerts = Alert.objects.filter(is_processed=False).order_by("-timestamp")
    new_alerts_count = new_alerts.count()
    if new_alerts_count > max_alert_noti:
        new_alerts = new_alerts[:max_alert_noti]
    # List and search rules
    search = request.GET.get("search")
    if search is None:
        rule_list = Rule.objects.all()
    else: 
        rule_list = Rule.objects.filter(Q(name__icontains=search) | Q(rule_class__icontains=search) | Q(description__icontains=search) | Q(ref__icontains=search) | Q(operator=search))

    paginator = Paginator(rule_list, max_rule_inpage)
    page_number = request.GET.get("page", 1)
    page_obj = paginator.get_page(page_number)
    return render(request, "monitorweb/rules.html", {"page_obj": page_obj, "search": search, "page_header": "Rules", "new_alerts": new_alerts})    

@login_required
def rule_detail(request, rule_id):
    # Notification
    max_alert_noti = 15
    new_alerts = Alert.objects.filter(is_processed=False).order_by("-timestamp")
    new_alerts_count = new_alerts.count()
    if new_alerts_count > max_alert_noti:
        new_alerts = new_alerts[:max_alert_noti]
    # Get rule
    rule = get_object_or_404(Rule, pk=rule_id)
    rule_components = RuleComponent.objects.filter(rule=rule)

    context = {"rule": rule, "rule_components": rule_components, "page_header": f"WAF Rule {rule.name}", "new_alerts": new_alerts}
    return render(request, "monitorweb/rule_detail.html", context)  

# Iptables rules

@login_required
def iptables_rules(request):
    # Notification
    max_alert_noti = 15
    max_rule_inpage = 100
    new_alerts = Alert.objects.filter(is_processed=False).order_by("-timestamp")
    new_alerts_count = new_alerts.count()
    if new_alerts_count > max_alert_noti:
        new_alerts = new_alerts[:max_alert_noti]
    # List and search iptables rules
    search = request.GET.get("search")
    if search is None:
        rule_list = IptablesRule.objects.all()
    else: 
        rule_list = IptablesRule.objects.filter(Q(name__icontains=search) | Q(agent__name__icontains=search) | Q(srcip__contains=search) | Q(protocol__icontains=search) | Q(chain__icontains=search) | Q(target__icontains=search))

    paginator = Paginator(rule_list, max_rule_inpage)
    page_number = request.GET.get("page", 1)
    page_obj = paginator.get_page(page_number)
    return render(request, "monitorweb/iptables_rules.html", {"page_obj": page_obj, "search": search, "page_header": "Iptables Rules", "new_alerts": new_alerts})    


# APIs
@login_required
def mark_alerts(request):
    # Mark all alerts as processed
    try:
        Alert.objects.filter(is_processed=False).update(is_processed=True, processed_by=request.user)
    except Exception as e:
        print(e)
    return redirect("monitorweb:alerts")

@login_required
def mark_alert(request, alert_id):
    # Mark an alert as processed
    try:
        alert = Alert.objects.get(pk=alert_id)
        alert.is_processed = True
        alert.processed_by = request.user
        alert.save()
    except Exception as e:
        print(e)
    return redirect("monitorweb:alerts")

@login_required
def delete_all_alerts(request):
    # Mark an alert as processed
    try:
        Alert.objects.all().delete()
    except Exception as e:
        print(e)
    return redirect("monitorweb:alerts")

@login_required
def delete_iptables_rule(request, rule_id):
    # Mark an alert as processed
    try:
        rule = IptablesRule.objects.get(pk=rule_id)
        rule.delete()
    except Exception as e:
        print(e)
    return redirect("monitorweb:iptables_rules")

@login_required
def import_rules(request):
    # Mark an alert as processed
    try:
        file = request.FILES["file"]
        for chunk in file.chunks():
            data = json.loads(chunk)
            for rule in data:
                r, created = Rule.objects.get_or_create(
                    name=rule["name"],
                    rule_class=rule['rule_class'],
                    is_denied=rule['is_denied'],
                    ref=rule['ref'],
                    description=rule['description'],
                    operator=rule['operator']
                )
                if created:
                    for component in rule['components']:
                        RuleComponent.objects.create(
                            rule=r,
                            filter_field=component['filter_field'],
                            regex=component['regex']
                        )
    except Exception as e:
        print("Import error: ", e)
    return redirect("monitorweb:rules")