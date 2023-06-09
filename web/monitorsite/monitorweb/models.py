from django.db import models
from monitorweb.utils.enums import Request, Operator
from django.contrib.auth.models import User

class Agent(models.Model):
    def __str__(self):
        return f"{self.name} - {self.ip}"
    name = models.CharField(unique=True, max_length=200)
    ip = models.GenericIPAddressField(unique=True)
    health = models.URLField(max_length=200)
    status = models.CharField(max_length=20)
    registered_at = models.DateTimeField(auto_now_add=True)

class Rule(models.Model):
    def __str__(self):
        return f"{self.rule_class} - {self.name}"
    name = models.CharField(max_length=200)
    rule_class = models.CharField(max_length=300)
    is_denied = models.BooleanField(default=True)
    ref = models.URLField(null=True,blank=True, max_length=500)
    description = models.CharField(null=True, blank=True, max_length=1000)
    OPERATOR_CHOICES = [
        (Operator.AND.value, "AND (&&)"),
        (Operator.OR.value, "OR (||)")
    ]
    operator = models.CharField(choices=OPERATOR_CHOICES, max_length=10, default=Operator.OR.value)

class RuleComponent(models.Model):
    def __str__(self):
        return f"{self.rule.rule_class} - {self.rule.name} - {self.filter_field} - {self.regex}"
    rule = models.ForeignKey(Rule, on_delete=models.CASCADE)
    FILTER_FIELD_CHOICES = [
        (Request.url.value, "Request (URL)"),
        (Request.status.value, "Status code"),
        (Request.bbs.value, "Body bytes sent"),
        (Request.user.value, "Remote user"),
        (Request.req_time.value, "Request time"),
        (Request.body.value, "Request body"),
        (Request.headers.value, "Request headers"),
        (Request.ip.value, "Source IP address"),
    ]
    filter_field = models.CharField(choices=FILTER_FIELD_CHOICES,max_length=30, default=Request.url.value)
    regex = models.CharField(null=True,max_length=1500)

class Alert(models.Model):
    def __str__(self):
        return f"{self.rule.rule_class} - {self.rule.name} - {self.agent.name}"
    agent = models.ForeignKey(Agent, on_delete=models.PROTECT)
    rule = models.ForeignKey(Rule, null=True, on_delete=models.SET_NULL)
    is_processed = models.BooleanField(default=False)
    processed_by = models.ForeignKey(User, null=True,blank=True, on_delete=models.SET_NULL)
    message = models.CharField(null=True,blank=True,max_length=200)
    remote_user = models.CharField(blank=True,max_length=200)
    status = models.PositiveIntegerField()
    body_bytes_sent = models.PositiveIntegerField()
    request_time = models.FloatField()
    request = models.CharField(max_length=200)
    body = models.CharField(blank=True,max_length=2000)
    headers = models.CharField(blank=True,max_length=1500)
    remote_addr = models.GenericIPAddressField()
    timestamp = models.DateTimeField(auto_now_add=True)
    