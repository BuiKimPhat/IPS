from django.db import models

class Agent(models.Model):
    def __str__(self):
        return self.name
    name = models.CharField(unique=True, max_length=200)
    ip = models.GenericIPAddressField(unique=True)
    health = models.URLField(max_length=200)
    status = models.CharField(max_length=20)
    registered_at = models.DateTimeField(auto_now_add=True)

class Rule(models.Model):
    def __str__(self):
        return self.name
    name = models.CharField(max_length=200)
    rule_id = models.CharField(null=True,unique=True,max_length=200)
    rule_class = models.CharField(max_length=300)
    FILTER_FIELD_CHOICES = [
        ("URL", "Request (URL)"),
        ("Status", "Status code"),
        ("BBS", "Body bytes sent"),
        ("User", "Remote user"),
        ("ReqTime", "Request time"),
        ("Body", "Request body"),
        ("Header", "Request headers"),
    ]
    filter_field = models.CharField(choices=FILTER_FIELD_CHOICES,max_length=30)
    regex = models.CharField(null=True,max_length=1500)
    deny = models.BooleanField(default=True)
    ref = models.URLField(null=True, max_length=500)
    description = models.CharField(null=True,max_length=1000)

class Alert(models.Model):
    def __str__(self):
        return self.rule.name + " on " + self.agent.name
    agent = models.ForeignKey(Agent, on_delete=models.PROTECT)
    rule = models.ForeignKey(Rule, null=True, on_delete=models.SET_NULL)
    remote_user = models.CharField(max_length=200)
    status = models.PositiveIntegerField()
    body_bytes_sent = models.PositiveIntegerField()
    request_time = models.FloatField()
    request = models.CharField(max_length=200)
    body = models.CharField(max_length=2000)
    headers = models.CharField(max_length=1500)
    remote_addr = models.GenericIPAddressField()
    timestamp = models.DateTimeField(auto_now_add=True)
    