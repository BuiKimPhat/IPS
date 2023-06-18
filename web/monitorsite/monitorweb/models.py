from django.db import models
from monitorweb.utils.enums import Request, Operator
from django.contrib.auth.models import User
from django.db.models.signals import post_save, post_delete
from django.dispatch import receiver
from channels.layers import get_channel_layer
from asgiref.sync import async_to_sync

class Agent(models.Model):
    def __str__(self):
        return f"{self.name} - {self.ip}"
    name = models.CharField(unique=True, max_length=200)
    ip = models.GenericIPAddressField(unique=True)
    health = models.URLField(null=True,blank=True,default='None',max_length=200)
    status = models.CharField(default='Registered',max_length=20)
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

class IptablesRule(models.Model):
    def __str__(self):
        return self.name
    name = models.CharField(null=True,blank=True,max_length=200)
    agent = models.ForeignKey(Agent, on_delete=models.PROTECT)
    srcip = models.GenericIPAddressField()
    protocol = models.CharField(max_length=10)
    chain = models.CharField(max_length=50)
    action = models.CharField(max_length=10)
    target = models.CharField(max_length=10)
    dport = models.PositiveIntegerField()

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


# Signal functions
@receiver(post_save, sender=Alert)
def create_iptables_rule(sender, instance, created, **kwargs):
    # Auto IPS for alerts
    if created and instance.rule.is_denied:
        IptablesRule.objects.create(name="Alert blocks IP", agent=instance.agent, srcip=instance.remote_addr, protocol='tcp', dport=80, chain='INPUT', action='A', target='DROP')

@receiver(post_save, sender=IptablesRule)
def append_iptables_rule(sender, instance, created, **kwargs):
    channel_layer = get_channel_layer()
    ips_agent_group = "ipsgroup_%s" % instance.agent.name
    async_to_sync(channel_layer.group_send)(ips_agent_group, {"type": "iptables_rule", "srcip": instance.srcip, "protocol": instance.protocol, "chain": instance.chain, "action": instance.action, "target": instance.target, "dport": instance.dport})
    
@receiver(post_delete, sender=IptablesRule)
def delete_iptables_rule(sender, instance, **kwargs):
    channel_layer = get_channel_layer()
    ips_agent_group = "ipsgroup_%s" % instance.agent.name
    async_to_sync(channel_layer.group_send)(ips_agent_group, {"type": "iptables_rule", "srcip": instance.srcip, "protocol": instance.protocol, "chain": instance.chain, "action": "D", "target": instance.target, "dport": instance.dport})

