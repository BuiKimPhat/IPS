from django.db import models

class Agent(models.Model):
    def __str__(self):
        return self.name
    name = models.CharField(max_length=200)
    ip = models.GenericIPAddressField()
    health = models.URLField(max_length=200)
    registered_at = models.DateTimeField(auto_now_add=True)

class Alert(models.Model):
    def __str__(self):
        return self.message
    agent = models.ForeignKey(Agent, on_delete=models.PROTECT)
    message = models.CharField(max_length=500)
    src = models.GenericIPAddressField()
    srcp = models.PositiveIntegerField(null=True)
    dst = models.GenericIPAddressField()
    dstp = models.PositiveIntegerField()
    protocol = models.CharField(max_length=10)
    action = models.CharField(max_length=50)