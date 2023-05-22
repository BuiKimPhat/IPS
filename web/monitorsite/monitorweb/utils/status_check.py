import time
from django.utils import timezone
from monitorweb.models import Agent

class AgentStatusUpdater:
    def __init__(self, update_interval, unhealthy_threshold):
        self.update_interval = update_interval
        self.unhealthy_threshold = unhealthy_threshold
        self.last_activity_times = {}

    def start(self):
        while True:
            # Get the current time
            now = time.time()

            # Check the activity status of each agent
            for agent in Agent.objects.all():
                last_activity_time = self.last_activity_times.get(agent.name)
                if last_activity_time is None:
                    # No activity record exists for the agent - set it to active
                    self.last_activity_times[agent.name] = now
                    agent.status = 'Healthy'
                    agent.save()
                else:
                    # Check if the agent has been inactive for too long
                    inactive_duration = (now - last_activity_time).total_seconds() // 60
                    if inactive_duration > self.threshold:
                        # Agent has been inactive for too long - update its status to Unhealthy
                        agent.status = 'Unhealthy'
                        agent.save()

            # Sleep for 1 minute before checking again
            time.sleep(self.update_interval)

    def update_last_activity_time(self, agent_name):
        self.last_activity_times[agent_name] = time.time()