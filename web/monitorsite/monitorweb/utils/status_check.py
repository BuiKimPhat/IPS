import time
from monitorweb.models import Agent
import requests

class AgentStatusUpdater:
    def __init__(self, update_interval = 30, unhealthy_threshold = 30):
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
                healthy = True

                # Health check URL
                if agent.health is not None and agent.health != "None" and agent.health != "":
                    # print("health checking")
                    try:
                        r = requests.get(agent.health)
                        healthy = True if r.status_code // 100 < 4 else False
                    except:
                        healthy = False

                if last_activity_time is None:
                    # No activity record exists for the agent - set it to active
                    self.last_activity_times[agent.name] = now
                    if healthy:
                        agent.status = 'Healthy'
                    else:
                        agent.status = "Unhealthy"
                    agent.save()
                else:
                    # Check if the agent has been inactive for too long
                    inactive_duration = now - last_activity_time
                    if inactive_duration > self.unhealthy_threshold or not healthy:
                        # Agent has been inactive for too long and health check is bad - update its status to Unhealthy
                        agent.status = 'Unhealthy'
                    else:
                        agent.status = 'Healthy'
                    agent.save()


            # Sleep for 1 minute before checking again
            time.sleep(self.update_interval)

    def update_last_activity_time(self, agent_name):
        self.last_activity_times[agent_name] = time.time()