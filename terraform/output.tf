output "monitor_ec2_public_ip" {
  value = module.monitor_ec2.public_ip
  description = "Public IP of monitor EC2 instance"
}
output "monitor_ec2_private_ip" {
  value = module.monitor_ec2.private_ip
  description = "Private IP of monitor EC2 instance"
}
output "agent1_ec2_public_ip" {
  value = module.agent1_ec2.public_ip
  description = "Public IP of agent1 EC2 instance"
}
output "agent1_ec2_private_ip" {
  value = module.agent1_ec2.private_ip
  description = "Private IP of agent1 EC2 instance"
}