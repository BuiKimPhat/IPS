output "monitor_ec2_public_ip" {
  value = module.monitor_ec2.public_ip
  description = "Public IP of monitor EC2 instance"
}
output "monitor_ec2_private_ip" {
  value = module.monitor_ec2.private_ip
  description = "Private IP of monitor EC2 instance"
}
output "static_web_ec2_public_ip" {
  value = module.static_web_ec2.public_ip
  description = "Public IP of static_web EC2 instance"
}
output "static_web_ec2_private_ip" {
  value = module.static_web_ec2.private_ip
  description = "Private IP of static_web EC2 instance"
}
output "dvwa_ec2_public_ip" {
  value = module.dvwa_ec2.public_ip
  description = "Public IP of dvwa EC2 instance"
}
output "dvwa_ec2_private_ip" {
  value = module.dvwa_ec2.private_ip
  description = "Private IP of dvwa EC2 instance"
}