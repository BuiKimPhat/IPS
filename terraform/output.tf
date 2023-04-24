output "monitor_ec2_ip" {
  value = aws_eip.monitor_ec2_public_ip.public_ip
  description = "Public IP of monitor EC2 instance"
}