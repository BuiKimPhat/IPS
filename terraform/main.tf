# Setting
provider "aws" {
  region = "us-east-1"
}

terraform {
  backend "s3" {
    bucket = "ips-terraform"
    key    = "ips-terraform-state"
    region = "us-east-1"
  }
}

# Create VPC
module "ips_vpc" {
  source             = "terraform-aws-modules/vpc/aws"
  name               = "ips-vpc"
  cidr               = var.vpc_cidr
  azs                = var.azs
  private_subnets    = var.private_subnets
  public_subnets     = var.public_subnets
  enable_nat_gateway = false
  enable_vpn_gateway = false
  tags = {
    Category    = "VPC"
    ProjectName = "IPS"
  }
}

# Create Security groups
data "http" "my_public_ip" {
  url = "https://icanhazip.com" # fetch my public IP
}
resource "aws_security_group" "allow_all" {
  name        = "ips-allow_all"
  description = "Allow all traffic"
  vpc_id      = module.ips_vpc.vpc_id
  ingress {
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks      = ["0.0.0.0/0"]
    ipv6_cidr_blocks = ["::/0"]
  }
  egress {
    from_port        = 0
    to_port          = 0
    protocol         = "-1"
    cidr_blocks      = ["0.0.0.0/0"]
    ipv6_cidr_blocks = ["::/0"]
  }
  tags = {
    Environment = "Dev"
    Category    = "VPC"
    ProjectName = "IPS"
  }
}

# Create EC2 intances

# Monitor EC2
module "monitor_ec2" {

  create_spot_instance = var.monitor_spot
  # Spot request specific attributes
  spot_price                          = var.monitor_spot_price
  spot_wait_for_fulfillment           = true
  spot_type                           = "persistent"
  spot_instance_interruption_behavior = "stop"

  source                      = "terraform-aws-modules/ec2-instance/aws"
  version                     = "~> 3.0"
  name                        = "ips-monitor-ec2"
  ami                         = var.ami
  instance_type               = var.monitor_ec2_type
  key_name                    = var.key_name
  monitoring                  = true
  vpc_security_group_ids      = [aws_security_group.allow_all.id]
  associate_public_ip_address = true
  subnet_id                   = module.ips_vpc.public_subnets[0]
  private_ip                  = var.monitor_private_ip
  user_data_base64            = var.monitor_user_data_64
  tags = {
    Environment = "Dev"
    Category    = "EC2"
    ProjectName = "IPS"
  }
}

module "static_web_ec2" {
  source                      = "terraform-aws-modules/ec2-instance/aws"
  version                     = "~> 3.0"
  name                        = "ips-static_web-ec2"
  ami                         = var.ami
  instance_type               = var.static_web_ec2_type
  key_name                    = var.key_name
  vpc_security_group_ids      = [aws_security_group.allow_all.id]
  associate_public_ip_address = true
  subnet_id                   = module.ips_vpc.public_subnets[1]
  private_ip                  = var.static_web_private_ip
  user_data_base64            = var.static_web_user_data_64
  tags = {
    Environment = "Dev"
    Category    = "EC2"
    ProjectName = "IPS"
  }
}

module "dvwa_ec2" {
  source                      = "terraform-aws-modules/ec2-instance/aws"
  version                     = "~> 3.0"
  name                        = "ips-dvwa-ec2"
  ami                         = var.ami
  instance_type               = var.dvwa_ec2_type
  key_name                    = var.key_name
  vpc_security_group_ids      = [aws_security_group.allow_all.id]
  associate_public_ip_address = true
  subnet_id                   = module.ips_vpc.public_subnets[1]
  private_ip                  = var.dvwa_private_ip
  user_data_base64            = var.dvwa_user_data_64
  tags = {
    Environment = "Dev"
    Category    = "EC2"
    ProjectName = "IPS"
  }
}