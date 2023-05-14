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
  source = "terraform-aws-modules/vpc/aws"
  name = "ips-vpc"
  cidr = var.vpc_cidr
  azs             = var.azs
  private_subnets = var.private_subnets
  public_subnets  = var.public_subnets
  enable_nat_gateway = false
  enable_vpn_gateway = false
  tags = {
    Category = "VPC"
    ProjectName = "IPS"
  }
}

# Create Security groups
data "http" "my_public_ip" {
  url = "https://icanhazip.com" # fetch my public IP
}
resource "aws_security_group" "ssh_http" {
  name        = "ssh_http"
  description = "Allow SSH and HTTP inbound traffic"
  vpc_id      = module.ips_vpc.vpc_id
  ingress {
    description      = "SSH from My public IP"
    from_port        = 22
    to_port          = 22
    protocol         = "tcp"
    cidr_blocks      = ["${chomp(data.http.my_public_ip.response_body)}/32"]
  }
  ingress {
    description      = "HTTP from My public IP"
    from_port        = 80
    to_port          = 80
    protocol         = "tcp"
    cidr_blocks      = ["${chomp(data.http.my_public_ip.response_body)}/32"]
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
    Category = "VPC"
    ProjectName = "IPS"
  }
}

# Create EC2 intances
module "monitor_ec2" {
  source  = "terraform-aws-modules/ec2-instance/aws"
  version = "~> 3.0"
  name = "monitor-ec2"
  ami                    = var.ami
  instance_type          = var.ec2_type
  key_name               = var.key_name
  monitoring             = true
  vpc_security_group_ids = [aws_security_group.ssh_http.id]
  subnet_id              = module.ips_vpc.public_subnets[0]
  user_data_base64 = var.monitor_user_data_64
  tags = {
    Environment = "Dev"
    Category = "EC2"
    ProjectName = "IPS"
  }
}
resource "aws_eip" "monitor_ec2_public_ip" {
  instance = module.monitor_ec2.id
  vpc      = true
  tags = {
    Category = "EC2"
    ProjectName = "IPS"
  }
}