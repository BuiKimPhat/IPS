# VPC
variable "azs" {
    type = list(string)
    default = [ "us-east-1a" ]
}
variable "vpc_cidr" {
    type = string
    default = "10.0.0.0/16"
}
variable "private_subnets" {
    type = list(string)
    default = ["10.0.1.0/24"]
}
variable "public_subnets" {
    type = list(string)
    default = ["10.0.101.0/24"]
}

# EC2
variable "ami" {
    type = string
    default = "ami-007855ac798b5175e"
}
variable "ec2_type" {
    type = string
    default = "t2.micro"
}
variable "key_name" {
    type = string
    default = "mykeypair"
}
variable "monitor_user_data_64" {
    type = string
    default = "IyEvYmluL2Jhc2gKYXB0LWdldCAteSB1cGRhdGUKYXB0LWdldCBpbnN0YWxsIC15IHB5dGhvbjMtcGlwIGRlZmF1bHQtbGlibXlzcWxjbGllbnQtZGV2IG15c3FsLXNlcnZlcgpwaXAgaW5zdGFsbCBEamFuZ28gbXlzcWxjbGllbnQgcHl0aG9uLWRvdGVudgpjZCAvaG9tZS91YnVudHUKZ2l0IGNsb25lIGh0dHBzOi8vQnVpS2ltUGhhdDpnaXRodWJfcGF0XzExQUlRUVVQUTBYblExOEdyMnI2UHBfOW1ncnRtUVgwamlZWWdRbFZKamhaYXNjTEIxaHdlWUg2Wk9KS2FkQ2JReEFKNVpXQUJCbE5NTkpVc3lAZ2l0aHViLmNvbS9CdWlLaW1QaGF0L0lQUy5naXQKY2hvd24gLVIgdWJ1bnR1IElQUy8KbXlzcWwgLWUgImNyZWF0ZSBkYXRhYmFzZSBpcHM7IgpteXNxbCAtZSAiY3JlYXRlIHVzZXIgJ2lwc19hZG1pbidAJ2xvY2FsaG9zdCcgaWRlbnRpZmllZCBieSAnUXgyYiZHeTBOOVIxKnVFbjhON1BNQEp1dyohTzhUJzsiCm15c3FsIC1lICJHUkFOVCBDUkVBVEUsIEFMVEVSLCBEUk9QLCBJTlNFUlQsIFVQREFURSwgREVMRVRFLCBTRUxFQ1QgT04gaXBzLiogVE8gJ2lwc19hZG1pbidAJ2xvY2FsaG9zdCcgV0lUSCBHUkFOVCBPUFRJT047Ig=="
}
