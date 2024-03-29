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
    default = ["10.0.101.0/24", "10.0.102.0/24"]
}

# EC2
variable "key_name" {
    type = string
    default = "mykeypair"
}

# Monitor EC2
variable "ami" {
    type = string
    default = "ami-007855ac798b5175e"
}
variable "monitor_spot" {
    type = bool
    default = true
}
variable "monitor_spot_price" {
    type = string
    default = "0.0375"
}
variable "monitor_ec2_type" {
    type = string
    default = "t3.medium"
}
variable "monitor_user_data_64" {
    type = string
    default = "IyEvYmluL2Jhc2gKdGltZWRhdGVjdGwgc2V0LXRpbWV6b25lIEFzaWEvSG9fQ2hpX01pbmgKY3VybCAtZnNTTCBodHRwczovL3BhY2thZ2VzLnJlZGlzLmlvL2dwZyB8IHN1ZG8gZ3BnIC0tZGVhcm1vciAtbyAvdXNyL3NoYXJlL2tleXJpbmdzL3JlZGlzLWFyY2hpdmUta2V5cmluZy5ncGcKZWNobyAiZGViIFtzaWduZWQtYnk9L3Vzci9zaGFyZS9rZXlyaW5ncy9yZWRpcy1hcmNoaXZlLWtleXJpbmcuZ3BnXSBodHRwczovL3BhY2thZ2VzLnJlZGlzLmlvL2RlYiAkKGxzYl9yZWxlYXNlIC1jcykgbWFpbiIgfCBzdWRvIHRlZSAvZXRjL2FwdC9zb3VyY2VzLmxpc3QuZC9yZWRpcy5saXN0CmFwdC1nZXQgLXkgdXBkYXRlCkRFQklBTl9GUk9OVEVORD1ub25pbnRlcmFjdGl2ZSBhcHQtZ2V0IGluc3RhbGwgLXkgcHl0aG9uMy1waXAgZGVmYXVsdC1saWJteXNxbGNsaWVudC1kZXYgbXlzcWwtc2VydmVyIHJlZGlzIHBrZy1jb25maWcKcGlwIGluc3RhbGwgRGphbmdvIG15c3FsY2xpZW50IHB5dGhvbi1kb3RlbnYgY2hhbm5lbHNbJ2RhcGhuZSddIGNoYW5uZWxzX3JlZGlzCnBpcCBpbnN0YWxsIC0tdXBncmFkZSBhdHRycwpjZCAvaG9tZS91YnVudHUKZ2l0IGNsb25lIGh0dHBzOi8vQnVpS2ltUGhhdDpnaXRodWJfcGF0XzExQUlRUVVQUTBYblExOEdyMnI2UHBfOW1ncnRtUVgwamlZWWdRbFZKamhaYXNjTEIxaHdlWUg2Wk9KS2FkQ2JReEFKNVpXQUJCbE5NTkpVc3lAZ2l0aHViLmNvbS9CdWlLaW1QaGF0L0lQUy5naXQKY2hvd24gLVIgdWJ1bnR1IElQUy8KbXlzcWwgLWUgImNyZWF0ZSBkYXRhYmFzZSBpcHM7IgpteXNxbCAtZSAiY3JlYXRlIHVzZXIgJ2lwc19hZG1pbidAJ2xvY2FsaG9zdCcgaWRlbnRpZmllZCBieSAnUXgyYiZHeTBOOVIxKnVFbjhON1BNQEp1dyohTzhUJzsiCm15c3FsIC1lICJHUkFOVCBDUkVBVEUsIEFMVEVSLCBEUk9QLCBJTlNFUlQsIFVQREFURSwgREVMRVRFLCBTRUxFQ1QsIFJFRkVSRU5DRVMsIElOREVYIE9OIGlwcy4qIFRPICdpcHNfYWRtaW4nQCdsb2NhbGhvc3QnIFdJVEggR1JBTlQgT1BUSU9OOyI="
}
variable "monitor_private_ip" {
    type = string
    default = "10.0.101.69"
}

# Static web EC2s
variable "static_web_ec2_type" {
    type = string
    default = "t2.micro"
}
variable "static_web_user_data_64" {
    type = string
    default = "IyEvYmluL2Jhc2gKdGltZWRhdGVjdGwgc2V0LXRpbWV6b25lIEFzaWEvSG9fQ2hpX01pbmgKYXB0LWdldCAteSB1cGRhdGUKYXB0LWdldCBpbnN0YWxsIC15IHB5dGhvbjMtcGlwIG5naW54CnBpcCBpbnN0YWxsIHdlYnNvY2tldHMgcHN1dGlsCmNkIC9ob21lL3VidW50dQpnaXQgY2xvbmUgaHR0cHM6Ly9CdWlLaW1QaGF0OmdpdGh1Yl9wYXRfMTFBSVFRVVBRMFhuUTE4R3IycjZQcF85bWdydG1RWDBqaVlZZ1FsVkpqaFphc2NMQjFod2VZSDZaT0pLYWRDYlF4QUo1WldBQkJsTk1OSlVzeUBnaXRodWIuY29tL0J1aUtpbVBoYXQvSVBTLmdpdApjaG93biAtUiB1YnVudHUgSVBTLwpjZCBJUFMvCi4vYWdlbnQvc2V0dXAuc2gKLi90ZXN0L3N0YXRpY19jb25maWcuc2gKcHl0aG9uMyBhZ2VudC9hZ2VudC5weSAtbiBzdGF0aWN3ZWJfYWdlbnQ="
}
variable "static_web_private_ip" {
    type = string
    default = "10.0.102.70"
}

# DVWA web EC2s
variable "dvwa_ec2_type" {
    type = string
    default = "t2.micro"
}
variable "dvwa_user_data_64" {
    type = string
    default = "IyEvYmluL2Jhc2gKdGltZWRhdGVjdGwgc2V0LXRpbWV6b25lIEFzaWEvSG9fQ2hpX01pbmgKYXB0LWdldCAteSB1cGRhdGUKYXB0LWdldCBpbnN0YWxsIC15IHB5dGhvbjMtcGlwIG5naW54CnBpcCBpbnN0YWxsIHdlYnNvY2tldHMgcHN1dGlsCmNkIC9ob21lL3VidW50dQpnaXQgY2xvbmUgaHR0cHM6Ly9CdWlLaW1QaGF0OmdpdGh1Yl9wYXRfMTFBSVFRVVBRMFhuUTE4R3IycjZQcF85bWdydG1RWDBqaVlZZ1FsVkpqaFphc2NMQjFod2VZSDZaT0pLYWRDYlF4QUo1WldBQkJsTk1OSlVzeUBnaXRodWIuY29tL0J1aUtpbVBoYXQvSVBTLmdpdApjaG93biAtUiB1YnVudHUgSVBTLwpjZCBJUFMvCi4vYWdlbnQvc2V0dXAuc2gKLi90ZXN0L2R2d2FfY29uZmlnLnNoCnB5dGhvbjMgYWdlbnQvYWdlbnQucHkgLW4gZHZ3YV9hZ2VudA=="
}
variable "dvwa_private_ip" {
    type = string
    default = "10.0.102.69"
}