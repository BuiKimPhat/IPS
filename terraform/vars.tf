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
    default = "t3.small"
}
variable "key_name" {
    type = string
    default = "mykeypair"
}
variable "monitor_user_data_64" {
    type = string
    default = "IyEvYmluL2Jhc2gKdGltZWRhdGVjdGwgc2V0LXRpbWV6b25lIEFzaWEvSG9fQ2hpX01pbmgKY3VybCAtZnNTTCBodHRwczovL3BhY2thZ2VzLnJlZGlzLmlvL2dwZyB8IHN1ZG8gZ3BnIC0tZGVhcm1vciAtbyAvdXNyL3NoYXJlL2tleXJpbmdzL3JlZGlzLWFyY2hpdmUta2V5cmluZy5ncGcKZWNobyAiZGViIFtzaWduZWQtYnk9L3Vzci9zaGFyZS9rZXlyaW5ncy9yZWRpcy1hcmNoaXZlLWtleXJpbmcuZ3BnXSBodHRwczovL3BhY2thZ2VzLnJlZGlzLmlvL2RlYiAkKGxzYl9yZWxlYXNlIC1jcykgbWFpbiIgfCBzdWRvIHRlZSAvZXRjL2FwdC9zb3VyY2VzLmxpc3QuZC9yZWRpcy5saXN0CmFwdC1nZXQgLXkgdXBkYXRlCmFwdC1nZXQgaW5zdGFsbCAteSBweXRob24zLXBpcCBkZWZhdWx0LWxpYm15c3FsY2xpZW50LWRldiBteXNxbC1zZXJ2ZXIgcmVkaXMKcGlwIGluc3RhbGwgRGphbmdvIG15c3FsY2xpZW50IHB5dGhvbi1kb3RlbnYgY2hhbm5lbHNbJ2RhcGhuZSddCnBpcCBpbnN0YWxsIC0tdXBncmFkZSBhdHRycwpjZCAvaG9tZS91YnVudHUKZ2l0IGNsb25lIGh0dHBzOi8vQnVpS2ltUGhhdDpnaXRodWJfcGF0XzExQUlRUVVQUTBYblExOEdyMnI2UHBfOW1ncnRtUVgwamlZWWdRbFZKamhaYXNjTEIxaHdlWUg2Wk9KS2FkQ2JReEFKNVpXQUJCbE5NTkpVc3lAZ2l0aHViLmNvbS9CdWlLaW1QaGF0L0lQUy5naXQKY2hvd24gLVIgdWJ1bnR1IElQUy8KbXlzcWwgLWUgImNyZWF0ZSBkYXRhYmFzZSBpcHM7IgpteXNxbCAtZSAiY3JlYXRlIHVzZXIgJ2lwc19hZG1pbidAJ2xvY2FsaG9zdCcgaWRlbnRpZmllZCBieSAnUXgyYiZHeTBOOVIxKnVFbjhON1BNQEp1dyohTzhUJzsiCm15c3FsIC1lICJHUkFOVCBDUkVBVEUsIEFMVEVSLCBEUk9QLCBJTlNFUlQsIFVQREFURSwgREVMRVRFLCBTRUxFQ1QsIFJFRkVSRU5DRVMsIElOREVYIE9OIGlwcy4qIFRPICdpcHNfYWRtaW4nQCdsb2NhbGhvc3QnIFdJVEggR1JBTlQgT1BUSU9OOyI="
}
