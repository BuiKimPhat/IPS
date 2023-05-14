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
    default = "IyEvYmluL2Jhc2gKdGltZWRhdGVjdGwgc2V0LXRpbWV6b25lIEFzaWEvSG9fQ2hpX01pbmgKY3VybCAtZnNTTCBodHRwczovL3BhY2thZ2VzLnJlZGlzLmlvL2dwZyB8IHN1ZG8gZ3BnIC0tZGVhcm1vciAtbyAvdXNyL3NoYXJlL2tleXJpbmdzL3JlZGlzLWFyY2hpdmUta2V5cmluZy5ncGcKZWNobyAiZGViIFtzaWduZWQtYnk9L3Vzci9zaGFyZS9rZXlyaW5ncy9yZWRpcy1hcmNoaXZlLWtleXJpbmcuZ3BnXSBodHRwczovL3BhY2thZ2VzLnJlZGlzLmlvL2RlYiAkKGxzYl9yZWxlYXNlIC1jcykgbWFpbiIgfCBzdWRvIHRlZSAvZXRjL2FwdC9zb3VyY2VzLmxpc3QuZC9yZWRpcy5saXN0CmFwdC1nZXQgLXkgdXBkYXRlCmFwdC1nZXQgaW5zdGFsbCAteSBweXRob24zLXBpcCBkZWZhdWx0LWxpYm15c3FsY2xpZW50LWRldiBteXNxbC1zZXJ2ZXIgcmVkaXMKcGlwIGluc3RhbGwgRGphbmdvIG15c3FsY2xpZW50IHB5dGhvbi1kb3RlbnYgZGFwaG5lCmNkIC9ob21lL3VidW50dQpnaXQgY2xvbmUgaHR0cHM6Ly9CdWlLaW1QaGF0OmdpdGh1Yl9wYXRfMTFBSVFRVVBRMFhuUTE4R3IycjZQcF85bWdydG1RWDBqaVlZZ1FsVkpqaFphc2NMQjFod2VZSDZaT0pLYWRDYlF4QUo1WldBQkJsTk1OSlVzeUBnaXRodWIuY29tL0J1aUtpbVBoYXQvSVBTLmdpdApjaG93biAtUiB1YnVudHUgSVBTLwpteXNxbCAtZSAiY3JlYXRlIGRhdGFiYXNlIGlwczsiCm15c3FsIC1lICJjcmVhdGUgdXNlciAnaXBzX2FkbWluJ0AnbG9jYWxob3N0JyBpZGVudGlmaWVkIGJ5ICdReDJiJkd5ME45UjEqdUVuOE43UE1ASnV3KiFPOFQnOyIKbXlzcWwgLWUgIkdSQU5UIENSRUFURSwgQUxURVIsIERST1AsIElOU0VSVCwgVVBEQVRFLCBERUxFVEUsIFNFTEVDVCwgUkVGRVJFTkNFUywgSU5ERVggT04gaXBzLiogVE8gJ2lwc19hZG1pbidAJ2xvY2FsaG9zdCcgV0lUSCBHUkFOVCBPUFRJT047Ig"
}
