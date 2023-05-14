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
    default = "IyEvYmluL2Jhc2gKdGltZWRhdGVjdGwgc2V0LXRpbWV6b25lIEFzaWEvSG9fQ2hpX01pbmgKYXB0LWdldCAteSB1cGRhdGUKYXB0LWdldCBpbnN0YWxsIC15IHB5dGhvbjMtcGlwIGRlZmF1bHQtbGlibXlzcWxjbGllbnQtZGV2IG15c3FsLXNlcnZlcgpwaXAgaW5zdGFsbCBEamFuZ28gbXlzcWxjbGllbnQgcHl0aG9uLWRvdGVudgpwaXAgaW5zdGFsbCAtVSBjaGFubmVsc1siZGFwaG5lIl0KY2QgL2hvbWUvdWJ1bnR1CmdpdCBjbG9uZSBodHRwczovL0J1aUtpbVBoYXQ6Z2l0aHViX3BhdF8xMUFJUVFVUFEwWG5RMThHcjJyNlBwXzltZ3J0bVFYMGppWVlnUWxWSmpoWmFzY0xCMWh3ZVlINlpPSkthZENiUXhBSjVaV0FCQmxOTU5KVXN5QGdpdGh1Yi5jb20vQnVpS2ltUGhhdC9JUFMuZ2l0CmNob3duIC1SIHVidW50dSBJUFMvCm15c3FsIC1lICJjcmVhdGUgZGF0YWJhc2UgaXBzOyIKbXlzcWwgLWUgImNyZWF0ZSB1c2VyICdpcHNfYWRtaW4nQCdsb2NhbGhvc3QnIGlkZW50aWZpZWQgYnkgJ1F4MmImR3kwTjlSMSp1RW44TjdQTUBKdXcqIU84VCc7IgpteXNxbCAtZSAiR1JBTlQgQ1JFQVRFLCBBTFRFUiwgRFJPUCwgSU5TRVJULCBVUERBVEUsIERFTEVURSwgU0VMRUNULCBSRUZFUkVOQ0VTLCBJTkRFWCBPTiBpcHMuKiBUTyAnaXBzX2FkbWluJ0AnbG9jYWxob3N0JyBXSVRIIEdSQU5UIE9QVElPTjsi"
}
