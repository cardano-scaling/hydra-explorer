[private]
default:
  @just --list

# AWS region; empty falls back to the profile's configured region
region := env_var_or_default("AWS_REGION", "")
# AWS CLI profile; empty falls back to the default credential chain
profile := env_var_or_default("AWS_PROFILE", "")
# Nitro instance type that supports UEFI boot; m6i/c6i/m7i do
instance_type := env_var_or_default("EC2_INSTANCE_TYPE", "m6i.2xlarge")
# Root EBS volume in GiB; the image is ~7 GiB and grows to this on first boot
volume_size := env_var_or_default("EC2_VOLUME_SIZE", "500")
# Name tag, security group name and AMI name prefix
name := env_var_or_default("EC2_NAME", "hydra-runner-and-explorer")
# EC2 key pair, as a break-glass root key (see README)
key_name := env_var_or_default("EC2_KEY_NAME", "")
# Resume a failed run without redoing the earlier steps
snapshot := env_var_or_default("EC2_SNAPSHOT", "")
ami := env_var_or_default("EC2_AMI", "")

export AWS_REGION := region
export AWS_PROFILE := profile
export EC2_INSTANCE_TYPE := instance_type
export EC2_VOLUME_SIZE := volume_size
export EC2_NAME := name
export EC2_KEY_NAME := key_name
export EC2_SNAPSHOT := snapshot
export EC2_AMI := ami

# format Nix, Haskell and JavaScript
fmt:
  nix fmt

# provision a new EC2 instance from scratch
provision:
  nix develop .#deploy -c ./scripts/ec2-provision.sh

# deploy to gcp server
# deploy-gce:
#   nix develop .#deploy -c nixos-rebuild switch \
#     --target-host root@34.153.175.57 \
#     --flake .#explorer-gce \
#     --sudo --ask-sudo-password

# deploy to the ec2 server; give `host` a default once the address settles
deploy-ec2 host:
  nix develop .#deploy -c nixos-rebuild switch \
    --target-host hydra@{{host}} \
    --flake .#explorer-ec2 \
    --sudo --ask-sudo-password
