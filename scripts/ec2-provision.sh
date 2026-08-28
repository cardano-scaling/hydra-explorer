#!/usr/bin/env bash
#
# Provision a hydra-explorer EC2 instance from scratch: build the UEFI image,
# push it into an EBS snapshot with coldsnap, register an AMI, ensure the
# security group and launch. Run it through `just provision`, which supplies
# the tooling via the .#deploy shell.
#
# Re-runnable: the host key and the security group are reused if they already
# exist, and EC2_SNAPSHOT / EC2_AMI skip the steps that produced them, so a run
# that dies late does not have to re-upload the image.

set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
readonly ROOT

log() { printf '==> %s\n' "$*" >&2; }
die() {
  printf 'error: %s\n' "$*" >&2
  exit 1
}

require() {
  local bin
  for bin in "$@"; do
    command -v "$bin" >/dev/null 2>&1 ||
      die "$bin not found; use 'just provision' or 'nix develop .#deploy'"
  done
}

require aws coldsnap jq nix ssh-keygen

# Must be a Nitro instance type that supports UEFI boot; m6i/c6i/m7i do.
: "${EC2_INSTANCE_TYPE:=m6i.2xlarge}"
# Root volume in GiB. The image is ~7 GiB; the root partition grows to fill
# this on first boot (boot.growPartition + fileSystems."/".autoResize).
: "${EC2_VOLUME_SIZE:=500}"
: "${EC2_NAME:=hydra-explorer}"
: "${EC2_SSH_CIDR:=0.0.0.0/0}"
: "${EC2_WEB_CIDR:=0.0.0.0/0}"

# An empty AWS_PROFILE (the justfile default) is worse than none at all.
if [ -z "${AWS_PROFILE:-}" ]; then unset AWS_PROFILE; fi
# An explicit AWS_REGION wins, otherwise fall back to the profile's.
if [ -z "${AWS_REGION:-}" ]; then
  AWS_REGION="$(aws configure get region 2>/dev/null || true)"
fi
[ -n "${AWS_REGION:-}" ] ||
  die "no AWS region; export AWS_REGION or use 'just region=eu-central-1 provision'"
export AWS_REGION

coldsnap_args=(--region "$AWS_REGION")
[ -z "${AWS_PROFILE:-}" ] || coldsnap_args+=(--profile "$AWS_PROFILE")

info() { jq -r ".$1" "$ROOT/result-ami/nix-support/image-info.json"; }

log "region:  $AWS_REGION"
log "profile: ${AWS_PROFILE:-<default credential chain>}"
aws sts get-caller-identity --output table >&2 ||
  die "not authenticated; 'aws configure sso' then 'aws sso login', or 'aws configure'"

# coldsnap's aws-config is compiled with default-features = false and without
# the `sso` feature, so an IAM Identity Center profile the CLI handles fine
# leaves it with "the credentials provider was not properly configured". Let the
# CLI do the resolving and hand the result over as environment variables, which
# every provider path understands. Static keys pass straight through.
if [ -z "${AWS_ACCESS_KEY_ID:-}" ]; then
  if creds="$(aws configure export-credentials --format process 2>/dev/null)"; then
    AWS_ACCESS_KEY_ID="$(jq -r .AccessKeyId <<<"$creds")"
    AWS_SECRET_ACCESS_KEY="$(jq -r .SecretAccessKey <<<"$creds")"
    AWS_SESSION_TOKEN="$(jq -r '.SessionToken // empty' <<<"$creds")"
    export AWS_ACCESS_KEY_ID AWS_SECRET_ACCESS_KEY
    [ -z "$AWS_SESSION_TOKEN" ] || export AWS_SESSION_TOKEN
    # Env credentials win over the profile in the SDK chain, and dropping
    # --profile keeps coldsnap away from an SSO profile it cannot parse.
    coldsnap_args=(--region "$AWS_REGION")
    log "exported resolved credentials for coldsnap"
  else
    log "warning: 'aws configure export-credentials' failed; coldsnap will resolve"
    log "         credentials itself, which does not work for SSO profiles"
  fi
fi

# --- host key -------------------------------------------------------------
# agenix decrypts secrets during activation using the host key, so a random
# per-instance one can never match secrets/secrets.nix. Pin a persistent key
# through user-data instead; ec2-data.nix installs it on first boot.
hostkey_dir="$ROOT/secrets/host-keys/$EC2_NAME"
hostkey="$hostkey_dir/ssh_host_ed25519_key"

if [ -n "${EC2_USER_DATA:-}" ]; then
  user_data="$EC2_USER_DATA"
  log "user-data: $user_data (provided)"
else
  if [ ! -e "$hostkey" ]; then
    mkdir -p "$hostkey_dir"
    chmod 700 "$hostkey_dir"
    ssh-keygen -q -t ed25519 -N "" -C "$EC2_NAME" -f "$hostkey"
    log "generated host key $hostkey"
  else
    log "reusing host key $hostkey"
  fi
  # ec2-data.nix reads these two markers, with '|' standing in for the newlines
  # of the private key. amazon-init.nix strips ^SSH_HOST_ lines before deciding
  # whether user-data is a config or a script, so a file holding only these two
  # lines is inert to it. Nothing else may go in here.
  user_data="$hostkey_dir/user-data.txt"
  {
    printf 'SSH_HOST_ED25519_KEY:%s\n' "$(tr '\n' '|' <"$hostkey")"
    printf 'SSH_HOST_ED25519_KEY_PUB:%s\n' "$(cat "$hostkey.pub")"
  } >"$user_data"
  chmod 600 "$user_data"
fi

# --- image ----------------------------------------------------------------
if [ -n "${EC2_AMI:-}" ]; then
  ami="$EC2_AMI"
  log "using ami $ami"
else
  if [ -n "${EC2_SNAPSHOT:-}" ]; then
    snapshot="$EC2_SNAPSHOT"
    log "using snapshot $snapshot"
  else
    log "building .#ami"
    nix build "$ROOT#ami" --out-link "$ROOT/result-ami"
  fi

  # Fail loudly rather than registering a legacy-bios image as UEFI.
  [ "$(info boot_mode)" = uefi ] ||
    die "image boot_mode is '$(info boot_mode)', expected uefi; check ec2.efi in flake.nix"

  if [ -z "${EC2_SNAPSHOT:-}" ]; then
    # The path comes from image-info.json, not a glob: make-disk-image names
    # the raw output .img even though image.extension says raw.
    image="$(info file)"
    log "uploading $image ($(($(info logical_bytes) / 1024 / 1024)) MiB)"
    # No --omit-zero-blocks: it is incompatible with encrypted snapshots, and
    # EBS encryption-by-default may be on for the account. coldsnap rounds the
    # image up to a whole GiB itself, so no padding is needed.
    snapshot="$(coldsnap "${coldsnap_args[@]}" upload --wait \
      --description "$EC2_NAME nixos $(info label)" \
      --tag "Key=Name,Value=$EC2_NAME" \
      "$image")"
    log "snapshot: $snapshot"
  fi

  # --imds-support v2.0 is safe: ec2-metadata-fetcher.sh speaks IMDSv2.
  ami="$(aws ec2 register-image \
    --name "$EC2_NAME-$(info label)-$(date -u +%Y%m%d-%H%M%S)" \
    --description "NixOS $EC2_NAME (UEFI)" \
    --architecture x86_64 \
    --boot-mode uefi \
    --virtualization-type hvm \
    --ena-support \
    --sriov-net-support simple \
    --imds-support v2.0 \
    --root-device-name /dev/xvda \
    --block-device-mappings \
    "DeviceName=/dev/xvda,Ebs={SnapshotId=$snapshot,VolumeSize=$EC2_VOLUME_SIZE,VolumeType=gp3,DeleteOnTermination=true}" \
    --query ImageId --output text)"
  log "ami: $ami"
fi

# --- network --------------------------------------------------------------
if [ -n "${EC2_SUBNET_ID:-}" ]; then
  subnet="$EC2_SUBNET_ID"
  vpc="$(aws ec2 describe-subnets --subnet-ids "$subnet" \
    --query 'Subnets[0].VpcId' --output text)"
else
  vpc="$(aws ec2 describe-vpcs --filters Name=isDefault,Values=true \
    --query 'Vpcs[0].VpcId' --output text)"
  [ "$vpc" != None ] || die "no default VPC in $AWS_REGION; set EC2_SUBNET_ID"
  subnet="$(aws ec2 describe-subnets --filters "Name=vpc-id,Values=$vpc" \
    --query 'Subnets[0].SubnetId' --output text)"
  [ "$subnet" != None ] || die "no subnet in $vpc; set EC2_SUBNET_ID"
fi

# The NixOS firewall on the instance already allows only 22/80/443; this is the
# second gate in front of it.
sg="$(aws ec2 describe-security-groups \
  --filters "Name=group-name,Values=$EC2_NAME" "Name=vpc-id,Values=$vpc" \
  --query 'SecurityGroups[0].GroupId' --output text)"
if [ "$sg" = None ]; then
  sg="$(aws ec2 create-security-group \
    --group-name "$EC2_NAME" \
    --description "$EC2_NAME (ssh, http, https)" \
    --vpc-id "$vpc" --query GroupId --output text)"
  log "created security group $EC2_NAME ($sg)"
else
  log "reusing security group $EC2_NAME ($sg)"
fi

authorize() {
  local port=$1 cidr=$2 out
  # Re-running provision should not be an error.
  if out="$(aws ec2 authorize-security-group-ingress --group-id "$sg" \
    --ip-permissions "IpProtocol=tcp,FromPort=$port,ToPort=$port,IpRanges=[{CidrIp=$cidr}]" 2>&1)"; then
    log "allowed tcp/$port from $cidr"
  elif ! grep -q InvalidPermission.Duplicate <<<"$out"; then
    die "$out"
  fi
}
authorize 22 "$EC2_SSH_CIDR"
authorize 80 "$EC2_WEB_CIDR"
authorize 443 "$EC2_WEB_CIDR"

# --- launch ---------------------------------------------------------------
# Subnet and group go inside the interface spec: passing them alongside
# --associate-public-ip-address at the top level is ambiguous to the CLI.
args=(
  --image-id "$ami"
  --instance-type "$EC2_INSTANCE_TYPE"
  --network-interfaces
  "DeviceIndex=0,AssociatePublicIpAddress=true,SubnetId=$subnet,Groups=$sg,DeleteOnTermination=true"
  --block-device-mappings
  "DeviceName=/dev/xvda,Ebs={VolumeSize=$EC2_VOLUME_SIZE,VolumeType=gp3,DeleteOnTermination=true}"
  --user-data "file://$user_data"
  --metadata-options 'HttpTokens=required,HttpEndpoint=enabled'
  --tag-specifications "ResourceType=instance,Tags=[{Key=Name,Value=$EC2_NAME}]"
)
# An EC2 key pair is break-glass only: apply-ec2-data appends it to
# /root/.ssh/authorized_keys, whereas normal access is the hydra user, whose
# declared keys live in /etc/ssh/authorized_keys.d/hydra.
[ -z "${EC2_KEY_NAME:-}" ] || args+=(--key-name "$EC2_KEY_NAME")

log "launching $EC2_INSTANCE_TYPE from $ami in $subnet"
id="$(aws ec2 run-instances "${args[@]}" --query 'Instances[0].InstanceId' --output text)"

aws ec2 wait instance-running --instance-ids "$id"
ip="$(aws ec2 describe-instances --instance-ids "$id" \
  --query 'Reservations[0].Instances[0].PublicIpAddress' --output text)"
log "waiting for status checks (first boot resizes the root filesystem)"
aws ec2 wait instance-status-ok --instance-ids "$id"

cat >&2 <<SUMMARY

  region     $AWS_REGION
  ami        $ami
  group      $sg
  instance   $id
  address    $ip

  ssh hydra@$ip
  just deploy-ec2 $ip     # for later config changes

SUMMARY

if [ -z "${EC2_USER_DATA:-}" ]; then
  cat >&2 <<NOTE
  The host key at $hostkey is pinned through user-data. For agenix to decrypt
  github-runner-token, put its public key in secrets/secrets.nix as 'hostKey':

    $(cat "$hostkey.pub")

  then 'cd secrets && nix run github:ryantm/agenix -- -r' and redeploy. The key
  is only installed late in the first boot, so reboot once before expecting the
  secret to decrypt. See the README for the rest.

NOTE
fi
