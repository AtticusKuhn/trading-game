#!/usr/bin/env bash
set -euo pipefail

# Updates an existing Ubuntu Lightsail instance; restarting clears in-memory games.
host=${1:?Usage: bash deploy.sh HOST SSH_KEY KNOWN_HOSTS}
key=${2:?Provide the SSH private key path}
known_hosts=${3:?Provide a known_hosts file verified against AWS host keys}
[[ "$host" =~ ^[a-zA-Z0-9.-]+$ ]]
key=$(realpath "$key")
known_hosts=$(realpath "$known_hosts")
ssh_options=(-i "$key" -o "UserKnownHostsFile=$known_hosts" -o StrictHostKeyChecking=yes)
project_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/../.." && pwd)
work_dir=$(mktemp -d)
trap 'rm -rf "$work_dir"' EXIT
cd "$project_dir"

# Build locally; the small server only needs the executable and runtime libraries.
nix build .#packages.x86_64-linux.web --out-link "$work_dir/web"
web_path=$(readlink -f "$work_dir/web")
nix-store --query --requisites "$web_path" > "$work_dir/closure-paths"
tar -czf "$work_dir/runtime.tar.gz" --files-from "$work_dir/closure-paths"
(cd "$work_dir" && sha256sum runtime.tar.gz > runtime.sha256)

remote_dir=$(ssh "${ssh_options[@]}" "ubuntu@$host" 'mktemp -d /home/ubuntu/trading-game-deploy.XXXXXXXX')
[[ "$remote_dir" =~ ^/home/ubuntu/trading-game-deploy\.[a-zA-Z0-9]+$ ]]
scp "${ssh_options[@]}" "$work_dir/runtime.tar.gz" "$work_dir/runtime.sha256" \
    deploy/lightsail/install.sh deploy/lightsail/nginx.conf "ubuntu@$host:$remote_dir/"
ssh "${ssh_options[@]}" "ubuntu@$host" \
    "cd '$remote_dir' && sha256sum -c runtime.sha256 && sudo tar -xzf runtime.tar.gz -C / && sudo bash install.sh '$web_path/bin/trading-game-web' && rm -rf '$remote_dir'"
curl --fail --silent --show-error --retry 5 --retry-connrefused "http://$host/" > /dev/null
printf 'Running at http://%s/\n' "$host"
