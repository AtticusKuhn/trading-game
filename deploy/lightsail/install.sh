#!/usr/bin/env bash
set -euo pipefail

# Run as root on Ubuntu after unpacking the Nix runtime closure into /nix/store.
web_binary=${1:?Usage: sudo bash install.sh /nix/store/.../bin/trading-game-web}
[[ "$web_binary" == /nix/store/*/bin/trading-game-web && -x "$web_binary" ]]
config_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)

export DEBIAN_FRONTEND=noninteractive
apt-get update -qq
apt-get install -y -qq nginx

cat > /etc/systemd/system/trading-game.service <<EOF
[Unit]
Description=Trading Game web server
After=network.target

[Service]
DynamicUser=yes
ExecStart=$web_binary 3000 3600 +RTS -N2 -RTS
Restart=always
RestartSec=3
NoNewPrivileges=yes
PrivateTmp=yes
ProtectHome=yes
ProtectSystem=strict

[Install]
WantedBy=multi-user.target
EOF

install -m 644 "$config_dir/nginx.conf" /etc/nginx/sites-available/trading-game
ln -sfn /etc/nginx/sites-available/trading-game /etc/nginx/sites-enabled/trading-game
rm -f /etc/nginx/sites-enabled/default
nginx -t
systemctl daemon-reload
systemctl enable trading-game nginx
systemctl restart trading-game
systemctl reload-or-restart nginx
