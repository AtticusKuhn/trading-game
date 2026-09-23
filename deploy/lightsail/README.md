# Lightsail deployment

Public URL: http://184.32.11.81/

- AWS region: `us-west-2` (Oregon)
- Instance: `trading-game`, Ubuntu 24.04, `micro_3_0` (1 GB RAM)
- Plan: $7/month before credits, taxes, or usage beyond the included allowance
- Attached static IP: `trading-game-ip`
- Service: `trading-game.service`, automatically started at boot
- nginx serves HTTP on port 80 and forwards to Warp on loopback port 3000.
  SSE buffering is disabled.

This is the existing public demo: player names select identities without passwords.
Games and sessions exist only in memory and reset on every app restart or deployment.
HTTPS and a custom domain are not configured.

## Deploy an update

From this repository on the original development machine:

```sh
bash deploy/lightsail/deploy.sh 184.32.11.81 \
  ~/.ssh/lightsail-us-west-2.pem \
  ~/.ssh/trading-game-lightsail-known-hosts
```

The script builds the x86_64 Linux app locally using Nix, uploads its runtime
closure, verifies the archive checksum, and restarts the app. The server does
not need GHC or Nix. Old runtime closures remain in `/nix/store` until manually
removed; do not remove the one referenced by the active systemd unit.

## Inspect the deployment

The AWS CLI login was verified. It has no default region configured, so the
commands below explicitly pass `--region us-west-2`.

```sh
aws lightsail get-instance --region us-west-2 --instance-name trading-game \
  --query 'instance.{state:state.name,ip:publicIpAddress}'

ssh -i ~/.ssh/lightsail-us-west-2.pem \
  -o UserKnownHostsFile=~/.ssh/trading-game-lightsail-known-hosts \
  -o StrictHostKeyChecking=yes ubuntu@184.32.11.81

# On the server:
sudo systemctl status trading-game nginx
sudo journalctl -u trading-game --since '1 hour ago'
```

SSH is restricted to the development machine's public IP at deployment time
(`73.158.177.190/32`). If it changes, update the port 22 rule in the Lightsail
networking panel. Port 80 is public; port 3000 is not exposed. The SSH private
key is stored outside the repository, and host keys were verified via the AWS API.

## Remove the deployment and its ongoing hosting charges

These commands permanently remove the server and release its IP:

```sh
aws lightsail delete-instance --region us-west-2 --instance-name trading-game
# After deletion completes:
aws lightsail release-static-ip --region us-west-2 --static-ip-name trading-game-ip
```

Stopping an instance does not remove its hosting charge. An unattached static
IP can also incur charges, so release the address when deleting the server.
