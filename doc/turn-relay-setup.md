# Self-hosted TURN relay for live wizard video

Companion to `doc/plugin-connectivity.md`'s "Worked example: live wizard video (participant
webcam → studymaster)" and `plugins/studymaster/README.md`. This is the Phase 4 piece of that
plan: NAT traversal for the **DFKI-hosted** deployment, where the wizard is on a different network
than the participant. LAN deployments don't need any of this — `ice_servers` stays empty there.

Decision made: **self-hosted [coturn](https://github.com/coturn/coturn), static long-term
credentials** (not the time-limited REST-API credential scheme) — good enough for piloting.
Revisit the credential scheme (below) before this carries real study data at scale.

## What you need before starting

- A small VPS with a public, static IP — any provider works. 1 vCPU / 512MB–1GB RAM is plenty;
  a TURN relay is bandwidth-bound, not compute-bound, and this is for one or two concurrent
  sessions, not a fleet.
- A DNS name pointing at that IP (e.g. `myturn.turn-rtc.de`) — needed for the TLS
  certificate below; a bare IP works too but complicates cert issuance.
- Ubuntu/Debian assumed below; coturn is packaged for most distros.

## 1. Install coturn

```bash
sudo apt update && sudo apt install -y coturn certbot
```

Enable the systemd service (it ships disabled by default on Debian/Ubuntu):

```bash
sudo sed -i 's/^#TURNSERVER_ENABLED=1/TURNSERVER_ENABLED=1/' /etc/default/coturn
```

## 2. Get a TLS certificate

Needed for the TURN-over-TLS listener on port 443 — this is what lets a wizard on a locked-down
network (the case that originally motivated this whole design) reach the relay disguised as
ordinary HTTPS traffic:

```bash
sudo certbot certonly --standalone -d myturn.turn-rtc.de
# certs land in /etc/letsencrypt/live/myturn.turn-rtc.de/{fullchain,privkey}.pem
```

Certbot's systemd timer handles renewal; coturn needs a restart after each renewal to pick up the
new cert (a `--deploy-hook` on the certbot renewal, restarting coturn, is the usual fix — add one
if this becomes a recurring papercut).

## 3. `/etc/turnserver.conf`

Replace `myturn.turn-rtc.de`, the IP addresses, and the credential below before deploying.

```ini
# --- identity -----------------------------------------------------------
realm=myturn.turn-rtc.de
server-name=myturn.turn-rtc.de

# --- listeners ------------------------------------------------------------
# Plain STUN/TURN over UDP — the fast path when a direct P2P route isn't
# available but the network doesn't block UDP outright.
listening-port=3478
# TURN over TLS on 443 — the one that matters for a wizard behind a
# restrictive firewall; looks like ordinary HTTPS to anything in between.
tls-listening-port=443
cert=/etc/letsencrypt/live/myturn.turn-rtc.de/fullchain.pem
pkey=/etc/letsencrypt/live/myturn.turn-rtc.de/privkey.pem

# Replace with this VPS's actual public IP (and its private IP if it has
# one, comma-separated: public-ip/private-ip) — coturn needs to know its
# own public address to hand out correct relay candidates.
external-ip=203.0.113.10

# --- relay port range -----------------------------------------------------
# The actual media relay traffic uses UDP ports in this range — open it on
# the VPS's firewall (see step 4). A few hundred ports is plenty for a
# handful of concurrent sessions.
min-port=49160
max-port=49260

# --- credentials (static, per the "static for now" decision) --------------
# One shared username/password handed to every browser via ice_servers.
# Anyone who opens devtools on the wizard or participant page can read
# this — acceptable for piloting, not for real data collection at scale.
# See "Revisiting credentials" below before that changes.
lt-cred-mech
user=vsmuser:CHANGE_ME_TO_A_REAL_PASSWORD

# --- hardening --------------------------------------------------------------
# A public TURN relay can otherwise be abused as an open relay into your
# own network or others' internal services — deny it from ever relaying
# to private/loopback ranges.
no-loopback-peers
no-multicast-peers
denied-peer-ip=0.0.0.0-0.255.255.255
denied-peer-ip=10.0.0.0-10.255.255.255
denied-peer-ip=100.64.0.0-100.127.255.255
denied-peer-ip=127.0.0.0-127.255.255.255
denied-peer-ip=169.254.0.0-169.254.255.255
denied-peer-ip=172.16.0.0-172.31.255.255
denied-peer-ip=192.0.0.0-192.0.0.255
denied-peer-ip=192.168.0.0-192.168.255.255
denied-peer-ip=198.18.0.0-198.19.255.255

# No point exposing the CLI management console publicly.
no-cli

fingerprint
log-file=/var/log/turnserver.log
```

## 4. Firewall

Open, on whatever firewall sits in front of this VPS:

- `3478/udp` and `3478/tcp` — plain STUN/TURN
- `443/tcp` — TURN over TLS
- `49160-49260/udp` (or whatever range you set above) — the actual relay traffic

## 5. Start it

```bash
sudo systemctl enable --now coturn
sudo systemctl status coturn   # confirm it's listening
```

Quick sanity check from any machine: Mozilla's public
[Trickle ICE tester](https://webrtc.github.io/samples/src/content/peerconnection/trickle-ice/) —
plug in `stun:myturn.turn-rtc.de:3478` and
`turn:myturn.turn-rtc.de:3478` (with the username/credential) and confirm it gathers a
`relay` candidate.

## 6. Point studymaster at it

Once it's confirmed working, set the DFKI-hosted project's `sm` device config — `ice_servers` —
to:

```json
[
  {"urls": "stun:myturn.turn-rtc.de:3478"},
  {"urls": "turn:myturn.turn-rtc.de:3478", "username": "vsmuser", "credential": "CHANGE_ME_TO_A_REAL_PASSWORD"},
  {"urls": "turns:myturn.turn-rtc.de:443?transport=tcp", "username": "vsmuser", "credential": "CHANGE_ME_TO_A_REAL_PASSWORD"}
]
```

Three entries: plain STUN (address discovery, always tried first), TURN over UDP (relay fallback
when direct fails but UDP is open), and TURN over TLS on 443 (last resort for networks that block
everything except HTTPS-looking traffic). ICE tries all of them and uses whichever pair actually
connects — no code change needed on VSM's side, this is purely the `ice_servers` config value
already wired up in `StudyMasterExecutor`/`VsmWebcamElement`.

Leave `ice_servers` empty on LAN-deployed projects, as before.

## Verifying end to end

Once configured: put the wizard and the participant on genuinely different networks, start a
session, and check `chrome://webrtc-internals` on either side — the selected candidate pair's
type tells you what actually happened (`host` = direct, `srflx` = STUN-discovered direct path,
`relay` = went through this TURN server). Confirm audio/video still comes through when forced onto
the relay path (e.g. by testing from a network you know is restrictive).

## Revisiting credentials later

The static `user=` line above is a permanent shared secret baked into a config file and shipped
verbatim to every browser via `ice_servers` — anyone with devtools access to a participant or
wizard page can read it. Fine for piloting; before this carries real study data at real scale,
switch to coturn's REST API credential mechanism instead: `use-auth-secret` +
`static-auth-secret=<server-only secret>` in `turnserver.conf`, and have
`StudyMasterExecutor.handleAuth`/`handleProducerHello` mint a fresh
`username:expiry` + HMAC-SHA1(secret, username) credential pair per connecting browser (the
[TURN REST API spec](https://datatracker.ietf.org/doc/html/draft-uberti-behave-turn-rest-00) both
coturn and every major client library implement) instead of reading a static value from config.
That's a real but small code change — a few lines in each handshake handler, no protocol change
on the browser side at all, since `ice_servers` already accepts a `username`/`credential` pair
per entry regardless of how they were generated.
