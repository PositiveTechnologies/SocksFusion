# SocksFusion

SocksFusion is a utility used to forward TCP messages via an encrypted tunnel through a firewall which client applications see as a Socks5 server. SocksFusion contains two components: **Proxy** which is installed on our side and provides agent identification and **Agent** which is a redistributable application connecting to Proxy.

Outline of network connections:

                                     NAT
    +--------+         +-------+     | |     +-------+         +--------+
    | Target | <------ | Agent | ----|-|---> | Proxy | <------ | Client |
    +--------+         +-------+     | |     +-------+         +--------+


## Agent

Agent connects to Proxy at proxy.bbs.ptsecurity.com:443 and attempts to identify itself using the specified key. If it succeeds, Agent attempts to reply to forwarded client packets and establish connections with a target requested according to the Socks5 protocol.

### Debug messages

During its operation, Agent can write the following timestamped debug messages to stdout:
* `Connecting to server...` - Agent is attempting to connect to Proxy.
* `Connection established` - Agent has established a TLS connection to Proxy.
* `This agent is not registered` - The Agent key is not registered in the system. This can happen if you download and verify a new copy of Agent.
* `Another agent already connected` - Another agent is using the same key. Agent tries to reconnect using the same key every 5 seconds.
* `Scan is started` - A scan request has been received. Agent is working as a Socks5 server.
* `Running` - A periodic message from Proxy. The connection is working properly.
* `Target is unreachable: <address>` - Failed to connect to the specified address. If this message appears again, something may be blocking outbound connections, or it may indicate network problems.

## Proxy

Proxy serves as a proxy server for inbound connections from agents and clients (scanner instances). Proxy establishes an encrypted TLS connection to agents and uses it to forward messages from clients during a scan.
