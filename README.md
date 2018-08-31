# SocksFusion

**SocksFusion** is a utility for creating an encrypted connection ("tunnel") to allow passing TCP messages through a firewall. **SocksFusion** consists of two components: the **Agent** (freely downloadable application) and the **Proxy** (BlackBox Scanner service). 

Network connection diagram:

                                           +-----------+
                                           | BBS Cloud |
                                           +-----------+
                                                 ^
                                     NAT         |
    +--------+         +-------+     | |     +-------+         +--------+
    | Target | <------ | Agent | ----|-|---> | Proxy | <------ | Client |
    +--------+         +-------+     | |     +-------+         +--------+


## Agent

The **agent** is a client application that forwards requests from the scanner. When launched, it connects with the **proxy**, performs a handshake, and sends its **token**. If authorization is successful, you can start scanning via the agent. The agent listens to the proxy and responds to requests in accordance with Socks5.

### Running the agent

Usage: Agent [-o|--once] [-v|--version] [-s|--show-info]

Available options:
  -p,--proxy host:port     Use indicated proxy server
  -a,--api url             Use indicated API server address
  --token string           Use indicated token
  --token-path path        Use indicated path to read/write token
  -i,--insecure            Keep going if SSL certificate cannot be verified
  -o,--once                Exit when scanning completes or an error occurs
  -d,--debug               Show packet debugging information
  -s,--show-info           Show agent version and build date
  -h,--help                Show this help text

## Proxy

The **proxy** is the server responsible for connecting agents and clients (scanner instances). The **proxy** makes the Socks5 interface available to client applications.

## Token

The **token** is a random string used for authorization of a particular agent. If the agent is unable to read the token from the command line or from file, get a new one from the API server with your activation code. By default, the token is stored in %APP_DATA%/bbs/RemoteAgent.token (Windows) or $HOME/.bbs/RemoteAgent.token (Linux).
