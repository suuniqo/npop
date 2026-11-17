# nano-pop

A small but robust **POP3 server** written in Haskell, with Maildir-based storage and virtual users.

## Table of Contents
- [Features](#features)
- [How to use](#how-to-use)
- [Installation](#installation)
- [Testing](#testing)
- [Limitations](#limitations)

## Features

### Protocol compliance

Implements the core of [RFC 1939](https://datatracker.ietf.org/doc/html/rfc1939): `USER`, `PASS`, `STAT`, `LIST`, `UIDL`, `RETR`, `DELE`, `RSET`, `NOOP` and `QUIT`. It has been tested against real clients such as `fetchmail` and `curl`.

### Maildir storage with virtual users

Messages are stored in a modified [Maildir](https://cr.yp.to/proto/maildir.html) layout, grouped under a single root directory instead of system home directories. A lock file guarantees there is never more than one simultaneous session per mailbox.

```
npop/
├── alice/
│   ├── cur/
│   └── new/
├── bob/
│   ├── cur/
│   └── new/
└── shadow
```

### Informative logging

Every log line includes the responsible thread, and errors carry the failing operation:

```
npop: ThreadId 1: fail: failed to build shadow: /var/npop/shadow: openFile: does not exist (No such file or directory)
```

## How to use

### Running the server

```bash
sudo npop
```

The server listens on the POP3 port (110) by default and starts accepting connections immediately. It can be stopped with `Ctrl-C`. Root privileges are needed to bind port 110 and to read the `shadow` file.

For long-running deployments, using a process manager such as `systemd` is recommended.

### Configuration

Configuration is optional. The server looks for `$XDG_CONFIG_HOME/npop/config.toml`, falling back to `$HOME/.config/npop/config.toml`. If neither exists, defaults are used. The options and their defaults are:

```toml
[network]
port           = "pop3"         # port or service to listen on
idle_timeout   = 600000000      # max client inactivity allowed
listen_backlog = 16             # max pending connections in listen
backoff_min    = 62500          # min delay for exponential backoff
backoff_max    = 16000000       # max delay for exponential backoff
read_chunk     = 4096           # bytes read per recv

[storage]
mail_root      = "/var/npop"    # root directory for mailboxes
lock_name      = ".lock"        # lock file name for mailboxes
pass_name      = "shadow"       # file containing user credentials
```

### Managing users and mail

The `npop-tools` helper script handles users and messages:

| Command | Description |
|---|---|
| `sudo npop-tools add <user>` | Create the user's `shadow` entry and Maildir directories |
| `sudo npop-tools del <user>` | Remove the user's `shadow` entry and Maildir directories |
| `npop-tools put <user>` | Deliver a message to the user's mailbox (end input with `Ctrl-D`) |


For example:
```bash
$ sudo npop-tools add alice
password:
npop-tools: added user alice

$ npop-tools put alice
message: hey alice!^D
npop-tools: sent message to alice
```

### Connecting a client

You can talk to the server manually with `telnet` or `netcat`:

```
$ telnet localhost pop3
+OK POP3 server ready
user bob
+OK
pass admin
+OK bob's maildrop has 3 messages (994 octets)
stat
+OK 3 994
quit
+OK POP3 server signing off (3 messages left)
```

Or use any regular mail client, e.g. `fetchmail -v -p POP3 -P 110 -u bob` or `curl pop3://localhost:110/1 --user "bob:admin"`.

## Installation

### Requirements

- [GHC](https://www.haskell.org/ghc/) and [cabal](https://www.haskell.org/cabal/) (easiest via [GHCup](https://www.haskell.org/ghcup/))
- A Unix system (Linux, macOS, or WSL on Windows), since the server relies on POSIX syscalls

### Build from source

```bash
git clone https://github.com/suuniqo/npop.git
cd npop
cabal build
cabal install
```

This installs the `npop` and `npop-tools` executables. If they aren't found afterwards, add cabal's bin directory to your `PATH`:

```bash
export PATH="$HOME/.cabal/bin:$PATH"
```

## Testing

```bash
cabal run npop-tests
```

Runs property-based fuzz tests on the command parser ([Hedgehog](https://hackage.haskell.org/package/hedgehog)) and unit tests on the storage module ([HSpec](https://hackage.haskell.org/package/hspec)).

## Limitations

- **No TLS:** credentials are sent in plaintext, so don't expose the server on untrusted networks.
- **No abuse protection:** there are no limits on failed logins or invalid queries, so it's susceptible to DoS.
