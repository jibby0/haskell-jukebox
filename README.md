# haskell-jukebox

**A work in progress.**

`haskell-jukebox` is a rewrite of [lan-jukebox](https://github.com/Josh1147582/lan-jukebox) in Haskell. I was unhappy with how `express` handled things like sockets through `socket.io`, so I'm playing around with a Haskell implementation.

As of now, the plan is to control an instance of `mpv` via JSON-RPC, and expose a web instance through `scotty` for control.
