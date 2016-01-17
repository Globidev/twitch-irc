Twitch-irc
=========

An extensible IRC relay application for [twitch.tv](http://www.twitch.tv/) chat rooms 


## Motivation

As **streaming** becomes more and more popular, the need for **community centered applications** increases.  
The world's leading video platform and community for gamers delivers over **10 billion messages** a day with its chat service ([source](http://blog.twitch.tv/2015/12/twitch-engineering-an-introduction-and-overview)).  
A lot of existing tools revolve around this IRC-based service and this is why we should ease the process of making such tools.

This project aims to provide a robust base to work with twitch's IRC chat rooms.


## Usage

With docker :whale::
```sh
docker run --rm -it \
    globidocker/twitch-irc channel script
```

### Examples
3 scripts are provided as a proof of extensibility

##### Echo
```sh
twitch-irc day9tv twitch-irc-echo
```

[This simple script](https://github.com/Globidev/twitch-irc/blob/master/scripts%2FEcho%2FMain.hs) will just display IRC messages on the standard output

##### Websocket
```sh
twitch-irc lirik twitch-irc-websocket [port]
```

[This script](https://github.com/Globidev/twitch-irc/blob/master/scripts%2FWebsocket%2FMain.hs) will relay private messages with a websocket server listening on the port `port`
You can subscribe to specific channels using the connection request's path `ws://host:port/channel`

This is the script used by [twitch-cast](https://github.com/best-coloc-ever/twitch-cast) and is available at `ws://datcoloc.com/chat/channel`

##### X11
```sh
twitch-irc your-channel twitch-irc-x11
```

Inspired by the social experiment [Twitch plays Pokémon](https://www.wikiwand.com/en/Twitch_Plays_Pok%C3%A9mon)

[This script](https://github.com/Globidev/twitch-irc/blob/master/scripts%2FX11%2FMain.hs) makes use of the X11 library to forward specific channel messages as keycodes to a X11 window.  
The chat can send game inputs!


## Building

with [Haskell's stack](http://docs.haskellstack.org/en/stable/README.html):

```sh
stack setup
stack build --copy-bins
```
