# wslogi
A websocket logging framework for Erlang/OTP

## Overview
Wslogi is a websocket logging framework for Erlang.

- Multiple log level
 - The websocket client can specify the log level
- You can specify the output path of the log
 - The websocket client can specify the path at the time of connection
 - Parent path also can get the log of the child path
- The websocket client can filter the log

## Usage
- [Sample](examples/wslogi_example)
- [Documents](doc)
- [Websocket client command](priv/help.md)

## Websocket Client
It may use what you like. I'm using the this.

- [progrium/wssh](https://github.com/progrium/wssh)

## Contribute
Pull request is welcome =D

## License

[MIT License](LICENSE)
