# wslogi
A websocket logging framework for Erlang/OTP

## Websocket Client

[progrium/wssh](https://github.com/progrium/wssh)

1. install libevent

```bash
$ sudo apt-get install libevent-dev
```

2. install [setuptools](https://pypi.python.org/pypi/setuptools)

```bash
$ wget https://bootstrap.pypa.io/ez_setup.py -O - | sudo python
```

3. install [progrium/wssh](https://github.com/progrium/wssh)

```bash
$ git clone git://github.com/progrium/wssh.git
$ cd wssh
$ python setup.py install
```

## Usage

```bash
$ wssh localhost:8080
```

## License

[MIT License](LICENSE)