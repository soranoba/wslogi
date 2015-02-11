wslogi_example
==============

Usage:

Terminal A:
```bash
$ pwd
/home/.../wslogi
$ make start
==> wslogi (compile)
Erlang R16B03 (erts-5.10.4) [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:true]

Eshell V5.10.4  (abort with ^G)
1> {ok,[ranch,crypto,cowlib,cowboy,gproc,wslogi]}

1> application:ensure_all_started(wslogi_example).
{ok,[wslogi_example]}
```

Terminal B:
```bash
$ wssh localhost:8080
level 10
start
```

Terminal C:
```bash
$ curl http://localhost:8000
```

Log is output to the terminal B !!
