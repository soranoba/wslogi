
The wslogi commands are:
- filter     Print the log that contain the specific string
    - filter add    <word>
    - filter remove <number>
    - filter show
    - filter ip     [<address>]
- help       Print how to use
- level      Change the log level
    e.g.
    ```
    level 1, 2
    level 1, 2-3, 6
    level +1
    level -1
    ```
- trace      Print stack trace
    - trace on
    - trace off
