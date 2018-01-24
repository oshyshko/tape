# tape
```
`tape` records and replays stdout/stderr produced by a command.

Usage:
  tape record  [-f=file] -- command [args]
  tape replay  [-f=file]
  tape inspect [-f=file] [-d]

Flags:
  -f --file=file  Use a file instead of stdin/stdout
  -d --delays     Reproduce delays (off by default)

Examples:
  tape record -- traceroute 8.8.8.8 > log.rec
  cat log.rec | tape replay -d
  cat log.rec | tape inspect
```

## Example
```
$ tape record -f log.rec -- ping 8.8.8.8
<wait few seconds>
^C

$ tape replay -f log.rec
PING 8.8.8.8 (8.8.8.8): 56 data bytes
64 bytes from 8.8.8.8: icmp_seq=0 ttl=46 time=13.834 ms
64 bytes from 8.8.8.8: icmp_seq=1 ttl=46 time=14.210 ms
64 bytes from 8.8.8.8: icmp_seq=2 ttl=46 time=15.961 ms
64 bytes from 8.8.8.8: icmp_seq=3 ttl=46 time=13.988 ms
64 bytes from 8.8.8.8: icmp_seq=4 ttl=46 time=14.163 ms
64 bytes from 8.8.8.8: icmp_seq=5 ttl=46 time=14.142 ms

--- 8.8.8.8 ping statistics ---
6 packets transmitted, 6 packets received, 0.0% packet loss
round-trip min/avg/max/stddev = 13.834/14.383/15.961/0.717 ms

$ tape inspect -f log.rec -d
1516736561054 -- Command "/home/john/" ["ping","8.8.8.8"]
1516736561070 -- Out <94>
1516736562075 -- Out <56>
1516736563079 -- Out <56>
1516736564078 -- Out <56>
1516736565083 -- Out <56>
1516736566084 -- Out <56>
1516736566577 -- Signal 2
1516736566577 -- Out <155>
1516736566578 -- Close Out
1516736566578 -- Close Err
1516736566578 -- Exit 0
```

## Building

```
$ stack install
```

## License

Copyright © 2018 Oleksandr Shyshko

The use and distribution terms for this software are covered by the
Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
which can be found in the file LICENSE at the root of this distribution.

By using this software in any fashion, you are agreeing to be bound by the terms of this license.
You must not remove this notice, or any other, from this software.
