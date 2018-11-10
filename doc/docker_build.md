# docker_build — Build docker image defined in current git working copy

## Synopsis

~~~ console
./tool/docker_build -n linux
./tool/docker_build trac jenkins reverseproxy
~~~

## Description

This utility builds docker images defined in the current git working
copy and tag them appropriately.

~~~
Usage: docker_build [-c CONFIG-DIR] [-n] IMAGE-1 …
 Build docker image defined in current git working copy
Options:
 -h Display this help message.
 -n Build without cache.
~~~
