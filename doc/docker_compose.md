# docker_compose — Start docker compose projects

## Synopsis

~~~ console
./tool/docker_compose -c $(pwd)/project/local up -d
./tool/docker_compose -c $(pwd)/project/local down
~~~

## Description

This utility runs docker-compose using an environment defined in the
current git working copy and the configuration and secrets specified
by the configuration directory.
