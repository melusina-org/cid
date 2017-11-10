env DEBIAN_FRONTEND=noninteractive apt-get install -y\
 haproxy

install -d /usr/local/var/run/haproxy /usr/local/etc/haproxy
install -o root -g root -m 755 /dev/null /usr/local/bin/entrypoint
cat > /usr/local/bin/entrypoint <<'ENTRYPOINT'
#!/bin/sh
set -e

if [ "${1#-}" != "$1" ]; then
  # first arg is `-F` or `--SOME-OPTION`
  set -- haproxy "$@"
fi

exec "$@"
ENTRYPOINT
