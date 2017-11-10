env DEBIAN_FRONTEND=noninteractive apt-get install -y\
 postgresql

install -d -o postgres -g postgres -m 750\
 /var/run/postgresql/9.6-main.pg_stat_tmp
