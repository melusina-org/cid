# hier_setup.sh -- Setup directories

for directory in lib bin var share man var/src var/distfiles var/log var/db var/run; do
    install -d -m 755 "/opt/local/${directory}";
    install -d -m 755 "/opt/cid/${directory}";
done

sed -i -e '/ENV_SUPATH\|ENV_PATH/s@PATH=@PATH=/opt/cid/bin:/opt/local/bin:@' /etc/login.defs
