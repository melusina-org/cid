env DEBIAN_FRONTEND=noninteractive apt-get install -y\
 sudo

useradd --system\
        --create-home\
        --home-dir /home/cid\
        --comment 'Continuous Integration and Deployment Suite'\
        cid

sed -i -e '
/Defaults[[:space:]]*env_reset/a\
Defaults        env_keep += "BLOCKSIZE"\
Defaults        env_keep += "COLORFGBG COLORTERM"\
Defaults        env_keep += "CHARSET LANG LANGUAGE LC_ALL LC_COLLATE LC_CTYPE"\
Defaults        env_keep += "LC_MESSAGES LC_MONETARY LC_NUMERIC LC_TIME"\
Defaults        env_keep += "LINES COLUMNS"\
Defaults        env_keep += "LSCOLORS"\
Defaults        env_keep += "SSH_AUTH_SOCK"\
Defaults        env_keep += "TZ"\
Defaults        env_keep += "DISPLAY XAUTHORIZATION XAUTHORITY"\
Defaults        env_keep += "EDITOR VISUAL"\
Defaults        env_keep += "HOME MAIL"\
Defaults        env_keep += "MAKEFLAGS"\
Defaults        env_keep += "MAKEOBJDIRPREFIX"\
Defaults        env_keep += "MAKEOBJDIR"

/User privilege specification/a\
cid     ALL=(ALL) NOPASSWD: ALL\
git     ALL=(www-data) NOPASSWD: /usr/bin/trac-admin
' /etc/sudoers

useradd --system\
        --create-home\
        --home-dir /var/git\
        --comment 'Version Control System'\
        --shell /usr/sbin/nologin\
        git

useradd --create-home\
        --system\
        --home-dir /var/lib/jenkins\
        --comment 'Automation Server'\
        --shell /usr/sbin/nologin\
        jenkins

usermod -a -G git jenkins
usermod -a -G git www-data
