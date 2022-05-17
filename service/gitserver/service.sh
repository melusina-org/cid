# service.sh — Service Definitions for gitserver

# El Cid (https://github.com/melusina-conseil/cid)
# This file is part of El Cid.
#
# Copyright © 2017–2022 Michaël Le Barbier
# All rights reserved.

# This file must be used under the terms of the MIT License.
# This source file is licensed as described in the file LICENSE, which
# you should have received as part of this distribution. The terms
# are also available at https://opensource.org/licenses/MIT

: ${gitdir:=/var/git}

# gitserver_volume_database
#  List data volumes for git environments

gitserver_volume_database()
{
    cat <<'EOF'
gitserver_data|/var/git
EOF
}

gitserver_list_repositories()
{
    find "${gitdir}" -mindepth 2 -maxdepth 2 -type d -name '*.git'\
	| sed -e 's|^/var/git/||'
}

gitserver_add_key_stdin()
{
    local key
    if [ ! -d "${gitdir}/.ssh" ]; then
	install -d -o git -g git -m 700 "${gitdir}/.ssh"
    fi

    if [ ! -f "${gitdir}/.ssh/authorized_keys" ]; then
	install -o git -g git -m 600 /dev/null "${gitdir}/.ssh/authorized_keys"
    fi

    cat >> "${gitdir}/.ssh/authorized_keys"
}

gitserver_create_repository()
{
    local repository
    case "$#-$1-$2" in
	2-*-*.git)
	    repository="${gitdir}/$1/$2"
	    ;;
	2-*-*)
	    repository="${gitdir}/$1/$2.git"
	    ;;
	1-*/*.git-)
	    repository="${gitdir}/$1/"
	    ;;
	*)
	    failwith 'gitserver_create_repository'
    esac

    install -d -o git -g git -m 750 "${repository}"
    su -l git -s /bin/sh -c "git init --bare ${repository}"

    (
	cd "${repository}/hooks"
	rm -f post-receive
	ln -s /opt/cid/libexec/cid/cid_githook_postreceive post-receive
    )
}

gitserver_delete_repository()
{
    local repository
    case "$#-$1-$2" in
	2-*-*.git)
	    repository="${gitdir}/$1/$2"
	    ;;
	2-*-*)
	    repository="${gitdir}/$1/$2.git"
	    ;;
	1-*/*.git-)
	    repository="${gitdir}/$1/"
	    ;;
	*)
	    failwith 'gitserver_create_repository'
    esac

    rm -rf "${repository}"
}

# gitserver_dump
#  Dump git repositories to dumpdir

gitserver_dump()
{
    install -d -o git -g git "${CID_NEXT_DUMPDIR}/git"

    wlog 'Info' 'gitserver: Dump git repositories.'
    (
	cd "${gitdir}" && find '.' | cpio -dump "${CID_NEXT_DUMPDIR}/git"
    ) 2>&1
}

# gitserver_restore DUMPFILE
#  Restore git repositories

gitserver_restore()
{
    wlog 'Info' 'gitserver: Restore git repositories.'
    tar xJfC "$1" "${gitdir}" --strip-components 2 './git/'
}

# End of file `service.sh'
