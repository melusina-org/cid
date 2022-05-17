# service.sh — Functions for services

# El Cid (https://github.com/melusina-conseil/cid)
# This file is part of El Cid.
#
# Copyright © 2017–2022 Michaël Le Barbier
# All rights reserved.

# This file must be used under the terms of the MIT License.
# This source file is licensed as described in the file LICENSE, which
# you should have received as part of this distribution. The terms
# are also available at https://opensource.org/licenses/MIT

# service_list
#  List services
#
# The output has one column:
#
#   SERVICE-DESIGNATOR

service_list()
{
    if [ -z "${servicedir}" ]; then
	:
    else
	find "${servicedir}/." -maxdepth 1 -type d\
	    | sed -n -e '
s|.*/||
/^[.]$/b
p
'\
	    | sort -u
    fi
}

# service_stub SERVICE
#  Prepare baseclass functions associated with SERVICE

service_stub()
{
    local service_name service_stub
    service_name="$1"
    service_stub=$(mktemp -t 'stub-XXXXXX')

    cat > "${service_stub}" <<SERVICE_STUB
${service_name}_name ()
{
    printf '%s' "${service_name}"
}

${service_name}_volume_database ()
{
    :
}

${service_name}_dump()
{
    :
}

${service_name}_restore()
{
    :
}

${service_name}_configure()
{
    :
}

${service_name}_wizard()
{
    :
}
SERVICE_STUB

    . "${service_stub}"
    rm -f "${service_stub}"
}


# service_do PROCEDURE [ SERVICE ]
#  Do PROCEDURE on SERVICE or on all services

service_do()
{
    local service
    local procedure

    procedure="$1"
    shift

    if [ $# -eq 0 ]; then
	set -- $(service_list)
    fi

    for service in "$@"; do	
	"${procedure}" "${service}"
    done
}


# service_load [ SERVICE ]
#  Load special functions bound to SERVICE.
#
# If no SERVICE is speficied then all services are loaded.

service_load()
{
    service_do service_load1 "$@"
}

service_load1()
{
    service_stub "$1"
    if [ -r "${servicedir}/$1/service.sh" ]; then
	. "${servicedir}/$1/service.sh"
    elif [ -r "${servicedir}/$1.sh" ]; then
	. "${servicedir}/$1.sh"
    fi
}

# service_volume_database [ TENANT ]
#  The database of service volumes
#
# It has the following columns
#
#  VOLUME | ADMIN-MOUNT-POINT | SERVICE

service_volume_database()
{
    service_list | while read service; do
	eval "${service}_volume_database" | {
	    case $# in
		0)
		    sed -e "s/\$/|${service}/"
		    ;;
		1)
		    sed -e "s/^/$1./;s/\$/|${service}/"
		    ;;
	    esac
	}
    done | sort -u
}

# service_volume_list TENANT
#  The list of service volumes for the given tenant

service_volume_list()
{
    service_volume_database | awk -F '|' -v tenant="$1" '
{
  volume[$1]
}

END {
  for(v in volume) {
    printf("%s.%s\n", tenant, v)
  }
}
' | sort -u
}


# service_volume_docker_args
#  Docker arguments to mount service volumes

service_volume_docker_args()
{
    service_volume_database "$(tenant_name)"\
	| awk -F '|' '{printf(" --volume %s:%s", $1, $2)}'
}



# service_admin_container_mount_point SERVICE VOLUME
#  Derive the the mount point of VOLUME for SERVICE in the admin container

service_admin_container_mount_point()
{
    service_volume_database | awk -F '|' -v service="$1" -v volume="$2" '
$3 == service && $1 == volume {
  printf("/service/%s%s", service, $2)
}
'
}

# service_configure [ SERVICE ]
#  Configure SERVICE or all services

service_configure()
{
    service_do service_configure1 "$@"
}

service_configure1()
{
    $1_configure
}


# service_wizard [ SERVICE ]
#  Wizard SERVICE or all services

service_wizard()
{
    service_do service_wizard1 "$@"
}

service_wizard1()
{
    $1_wizard
}

# End of file `service.sh'
