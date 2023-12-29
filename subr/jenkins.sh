# jenkins.sh — Jenkins interaction support

# El Cid (https://github.com/melusina-org/cid)
# This file is part of El Cid.
#
# Copyright © 2015–2023 Michaël Le Barbier
# All rights reserved.

# This file must be used under the terms of the MIT License.
# This source file is licensed as described in the file LICENSE, which
# you should have received as part of this distribution. The terms
# are also available at https://opensource.org/licenses/MIT

: ${jenkinsdir:=/var/lib/jenkins}

# jenkins_is_enabled
#  Predicate telling if the trac service is enabled

jenkins_is_enabled()
{
    config_service_is_enabled 'jenkins'
}


# jenkins_dump FILENAME
#  Dump Jenkins state

jenkins_dump()
{
    local environment
    install -d -o jenkins -g jenkins "${tmpdir}/jenkins"

    wlog 'Info' 'jenkins: Dump Jenkins state.'
    ( cd "${jenkinsdir}" && find '.' | cpio -dump "${tmpdir}/jenkins" )\
        2>&1
}


# jenkins_restore DUMPFILE
#  Restore Jenkins state

jenkins_restore()
{
    wlog 'Info' 'jenkins: Restore Jenkins state.'
    tar xJfC "$1" "${jenkinsdir}" --strip-components 2 ./jenkins/
}


# jenkins_volume_db CONFIG-PROJECT
#  List data volumes for jenkins environments

jenkins_volume_db()
{
    cat <<EOF
jenkins|cid-$1-jenkins|/var/lib/jenkins
EOF
}


# jenkins_configure
#  This configure the Jenkins service.

jenkins_configure()
{
    :
}


#
# Jenkins Jobs
#


# jenkins_job_ls
#  List defined jenkins jobs

jenkins_job_ls()
{
    find "${jenkinsdir}/jobs" -type f -name 'config.xml'
}


# jenkins_job_export [-i INCLUDE-PATTERN] [-x EXCLUDE-PATTERN]
#  Export Jenkins job definitions
#
# This writes a tarball to stdout, containing all Jenkins job
# definitions.
#
# Options:
# -x EXCLUDE-PATTERN
#    All jobs whose path match the EXCLUDE-PATTERN are excluded from
#    the export.
# -i INCLUDE-PATTERN
#    Only jobs whose path match the INCLUDE-PATTERN are eligible for
#    the export.  The EXCLUDE-PATTERN, if given, is still applied.

jenkins_job_export()
{
    local OPTIND OPTION OPTARG
    local include_pattern exclude_pattern
    OPTIND=1

    include_pattern='*'
    exclude_pattern=''

    while getopts 'i:x:' OPTION; do
        case ${OPTION} in
            i)	include_pattern="${OPTARG}";;
            x)	exclude_pattern="${OPTARG}";;
            *)	failwith 70 'cid_job_export: %s: Unsupported option.' "${OPTION}";;
        esac
    done
    shift $(expr ${OPTIND} - 1)
    wlog 'Info' 'Export Jenkins job definitions.'
    (
        cd "${jenkinsdir}/jobs"
        if [ -z "${exclude_pattern}" ]; then
            find .\
                 -type f\
                 -name 'config.xml'\
                 -path "${include_pattern}"
        else
            find .\
                 -type f\
                 -name 'config.xml'\
                 -path "${include_pattern}"\
                 -not -path "${exclude_pattern}"
        fi | cpio -ov -H ustar
    ) || failwith '%s: Cannot export Jenkins job definitions from this directory.' "${jenkinsdir}"
}


# jenkins_job_import
#  Import Jenkins job definitions
#
# This reads a tarball containing Jenkins job definitions from stdin
# and installs them.

jenkins_job_import()
{
    local script
    wlog 'Info' 'Import Jenkins job definitions.'
    script=$(printf "tar xfC - '%s'" "${jenkinsdir}/jobs")
    su jenkins -l -s /bin/sh -c "${script}"
}

# End of file `jenkins.sh'
