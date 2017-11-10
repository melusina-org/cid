# wlog PRINTF-LIKE-ARGV
#  Same as printf with output on STDERR
#
# A newline is automatically added to the output.

wlog_level='Info'

wlog__numeric_level()
{
    local level
    case "$1" in
        Emergency)	level=0;;
        Alert)		level=1;;
        Critical)	level=2;;
        Error)		level=3;;
        Warning)	level=4;;
        Notice)		level=5;;
        Info)		level=6;;
        Debug)		level=7;;
    esac
    printf '%s' "${level}"
}

wlog__is_interesting()
{
    [ $(wlog__numeric_level ${wlog_level}) -le $(wlog__numeric_level $1) ]
}

wlog()
{
    local level
    level="$1"
    shift

    if wlog__is_interesting "${level}"; then
        {
            printf '%s: ' "${level}"
            printf "$@"
            printf '\n'
        } 1>&2
    fi
}


# logfile FILENAME
#  Redirect stdin and stderr to FILENAME unless bound to a tty

logfile()
{
    if ! [ -t 1 ]; then
        exec 1>>"$1" 2>&1
    fi
}


# failwith [-x STATUS] PRINTF-LIKE-ARGV
#  Fail with the given diagnostic message
#
# The -x flag can be used to convey a custom exit status, instead of
# the value 1.  A newline is automatically added to the output.

failwith()
{
    local OPTIND OPTION OPTARG status

    status=1
    OPTIND=1

    while getopts 'x:' OPTION; do
        case ${OPTION} in
            x)	status="${OPTARG}";;
            *)	eprintf 'failwith: %s: Unsupported option.' "${OPTION}";;
        esac
    done

    shift $(expr ${OPTIND} - 1)
    {
        printf 'Failure: '
        printf "$@"
        printf '\n'
    } 1>&2
    exit "${status}"
}


# tmpdir_initializer
#  Create a temporary directory
#
# The path to that directory is saved in tmpdir. A hook is registered
# to remove that directory upon program termination.

tmpdir_initializer()
{
    tmpdir=$(mktemp -d -t "${PACKAGE}-XXXXXX")
    trap 'rm -r -f "${tmpdir:?}"' INT TERM EXIT
    export tmpdir
}

# tmpfile_initializer TMPFILE
#  Create a temporary file
#
# The path to that file is saved in TMPFILE. A hook is registered
# to remove that file upon program termination.

tmpfile_initializer()
{
    local _tmpfile _script
    _tmpfile=$(mktemp -t "${PACKAGE}-XXXXXX")
    _script=$(printf 'rm -f "%s"' "${_tmpfile}")
    trap "${_script}" INT TERM EXIT
    eval $1="${_tmpfile}"
    export $1
}
