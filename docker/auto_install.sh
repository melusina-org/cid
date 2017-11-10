### auto_install.sh

opam_root='/opt/opam'

auto_spec()
{
    printf '%s\n' "$@" | auto_spec__filter
}

auto_spec__filter()
{
    sed -e '
/^opam:/{
 s/:/|/
 s/^[^@]*$/&@REPOSITORY/g
 s/@/|/
}

/^bsdowl:/{
 s/:/|/
}

/^npm:/{
 s/:/|/
}

/^pip:/{
 s/:/|/
}
'
}

auto_install()
{
    local method restarg
    while IFS='|' read method restarg; do
        (IFS='|'; set --  ${restarg}; auto_install__${method} "$@")
    done
}

auto_install__autoconf()
{
    (cd "$1" || exit 1
     if [ -r './configure.ac' ] && ! [ -r './configure' ]; then
         autoconf
     fi)
}

auto_install__opam()
{
    case "$2" in
        REPOSITORY) opam install "$1";;
        *) (auto_install__opam_pin "$1" "$2");;
    esac
}

auto_install__opam_pin()
{
    local devrepo

    devrepo=$(opam info --field dev-repo "$1")
    git clone --depth 1 --single-branch --branch "$2"\
        "${devrepo}" "/opt/quintly/var/sources/$1"

    auto_install__autoconf "/opt/quintly/var/sources/$1"
    opam pin --yes add "$1" "/opt/quintly/var/sources/$1"
}


auto_install__bsdowl()
{
    auto_install__autoconf "/opt/quintly/var/sources/$1"
    (cd "/opt/quintly/var/sources/$1"
     if [ -x './configure' ]; then
         eval './configure' --prefix '/opt/quintly' ${auto_install_configure_argv}
     fi) || exit 1

    (cd "/opt/quintly/var/sources/$1"
     bmake -I /usr/share/bsdowl all
     bmake -I /usr/share/bsdowl install) || exit 1
}

auto_install__npm()
{
    (cd "$1" && npm install)
}

auto_install__pip()
{
    pip install "$1"
}

auto_install__init()
{
    opam init --compiler 4.02.3
}

if command which opam > /dev/null; then
    OPAMROOT="${opam_root}"
    export OPAMROOT
    eval $(opam config env)
fi

auto_install_init='no'
auto_install_configure_argv=''

while getopts 'iC:' OPTION; do
    case "${OPTION}" in
        i)	auto_install_init='yes';;
        C)	auto_install_configure_argv="${OPTARG}";;
        ?)	exit 64;;
    esac
done
shift $(expr ${OPTIND} - 1)

if [ "${auto_install_init}" = 'yes' ]; then
    auto_install__init
fi

if [ $# -lt 1 ]; then exit 0; fi
auto_spec "$@" | auto_install
