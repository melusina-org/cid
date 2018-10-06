### config.sh -- Read Application configuration

: ${config_file:=/dev/null}

# config CONFIGURATION-KEY
# config CONFIGURATION-KEY CONFIGURATION-VALUE
#  Get and set configuration values
#
# The configuration values are read from the file `config_file`

config()
{
    git config -f "${config_file}" "$@"
}


# config_db
#  Print the list of all configuration mappings
#
# These configuration mappings are delimited using the `|` character.

config_db()
{
    git config -f "${config_file}" --list | sed -e 's/=/|/'
}

### End of file `config.sh'
