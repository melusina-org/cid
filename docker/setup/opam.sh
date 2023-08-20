# opam.sh — Setup OPAM

# El Cid (https://github.com/melusina-org/cid)
# This file is part of El Cid.
#
# Copyright © 2015–2023 Michaël Le Barbier
# All rights reserved.

# This file must be used under the terms of the MIT License.
# This source file is licensed as described in the file LICENSE, which
# you should have received as part of this distribution. The terms
# are also available at https://opensource.org/licenses/MIT

#
# OPAM
#

# Parameters:
#
#  opam_src:
#   The path where sources are stored
#
#  opam_root:
#   The path where OPAM system is installed
#
#  opam_switch:
#   The opam switch to work with
#
#  opam_user:
#   The UNIX user owning OPAM files
#
#  opam_group:
#   The UNIX group owning OPAM files
#
#
# Functions:
#
#  opam_init
#   Init the opam system
#
#  opam_pin PACKAGE
#    Add a pin for the given package

: ${opam_src:=/opt/cid/var/src}
: ${opam_root:=/opt/opam}
: ${opam_switch:=4.04.0}
: ${opam_user:root}
: ${opam_group:root}

# opam_init
#  Initialise OPAM

opam_init()
{
    env DEBIAN_FRONTEND=noninteractive apt-get install -y\
        aspcud\
        bmake\
        curl\
        gawk\
        git\
        m4\
        ocaml\
        opam\
        pkg-config

    install -d -o "${opam_user}" -g "${opam_group}" "${opam_root}"
    su "${opam_user}" -l -c "opam init --root ${opam_root} --no-setup"
    su "${opam_user}" -l -c "opam switch --root ${opam_root} ${opam_switch}"
}

opam_init

# End of file `opam.sh'
