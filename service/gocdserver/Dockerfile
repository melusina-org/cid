# Dockerfile -- Continuous Delivery Server

# El Cid (https://github.com/melusina-conseil/cid)
# This file is part of El Cid.
#
# Copyright © 2017–2022 Michaël Le Barbier
# All rights reserved.

# This file must be used under the terms of the MIT License.
# This source file is licensed as described in the file LICENSE, which
# you should have received as part of this distribution. The terms
# are also available at https://opensource.org/licenses/MIT

# Variables:
#
# GO_VERSION
#  The GoCD Version we use.
#
#  Possible values: v19.2.0 v20.1.0

ARG GO_VERSION=v21.4.0
FROM gocd/gocd-server:${GO_VERSION}

RUN mkdir -p /godata/plugins/external

# End of file `Dockerfile'
