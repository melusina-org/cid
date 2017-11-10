# Volumes

- cid_${project}_git: Storage for git repositories.
- cid_${project}_trac: Storage for the trac database.
- cid_${project}_jenkins: Storage for the jenkins data.
- cid_${project}_credential: Storage for project credentials.


# Docker images

- cid_tracdb: Run a PostgreSQL database for trac.
- cid_trac: Run an Apache server with trac.
- cid_jenkins: Run a Jenkins server.
- cid_util: Run cid utilities, e.g. to populate volumes.


# Build configuration

- SSH credentials for Jenkins
- SSH credentials for git
- List of remote repositories
