### run.sh -- Build a CID docker project

cid_project='example'
cid_image='cid/jenkins:latest'
cid_home='/var/lib/jenkins'

# cid_docker_run DOCKER-LIKE-ARGV
#  Run a docker program for CID

cid_docker_run()
{
    docker run --rm\
           --network "cid_${cid_project}"\
           --volume "cid_${cid_project}_jenkins:/var/lib/jenkins"\
           --volume "cid_${cid_project}_git:/var/git"\
           --volume "cid_${cid_project}_gpg:${cid_home}/.gnupg"\
           --volume "cid_${cid_project}_ssh:${cid_home}/.ssh"\
           --publish 5000:5000\
           --publish 80:8080\
           "$@"
}

cid_project='local'

cid_docker_run -d --name "cid_${cid_project}_jenkins" "${cid_image}"
