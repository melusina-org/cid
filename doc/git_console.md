# git_console – Operate on git repositories

# Synopsis

~~~ console
# git_console list_repositories
# git_console describe_repository ENVIRONMENT REPOSITORY-NAME
# git_console create_repository ENVIRONMENT REPOSITORY-NAME
# git_console delete_repository ENVIRONMENT REPOSITORY-NAME
# git_console add_key SSH-PUBLIC-KEY-FILE
~~~

# Sample SSH Client Configuration

~~~
Host git.cid.local
 Hostname git.cid.local
 User git
 Port 2022
 StrictHostKeyChecking no
~~~
