---
title: Unattended Installation
toc: true
menu: Unattended
---

Bitrock framework allow unattended installation. With this mode, the
installer will not prompt the user for any information and will
instead use the default settings configured for each of the
parameters. That way, no input is required from the user during the
installation. It's also possible to define the values through command
line or using a configuration file

```sh
./sample-installer.bin --mode unattended --param1 value1 --param2 value2 ....
./sample-installer.bin --mode unattended --optionfile optionfile
```

Where optionfile should contain a list of pairs key=value. For instance:

```
param1=value1
param2=value2
```

# Available parameters for ejabberd installer

Installer recognizes those options:

**`--admin <username>`**:  sets user name used that should have admin access

**`--adminpw <password>`**:  password used by admin user

**`--ejabberddomain <domain>`**:  domain that should be used XMPP server

**`--installdir <path>`**:  installation directory

**`--cluster <0|1>`**:  setting this to 1 would setup this installation as a part of
   cluster (by default it's value is assumed 0)

**`--hostname <name>`**:  node id used by ejabberd to use as part of erlang node id (has
   only be set when using clustered environment
