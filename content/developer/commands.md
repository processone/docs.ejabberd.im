---
title: Global commands interface | ejabberd documentation
---

# ejabberd commands

By defining command using api available through `ejabberd_commands`
module, it's possible to add operations that would be available to
users through `ejabberdctl` command, XML-RPC socket or JSON based REST
service.

Each command needs to provide informations about required arguments
and produced result by filling `#ejabberd_commands` record and
registering it in dispatcher by calling
`ejabberd_commands:register_commands([ListOfEjabberdCommandsRecords])`.

## Structure of `#ejabberd_commands` record

## Writing ejabberd commands supporting OAuth

If you have existing commands that you want to make OAuth compliant,
you can make them OAuth compliant very easily.

An ejabberd command is defined by an `#ejabberd_commands` Erlang
record. The record requires a few fields:

- **name**: This is an atom defining the name of the command.
- **tags**: This is a list of atoms used to group the command into
  consistent group of commands. This is mostly used to group commands
  in `ejabberdctl` command-line tool. Existing categories are:

    - `session`: For commands related to user XMPP sessions.
    - `roster`: Commands related to contact list management.
    - TODO: List other tags that we already use.
- **desc**: Description of the command for online help.
- **module** and **function**: Module and function to call to execute
  the command logic.
- **args**: Argument of the command. An argument is defined by a tuple
  of atoms of the form `{argument_name, data_type}`. `data_type` can be
  one of:

    - binary
    - TODO other types
- **result**: defines what the command will return.
- **policy**: Is an optional field, containing an atom that define
  restriction policy of the command. It can be on of: `open`, `admin`,
  `user`, `restricted`. Default is `restricted`, meaning the command
  can be used from ejabberdctl command-line tool.

<!-- TODO explain what the result field should look likes -->

To define a command that can be used by server user over ReST or
XML-RPC API, you just have to define it with policy `user`. Then, you
have to make sure that the function will take a user binary and a host
binary as first parameter of the function. They do not have to be put
in the `args` list in `#ejabberd_commands` record as the `user policy
implicitely expect them.

That's all you need to have commands that can be used in a variety of
ways.

Here is a example way to register commands when

```
start(_Host, _Opts) ->
    ejabberd_commands:register_commands(commands()).

stop(_Host) ->
    ejabberd_commands:unregister_commands(commands()).

%%%
%%% Register commands
%%%

commands() ->
    [#ejabberd_commands{name = user_get_roster,
                        tags = [roster],
                        desc = "Retrieve the roster",
                        longdesc =
                            "Returns a list of the contacts in a "
                            "user roster.\n\nAlso returns the state "
                            "of the contact subscription. Subscription "
                            "can be either  \"none\", \"from\", \"to\", "
                            "\"both\". Pending can be \"in\", \"out\" "
                            "or \"none\".",
                        module = ?MODULE, function = get_roster,
                        args = [],
                        policy = user,
                        result =
                            {contacts,
                             {list,
                              {contact,
                               {tuple,
                                [{jid, string},
                                 {groups, {list, {group, string}}},
                                 {nick, string}, {subscription, string},
                                 {pending, string}]}}}}}
        ].
```
