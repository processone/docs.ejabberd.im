---
title: Internationalization and Localization
menu: Localization
---

The source code of `ejabberd` supports localization:
all built-in modules support the `xml:lang` attribute inside IQ queries,
and the Web Admin supports the `Accept-Language` HTTP header.

There are two ways to improve the translation of a language:

- Edit the corresponding .po file in
[ejabberd-po git repository](https://github.com/processone/ejabberd-po)
with a gettext-compatible
program (Poedit, KBabel, Lokalize, ...).
Then submit a Pull Request.

- Using the
[ejabberd-po Weblate](https://hosted.weblate.org/projects/ejabberd/ejabberd-po/)
online service.

Once the translators have improved the `po` files,
you can run `make translations`.
With that command, the translatable
strings are extracted from source code to generate the file
`ejabberd.pot`. This file is merged with each .po file to produce
updated .po files. Finally those .po files are exported to .msg files,
that have a format easily readable by `ejabberd`.
