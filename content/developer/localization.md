
# Internationalization and Localization

The source code of `ejabberd` supports localization. The translators can
edit the [`gettext`][1] .po files
using any capable program (KBabel, Lokalize, Poedit...) or a simple text
editor.

Then gettext is used to extract, update and export those .po files to
the .msg format read by `ejabberd`. To perform those management tasks,
in the `src/` directory execute `make translations`. The translatable
strings are extracted from source code to generate the file
`ejabberd.pot`. This file is merged with each .po file to produce
updated .po files. Finally those .po files are exported to .msg files,
that have a format easily readable by `ejabberd`.

All built-in modules support the `xml:lang` attribute inside IQ queries.
Figure [fig:discorus], for example, shows the reply to the following
query:

	#!xml
	<iq id='5'
	    to='example.org'
	    type='get'
	    xml:lang='ru'>
	  <query xmlns='http://jabber.org/protocol/disco#items'/>
	</iq>

The Web Admin also supports the `Accept-Language` HTTP header.

[1]:	http://www.gnu.org/software/gettext/