# ejabberd Docs Source Code

The [ejabberd](https://www.ejabberd.im/) Community Server
has its source code available in the [ejabberd git repository](https://github.com/processone/ejabberd).
Its documentation is published in the [ejabberd Docs](https://docs.ejabberd.im) website,
and its source code is available in the [docs git repository](https://github.com/processone/docs.ejabberd.im).

This is a community effort and you are welcome to submit issues or pull requests
in order to improve the docs and benefit the ejabberd community.

This documentation site is built using [MkDocs](https://www.mkdocs.org/)
and [Material for MkDocs](https://squidfunk.github.io/mkdocs-material/).

## Installation

To build the site you need Python 3.6 or later, then install the dependencies:

### pip

```bash
mkdir -p .venv
python3 -m venv .venv
source .venv/bin/activate
pip install -r requirements.txt
```

!!! info
    From now on, remember to run `source .venv/bin/activate` before running any `mkdocs [...]` command.

!!! tip
    You can freeze the dependencies to a file using `pip freeze > requirements.txt`.

### Debian

You could install most dependencies using APT:

```bash
apt-get install mkdocs \
                mkdocs-material \
                mkdocs-material-extensions \
                mkdocs-redirects \
                python3-bs4
```

!!! warning
    Unfortunately Debian doesn't package `mkdocs-with-pdf`, so you should remove `with-pdf` plugin from `mkdocs.yml`.

## Building

Now you can start a small webserver that builds the site dynamically:

```bash
mkdocs serve
```

or build the site into static html files in the `site/` directory:

```bash
mkdocs build
```

## Testing

To verify the internal URLs in the site:

```bash
TEST=true mkdocs serve
```

To verify the internal URLs and also the external links:

```bash
TEST=true TEST_EXTERNAL=true mkdocs serve
```

## Updating content

Some pages in this documentation are extracted from a running ejabberd node:

- [admin/configuration/toplevel.md](admin/configuration/toplevel.md)
- [admin/configuration/modules.md](admin/configuration/modules.md)
- [developer/ejabberd-api/admin-api.md](developer/ejabberd-api/admin-api.md)
- [developer/ejabberd-api/admin-tags.md](developer/ejabberd-api/admin-tags.md)

To update those pages, install ejabberd, start it and run `make all` in this repository.
This gets documentation from ejabberd, processes it to obtain markdown content
and moves the files to this repository.

Additionally, there are several other pages that are markdown files copied from
ejabberd git repository and docker-ejabberd git repository. Those repositories
must be available next to docs.ejabberd.im before running `make all`.

## Markdown Shorthands

When editing ejabberd source code to document top-level options, modules or API commands,
there is some additional syntax supported to generate HTML URLs:

For example, this text in the ejabberd source code:

``` erlang
_`mod_muc_admin`_
_`bookmarks_to_pep`_ API
_`default_db`_
_`basic.md#captcha|CAPTCHA`_
https://xmpp.org/extensions/xep-0045.html[XEP-0045]
```

gets converted into this markdown:

``` markdown
[mod_muc_admin](../../admin/configuration/modules.md#mod_muc_admin)
[bookmarks_to_pep](../../developer/ejabberd-api/admin-api.md#bookmarks_to_pep) API
[default_db](toplevel.md#default_db)
[CAPTCHA](basic.md#captcha)
[XEP-0045](https://xmpp.org/extensions/xep-0045.html)
```

There are example usage of those shorthands in ejabberd, for example in `mod_muc.erl`.
