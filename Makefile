
#' definitions
#

EJA = `pwd`/../ejabberd/
DOCKER = `pwd`/../docker-ejabberd/

VERSIONPRE = $(shell git tag --sort=creatordate | tail -n 2 | head -n 1)
VERSION = $(shell git tag --sort=creatordate | tail -n 1)
VERSION- = $(shell git tag --sort=creatordate | tail -n 1 | tr . -)
VERSIONU = $(shell git tag --sort=creatordate | tail -n 1 | tr -d .)
ARCHIVE = archive/$(VERSION)
DEST = content/$(ARCHIVE)
ARCHIVESTRING=" If you are using an old ejabberd release, please refer to the corresponding archived version of this page in the [Archive](../../archive/index.md)."

READMES=content/README-DOCS.md

TTOPLEVEL=temp/toplevel.md
TTOPLEVELO=$(TTOPLEVEL).orig
TOPLEVEL=content/admin/configuration/toplevel.md

TMODULES=temp/modules.md
TMODULES0=$(TMODULES).orig
MODULES=content/admin/configuration/modules.md

TAPI=temp/admin-api.md
TAPI0=$(TAPI).orig
API=content/developer/ejabberd-api/admin-api.md

TTAGS=temp/admin-tags.md
TTAGS0=$(TTAGS).orig
TAGS=content/developer/ejabberd-api/admin-tags.md

PDFNAME=ejabberd-docs-$(VERSION).pdf
PDF=content/ejabberd-docs.pdf
PDFV=content/$(PDFNAME)

ZIPNAME=ejabberd-docs-$(VERSION).zip
ZIP=content/ejabberd-docs.zip
ZIPV=content/$(ZIPNAME)

#.
#' help
#

help:
	@echo ""
	@echo "  [help]   Show this help"
	@echo ""
	@echo "  all      clean + extract + site + pdf + zip (requires ejabberd running)"
	@echo ""
	@echo "  clean    Remove temporary files"
	@echo ""
	@echo "  extract  Extract some documentation from ejabberd (requires ejabberd running)"
	@echo ""
	@echo "  site     Generate site as HTML files"
	@echo "  pdf      Generate PDF file of the offline site"
	@echo "  zip      Generate ZIP file of the offline site"
	@echo ""
	@echo "  serve    Start MkDocs web server"
	@echo "  dev      Start MkDocs web server in development mode"
	@echo ""
	@echo "  test     Test URLs"
	@echo ""
	@echo "  archive  Copy some files and generate Archive page"
	@echo ""

all: clean extract site zip pdf

#.
#' extract
#

extract: temp home livebooks $(READMES) $(TOPLEVEL) $(MODULES) $(API) $(TAGS)

#.
#' copy markdown files
#

home:
	sed -i 's|\(ejabberd-docs-[0-9\.]*.zip\)|$(ZIPNAME)|g' overrides/home.html
	sed -i 's|\(ejabberd-docs-[0-9\.]*.pdf\)|$(PDFNAME)|g' overrides/home.html

livebooks:
	cd content/livebooks/ && for i in $$(ls *.livemd); do cp $$i $${i%.livemd}.md; done
	cd content/livebooks/ && for i in $$(ls *.md); do sed -i '2r markdown_head.txt' $$i ; done
	cd content/livebooks/ && for i in $$(ls *.md); do sed -i "s|NAME|$${i%.md}.livemd|" $$i ; done

$(READMES):
	cp README.md content/README-DOCS.md
	cp $(EJA)/CONTAINER.md content/CONTAINER.md
	cp $(EJA)/CONTRIBUTING.md content/contributing/index.md
	cp $(EJA)/CODE_OF_CONDUCT.md content/contributing/CODE_OF_CONDUCT.md
	cp $(EJA)/CONTRIBUTORS.md content/contributing/CONTRIBUTORS.md
	cp $(EJA)/README.md content/README-GIT.md
	sed -i '1i# Readme\n' content/README-GIT.md
	sed -i 's|(COMPILE.md)|(admin/install/source.md)|g' content/README-GIT.md
	sed -i 's|(COPYING)|(COPYING.md)|g' content/README-GIT.md
	sed -i 's|(CONTRIBUTING.md)|(contributing/index.md)|g' content/README-GIT.md
	cp $(EJA)/CHANGELOG.md content/CHANGELOG.md
	sed -i 's|^\([a-zA-Z0-9_ ]*:\)|#### \1\n|g' content/CHANGELOG.md
	sed -i '1i# ChangeLog\n' content/CHANGELOG.md
	sed -i '1i---\nsearch:\n  boost: -1\n---\n' content/CHANGELOG.md
	cp $(DOCKER)/ecs/README.md content/README-ECS.md
	sed -i 's|# ejabberd Community Server|# `ecs` Container Image|g' content/README-ECS.md
	sed -i 's|HUB-README.md|README-HUB.md|g' content/README-ECS.md
	cp $(DOCKER)/ecs/HUB-README.md content/README-HUB.md

#.
#' get files
#

temp:
	mkdir temp

TTXT=temp/ejabberd.yml.5.txt

$(TTXT):
	ejabberdctl man
	mv $(EJA)/ejabberd.yml.5.txt $(TTXT)

TXML=temp/ejabberd.yml.5.xml

$(TXML): $(TTXT)
	# This prevents suboptions (like acl->ip, ...) to get a TOC entry:
	sed -E 's/\*(.*)\*: (.*):::/- *\1*: \2/g' $(TTXT) >$(TTXT).1
	mv $(TTXT).1 $(TTXT)

	a2x -f docbook $(TTXT)

	# Workaround required since Pandoc 2.8
	# https://github.com/jgm/pandoc/commit/9b5082b086359a63b92bdb40166fa59dea27afe1
	# Alternate workaround would be: https://github.com/jgm/pandoc/issues/6906
	sed -i 's|<warning><simpara>\(.*\)</simpara></warning>|<blockquote><formalpara><title>Warning</title><simpara>\1</simpara></formalpara></blockquote>|g' $(TXML)
	sed -i 's|<note><simpara>\(.*\)</simpara></note>|<blockquote><formalpara><title>Note</title><simpara>\1</simpara></formalpara></blockquote>|g' $(TXML)

temp/man-02 temp/man-03: $(TXML)
	pandoc -f docbook -t markdown_strict+fenced_code_blocks --markdown-headings=setext $(TXML) -o temp/man-tmp1

	# Also required by the Pandoc 2.8 workaround
	sed -i 's|> \*\*\(.*\)\.\*\*|> **\1**|g' temp/man-tmp1

	# Allow to split the man page in sections:
	# for pandoc 2.9.2, or pandoc 3 with --markdown-headings=setext
	sed -E '$!N;s/[A-Z ]+\n===+/===---===/;P;D' temp/man-tmp1 >temp/man-tmp2
	# for pandoc 2.17.1
	#sed -E '$!N;s/^# [A-Z ]+/===---===/;P;D' man-tmp1 >man-tmp2

	csplit temp/man-tmp2 /===---===/ {*} -f temp/man- --suppress-matched

$(TTOPLEVEL): temp/man-02
	mv temp/man-02 $(TTOPLEVEL)

$(TMODULES): temp/man-02
	mv temp/man-03 $(TMODULES)

$(TAPI):
	ejabberdctl gen_markdown_doc_for_commands `pwd`/$(TAPI) "runtime" json

$(TTAGS):
	ejabberdctl gen_markdown_doc_for_tags `pwd`/$(TTAGS)

#.
#' process markdown
#

$(TOPLEVEL): $(TTOPLEVEL)
	mv $(TTOPLEVEL) $(TTOPLEVELO)
	echo "# Top-Level Options" >$(TTOPLEVEL)
	cat $(TTOPLEVELO) >>$(TTOPLEVEL)
	# Add headers to options so they get TOC entries:
	sed -i 's/^\*\*\(.*\)\*\*: \(.*\)/## \1\n\n\2\n/g' $(TTOPLEVEL)
	# Split paragraph of suboptions, it's nicer to read
	sed -i 's/^-   \*\*\(.*\)\*\*: \*\([^*]*\)\* \(.*\)/-   **\1**: *\2*  \n    \3/g' $(TTOPLEVEL)
	# Add disclaimer about Archive page for older ejabberd releases
	sed -i 's|\(This section.*\)\. |> \1. '$(ARCHIVESTRING)' |g' $(TTOPLEVEL)
	# Convert *`mod_something`* into a link to modules section
	sed -i 's|\*`mod_\([a-z_]*\)`\*|[mod_\1](modules.md#mod_\1)|g' $(TTOPLEVEL)
	# Convert *`something`* API into a link to API Reference
	sed -i 's|\*`\([a-z0-9_]*\)`\* API|[\1](/developer/ejabberd-api/admin-api/#\1) API|g' $(TTOPLEVEL)
	# Convert *`something`* into a link to top-level options
	sed -i 's|\*`\([a-z0-9_]*\)`\*|[\1](#\1)|g' $(TTOPLEVEL)
	# Convert note to div HTML elements
	sed -i 's|\*Note\* about this option: \(.*\)\. |<!-- md:version \1 -->\n\n|g' $(TTOPLEVEL)
	# Convert _`url|something`_ into a relative link
	sed -i 's|\*`\(.*\)\|\(.*\)`\*|[\2](\1)|g' $(TTOPLEVEL)
	# Link to Archive when mentioning an ejabberd release 2x.xx
	sed -i 's| \([2-9][0-9]\)\.\([0-9][0-9]\)| [\1.\2](../../archive/\1.\2/index.md)|g' $(TTOPLEVEL)
	# Add search boost:
	sed -i '1i---\nsearch:\n  boost: 1\n---\n' $(TTOPLEVEL)
	cp $(TTOPLEVEL) $(TOPLEVEL)

$(MODULES): $(TMODULES)
	mv $(TMODULES) $(TMODULES0)
	echo "# Modules Options" >$(TMODULES)
	cat $(TMODULES0) >>$(TMODULES)
	# Remove second-level headers so they don't get TOC entries:
	sed -i 's/^### \(.*\)/__\1__/g' $(TMODULES)
	# Prepend a space to list character so they get indented one level:
	# And split paragraph of suboptions, it's nicer to read
	sed -i 's/^-   \*\*\(.*\)\*\*: \*\([^*]*\)\* \(.*\)/    - **\1**: *\2*  \n   \3/g' $(TMODULES)
	# Previous line doesn't process some subptions that span over two lines, workaround:
	sed -i 's/^-   \*\*\(.*\)\*\*\(.*\)/    - **\1**\2/g' $(TMODULES)
	sed -i 's/^-   \*\(.*\)\*\ \(.*\)/    - *\1* \2/g' $(TMODULES)
	# and then add list character to options:
	sed -i 's/^\*\*.*\*\*: /- &/g' $(TMODULES)
	sed -i 's/^\*\*.*\*\*  /- &/g' $(TMODULES)
	# Convert *`mod_something`* into a link to $(TMODULES)
	sed -i 's|\*`mod_\([a-z_]*\)`\*|[mod_\1](#mod_\1)|g' $(TMODULES)
	# Convert *`something`* API into a link to API Reference
	sed -i 's|\*`\([a-z_]*\)`\* API|[\1](../../developer/ejabberd-api/admin-api.md#\1) API|g' $(TMODULES)
	# Convert *`something`* into a link to top-level options
	sed -i 's|\*`\([a-z_]*\)`\*|[\1](toplevel.md#\1)|g' $(TMODULES)
	# Convert _`url|something`_ into a relative link
	sed -i 's|\*`\(.*\)\|\(.*\)`\*|[\2](\1)|g' $(TMODULES)
	# Add disclaimer about Archive page for older ejabberd releases
	sed -i 's|\(This section.*\)\. |> \1. '$(ARCHIVESTRING)' |g' $(TMODULES)
	# Convert note to div HTML elements
	sed -i 's|\*Note\* about this option: \(.*\)\.|<!-- md:version \1 -->\n|g' $(TMODULES)
	# Link to Archive when mentioning an ejabberd release 2x.xx
	sed -i 's| \([2-9][0-9]\)\.\([0-9][0-9]\)| [\1.\2](../../archive/\1.\2/index.md)|g' $(TMODULES)
	# Increase indentation of examples of modules options
	vim $(TMODULES) -c "set expandtab" -c "g/\*\*Example\*\*:\n\n.*\~\~\~ yaml\n.*/normal V}}>" -c "wq"
	vim $(TMODULES) -c "set expandtab" -c "g/\*\*Examples\*\*:\n\n.*\n\n.*\~\~\~ yaml\n.*/normal V}}}>" -c "wq"
	# Add search boost:
	sed -i '1i---\nsearch:\n  boost: 1\n---\n' $(TMODULES)
	cp $(TMODULES) $(MODULES)

$(API): $(TAPI)
	cp $(TAPI) $(TAPI0)
	# Remove three consecutive empty lines after Tags: and Modules:
	sed -i 's/^__Tags:__/__Tags:__\nCLEANLINES/g' $(TAPI)
	sed -i 's/^__Module:__/__Module:__\nCLEANLINES/g' $(TAPI)
	sed -i '/^CLEANLINES/{N;N;N;//d}' $(TAPI)
	# Add disclaimer about Archive page for older ejabberd releases
	sed -i 's|\(This section.*\)\. |> \1. '$(ARCHIVESTRING)' |g' $(TAPI)
	# Convert note to div HTML elements
	sed -i 's|\*Note\* about this command: \(.*\)\.|\n<!-- md:version \1 -->|g' $(TAPI)
	# Link to Archive when mentioning an ejabberd release 2x.xx
	sed -i 's| \([2-9][0-9]\)\.\([0-9][0-9]\)| [\1.\2](../../archive/\1.\2/index.md)|g' $(TAPI)
	# Convert _`mod_something`_ from the user into a link to modules section
	sed -i 's|_`mod_\([a-z0-9_]*\)`_|[mod_\1](../../admin/configuration/modules.md#mod_\1)|g' $(TAPI)
	# Convert _`something`_ API into a link to API Reference
	sed -i 's|_`\([a-z0-9_]*\)`_ API|[\1](#\1) API|g' $(TAPI)
	# Convert _`something`_ into a link to tags section
	sed -i 's|_`\([a-z0-9_]*\)`_|[\1](admin-tags.md#\1)|g' $(TAPI)
	# Convert _`url|something`_ into a relative link
	sed -i 's|_`\(.*\)\|\(.*\)`_|[\2](\1)|g' $(TAPI)
	# Add search boost:
	sed -i '1i---\nsearch:\n  boost: 1\n---\n' $(TAPI)
	cp $(TAPI) $(API)

$(TAGS): $(TTAGS)
	cp $(TTAGS) $(TTAGS0)
	# Link to Archive when mentioning an ejabberd release 2x.xx
	sed -i 's| \([2-9][0-9]\)\.\([0-9][0-9]\)| [\1.\2](../../archive/\1.\2/index.md)|g' $(TTAGS)
	# Convert _`something`_ API into a link to API Reference
	sed -i 's|_`\([a-z0-9_]*\)`_|[\1](admin-api.md#\1)|g' $(TTAGS)
	# Add disclaimer about Archive page for older ejabberd releases
	sed -i 's|\(This section.*\)\. |> \1. '$(ARCHIVESTRING)' |g' $(TTAGS)
	cp $(TTAGS) $(TAGS)

#.
#' mkdocs
#

site:
	OFFLINE=true mkdocs build
	find site/* -type f \! -exec sed -i 's/href="\(.*\)" \(title="ejabberd Docs"\)/href="\1\/index.html" \2/g' {} \;

pdf: $(PDF)

$(PDF):
	WITH_PDF=1 mkdocs build
	mv $(PDF) $(PDFV)

zip: $(ZIP)

$(ZIP): site
	mv site ejabberd-docs-$(VERSION)
	zip -r $(ZIP) ejabberd-docs-$(VERSION)/*
	mv $(ZIP) $(ZIPV)
	mv ejabberd-docs-$(VERSION) temp

dev:
	mkdocs serve --no-livereload --dirty

serve:
	mkdocs serve

clean:
	rm -rf site
	rm -rf temp
	rm -rf content/ejabberd-docs-*
	rm -f $(READMES)
	rm -f $(TOPLEVEL)
	rm -f $(MODULES)
	rm -f $(API)
	rm -f $(TAGS)
	rm -f $(PDF)
	rm -f $(ZIP)

#.
#' archive
#

archive:
	#
	mkdir $(DEST)
	#
	cp content/admin/configuration/listen.md $(DEST)/listen.md
	cp content/admin/configuration/listen-options.md $(DEST)/listen-options.md
	cp content/admin/configuration/modules.md $(DEST)/modules.md
	cp content/admin/configuration/toplevel.md $(DEST)/toplevel.md
	cp content/developer/ejabberd-api/admin-api.md $(DEST)/admin-api.md
	cp content/developer/ejabberd-api/admin-tags.md $(DEST)/admin-tags.md
	#
	sed -i 's|  boost: 1|  exclude: true|' $(DEST)/*.md
	cd $(DEST) && for i in "admin-tags" "listen-options" "listen"; do sed -i '1i---\nsearch:\n  exclude: true\n---\n' $$i.md; done
	#
	sed -i 's|(../../admin/configuration/modules|(modules|g' $(DEST)/*.md
	sed -i 's|(../configuration/|(../../admin/configuration/|g' $(DEST)/*.md
	sed -i 's|(../guide/clustering.md|(../../admin/guide/clustering.md|g' $(DEST)/*.md
	sed -i 's|(../guide/managing.md|(../../admin/guide/managing.md|g' $(DEST)/*.md
	sed -i 's|(../guide/mqtt/index.md|(../../admin/guide/mqtt/index.md|g' $(DEST)/*.md
	sed -i 's|(authentication.md|(../../admin/configuration/authentication.md|g' $(DEST)/*.md
	sed -i 's|(basic.md|(../../admin/configuration/basic.md|g' $(DEST)/*.md
	sed -i 's|(database.md|(../../admin/configuration/database.md|g' $(DEST)/*.md
	sed -i 's|(ldap.md|(../../admin/configuration/ldap.md|g' $(DEST)/*.md
	sed -i 's|(oauth.md|(../../developer/ejabberd-api/oauth.md|g' $(DEST)/*.md
	sed -i 's|(old.md|(../../admin/configuration/old.md|g' $(DEST)/*.md
	#
	sed -i 's|\(RELEASE_LIST -->\)|\1\n* [$(VERSION)]($(VERSION)/index.md)|g' content/archive/index.md
	#
	echo "# Archived Documentation for $(VERSION)" >>$(DEST)/index.md
	echo "" >>$(DEST)/index.md
	echo "This section contains some archived sections for ejabberd $(VERSION)." >>$(DEST)/index.md
	echo "" >>$(DEST)/index.md
	echo "If you are upgrading ejabberd from a previous release, you can check:" >>$(DEST)/index.md
	echo "" >>$(DEST)/index.md
	echo "* [Specific version upgrade notes](upgrade.md)" >>$(DEST)/index.md
	echo "* [ejabberd $(VERSION) release announcement](https://www.process-one.net/blog/ejabberd-$(VERSION-)/)" >>$(DEST)/index.md
	echo "* [Docs Github Compare from $(VERSIONPRE)](https://github.com/processone/docs.ejabberd.im/compare/$(VERSIONPRE)..$(VERSION))" >>$(DEST)/index.md
	echo "* [ejabberd Github Compare from $(VERSIONPRE)](https://github.com/processone/ejabberd/compare/$(VERSIONPRE)..$(VERSION))" >>$(DEST)/index.md
	echo "" >>$(DEST)/index.md
	#

#.
#' test
#

test:
	#linkchecker --no-status http://127.0.0.1:8000/mkdocs/
	export TEST=true && mkdocs build
	#export TEST=true && export TEST_EXTERNAL=true && mkdocs build

#.
#'
# vim: foldmarker=#',#. foldmethod=marker:
