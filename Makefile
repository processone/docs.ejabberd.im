
EJA = /usr/local/var/lib/ejabberd/

VERSIONPRE = $(shell git tag --sort=creatordate | tail -n 2 | head -n 1)
VERSION = $(shell git tag --sort=creatordate | tail -n 1)
VERSION_ = $(shell git tag --sort=creatordate | tail -n 1 | tr . _)
VERSION- = $(shell git tag --sort=creatordate | tail -n 1 | tr . -)
ARCHIVE = archive/$(VERSION_)
DEST = content/$(ARCHIVE)
ARCHIVESTRING=" If you are using an old ejabberd release, please refer to the corresponding archived version of this page in the [Archive](/archive/)."

all: api man

api:
	ejabberdctl gen_markdown_doc_for_commands `pwd`/admin-api.md "." json
	# Add disclaimer about Archive page for older ejabberd releases
	sed -i 's|\(This section.*\)|> \1'$(ARCHIVESTRING)'|g' admin-api.md
	# Make URL when mentioning an ejabberd release
	sed -i 's| \([0-9][0-9]\)\.\([0-9][0-9]\)| <a href="/archive/\1_\2/">\1.\2</a>|g' admin-api.md
	# Convert tricky absolute URLs into valid relative URLS
	sed -i 's|http://\./\(#.*\)\(\[[_a-z]*\]\)|\2(/developer/ejabberd-api/admin-api/\1)|g' admin-api.md
	mv admin-api.md content/developer/ejabberd-api

test:
	# URLs without trailing slash like this cause broken links: [Basic](basic)
	git grep "\[.*\]([-\.\/a-z0-9]*[a-z])" content/ | grep -v ".png"
	linkchecker --no-status http://localhost:8080/

man:
	ejabberdctl man

	# This prevents suboptions (like acl->ip, ...) to get a TOC entry:
	sed -E 's/\*(.*)\*: (.*):::/- *\1*: \2/g' $(EJA)/ejabberd.yml.5.txt >ejabberd.yml.5.txt

	a2x -f docbook ejabberd.yml.5.txt
	pandoc -f docbook -t markdown_strict ejabberd.yml.5.xml -o man-tmp1.md

	# Allow to split the man page in sections:
	sed -E '$!N;s/[A-Z ]+\n===+/===---===/;P;D' man-tmp1.md >man-tmp2.md
	csplit man-tmp2.md /===---===/ {*} -f man- --suppress-matched

	echo "---\ntitle: Top-Level Options\ntoc: true\nmenu: Top-Level Options\norder: 80\n---" >toplevel.md
	cat man-02 >>toplevel.md
	# Add headers to options so they get TOC entries:
	sed -i 's/^\*\*\(.*\)\*\*: \(.*\)/## \1\n\n\2\n/g' toplevel.md
	# Split paragraph of suboptions, it's nicer to read
	sed -i 's/^-   \*\*\(.*\)\*\*: \*\([^*]*\)\* \(.*\)/-   **\1**: *\2*  \n    \3/g' toplevel.md
	# Add disclaimer about Archive page for older ejabberd releases
	sed -i 's|\(This section.*\)|> \1'$(ARCHIVESTRING)'|g' toplevel.md
	# Convert tricky absolute URLs into valid relative URLS
	sed -i 's|http://\.\.|/admin/configuration|g' toplevel.md
	# Convert note to div HTML elements
	sed -i 's|\*Note\* about the next option: \(.*\):|<div class="note-down">\1</div>|g' toplevel.md
	# Make URL when mentioning an ejabberd release
	sed -i 's| \([0-9][0-9]\)\.\([0-9][0-9]\)| <a href="/archive/\1_\2/">\1.\2</a>|g' toplevel.md

	echo "---\ntitle: Modules Options\ntoc: true\nmenu: Modules Options\norder: 95\n---" >modules.md
	cat man-03 >>modules.md
	# Remove second-level headers so they don't get TOC entries:
	sed -i 's/^### \(.*\)/__\1__/g' modules.md
	# Prepend a space to list character so they get indented one level:
	# And split paragraph of suboptions, it's nicer to read
	sed -i 's/^-   \*\*\(.*\)\*\*: \*\([^*]*\)\* \(.*\)/ - **\1**: *\2*  \n   \3/g' modules.md
	# Previous line doesn't process some subptions that span over two lines, workaround:
	sed -i 's/^-   \*\*\(.*\)\*\*\(.*\)/ - **\1**\2/g' modules.md
	sed -i 's/^-   \*\(.*\)\*\ \(.*\)/ - *\1* \2/g' modules.md
	# and then add list character to options:
	sed -i 's/^\*\*.*\*\*: /- &/g' modules.md
	sed -i 's/^\*\*.*\*\*  /- &/g' modules.md
	# Convert tricky absolute URLs into valid relative URLS
	sed -i 's|http://\.\.|/admin/configuration|g' modules.md
	# Add disclaimer about Archive page for older ejabberd releases
	sed -i 's|\(This section.*\)|> \1'$(ARCHIVESTRING)'|g' modules.md
	# Convert note to div HTML elements
	sed -i 's|\*Note\* about the next option: \(.*\):|<div class="note-left">\1</div>|g' modules.md
	# Make URL when mentioning an ejabberd release
	sed -i 's| \([0-9][0-9]\)\.\([0-9][0-9]\)| <a href="/archive/\1_\2/">\1.\2</a>|g' modules.md
	# Increase indentation of examples of modules options
	vim modules.md -c "set expandtab" -c "g/\*\*Example\*\*:\n\n    .*\n.*/normal V}}>" -c "wq"

	rm ejabberd.yml.5.txt
	rm ejabberd.yml.5.xml
	rm man-*
	mv toplevel.md content/admin/configuration
	mv modules.md content/admin/configuration

archive:
	mkdir $(DEST)
	cp content/admin/configuration/listen.md $(DEST)/listen.md
	cp content/admin/configuration/listen-options.md $(DEST)/listen-options.md
	cp content/admin/configuration/modules.md $(DEST)/modules.md
	cp content/admin/configuration/toplevel.md $(DEST)/toplevel.md
	cp content/developer/ejabberd-api/admin-api.md $(DEST)/admin-api.md
	cp content/developer/sql-schema.md $(DEST)/sql-schema.md
	sed -i '/order: 40/d' $(DEST)/admin-api.md
	sed -i '/^> This section /d' $(DEST)/*.md
	sed -i 's|/admin/configuration/listen/|/'$(ARCHIVE)'/listen/|g' $(DEST)/*.md
	sed -i 's|/admin/configuration/listen-options/|/'$(ARCHIVE)'/listen-options/|g' $(DEST)/*.md
	sed -i 's|/admin/configuration/modules/|/'$(ARCHIVE)'/modules/|g' $(DEST)/*.md
	sed -i 's|/admin/configuration/toplevel/|/'$(ARCHIVE)'/toplevel/|g' $(DEST)/*.md
	sed -i 's|/developer/ejabberd-api/admin-api/|/'$(ARCHIVE)'/admin-api/|g' $(DEST)/*.md
	sed -i 's|/developer/sql-schema/|/'$(ARCHIVE)'/sql-schema/|g' $(DEST)/*.md
	echo "* [$(VERSION)](/archive/$(VERSION_)/)" >>content/archive/index.md
	echo "---" >$(DEST)/index.md
	echo "title: Archived Documentation for $(VERSION)" >>$(DEST)/index.md
	echo "menu: $(VERSION)" >>$(DEST)/index.md
	echo "---" >>$(DEST)/index.md
	echo "" >>$(DEST)/index.md
	echo "This section contains some archived sections for ejabberd $(VERSION)." >>$(DEST)/index.md
	echo "" >>$(DEST)/index.md
	echo "If you are upgrading ejabberd from a previous release, you can check:" >>$(DEST)/index.md
	echo "" >>$(DEST)/index.md
	echo "* [Specific version upgrade notes](/admin/upgrade/#specific-version-upgrade-notes)" >>$(DEST)/index.md
	echo "* [ejabberd $(VERSION) release announcement](https://www.process-one.net/blog/ejabberd-$(VERSION-)/)" >>$(DEST)/index.md
	echo "* [Docs Github Compare from $(VERSIONPRE)](https://github.com/processone/docs.ejabberd.im/compare/$(VERSIONPRE)...$(VERSION))" >>$(DEST)/index.md
	echo "* [ejabberd Github Compare from $(VERSIONPRE)](https://github.com/processone/ejabberd/compare/$(VERSIONPRE)...$(VERSION))" >>$(DEST)/index.md
	echo "" >>$(DEST)/index.md
