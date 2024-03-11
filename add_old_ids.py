import mkdocs.plugins
from bs4 import BeautifulSoup

@mkdocs.plugins.event_priority(-50)

def on_page_content(content, page, **kwargs):
    path = page.file.src_uri
    if (path == 'admin/configuration/toplevel.md' or
        path == 'admin/configuration/modules.md' or
        path == 'admin/configuration/listen.md' or
        path == 'admin/configuration/listen-options.md' or
        path == 'developer/ejabberd-api/admin-api.md' or
        path == 'developer/ejabberd-api/admin-tags.md'):
        return add_old_anchors(content)

def add_old_anchors(html):
    soup = BeautifulSoup(html, 'html.parser')
    for h2 in soup.find_all('h2'):
        idcurrent = h2.get('id')
        if idcurrent and '_' in idcurrent:
            h2.insert_before(soup.new_tag('a', id=idcurrent.replace('_', '-')))
    return str(soup)
