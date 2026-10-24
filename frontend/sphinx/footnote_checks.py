"""Reject footnote references whose definitions are absent from the output."""

from docutils import nodes
from sphinx.errors import SphinxError
from sphinx.transforms.post_transforms import SphinxPostTransform


class CheckFootnoteDefinitions(SphinxPostTransform):
    # Check after conditional content is removed, before LaTeX's URL-footnote
    # numbering can make a missing definition look like a repeated reference.
    default_priority = 390

    def run(self, **kwargs):
        current_docname = self.env.current_document.docname
        definitions = {
            (footnote.get('docname', current_docname), identifier)
            for footnote in self.document.findall(nodes.footnote)
            for identifier in footnote['ids']
        }
        for reference in self.document.findall(nodes.footnote_reference):
            identifier = reference.get('refid')
            if identifier is None:
                # Docutils reports references that could not resolve an ID.
                continue
            docname = reference.get('docname', current_docname)
            if (docname, identifier) in definitions:
                continue

            location = str(self.env.doc2path(docname))
            ancestor = reference
            while ancestor is not None:
                if ancestor.line is not None:
                    location += f':{ancestor.line}'
                    break
                ancestor = ancestor.parent
            raise SphinxError(
                f"{location}: footnote reference {identifier!r} has no "
                "definition in this document. Add the missing footnote "
                "definition; for :footcite: citations, place "
                "'.. footbibliography::' after the last citation in the "
                "document."
            )


def setup(app):
    app.add_post_transform(CheckFootnoteDefinitions)
    return {
        'version': '1.0',
        'parallel_read_safe': True,
        'parallel_write_safe': True,
    }
