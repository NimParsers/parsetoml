# -*- coding: utf-8 -*-
"""
    sphinx.domains.nim
    ~~~~~~~~~~~~~~~~~~

    The Nim language domain, derived from the C language domain bundled
    with Sphinx.

    :copyright: Copyright 2014 by Maurizio Tomasi
    :license: BSD, see LICENSE for details.
"""

import re
import string

from docutils import nodes

from sphinx import addnodes
from sphinx.roles import XRefRole
from sphinx.locale import l_, _
from sphinx.domains import Domain, ObjType
from sphinx.directives import ObjectDescription
from sphinx.util.nodes import make_refnode
from sphinx.util.docfields import Field, TypedField

# RE to split at word boundaries
wsplit_re = re.compile(r'(\W+)')

# REs for Nim signatures
nim_proc_sig_re = re.compile(
    r'''^proc\ ?
        ([^( ]+)           # the procedure name
        (\([^)]*\))?       # arguments (optional)
        \ *(:\ *[^{=]+)?   # return type (optional)
        \ *(\{\..*\.\})?   # pragmas (optional)
        \ *$''', re.VERBOSE)

nim_enum_sig_re = re.compile(
    r'''^([\w]+)            # Enumeration type name
    \ *=\ *                 # Equal sign
    enum\ *
    (.+)                    # Enumeration values
    \ *$''', re.VERBOSE)

nim_object_sig_re = re.compile(
    r'''^([\w]+)            # Object type name
    \ *=\ *                 # Equal sign
    (object|ref)
    .*$''', re.VERBOSE)

nim_arg_sig_re = re.compile(r'\(\ *([^)]+)?\ *\)')
nim_rettype_sig_re = re.compile(r'\ *:\ *([^{=]+)')

################################################################################

class NimObject(ObjectDescription):
    """
    Description of a Nim language object.
    """

    doc_field_types = [
        TypedField('parameter', label=l_('Parameters'),
                   names=('param', 'parameter', 'arg', 'argument'),
                   typerolename='type', typenames=('type',)),
        Field('returnvalue', label=l_('Returns'), has_arg=False,
              names=('returns', 'return')),
        Field('returntype', label=l_('Return type'), has_arg=False,
              names=('rtype',)),
    ]

    # These Nim types aren't described anywhere, so don't try to create
    # a cross-reference to them
    stopwords = set((
        'int', 'uint', 'float', 'bool',
        'int8', 'int16', 'int32', 'int64',
        'uint8', 'uint16', 'uint32', 'uint64',
        'float32', 'float64',
        'char', 'string', 'cstring', 'pointer',
        'void', 'expr', 'typedesc',
        'tsignedint', 'tunsignedint', 'tinteger', 'tordinal', 'treal', 'tnumber',
        'byte', 'natural', 'positive',
        'exception'
    ))

    def _parse_type(self, node, ctype):
        # add cross-ref nodes for all words
        for part in filter(None, wsplit_re.split(ctype)):
            tnode = nodes.Text(part, part)
            if part[0] in string.ascii_letters + '_' and \
                   part.lower() not in self.stopwords:
                pnode = addnodes.pending_xref(
                    '', refdomain='nim', reftype='type', reftarget=part,
                    modname=None, classname=None)
                pnode += tnode
                node += pnode
            else:
                node += tnode

    def _handle_proc_signature(self, sig, signode, m):
        "Transform a Nim proc node into RST nodes."

        name, arglist, rettype, pragmas = m.groups()

        signode += addnodes.desc_type('proc', 'proc')
        signode += addnodes.desc_name(name, name)

        if arglist is None:
            signode += addnodes.desc_parameterlist()
        else:
            arguments = nim_arg_sig_re.match(arglist).groups()[0]
            signode += addnodes.desc_parameterlist(arguments, arguments)

        if rettype is not None:
            retnode = addnodes.desc_returns()
            self._parse_type(retnode, nim_rettype_sig_re.match(rettype).groups()[0])
            signode += retnode

        if pragmas:
            signode += addnodes.desc_addname(pragmas, pragmas)
        return name

    def _handle_enum_signature(self, sig, signode, m):
        "Transform a Nim enum into RST nodes."
        name, values = m.groups()

        signode += addnodes.desc_type('enum', 'enum')
        signode += addnodes.desc_name(name, name)
        signode += addnodes.desc_addname(values, '= ' + values)
        return name

    def _handle_object_signature(self, sig, signode, m):
        "Transform a Nim object into RST nodes."
        name = m.groups()[0]

        signode += addnodes.desc_type('object', 'object')
        signode += addnodes.desc_name(name, name)
        return name

    def handle_signature(self, sig, signode):
        "Transform a Nim signature into RST nodes."

        m = nim_proc_sig_re.match(sig)
        if m is not None:
            return self._handle_proc_signature(sig, signode, m)

        m = nim_enum_sig_re.match(sig)
        if m is not None:
            return self._handle_enum_signature(sig, signode, m)

        m = nim_object_sig_re.match(sig)
        if m is not None:
            return self._handle_object_signature(sig, signode, m)

        raise ValueError('no match')

    def get_index_text(self, name):
        if self.objtype == 'proc':
            return _('%s (Nim procedure)') % name
        elif self.objtype == 'template':
            return _('%s (Nim template)') % name
        elif self.objtype == 'enum':
            return _('%s (Nim enumeration)') % name
        elif self.objtype == 'type':
            return _('%s (Nim type)') % name
        elif self.objtype == 'const':
            return _('%s (Nim constant)') % name
        else:
            return ''

    def add_target_and_index(self, name, sig, signode):
        # for Nim API items we add a prefix since names are usually not qualified
        # by a module name and so easily clash with e.g. section titles
        targetname = 'nim.' + name
        if targetname not in self.state.document.ids:
            signode['names'].append(targetname)
            signode['ids'].append(targetname)
            signode['first'] = (not self.names)
            self.state.document.note_explicit_target(signode)
            inv = self.env.domaindata['nim']['objects']
            if name in inv:
                self.state_machine.reporter.warning(
                    'duplicate Nim object description of %s, ' % name +
                    'other instance in ' + self.env.doc2path(inv[name][0]),
                    line=self.lineno)
            inv[name] = (self.env.docname, self.objtype)

        indextext = self.get_index_text(name)
        if indextext:
            self.indexnode['entries'].append(('single', indextext,
                                              targetname, ''))

    def before_content(self):
        self.typename_set = False
        if self.name == 'nim:type':
            if self.names:
                self.env.temp_data['nim:type'] = self.names[0]
                self.typename_set = True

    def after_content(self):
        if self.typename_set:
            self.env.temp_data['nim:type'] = None

################################################################################

class NimXRefRole(XRefRole):
    def process_link(self, env, refnode, has_explicit_title, title, target):
        if not has_explicit_title:
            target = target.lstrip('~') # only has a meaning for the title
            # if the first character is a tilde, don't display the module/class
            # parts of the contents
            if title[0:1] == '~':
                title = title[1:]
                dot = title.rfind('.')
                if dot != -1:
                    title = title[dot+1:]
        return title, target

################################################################################

class NimDomain(Domain):
    """Nim language domain."""
    name = 'nim'
    label = 'Nim'
    object_types = {
        'proc': ObjType(l_('proc'), 'proc'),
        'template': ObjType(l_('template'), 'template'),
        'enum': ObjType(l_('enum'), 'enum'),
        'type': ObjType(l_('type'), 'type'),
        'object': ObjType(l_('object'), 'object'),
        'const': ObjType(l_('const'), 'const'),
    }

    directives = {
        'proc': NimObject,
        'template': NimObject,
        'enum': NimObject,
        'type': NimObject,
        'object': NimObject,
        'const': NimObject
    }
    roles = {
        'proc' : NimXRefRole(fix_parens=True),
        'template': NimXRefRole(),
        'enum': NimXRefRole(),
        'type': NimXRefRole(),
        'object': NimXRefRole(),
        'const': NimXRefRole(),
    }
    initial_data = {
        'objects': {},  # fullname -> docname, objtype
    }

    def clear_doc(self, docname):
        for fullname, (fn, _) in self.data['objects'].items():
            if fn == docname:
                del self.data['objects'][fullname]

    def resolve_xref(self, env, fromdocname, builder,
                     typ, target, node, contnode):
        # strip pointer asterisk
        target = target.rstrip(' *')
        if target not in self.data['objects']:
            return None
        obj = self.data['objects'][target]
        return make_refnode(builder, fromdocname, obj[0], 'nim.' + target,
                            contnode, target)

    def get_objects(self):
        for refname, (docname, type) in self.data['objects'].iteritems():
            yield (refname, refname, type, docname, 'nim.' + refname, 1)

################################################################################

def setup(app):
    app.add_domain(NimDomain)
