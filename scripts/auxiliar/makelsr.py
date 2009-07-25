#!/usr/bin/env python

import sys
import os
import glob
import re

os.environ['PYTHONPATH'] += ':python'
import langdefs

DEST = os.path.join ('Documentation', 'snippets')
NEW_LYS = os.path.join ('Documentation', 'snippets', 'new')
TEXIDOCS = [os.path.join ('Documentation', language_code, 'texidocs')
            for language_code in langdefs.LANGDICT]

USAGE = '''  Usage: makelsr.py [LSR_SNIPPETS_DIR]
This script must be run from top of the source tree;
it updates snippets %(DEST)s with snippets
from %(NEW_LYS)s or LSR_SNIPPETS_DIR.
If a snippet is present in both directories, the one
from %(NEW_LYS)s is preferred.
''' % vars ()

LY_HEADER_LSR = '''%% Do not edit this file; it is automatically
%% generated from LSR http://lsr.dsi.unimi.it
%% This file is in the public domain.
'''

LY_HEADER_NEW = '''%% Do not edit this file; it is automatically
%% generated from %s
%% This file is in the public domain.
''' % NEW_LYS

TAGS = []
# NR 1
TAGS.extend (['pitches', 'rhythms', 'expressive-marks',
'repeats', 'simultaneous-notes', 'staff-notation',
'editorial-annotations', 'text'])
# NR 2
TAGS.extend (['vocal-music', 'chords', 'keyboards',
'percussion', 'fretted-strings', 'unfretted-strings',
'ancient-notation', 'winds', 'world-music'
])

# other
TAGS.extend (['contexts-and-engravers', 'tweaks-and-overrides',
'paper-and-layout', 'breaks', 'spacing', 'midi', 'titles', 'template'])

def exit_with_usage (n=0):
    sys.stderr.write (USAGE)
    sys.exit (n)

if len (sys.argv) >= 2:
    in_dir = sys.argv[1]
    if len (sys.argv) >= 3:
        exit_with_usage (2)
    if not (os.path.isdir (DEST) and os.path.isdir (NEW_LYS)):
        exit_with_usage (3)
else:
    in_dir = ''

unsafe = []
unconverted = []
notags_files = []

# mark the section that will be printed verbatim by lilypond-book
end_header_re = re.compile ('(\\header {.+?doctitle = ".+?})\n', re.M | re.S)

def mark_verbatim_section (ly_code):
    return end_header_re.sub ('\\1 % begin verbatim\n\n', ly_code, 1)

# '% LSR' comments are to be stripped
lsr_comment_re = re.compile (r'\s*%+\s*LSR.*')

begin_header_re = re.compile (r'\\header\s*{', re.M)

ly_new_version_re = re.compile (r'\\version\s*"(.+?)"')

# add tags to ly files from LSR
def add_tags (ly_code, tags):
    return begin_header_re.sub ('\\g<0>\n  lsrtags = "' + tags + '"\n',
                                ly_code, 1)

# for snippets from input/new, add message for earliest working version
def add_version (ly_code):
    return '''%% Note: this file works from version ''' + \
        ly_new_version_re.search (ly_code).group (1) + '\n'

def copy_ly (srcdir, name, tags):
    global unsafe
    global unconverted
    dest = os.path.join (DEST, name)
    tags = ', '.join (tags)
    s = open (os.path.join (srcdir, name)).read ()

    for path in TEXIDOCS:
        texidoc_translation_path = \
            os.path.join (path, os.path.splitext (name)[0] + '.texidoc')
        if os.path.exists (texidoc_translation_path):
            texidoc_translation = open (texidoc_translation_path).read ()
            # Since we want to insert the translations verbatim using a 
            # regexp, \\ is understood as ONE escaped backslash. So we have
            # to escape those backslashes once more...
            texidoc_translation = texidoc_translation.replace ('\\', '\\\\')
            s = begin_header_re.sub ('\\g<0>\n' + texidoc_translation, s, 1)

    if in_dir and in_dir in srcdir:
        s = LY_HEADER_LSR + add_tags (s, tags)
    else:
        s = LY_HEADER_NEW + add_version (s) + s

    s = mark_verbatim_section (s)
    s = lsr_comment_re.sub ('', s)
    open (dest, 'w').write (s)

    e = os.system ("convert-ly -e '%s'" % dest)
    if e:
        unconverted.append (dest)
    if os.path.exists (dest + '~'):
        os.remove (dest + '~')
    # no need to check snippets from input/new
    if in_dir and in_dir in srcdir:
        # -V seems to make unsafe snippets fail nicer/sooner
        e = os.system ("lilypond -V -dno-print-pages -dsafe -o /tmp/lsrtest '%s'" % dest)
        if e:
            unsafe.append (dest)

def read_source_with_dirs (src):
    s = {}
    l = {}
    for tag in TAGS:
        srcdir = os.path.join (src, tag)
        l[tag] = set (map (os.path.basename,
                           glob.glob (os.path.join (srcdir, '*.ly'))))
        for f in l[tag]:
            if f in s:
                s[f][1].append (tag)
            else:
                s[f] = (srcdir, [tag])
    return s, l


tags_re = re.compile ('lsrtags\\s*=\\s*"(.+?)"')

def read_source (src):
    s = {}
    l = dict ([(tag, set()) for tag in TAGS])
    for f in glob.glob (os.path.join (src, '*.ly')):
        basename = os.path.basename (f)
        m = tags_re.search (open (f, 'r').read ())
        if m:
            file_tags = [tag.strip() for tag in m.group (1). split(',')]
            s[basename] = (src, file_tags)
            [l[tag].add (basename) for tag in file_tags if tag in TAGS]
        else:
            notags_files.append (f)
    return s, l


def dump_file_list (file, file_list, update=False):
    if update:
        old_list = set (open (file, 'r').read ().splitlines ())
        old_list.update (file_list)
        new_list = list (old_list)
    else:
        new_list = file_list
    f = open (file, 'w')
    f.write ('\n'.join (sorted (new_list)) + '\n')

if in_dir:
    ## clean out existing lys and generated files
    map (os.remove, glob.glob (os.path.join (DEST, '*.ly')) +
         glob.glob (os.path.join (DEST, '*.snippet-list')))

    # read LSR source where tags are defined by subdirs
    snippets, tag_lists = read_source_with_dirs (in_dir)

    # read input/new where tags are directly defined
    s, l = read_source (NEW_LYS)
    snippets.update (s)
    for t in TAGS:
        tag_lists[t].update (l[t])
else:
    snippets, tag_lists = read_source (NEW_LYS)

for (name, (srcdir, tags)) in snippets.items ():
    copy_ly (srcdir, name, tags)
for (tag, file_set) in tag_lists.items ():
    dump_file_list (os.path.join (DEST, tag + '.snippet-list'),
                    file_set, update=not(in_dir))
if unconverted:
    sys.stderr.write ('These files could not be converted successfully by convert-ly:\n')
    sys.stderr.write ('\n'.join (unconverted) + '\n\n')
if notags_files:
    sys.stderr.write ('No tags could be found in these files:\n')
    sys.stderr.write ('\n'.join (notags_files) + '\n\n')
if unsafe:
    dump_file_list ('lsr-unsafe.txt', unsafe)
    sys.stderr.write ('''

Unsafe files printed in lsr-unsafe.txt: CHECK MANUALLY!
  git add %s/*.ly
  xargs git diff HEAD < lsr-unsafe.txt

''' % DEST)
