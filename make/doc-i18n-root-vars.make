HTML_PAGE_NAMES = index translations
HTML_FILES = $(HTML_PAGE_NAMES:%=%.html)
OUT_HTML_FILES = $(HTML_PAGE_NAMES:%=$(outdir)/%.html)

# ISOLANG must be defined

LANGS = $(shell $(PYTHON) $(buildscript-dir)/langdefs.py)

CSS_SOURCE_FILES = $(shell ls $(top-src-dir)/Documentation/lilypond*.css)
SOURCE_PNG_IMAGES = $(shell ls $(top-src-dir)/Documentation/*.png)
OUT_PNG_IMAGES = \
  $(SOURCE_PNG_IMAGES:$(top-src-dir)/Documentation/%.png=$(outdir)/%.png) \
  $(outdir)/context-example.png

TELY_FILES := $(call src-wildcard,*.tely)
MASTER_TEXI_FILES := $(TELY_FILES:%.tely=$(outdir)/%.texi)
BIG_PAGE_HTML_FILES := $(TELY_FILES:%.tely=$(top-build-dir)/Documentation/$(outdir)/%-big-page.$(ISOLANG).html)
DEEP_HTML_FILES := $(TELY_FILES:%.tely=$(top-build-dir)/Documentation/$(outdir)/%/index.$(ISOLANG).html)
PDF_FILES := $(TELY_FILES:%.tely=$(top-build-dir)/Documentation/$(outdir)/%.$(ISOLANG).pdf)

ITELY_FILES := $(call src-wildcard,*.itely)
ITEXI_FILES := $(call src-wildcard,*.itexi)

DOCUMENTATION_INCLUDES = \
  -I $(top-src-dir)/Documentation \
  -I $(top-build-dir)/Documentation/$(outdir)

LILYPOND_BOOK_INCLUDES += $(DOCUMENTATION_INCLUDES)
MAKEINFO_FLAGS += --force --enable-encoding $(DOCUMENTATION_INCLUDES)
MAKEINFO = LANG= $(MAKEINFO_PROGRAM) $(MAKEINFO_FLAGS)

# texi2html xref map files
XREF_MAPS_DIR=$(top-build-dir)/out/xref-maps
XREF_MAPS_FILES=$(TELY_FILES:%.tely=$(XREF_MAPS_DIR)/%.$(ISOLANG).xref-map)
XREF_MAP_FLAGS += -I $(outdir)

# texi2html flags
TEXI2HTML_INIT= --init-file=$(top-src-dir)/Documentation/lilypond-texi2html.init
TEXI2HTML_LANG=--lang=$(ISOLANG)
TEXI2HTML_FLAGS += $(TEXI2HTML_LANG) $(DOCUMENTATION_INCLUDES) \
  -I $(XREF_MAPS_DIR)
TEXI2HTML = PERL_UNICODE=SD LANG= $(TEXI2HTML_PROGRAM)

TEXI2PDF_FLAGS += --batch $(DOCUMENTATION_INCLUDES)

ifdef QUIET_BUILD
TEXI2PDF_FLAGS += -q
endif

DOCUMENTATION_LOCALE_TARGET = $(outdir)/doc-po
TRANSLATION_LILY_IMAGES = $(outdir)/translation-lily-images
