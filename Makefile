DOCS_SRC_DIR  = $(addprefix , docs)
ifdef FILE
DOCS_FILE = $(DOCS_SRC_DIR)/$(FILE)
else
DOCS_FILE ?= $(foreach sdir, $(DOCS_SRC_DIR), $(wildcard $(sdir)/*.qmd))
endif

HTML_DIR := $(DOCS_SRC_DIR)

HTML_FILES := $(addprefix $(HTML_DIR)/, $(notdir $(patsubst %.qmd, %.html, $(DOCS_FILE))))

$(info ************************************)
$(info DOCS Source directory:     $(DOCS_SRC_DIR))
$(info DOCS Source files:         $(DOCS_FILE))
$(info HTML director:             $(HTML_DIR))
$(info HTML files:                $(HTML_FILES))
$(info ************************************)

all: $(HTML_FILES)
	@echo "All are now up to date"

$(HTML_DIR)/%.html: $(DOCS_SRC_DIR)/%.qmd
	@echo "Compiling documntation for analyses"
	$(info Source = $<; Destination = $@)
	quarto render $< --to=html,pdf
