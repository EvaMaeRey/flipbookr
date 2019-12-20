TARGETS = ../data/local_code.rda
TARGETS += ../data/local_code_regular_assignment.rda

all : $(TARGETS)

$(TARGETS) : local_code.Rout
	@if test -f $@; then :; else\
		$(RM) $<; \
		$(MAKE) $<; \
	fi

local_code.Rout : local_code.R
	R CMD BATCH --vanilla $<
