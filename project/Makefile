#
# Installation Makefile for Clean
#
#
#	make (= make install):
#		install the Clean compiler
#
#	make cleanup:
#		remove all files that can be recreated
#
# This Makefile can be used with both make (Sun) and gmake (GNU make)
#

#
# START site dependent make variables
#

#
# By default the Clean compiler is installed in the current
# directory. You can change the following variables to install
# it elsewhere. 
#

INSTALL_DIR = $(CURRENTDIR)

INSTALL_BIN_DIR = $(INSTALL_DIR)/bin

INSTALL_LIB_DIR = $(INSTALL_DIR)/lib

INSTALL_MAN_DIR = $(INSTALL_DIR)/man

INSTALL_TEMP_DIR = $(INSTALL_DIR)/Temp

#
# END site dependent make variables
#


#
# The default entry
#
default: install

#
# Be quiet
#
.SILENT:
MAKEFLAGS = s

#
# The following piece of witchcraft sets the value of CURRENTDIR
# for both make and gmake.
#
CURRENTDIR = $(MAKECURRENTDIR)$(GMAKECURRENTDIR)
MAKECURRENTDIR : sh=pwd
GMAKECURRENTDIR = ${shell echo `pwd`}

#
# Install section
#
install: install_bin install_stdenv install_man install_doc install_ArgEnv install_Generics install_Gast install_StdLib install_Directory install_MersenneTwister install_TCPIP install_Platform $(PATCHES)
	$(PATCH_BIN) $(INSTALL_CLM) CLEANLIB $(INSTALL_EXE_DIR)
	
io: install
cleanup: cleanup_stdenv

#
# Commands section
#
CLM = \
'CLEANLIB=$(CURRENTDIR)/exe;export CLEANLIB;CLEANPATH=$(CURRENTDIR)/StdEnv;export CLEANPATH;$(CURRENTDIR)/bin/clm'
CLMFLAGS = -nw
HTOCLEAN = $(CURRENTDIR)/bin/htoclean
FCLC = $(CURRENTDIR)/bin/fclc

PATCH_BIN = $(CURRENTDIR)/bin/patch_bin

#
# Binaries
#
BIN_FILES = clm htoclean
EXE_FILES = cocl cocl_itask cg

INSTALL_BIN_FILES = $(BIN_FILES:%=$(INSTALL_BIN_DIR)/%)
INSTALL_CLM = $(INSTALL_BIN_DIR)/clm

INSTALL_EXE_DIR = $(INSTALL_LIB_DIR)/exe
INSTALL_EXE_FILES = $(EXE_FILES:%=$(INSTALL_EXE_DIR)/%)

install_bin : $(INSTALL_BIN_DIR) $(INSTALL_BIN_FILES) \
				$(INSTALL_EXE_DIR) $(INSTALL_EXE_FILES) $(INSTALL_TEMP_DIR)

DEL = %

$(INSTALL_BIN_DIR):
	mkdir -m 755 -p $@

$(INSTALL_BIN_DIR)/% : bin/%
	cp $< $@
	chmod 711 $@

$(INSTALL_EXE_DIR):
	mkdir -m 755 -p $@

$(INSTALL_EXE_DIR)/% : exe/%
	cp $< $@
	chmod 711 $@

$(INSTALL_TEMP_DIR):
	mkdir -m 755 -p $@

#	cp CleanIDEConfig $@
#	chmod 644 $@

#
# Clean Standard Environment
#
INSTALL_STDENV_DIR = $(INSTALL_LIB_DIR)/StdEnv

#	$(MAKE) install \
#		INSTALL_STDENV_DIR=$(INSTALL_STDENV_DIR) \
#		CLM=$(CLM) \
#		CLMFLAGS=$(CLMFLAGS)

install_stdenv :
	cd StdEnv; \
	export CLEANLIB=$(CURRENTDIR)/exe; \
	export CLEANPATH=$(CURRENTDIR)/StdEnv; \
	export CLMP=$(CURRENTDIR)/bin/clm; \
	export INSTALL_STDENV_DIR=$(INSTALL_STDENV_DIR); \
	export CLMFLAGS=$(CLMFLAGS); \
	./install.sh; \
	$(PATCH_BIN) $(INSTALL_CLM) CLEANPATH $(INSTALL_STDENV_DIR); \
	$(PATCH_BIN) $(INSTALL_CLM) CLEANILIB $(INSTALL_LIB_DIR);

cleanup_stdenv :
	cd StdEnv; $(MAKE) cleanup

#
# Clean man pages
#

MAN_FILES = man1/clm.1

INSTALL_MAN_FILES = $(MAN_FILES:%=$(INSTALL_MAN_DIR)/%)

install_man : $(INSTALL_MAN_DIR)/man1 $(INSTALL_MAN_FILES)

$(INSTALL_MAN_DIR)/man1 :
	mkdir -p $@

$(INSTALL_MAN_DIR)/man1/% : man/man1/%
	cp $< $@
	chmod 644 $@

$(INSTALL_LIB_DIR)/doc :
	mkdir -p $@

install_doc : $(INSTALL_LIB_DIR)/doc
	cp -R doc/* $(INSTALL_LIB_DIR)/doc

$(INSTALL_LIB_DIR)/ArgEnv :
	mkdir -p $@
	mkdir -p $@/"Clean System Files"

install_ArgEnv : $(INSTALL_LIB_DIR)/ArgEnv
	( cd data/ArgEnv ; make ArgEnvC.o )
	cp -R data/ArgEnv/* $(INSTALL_LIB_DIR)/ArgEnv

$(INSTALL_LIB_DIR)/Generics :
	mkdir -p $@
	mkdir -p $@/"Clean System Files"

install_Generics : $(INSTALL_LIB_DIR)/Generics
	cp -R data/Generics/* $(INSTALL_LIB_DIR)/Generics

$(INSTALL_LIB_DIR)/Gast :
	mkdir -p $@
	mkdir -p $@/"Clean System Files"

install_Gast : $(INSTALL_LIB_DIR)/Gast
	cp -R data/Gast/* $(INSTALL_LIB_DIR)/Gast

$(INSTALL_LIB_DIR)/StdLib :
	mkdir -p $@
	mkdir -p $@/"Clean System Files"

install_StdLib : $(INSTALL_LIB_DIR)/StdLib
	cp -R data/StdLib/* $(INSTALL_LIB_DIR)/StdLib

$(INSTALL_LIB_DIR)/Directory:
	mkdir -p $@
	mkdir -p $@/"Clean System Files"

install_Directory: $(INSTALL_LIB_DIR)/Directory
	( cd "data/Directory/Clean System Files"; gcc -c -O cDirectory.c -o cDirectory.o )
	cp -R data/Directory/* $(INSTALL_LIB_DIR)/Directory

$(INSTALL_LIB_DIR)/MersenneTwister:
	mkdir -p $@
	mkdir -p $@/"Clean System Files"

install_MersenneTwister: $(INSTALL_LIB_DIR)/MersenneTwister
	cp -R data/MersenneTwister/* $(INSTALL_LIB_DIR)/MersenneTwister

$(INSTALL_LIB_DIR)/TCPIP:
	mkdir -p $@
	mkdir -p $@/"Clean System Files"

install_TCPIP: $(INSTALL_LIB_DIR)/TCPIP
	cp -R data/TCPIP/* $(INSTALL_LIB_DIR)/TCPIP

$(INSTALL_LIB_DIR)/Platform:
	mkdir -p $@
	mkdir -p $@/"Clean System Files"

install_Platform: $(INSTALL_LIB_DIR)/Platform
	cp -R data/Platform/* $(INSTALL_LIB_DIR)/Platform

