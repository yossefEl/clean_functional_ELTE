#
#	Makefile for the Clean Standard Environment
#

#
#		make (= make stdenv):
#			compiles all modules
#
#		make install:
#			compiles all modules
#			copies files to install directory
#
#		make cleanup:
#			removes all files in this directory that can be recreated
#

CLEAN_MODULES = \
	StdArray StdCharList StdClass StdDebug StdFunc StdFunctions StdList \
	StdOrdList StdOverloaded StdTuple _SystemEnum StdEnum _SystemArray StdEnv \
	_SystemStrictLists _SystemEnumStrict StdStrictLists StdOverloadedList StdGeneric \
	_SystemStrictMaybes StdMaybe _SystemDynamic StdDynamic

SYSTEM_MODULES = \
	StdBool StdChar StdFile StdInt StdMisc StdReal StdString

ABC_MODULES = \
	_system

OBJECT_MODULES = \
	_startup _startupProfile _startupTrace

SYSTEM_OBJECT_MODULES = \

CLEAN_MODULES_ABC = $(CLEAN_MODULES:%=Clean\ System\ Files/%.abc)

SYSTEM_MODULES_ABC = $(SYSTEM_MODULES:%=Clean\ System\ Files/%.abc)

SYSTEM_MODULES_ICL = $(SYSTEM_MODULES:%=%.icl)

ABC_MODULES_ABC = $(ABC_MODULES:%=Clean\ System\ Files/%.abc)

CLEAN_MODULES_O = $(CLEAN_MODULES:%=Clean\ System\ Files/%.o)

SYSTEM_MODULES_O = $(SYSTEM_MODULES:%=Clean\ System\ Files/%.o)

ABC_MODULES_O = $(ABC_MODULES:%=Clean\ System\ Files/%.o)

OBJECT_MODULES_O = $(OBJECT_MODULES:%=Clean\ System\ Files/%.o)

SYSTEM_OBJECT_MODULES_O = $(SYSTEM_OBJECT_MODULES:%=Clean\ System\ Files/%.o)

STDENV_FILES = \
	$(CLEAN_MODULES:%=%.icl) \
	$(CLEAN_MODULES:%=%.dcl) \
	$(SYSTEM_MODULES:%=%.dcl) \
	$(SYSTEM_MODULES_ABC) \
	$(CLEAN_MODULES_ABC) \
	$(ABC_MODULES_ABC) \
	wait1 \
	$(SYSTEM_MODULES_O) \
	$(CLEAN_MODULES_O) \
	$(ABC_MODULES_O) \
	$(OBJECT_MODULES_O) \
	$(SYSTEM_OBJECT_MODULES_O)

wait1:
	sleep 1

#
# Compile
#

stdenv: $(STDENV_FILES)

#
# Install
#
# the order here is important, because the files will be
# installed in the order given below.

INSTALL_STDENV_FILES = \
	$(CLEAN_MODULES:%=$(INSTALL_STDENV_DIR)/%.icl) \
	$(CLEAN_MODULES:%=$(INSTALL_STDENV_DIR)/%.dcl) \
	$(SYSTEM_MODULES:%=$(INSTALL_STDENV_DIR)/%.dcl) \
	$(ABC_MODULES:%=$(INSTALL_STDENV_DIR)/%.dcl) \
	$(OBJECT_MODULES:%=$(INSTALL_STDENV_DIR)/%.dcl) \
	$(SYSTEM_MODULES:%=$(INSTALL_STDENV_DIR)/Clean\ System\ Files/%.abc) \
	$(CLEAN_MODULES:%=$(INSTALL_STDENV_DIR)/Clean\ System\ Files/%.abc) \
	$(ABC_MODULES:%=$(INSTALL_STDENV_DIR)/Clean\ System\ Files/%.abc) \
	wait2 \
	$(SYSTEM_MODULES:%=$(INSTALL_STDENV_DIR)/Clean\ System\ Files/%.o) \
	$(CLEAN_MODULES:%=$(INSTALL_STDENV_DIR)/Clean\ System\ Files/%.o) \
	$(ABC_MODULES:%=$(INSTALL_STDENV_DIR)/Clean\ System\ Files/%.o) \
	$(OBJECT_MODULES:%=$(INSTALL_STDENV_DIR)/Clean\ System\ Files/%.o) \
	$(SYSTEM_OBJECT_MODULES:%=$(INSTALL_STDENV_DIR)/Clean\ System\ Files/%.o)

install: stdenv $(INSTALL_STDENV_DIR) $(INSTALL_STDENV_DIR)/Clean\ System\ Files $(INSTALL_STDENV_FILES)

wait2:
	sleep 1

$(INSTALL_STDENV_DIR) : 
	mkdir -p "$@"

$(INSTALL_STDENV_DIR)/Clean\ System\ Files : 
	mkdir -p "$@"

$(INSTALL_STDENV_DIR)/% : %
	cp -p "$<" "$@"
	chmod 644 "$@"

#
# Cleanup
#

cleanup:
	$(RM) $(CLEAN_MODULES_ABC) \
	$(RM) $(CLEAN_MODULES_O) \
	$(RM) $(SYSTEM_MODULES_O) \
	$(RM) $(ABC_MODULES_O)

#
# Commands
#

CLM = clm -P .
CLMFLAGS =

.SUFFIXES: .abc .dcl .icl .o

.icl.abc:
	$(CLM) $(CLMFLAGS) -ABC $*

.icl.o:
	$(CLM) $(CLMFLAGS) -O `basename "$*" .icl`

.abc.o:
	$(CLM) $(CLMFLAGS) -O `basename "$*" .icl`


Clean\ System\ Files/StdArray.o : StdArray.icl StdArray.dcl
	$(CLM) -O StdArray

Clean\ System\ Files/StdArray.abc : StdArray.icl StdArray.dcl
	$(CLM) -ABC StdArray

Clean\ System\ Files/StdCharList.o : StdCharList.icl StdCharList.dcl
	$(CLM) -O StdCharList

Clean\ System\ Files/StdCharList.abc : StdCharList.icl StdCharList.dcl
	$(CLM) -ABC StdCharList

Clean\ System\ Files/StdClass.o : StdClass.icl StdClass.dcl
	$(CLM) -O StdClass

Clean\ System\ Files/StdClass.abc : StdClass.icl StdClass.dcl
	$(CLM) -ABC StdClass

Clean\ System\ Files/StdDebug.o : StdDebug.icl StdDebug.dcl
	$(CLM) -O StdDebug

Clean\ System\ Files/StdDebug.abc : StdDebug.icl StdDebug.dcl
	$(CLM) -ABC StdDebug

Clean\ System\ Files/StdEnum.o : StdEnum.icl StdEnum.dcl
	$(CLM) -O StdEnum

Clean\ System\ Files/StdEnum.abc : StdEnum.icl StdEnum.dcl
	$(CLM) -ABC StdEnum

Clean\ System\ Files/StdEnv.o : StdEnv.icl StdEnv.dcl
	$(CLM) -O StdEnv

Clean\ System\ Files/StdEnv.abc : StdEnv.icl StdEnv.dcl
	$(CLM) -ABC StdEnv

Clean\ System\ Files/StdFunc.o : StdFunc.icl StdFunc.dcl
	$(CLM) -O StdFunc

Clean\ System\ Files/StdFunc.abc : StdFunc.icl StdFunc.dcl
	$(CLM) -ABC StdFunc

Clean\ System\ Files/StdFunctions.o : StdFunctions.icl StdFunctions.dcl
	$(CLM) -O StdFunctions

Clean\ System\ Files/StdFunctions.abc : StdFunctions.icl StdFunctions.dcl
	$(CLM) -ABC StdFunctions

Clean\ System\ Files/StdMaybe.o : StdMaybe.icl StdMaybe.dcl
	$(CLM) -O StdMaybe

Clean\ System\ Files/StdMaybe.abc : StdMaybe.icl StdMaybe.dcl
	$(CLM) -ABC StdMaybe

Clean\ System\ Files/StdList.o : StdList.icl StdList.dcl
	$(CLM) -O StdList

Clean\ System\ Files/StdList.abc : StdList.icl StdList.dcl
	$(CLM) -ABC StdList

Clean\ System\ Files/StdOrdList.o : StdOrdList.icl StdOrdList.dcl
	$(CLM) -O StdOrdList

Clean\ System\ Files/StdOrdList.abc : StdOrdList.icl StdOrdList.dcl
	$(CLM) -ABC StdOrdList

Clean\ System\ Files/StdOverloaded.o : StdOverloaded.icl StdOverloaded.dcl
	$(CLM) -O StdOverloaded

Clean\ System\ Files/StdOverloaded.abc : StdOverloaded.icl StdOverloaded.dcl
	$(CLM) -ABC StdOverloaded

Clean\ System\ Files/StdTuple.o : StdTuple.icl StdTuple.dcl
	$(CLM) -O StdTuple

Clean\ System\ Files/StdTuple.abc : StdTuple.icl StdTuple.dcl
	$(CLM) -ABC StdTuple

Clean\ System\ Files/_SystemEnum.o : _SystemEnum.icl _SystemEnum.dcl
	$(CLM) -O _SystemEnum

Clean\ System\ Files/_SystemEnum.abc : _SystemEnum.icl _SystemEnum.dcl
	$(CLM) -ABC _SystemEnum

Clean\ System\ Files/StdBool.o : StdBool.dcl Clean\ System\ Files/StdBool.abc
	$(CLM) -O StdBool

Clean\ System\ Files/StdBool.abc : StdBool.icl StdBool.dcl
	$(CLM) -pt -ABC StdBool

Clean\ System\ Files/StdChar.o : StdChar.dcl Clean\ System\ Files/StdChar.abc
	$(CLM) -O StdChar

Clean\ System\ Files/StdChar.abc : StdChar.icl StdChar.dcl
	$(CLM) -pt -ABC StdChar

Clean\ System\ Files/StdFile.o : StdFile.dcl Clean\ System\ Files/StdFile.abc
	$(CLM) -O StdFile

Clean\ System\ Files/StdFile.abc : StdFile.icl StdFile.dcl
	$(CLM) -pt -ABC StdFile

Clean\ System\ Files/StdInt.o : StdInt.dcl Clean\ System\ Files/StdInt.abc
	$(CLM) -O StdInt

Clean\ System\ Files/StdInt.abc : StdInt.icl StdInt.dcl
	$(CLM) -pt -ABC StdInt

Clean\ System\ Files/StdMisc.o : StdMisc.dcl Clean\ System\ Files/StdMisc.abc
	$(CLM) -O StdMisc

Clean\ System\ Files/StdMisc.abc : StdMisc.icl StdMisc.dcl
	$(CLM) -pt -ABC StdMisc

Clean\ System\ Files/StdReal.o : StdReal.dcl Clean\ System\ Files/StdReal.abc
	$(CLM) -O StdReal

Clean\ System\ Files/StdReal.abc : StdReal.icl StdReal.dcl
	$(CLM) -pt -ABC StdReal

Clean\ System\ Files/StdString.o : StdString.dcl Clean\ System\ Files/StdString.abc
	$(CLM) -O StdString

Clean\ System\ Files/StdString.abc : StdString.icl StdString.dcl
	$(CLM) -pt -ABC StdString

Clean\ System\ Files/_SystemArray.o : _SystemArray.dcl Clean\ System\ Files/_SystemArray.abc
	$(CLM) -O _SystemArray

Clean\ System\ Files/_SystemArray.abc : _SystemArray.icl _SystemArray.dcl
	$(CLM) -ABC _SystemArray

Clean\ System\ Files/_SystemStrictLists.o : _SystemStrictLists.dcl Clean\ System\ Files/_SystemStrictLists.abc
	$(CLM) -O _SystemStrictLists

Clean\ System\ Files/_SystemStrictLists.abc : _SystemStrictLists.icl _SystemStrictLists.dcl
	$(CLM) -ABC _SystemStrictLists

Clean\ System\ Files/_SystemEnumStrict.o : _SystemEnumStrict.dcl Clean\ System\ Files/_SystemEnumStrict.abc
	$(CLM) -O _SystemEnumStrict

Clean\ System\ Files/_SystemEnumStrict.abc : _SystemEnumStrict.icl _SystemEnumStrict.dcl
	$(CLM) -ABC _SystemEnumStrict

Clean\ System\ Files/StdStrictLists.o : StdStrictLists.dcl Clean\ System\ Files/StdStrictLists.abc
	$(CLM) -O StdStrictLists

Clean\ System\ Files/StdStrictLists.abc : StdStrictLists.icl StdStrictLists.dcl
	$(CLM) -ABC StdStrictLists

Clean\ System\ Files/StdOverloadedList.o : StdOverloadedList.dcl Clean\ System\ Files/StdOverloadedList.abc
	$(CLM) -O StdOverloadedList

Clean\ System\ Files/StdOverloadedList.abc : StdOverloadedList.icl StdOverloadedList.dcl
	$(CLM) -ABC StdOverloadedList

Clean\ System\ Files/StdGeneric.abc : StdGeneric.icl StdGeneric.dcl
	$(CLM) -ABC StdGeneric

Clean\ System\ Files/StdGeneric.o : StdGeneric.dcl Clean\ System\ Files/StdGeneric.abc
	$(CLM) -O StdGeneric

Clean\ System\ Files/_SystemStrictMaybes.abc : _SystemStrictMaybes.icl _SystemStrictMaybes.dcl
	$(CLM) -ABC _SystemStrictMaybes

Clean\ System\ Files/_SystemStrictMaybes.o : _SystemStrictMaybes.dcl Clean\ System\ Files/_SystemStrictMaybes.abc
	$(CLM) -O _SystemStrictMaybes

Clean\ System\ Files/_SystemDynamic.o : _SystemDynamic.icl _SystemDynamic.dcl
	$(CLM) -dynamics -O _SystemDynamic

Clean\ System\ Files/_SystemDynamic.abc : _SystemDynamic.icl _SystemDynamic.dcl
	$(CLM) -dynamics -ABC _SystemDynamic

Clean\ System\ Files/StdDynamic.o : StdDynamic.icl StdDynamic.dcl
	$(CLM) -dynamics -O StdDynamic

Clean\ System\ Files/StdDynamic.abc : StdDynamic.icl StdDynamic.dcl
	$(CLM) -dynamics -ABC StdDynamic

Clean\ System\ Files/_system.o : Clean\ System\ Files/_system.abc
	$(CLM) -O _system

# compile system modules twice, to make sure all inlining is done
system_abc: $(SYSTEM_MODULES_ABC) system_abc_touch

system_abc_touch:
	touch $(SYSTEM_MODULES_ICL)
	$(MAKE) system_abc_again

system_abc_again: $(SYSTEM_MODULES_ABC)
