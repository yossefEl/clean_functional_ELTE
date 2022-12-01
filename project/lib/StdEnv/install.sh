
set -e

./make.sh

CALL_CP="cp -p"
CALL_CHMOD="chmod 644"

CLEAN_MODULES="StdArray StdCharList StdClass StdDebug StdFunc StdFunctions StdList StdOrdList \
		StdOverloaded StdTuple _SystemEnum StdEnum _SystemArray StdEnv _SystemStrictLists \
		_SystemEnumStrict StdStrictLists StdOverloadedList StdGeneric StdMaybe _SystemStrictMaybes \
		_SystemDynamic StdDynamic"

SYSTEM_MODULES="StdBool StdChar StdFile StdInt StdMisc StdReal StdString"

ABC_MODULES="_system"

OBJECT_MODULES="_startup _startupProfile _startupTrace"

mkdir -p $INSTALL_STDENV_DIR

mkdir -p $INSTALL_STDENV_DIR/"Clean System Files"

for M in $CLEAN_MODULES; do
	$CALL_CP $M.icl $INSTALL_STDENV_DIR/$M.icl
	$CALL_CHMOD $INSTALL_STDENV_DIR/$M.icl
done

for M in $CLEAN_MODULES $SYSTEM_MODULES $ABC_MODULES $OBJECT_MODULES; do
	$CALL_CP $M.dcl $INSTALL_STDENV_DIR/$M.dcl
	$CALL_CHMOD $INSTALL_STDENV_DIR/$M.dcl
done

for M in $SYSTEM_MODULES $CLEAN_MODULES $ABC_MODULES; do
	$CALL_CP "Clean System Files"/$M.abc $INSTALL_STDENV_DIR/"Clean System Files"/$M.abc
	$CALL_CHMOD $INSTALL_STDENV_DIR/"Clean System Files"/$M.abc
done

sleep 1

for M in $SYSTEM_MODULES $CLEAN_MODULES $ABC_MODULES $OBJECT_MODULES; do
	$CALL_CP "Clean System Files"/$M.o $INSTALL_STDENV_DIR/"Clean System Files"/$M.o
	$CALL_CHMOD $INSTALL_STDENV_DIR/"Clean System Files"/$M.o
done

