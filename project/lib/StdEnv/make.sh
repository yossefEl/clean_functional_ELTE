# before calling the script define: INSTALL_STDENV_DIR CLEANLIB CLEANPATH CLMP CLMFLAGS

set -e

CLEAN_MODULES="StdArray StdCharList StdClass StdDebug StdFunc StdFunctions StdList StdOrdList \
		StdOverloaded StdTuple _SystemEnum StdEnum _SystemArray StdEnv _SystemStrictLists \
		_SystemEnumStrict StdStrictLists StdOverloadedList StdGeneric StdMaybe _SystemStrictMaybes"

CLEAN_DYNAMIC_MODULES="_SystemDynamic StdDynamic"

SYSTEM_MODULES="StdBool StdChar StdFile StdInt StdMisc StdReal StdString"

ABC_MODULES="_system"

CALL_CLM="$CLMP -P . $CLMFLAGS"
CALL_CLM_PT="$CLMP -P . -pt $CLMFLAGS"

for M in $SYSTEM_MODULES ; do
	$CALL_CLM_PT -ABC $M
done

# and again for inlining

for M in $SYSTEM_MODULES ; do
	rm "Clean System Files"/$M.abc
	$CALL_CLM_PT -ABC $M
done

for M in $CLEAN_DYNAMIC_MODULES; do
	$CALL_CLM -dynamics -ABC $M
done

for M in $CLEAN_MODULES $ABC_MODULES; do
	$CALL_CLM -ABC $M
done

sleep 1

for M in $CLEAN_DYNAMIC_MODULES; do
	$CALL_CLM -dynamics -O $M
done

for M in $SYSTEM_MODULES $CLEAN_MODULES $ABC_MODULES; do
	$CALL_CLM -O $M
done

