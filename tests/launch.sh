#!/usr/bin/env bash

CALL_PATH="$(dirname "$0")"

MAGIC_MSG="== Expected compiler output =="
PGOC="./pgoc"


# void launchTest(String folderPath)
function launchTest()
{
    echo -n "Launch $1: "

    compareFiles <(${PGOC} "./tests/$1.go" 2>&1) \
    		 <(awk "/${MAGIC_MSG}/,0" "${CALL_PATH}/$1.go" | sed -e '1d;$d')

    echo ""
}


# void compareFiles(String file, String expectedFile)
function compareFiles()
{
    DIFF_OUTPUT=$(diff -Bw "$1" "$2")

    if [[ $? -eq 0 ]]; then

        tput setaf 2; echo -n "OK"; tput setaf 7

    else

        tput setaf 1; echo -n "KO"; tput setaf 7

	echo ""
	echo "${DIFF_OUTPUT[0]}"

    fi
}


launchTest "bad/func/multiple_func_def"
launchTest "bad/func/multiple_param_func_def"
launchTest "bad/func/unknown_param_func_type"

launchTest "bad/main/missing_main"
launchTest "bad/main/parameters_main"
launchTest "bad/main/return_value_main"

launchTest "bad/struct/multiple_struct_def"
