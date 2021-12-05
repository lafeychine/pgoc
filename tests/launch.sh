#!/usr/bin/env bash

CALL_PATH="$(dirname "$0")"

MSG_EXPECTED_COMPILER="== Expected compiler output =="
MSG_EXPECTED_TAST="== Expected TAST =="
PGOC="./pgoc --no-pretty"


# void launchTest(String filePath)
function launchTest()
{
    FILEPATH="${1%".go"}"

    echo "Launch ${FILEPATH#"${CALL_PATH}/"}:"

    if grep -q "${MSG_EXPECTED_COMPILER}" "$1"; then

        compareFiles "Compiler output" \
                     <(${PGOC} --type-only "$1" 2>&1) \
                     <(awk "/${MSG_EXPECTED_COMPILER}/,0" "$1" | awk "1;/*\//{exit}" | sed -e '1d;$d')

    elif grep -q "${MSG_EXPECTED_TAST}" "$1"; then

        compareFiles "No error" \
                     <(${PGOC} --debug --type-only "$1" 2>&1) \
                     <(echo -ne "")

        compareFiles "TAST output" \
                     "${FILEPATH}_tast.dot" \
                     <(awk "/${MSG_EXPECTED_TAST}/,0" "${FILEPATH}.go" | awk "1;/*\//{exit}" | sed -e '1d;$d')

        rm -f "${FILEPATH}_ast.dot"
        rm -f "${FILEPATH}_tast.dot"

    else

        compareFiles "No error" \
                     <(${PGOC} --type-only "$1" 2>&1) \
                     <(echo -ne "")

    fi

    echo ""
}


# void compareFiles(String msg, String file, String expectedFile)
function compareFiles()
{
    echo -ne " - $1: "

    if DIFF_OUTPUT=$(diff -Bw "$2" "$3"); then

        tput setaf 2; echo -n "OK"; tput setaf 7

    else

        tput setaf 1; echo -n "KO"; tput setaf 7

        echo ""
        echo "${DIFF_OUTPUT[0]}"

        FAILED_TEST=true

    fi

    echo ""
}


find "${CALL_PATH}" -name "*.go" -print0 |
    while IFS= read -r -d '' FILE; do

        launchTest "${FILE}"

        [[ -n ${FAILED_TEST} ]] && exit 1;

    done
