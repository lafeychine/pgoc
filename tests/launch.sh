#!/usr/bin/env bash

CALL_PATH="$(dirname "$0")"

MSG_EXPECTED_COMPILER="== Expected compiler output =="
MSG_EXPECTED_TAST="== Expected TAST =="
PGOC="./pgoc --no-pretty"


# void launchTest(String folderPath)
function launchTest()
{
    echo "Launch $1:"

    if [[ $(grep "${MSG_EXPECTED_COMPILER}" "./tests/$1.go") ]]
    then

        compareFiles "Compiler output" \
                     <(${PGOC} --type-only "./tests/$1.go" 2>&1) \
                     <(awk "/${MSG_EXPECTED_COMPILER}/,0" "${CALL_PATH}/$1.go" | awk "1;/*\//{exit}" | sed -e '1d;$d')

    fi

    if [[ $(grep "${MSG_EXPECTED_TAST}" "./tests/$1.go") ]]
    then

        compareFiles "No error" \
                     <(${PGOC} --debug --type-only "./tests/$1.go" 2>&1) \
                     <(echo -ne "")

        compareFiles "TAST output" \
                     "./tests/$1_tast.dot" \
                     <(awk "/${MSG_EXPECTED_TAST}/,0" "${CALL_PATH}/$1.go" | awk "1;/*\//{exit}" | sed -e '1d;$d')

        rm -f "./tests/$1_ast.dot"
        rm -f "./tests/$1_tast.dot"

    fi

    echo ""
}


# void compareFiles(String msg, String file, String expectedFile)
function compareFiles()
{
    echo -ne " - $1: "

    DIFF_OUTPUT=$(diff -Bw "$2" "$3")

    if [[ $? -eq 0 ]]; then

        tput setaf 2; echo -n "OK"; tput setaf 7

    else

        tput setaf 1; echo -n "KO"; tput setaf 7

        echo ""
        echo "${DIFF_OUTPUT[0]}"

    fi

    echo ""
}


launchTest "bad/fmt/unused_fmt"
launchTest "bad/fmt/unimported_fmt"

launchTest "bad/dot/non_identifier"
launchTest "bad/dot/nul_struct"
launchTest "bad/dot/on_non_struct"
launchTest "bad/dot/unknown_field_struct"

launchTest "bad/func/multiple_func_def"
launchTest "bad/func/multiple_param_func_def"
launchTest "bad/func/unknown_param_func_type"
launchTest "bad/func/unknown_return_value_func_type"

launchTest "bad/main/missing_main"
launchTest "bad/main/parameters_main"
launchTest "bad/main/return_value_main"

launchTest "bad/return/stmt_after_return"
launchTest "bad/return/stmt_after_return_block"
launchTest "bad/return/bad_return_type"

launchTest "bad/struct/multiple_struct_def"
launchTest "bad/struct/multiple_fields_struct_def"
launchTest "bad/struct/mutually_recursive_struct_def"
launchTest "bad/struct/recursive_struct_def"
launchTest "bad/struct/unknown_field_struct"

launchTest "bad/vars/unbound"
launchTest "bad/vars/unused"
launchTest "bad/vars/unused_block"


launchTest "good/print/string"

launchTest "good/vars/declare"
launchTest "good/vars/declare_struct"
