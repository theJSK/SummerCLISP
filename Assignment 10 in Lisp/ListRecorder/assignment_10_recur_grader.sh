score=0
printf "====================\n"
printf " AUTOGRADER RESULTS\n"
printf "====================\n"
printf " ASSIGNMENT 10\n"
printf "====================\n\n"

printf "==========\n"
printf "PART 1:\n"
printf "(10 points) Checking to see if a file called recursive_list_print.lisp was submitted\n"
printf "==========\n"
if [ -e recursive_list_print.lisp ]
then
    printf "recursive_list_print.lisp exists! +10\n"
    let score+=10
else
    printf "Could not find recursive_list_print.lisp.  It wasn't submitted?\n"
fi

printf "\n\n==========\n"
printf "PART 2:\n"
printf "(20 points) Trying to compile recursive_list_print.lisp\n"
printf "==========\n"

echo clisp -c recursive_list_print.lisp
clisp -c recursive_list_print.lisp

if [ $? -eq 0 ]
then
    printf "Compile was successful.  +20\n"
    let score+=20
else
    printf "Compile was not successful.\n"
fi

violations=0

printf "\n\n==========\n"
printf "PART 3a:\n"
printf "Checking for the number of calls to setq.\n"
printf "You should not have more than 5 calls to setq.\n"
printf "==========\n"

numberofequals=`grep -c -e 'setq' recursive_list_print.lisp`

if [ $numberofequals -gt 6 ]
then
    printf "You have too many calls to setq in your file!!!  Automatic zero!\n"
    printf "$numberofequals calls to setq found at these locations:\n\n"
    grep -n -C 1 -e 'setq' recursive_list_print.lisp
    score=0
    violations=1
else
    printf "Required number of calls to setq found.\n\n"
fi

printf "\n\n==========\n"
printf "PART 3b:\n"
printf "Checking for the number of calls to format.\n"
printf "These should not appear in your program more than four times.\n"
printf "==========\n"

numberofprint=`grep -c -e 'format' recursive_list_print.lisp`

if [ $numberofprint -gt 4 ]
then
    printf "You have too many calls to format in your file!!!  Automatic zero!\n"
    printf "$numberofprint calls found at these locations:\n\n"
    grep -n -C 1 -e 'format' recursive_list_print.lisp
    score=0
    violations=1
else
    printf "Required number of calls to format found.\n\n"
fi

if [ $violations -eq 0 ]
then
    printf "\n\n==========\n"
    printf "PART 4:\n"
    printf "(20 points) Trying to run recursive_list_print.lisp on some inputs to see if it runs. \n"
    printf "==========\n"

    printf "Program output was:\n"
    clisp list_recorder.fas <<STDIN
1 2 37 8 9 208 9 0 8 -20
STDIN

    if [ $? -eq 0 ]
    then
	printf "recursive_list_print.lisp ran successfully.  +20\n"
	let score+=20
    else
	printf "recursive_list_print.lisp did not run successfully.\n"
    fi

    printf "\n\n==========\n"
    printf "PART 5:\n"
    printf "(50 points) Running recursive_list_print.lisp on 50 test cases.\n"
    printf "==========\n"

    while read list
    do
	submission_answer=`clisp list_recorder.fas <<STDIN | tail -n 1
$list
STDIN`
	echo "(1 point) testing that inputs ${list} returned \"${list}\""
	#putting the input through xargs deals with all of the whitespace issues
	sub_minus_whitespace=`xargs<<<"$submission_answer"`
	if [[ "$list" == "$sub_minus_whitespace" ]]
	then
	    printf "\t\t\t... your program returned \"$sub_minus_whitespace\". correct! +1\n"
	    let score+=1   
	else
	    printf "\t\t\t... your program returned \"$sub_minus_whitespace\". incorrect! +0\n"
	fi
    done < test_cases.csv
fi
    
printf "\n\n====================\n"
printf "Final score: ${score} out of 100\n"
printf "====================\n\n"

echo $score > points.txt


printf "\n\n============================================================\n"
printf "============================================================\n"

printf "\n\n====================\n"
printf "Text of submitted files\n"
printf "====================\n"
printf "recursive_list_print.lisp:\n\n"

cat recursive_list_print.lisp

