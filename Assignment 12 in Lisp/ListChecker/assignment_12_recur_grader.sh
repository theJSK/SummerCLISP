score=0
printf "====================\n"
printf " AUTOGRADER RESULTS\n"
printf "====================\n"
printf " ASSIGNMENT 12\n"
printf "====================\n\n"

printf "==========\n"
printf "PART 1:\n"
printf "(5 points) Checking to see if a file called recur_list_checker.lisp was submitted\n"
printf "==========\n"
if [ -e recur_list_checker.lisp ]
then
    printf "recur_list_checker.lisp exists! +5\n"
    let score+=5
else
    printf "Could not find recur_list_checker.lisp.  It wasn't submitted?\n"
fi

printf "\n\n==========\n"
printf "PART 2:\n"
printf "(10 points) Trying to compile recur_list_checker.lisp\n"
printf "==========\n"

echo clisp -c recur_list_checker.lisp
clisp -c recur_list_checker.lisp

if [ $? -eq 0 ]
then
    printf "Compile was successful.  +10\n"
    let score+=10
else
    printf "Compile was not successful.\n"
fi

printf "\n\n==========\n"
printf "PART 4:\n"
printf "(10 points) Trying to run recur_list_checker.lisp on some inputs to see if it runs. \n"
printf "==========\n"

printf "Program output was:\n"
clisp list_checker.fas <<STDIN
1 2 37 8 9 208 9 0 8 -20
STDIN

if [ $? -eq 0 ]
then
    printf "recur_list_checker.lisp ran successfully.  +10\n"
    let score+=10
else
    printf "recur_list_checker.lisp did not run successfully.\n"
fi

printf "\n\n==========\n"
printf "PART 5:\n"
printf "(75 points) Running recur_list_checker.lisp on 75 test cases.\n"
printf "==========\n"

while read list
do
    submission_answer=`clisp list_checker.fas <<STDIN | tail -n 1 | tail -c 1000
$list
STDIN`
    read correct_answer
    echo "(1 point) testing that inputs ${list} returned \"${correct_answer}\""
    if [[ "$correct_answer" == "$submission_answer" ]]
    then
	printf "\t\t\t... your program returned \"$submission_answer\". correct! +1\n"
	let score+=1   
    else
	printf "\t\t\t... your program returned \"$submission_answer\". incorrect! +0\n"
    fi
done < test_cases.csv

    
printf "\n\n====================\n"
printf "Final score: ${score} out of 100\n"
printf "====================\n\n"

echo $score > points.txt

printf "\n\n============================================================\n"
printf "============================================================\n"

printf "\n\n====================\n"
printf "Text of submitted files\n"
printf "====================\n"
printf "recur_list_checker.lisp:\n\n"

cat recur_list_checker.lisp

