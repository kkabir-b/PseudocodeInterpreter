# PseudocodeInterpreter
An interpreter used for CAIE pseudocode written in python

## Grammar Rules
This interpreter makes use of the CAIE pseudocode IGCSE and A-Level syntax, however declarations of variables is neither required not supported.

Currently it supports:
- Normal arithmetic operations(Including addition, subtraction, multiplication, division, and powers)
- Data types: Integers, Real, Strings, and Booleans(Booleans are represented and used internally using the Integer values 1 and 0)
- Iterative loops including For loop, While loops, and Repeat until loops
- Subroutines including functions(supporting return statements) and procedures
- Selection statements including if-else and case-of statements

More information can be found in grammar.txt
  
## Examples
- Example 1(for arthimetic operations and boolean) in Example1.txt: Returns output for Arithmetic operations and data types
- Example 2(for iterative loops) in Example2.txt: Showcases funcionality of for, while, and repeat until loops
- Example 3(for subroutines) in Example3.txt: Showcases the use of procedures and functions
- Example 4(for selection statements) in Example4.txt: Showcases the use of if-else and case-of statements

## How to run
You are able to run it through the use of the runPseudocode function in shell.py which takes in the file in string format. 

Shell.py comes included with a simple input command to take in a file name and open it and run it through the runPseudocode function. 
You are able to run the examples included by entering their name(such as 'Example1') into the prompt or create your own file in the local directory and run it through that prompt. 

## BuiltIn Functions
The BuiltIn functions are based upon the CAIE insert.

Currently these builtin functions include: OUTPUT, MID, LEFT, LENGTH, ASC and CHR


## Additional notes
The interpreter supports the use of semi-colons(;) as newlines while semi-colons are not recognised by CAIE 
A version of the grammar rules are shown in grammar.txt
