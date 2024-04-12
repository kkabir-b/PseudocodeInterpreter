import pseudocode
#run function
def runPseudocode(f):
    f.replace('\n',';')
    _,error = pseudocode.run(fn,f)
    if error:print(error.as_string())

#simple script to run a file
fn = input('Enter file name(without .txt)->')
file = open(fn+'.txt','r').read()
runPseudocode(file)
