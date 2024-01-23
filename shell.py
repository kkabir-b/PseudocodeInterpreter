import pseudocode
# while True:
#     text = input('stdin >')
#     result,error = basic.run('Standard input',text)
#     if error: print(error.as_string())
#     elif result:
#         if len(result.elements) == 1:
#             print(repr(result.elements[0]))
#         else:
#             print(repr(result))
fn = input('Enter file name->')
f = open(fn+'.txt','r').read()
_,error = basic.run(fn,f)
if error:print(error.as_string())
