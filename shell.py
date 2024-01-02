import basic


while True:
    text = input('stdin >')
    result,error = basic.run('Standard input',text)
    if error: print(error.as_string())
    elif result:print(result)
