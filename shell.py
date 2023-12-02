import basic

while True:
    text = input('stdin >')
    result,error = basic.run('file',text)

    if error: print(error.as_string())
    else:print(result)

  
