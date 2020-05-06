def main():

     data = "this is where I want to break"
     for char in data:
       if char!='b':
         print(char)
       else:
         print('else')
         break
     else:
       print('not for')

main()
