'''
Created on Feb 28, 2018

@author: Supratik Neupane
person number: 50160008
'''

# static integers that keep track of the data types
any = 0
integer = 1
string = 2
name = 3
boolean = 4
error = 5
unit = 6  # unit =["unit",unit,varname,val/arg,perv type/indices]
fun = 7


# check if the given input can be changed to an integer
def isInteger(x):
    try:
        return float(x).is_integer()
    except ValueError:
        return False    

                
# check if the given input is a string
def isString(x):
    length = len(x)
    return x[0] == '\"' and x[length - 1] == '\"'


# check if the given input is a boolean or error
def isBoolean(x):
    return x == ":true:" or x == ":false:"  # or x==":error:"


# check is the given input is a valid variable name
def isName(x):
    return not(isInteger(x[0])) and not('+' in x) and not ('-' in x)


# append the three elems back and error
def appendBackErrorTopZ(top, next, z, stack):
    stack.append(z)
    stack.append(next)
    stack.append(top)
    stack.append([":error:", error])
     
    
# append the top element back and error
def appendBackErrorTop(top, stack):
    stack.append(top)
    stack.append([":error:", error])


# append the two elements back and append error to the stack 
def appendBackError(top, next, stack):
    stack.append(next)
    stack.append(top)
    stack.append([":error:", error])


def checkFunction(top, functionmap):
    if(top[1] == unit):  # possible mistake here
        if((type == any or top[4] == type)):
            top = [top[3], top[4]]
            
    if(top[1] == name):
        if(top[0] in functionmap):
            top = [top[0], fun]
            
    return top        


# check Element
def checkElement(top, type, bindmap):
            
    if(top[1] == name):
        if(top[0] in bindmap):
            if(type == any or bindmap[top[0]][1] == type):
                top = bindmap[top[0]]
    
    return top   
    

# compute the result for the arithmetic operation       
def arithmetic(command, stack, bindmap):
   
    length = len(stack)
    # less than 2 elements in the stack then error
    if(length < 2):
        stack.append([":error:", error])
        return
    top = stack.pop()  # top most element in the stack
    next = stack.pop()  # the next element after the topmost element
    # no type check required for 
    if(command == "swap"):
        stack.append(top)
        stack.append(next)
        return
    
    newTop = checkElement(top, integer, bindmap)
    newNext = checkElement(next, integer, bindmap)
    
    # type check if the two elements are indeed integers, put error on the stack otherwise
    if (newTop[1] != integer or newNext[1] != integer):
        appendBackError(top, next, stack)
        return

    y = int(newTop[0])
    x = int(newNext[0])
    res = 0
    # add
    if(command == "add"):
        res = x + y
    # subtract    
    elif(command == "sub"):
        res = x - y 
    # multiply      
    elif(command == "mul"):
        res = x * y   
    # remainder    
    elif(command == "rem"):
        # mod by zero
        if(y == 0):
            appendBackError(top, next, stack)
            return
        else:
            res = int(x % y)
    # divide    
    elif(command == "div"):
        # divide by zero
        if(y == 0):
            appendBackError(top, next, stack)
            return
        else:
            res = int(x / y)
    else:
        appendBackError(top, next, stack)
        return
            
    stack.append([res, integer])

    
# output everything in the stack to the file    
def outputToFile(output, stack):
    file = open(output, "w")
    for elem in reversed(stack):
        
        file.write(str(elem[0]) + "\n")
        
    file.close()
    
    
# join string    
def stringCat(stack, bindmap):
    if (len(stack) < 2):
        stack.append([":error:", error])
        return
    
    top = stack.pop()
    next = stack.pop()
    
    newTop = checkElement(top, string, bindmap)
    newNext = checkElement(next, string, bindmap)
    
    if(newTop[1] != string or newNext[1] != string):
        appendBackError(top, next, stack)
        return
    
    stack.append([newNext[0] + newTop[0], string])
    
           
# boolean and          
def performAnd(stack, bindmap):
    if(len(stack) < 2):
        stack.append([":error:", error])
        return
    
    top = stack.pop()
    next = stack.pop()
    
    newTop = checkElement(top, boolean, bindmap)
    newNext = checkElement(next, boolean, bindmap)
    
    if(newTop[1] != boolean or newNext[1] != boolean):
        stack.append(next)
        stack.append(top)
        stack.append([":error:", error])
        return
    
    res = ""
    
    if(newTop[0] == ":true:" and newNext[0] == ":true:"):
        res = ":true:"    
    else:
        res = ":false:"
    
    stack.append([res, boolean])                


# boolean or
def performOr(stack, bindmap):
    if(len(stack) < 2):
        stack.append([":error:", error])
        return
    
    top = stack.pop()
    next = stack.pop()
    
    newTop = checkElement(top, boolean, bindmap)
    newNext = checkElement(next, boolean, bindmap)
    
    if(newTop[1] != boolean or newNext[1] != boolean):
        stack.append(next)
        stack.append(top)
        stack.append([":error:", error])
        return
    
    res = ""
    
    if(newTop[0] == ":false:" and newNext[0] == ":false:"):
        res = ":false:"    
    else:
        res = ":true:"
    
    stack.append([res, boolean])                   


# boolean not
def performNot(stack, bindmap):
    if(len(stack) < 1):
        stack.append([":error:", error])
        return
    
    top = stack.pop()
    
    newTop = checkElement(top, boolean, bindmap)
    
    if(newTop[1] != boolean):
        
        stack.append(top)
        stack.append([":error:", error])
        return
    
    res = ""
    
    if(newTop[0] == ":false:"):
        res = ":true:"    
    else:
        res = ":false:"
    
    stack.append([res, boolean])                    


# boolean equals
def performEquals(stack, bindmap):
    if(len(stack) < 2):
        stack.append([":error:", error])
        return
    
    top = stack.pop()
    next = stack.pop()
    
    newTop = checkElement(top, integer, bindmap)
    newNext = checkElement(next, integer, bindmap)
    
    if(newTop[1] != integer or newNext[1] != integer):
        stack.append(next)
        stack.append(top)
        stack.append([":error:", error])
        return
    
    res = ""
    
    if(int(newTop[0]) == int(newNext[0])):
        res = ":true:"    
    else:
        res = ":false:"
    
    stack.append([res, boolean])         

                 
# boolean lessThan
def performLessThan(stack, bindmap):
    if(len(stack) < 2):
        stack.append([":error:", error])
        return
    
    top = stack.pop()
    next = stack.pop()
    
    newTop = checkElement(top, integer, bindmap)
    newNext = checkElement(next, integer, bindmap)
    
    if(newTop[1] != integer or newNext[1] != integer):
        stack.append(next)
        stack.append(top)
        stack.append([":error:", error])
        return
    
    res = ""
    
    if(int(newNext[0]) < int(newTop[0])):
        res = ":true:"    
    else:
        res = ":false:"
    
    stack.append([res, boolean])   

    
# if operation
def performIf(stack, bindmap):
    if(len(stack) < 3):
        stack.append([":error:", error])
        return
    
    top = stack.pop()
    next = stack.pop()
    z = stack.pop()
    
    newZ = checkElement(z, boolean, bindmap)
    
    if(newZ[1] != boolean):
        stack.append(next)
        stack.append(top)
        stack.append([":error:", error])
        return  
     
    if(newZ[0] == ":true:"):
        stack.append(next)   
    else:
        stack.append(top)
    
                   
def performBind(stack, bindmap):
    if (len(stack) < 2):
        stack.append([":error:", error])
        return
    
    top = stack.pop()
    next = stack.pop()
    val = top
    
    if(top[1] == error or next[1] != name):
        stack.append(next)
        stack.append(top)
        stack.append([":error:", error])
        return
    # double check unit
    
    if(top[1] == name):
        if(top[0] in bindmap):
            val = bindmap[top[0]]
            stack.append([":unit:", unit, next[0], val[0], val[1]])    
    
        else:
            stack.append(next)
            stack.append(top)
            stack.append([":error:", error])
            return
    else:
        
        stack.append([":unit:", unit, next[0], val[0], val[1]])
        
    bindmap[next[0]] = val
         
        
# find the matching end to a let       
def findEnd(start, end, list):
    stack = [1]
    start += 1;
    while(start < end):
        if(list[start][0] == "let"):
            stack.append(1)
            
        if(list[start][0] == "end"):
            stack.pop()
            if(len(stack) == 0):
                return start    
            
        start += 1     
           
    return -1


# all the let scoping
def performLet(output, start, end, list, stack, bindmap, functionmap):
    newfunctionmap = functionmap.copy()
   
    interpret(output, list, stack, bindmap, start, end, newfunctionmap)
    
    if(len(stack) == 0):
        return []
    return checkFunction(checkElement(stack.pop(), any, bindmap), functionmap)


# hopefully fix negation
def negation(stack, bindmap):
    if(len(stack) < 1):
        stack.append([":error:", error])
        return
    
    top = stack.pop()
    
    newTop = checkElement(top, integer, bindmap)
    
    if(newTop[1] != integer):
        
        stack.append(top)
        stack.append([":error:", error])
        return
    
    stack.append([str(-int(newTop[0])), integer])


# returns the starting and ending index of a function call    
def functionList(list, start, end):
    
    begin = start
    counter = 0
    while(start != end):
        instruction = list[start]
        if(instruction[0] == "fun" or instruction[0] == "inOutFun"):
            counter += 1
        elif(instruction[0] == "funEnd"):
            if(counter == 0):
                return [begin, start]
            else:
                counter -= 1
        start += 1      
        
    return [begin, start]    


# main part of phase 3
def performFunctionCall(output, list, stack, bindmap, functionmap):
    if(len(stack) < 2):
        stack.append([":error:", error])
        return
    
    top = stack.pop()
    next = stack.pop()
    
    t = checkFunction(checkElement(top, any, bindmap), functionmap)
    
    newnext = checkElement(next, fun, bindmap)
    
    if((newnext[0] not in functionmap) or (t[1] == error) or (t[1] == name and t[0] not in bindmap)):
        appendBackError(top, next, stack)
        return
    # make a copy of the binding map to restore the environment
    newbindmap = functionmap[newnext[0]][3].copy()
    
    # bound the argument variable name to the value in the new environment 
        
    newbindmap[functionmap[newnext[0]][0]] = t
    # make a new stack for the current environment
    newStack = []
    # make a new function map for the current environment
    newfunctionmap = functionmap.copy()
    
    # starting and ending index
    start = functionmap[newnext[0]][1][0]
    end = functionmap[newnext[0]][1][1]
    
    # perform the interpretation of the function
    append = interpret(output, list, newStack, newbindmap, start, end, newfunctionmap)
    
    elem = newStack.pop()
    
    # if the function had a return then append to the stack
    if (append == 1):
        if((elem[0] in newbindmap) or (elem[0] in newfunctionmap)):
            elem = checkFunction(checkElement(elem, any, newbindmap), newfunctionmap)
        
        stack.append(elem)    
    
    # possibly add the function to the function map
    if(elem[1] == fun):
        functionmap[elem[0]] = newfunctionmap[elem[0]]
  
    # change the binding for the argument if an in out function
    if(functionmap[newnext[0]][2]):
        bindmap[top[0]] = newbindmap[functionmap[newnext[0]][0]] 
        
        
# start performing the interpretation        
def interpret(output, list, stack, bindmap, start, end, functionmap):
    
    while (start != end):
        instruction = list[start]
        length = len(instruction)
        if(length == 3):
            if(instruction[0] == "fun"):
                    
                start += 1
                funclist = functionList(list, start, end)
                start = funclist[1]
                functionmap[instruction[1]] = [instruction[2], funclist, False, bindmap.copy()]  # functionmap = arg,indices,isInOut
                stack.append([":unit:", fun, instruction[1], [instruction[2], funclist], fun])  
                
            elif(instruction[0] == "inOutFun"):
                    
                start += 1
                funclist = functionList(list, start, end)
                start = funclist[1]
                functionmap[instruction[1]] = [instruction[2], funclist, True, bindmap.copy()]  # functionmap = arg,indices,isInOut
                stack.append([":unit:", fun, instruction[1], [instruction[2], funclist], fun])       
                
            else:
                stack.append([":error:", error])   
            
        elif(length == 2):
            # push 
            if (instruction[0] == "push"):
                elem = []
                if(not(isInteger(instruction[1])) and (isString(instruction[1]))):
                    for i in range(2):
                        instruction[1] = instruction[1].replace('"', '')
                    elem = [instruction[1], string]   
                     
                elif(isBoolean(instruction[1])):
                    elem = [instruction[1], boolean]
                    
                elif(':' in instruction[1] and isBoolean(instruction[1]) == False):
                    elem = [":error:", error]
                    
                elif(isInteger(instruction[1])):
                    stack.append([str(int(instruction[1])), integer])
                    start += 1
                    continue
                
                elif ((',' in instruction[1]) or ('"' in instruction[1]) or(' ' in instruction[1]) 
                       or (';' in instruction[1])):
                    elem = [":error:", error]
                
                elif(isName(instruction[1])):
                        elem = [instruction[1], name]
                        
                elif(isName(instruction[1]) == False):
                    elem = [":error:", error]
                    
                stack.append(elem)
                        
        elif(length == 1):
            # phase 3
            if(instruction[0] == "call"):
                performFunctionCall(output, list, stack, bindmap, functionmap)    
                
            elif(instruction[0] == "return"):
                return 1
            
            # pop command
            elif(instruction[0] == "pop"):
                if(len(stack) >= 1):
                    stack.pop()
                    
                else:
                    stack.append([":error:", error])
            # negation command    
            elif(instruction[0] == "neg"):
                negation(stack, bindmap)
            # if command is quit, write to output file and close the program        
            elif(instruction[0] == "quit"):
                outputToFile(output, stack)
                return 0
            
            # other operations not part of phase1
            elif(instruction[0] == "cat"):
                stringCat(stack, bindmap)
                
            elif(instruction[0] == "and") :
                performAnd(stack, bindmap)
                
            elif(instruction[0] == "or") :
                performOr(stack, bindmap)
                
            elif(instruction[0] == "not") :
                performNot(stack, bindmap)
                
            elif(instruction[0] == "equal") :
                performEquals(stack, bindmap)
                
            elif(instruction[0] == "lessThan") :
                performLessThan(stack, bindmap)
                
            elif(instruction[0] == "if") :
                performIf(stack, bindmap)    
            
            elif(instruction[0] == "bind"):
                performBind(stack, bindmap)    
                  
            elif(instruction[0] == "end"):
                stack.append([":error:", error])   
                
            elif(instruction[0] == "let"):
                letEnd = findEnd(start, end, list)        
                if(letEnd == (-1)):
                    stack.append([":error:", error]) 
                else:
                    oldMap = bindmap.copy()  # possibly optimize here
                    val = performLet(output, start + 1, letEnd, list, [], oldMap, functionmap)
                    if(val != []):
                        stack.append(val)  # double check this bitch     
                    start = letEnd 
                                    
            else:
                # perform arithmetic operations from phase 1
                arithmetic(instruction[0], stack, bindmap)    
        else:
            # put error on the stack for anything else
            stack.append([":error:", error])        
    
        start += 1    
    
    return 0

    
# function for the assignment - converts the input file into a list of commands                      
def interpreter(input, output):
    file1 = open(input, "r")
    
    list = []  # store everything from the input file
    stack = []  # main stack that stores the results of commands
    
    for line in file1:
        # remove the trailing newline character
        line = line.rstrip()
        try:
            # for two words in the line
            lineArr = []
            if(len(line) > 3 and (line[0:4]) == "push"):
                lineArr = line.split(" ", 1)
                
            else:
                lineArr = line.split(" ")
            
            list.append(lineArr)
            # for single word in the line
        except AttributeError:
            arr = {line}
            list.append(arr)
    # close the reading file after done reading     
    file1.close()
    functionmap = {}
    bindmap = {}  # map that stores the variable names and their values
    
    interpret(output, list, stack, bindmap, 0, len(list), functionmap)
    
    
#interpreter("input.txt", "output.txt")
