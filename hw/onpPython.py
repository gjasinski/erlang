def rpn(lst):
    """ Function used to evaluate expression using Reverse Polish Notation"""

    exp = lst.split(' ')
    stack = []
    for symbol in exp:
        if symbol.isdigit():
            stack.append(symbol)
        else:
            b = stack.pop()
            a = stack.pop()
            if symbol == '+':
                stack.append(float(a) + float(b))
            elif symbol == '-':
                stack.append(float(a) - float(b))
            elif symbol == '*':
                stack.append(float(a) * float(b))
            else:
                stack.append(float(a) / float(b))

    return stack[0]

#testing!
test_str = '12 2 3 4 * 10 5 / + * +'
print rpn(test_str)