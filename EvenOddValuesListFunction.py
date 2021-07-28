def is_list_even(my_list):
    
    for j in my_list:
        if j % 2 != 0:
            return False
    return True
    

def is_list_odd(my_list):
    for k in my_list:
        if k % 2 == 0:
            return False
    return True
    
    
if __name__ == '__main__': 
    first_value = int(input())
    my_list = []
    
    for i in range(first_value):
        my_values = int(input())
        my_list.append(my_values)
    
    if is_list_even(my_list):
        print('all even')
    elif is_list_odd(my_list):
        print('all odd')
    else:
        print('not even or odd')
    
    
    
    
