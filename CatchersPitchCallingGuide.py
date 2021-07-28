# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""
    

# first_input = int(input())

# input_list = first_input.split(' ')

# for i in input_list:
#     last_input = int(input())
#     if i == last_input:
#         print(input_list[i])


#Calls and Philosophies Program



phils_by_atbat = {1: 'FB heavy, start at-bats with FBs',
                  2: 'Begin to mix it up, hitters 1-4 go with offspeed first pitch or use mental note of who hit FB hard last time.',
                  3: 'Should have plenty of evidence as to which pitches were squared up and which were not.\nIf pitcher has a solid 3rd pitch, likely go with that at hitters 1-4 on first pitch.'
                  }

phils_by_count = {'0-0':"Can't go wrong with first pitch FB (but keep what at bat it is for that hitter in mind).",
                  '0-1':'Opposite of whatever pitch got you here, unless the batter has an obvious show (lost on CB, way late on FB).',
                  '0-2':'FB down and away (entice hitter to swing, so not a strike but close enough to be called).\nOther option is FB high and tight to back hitter off plate, setting up offspeed down/away.',
                  '1-0':'FB.',
                  '1-1':'FB or Offspeed if this will be 3rd FB in a row.',
                  '2-1':'FB or Offspeed (pending pitchers contorl of offspeed) if this will be 3rd FB in a row.',
                  '3-1':'FB or Offspeed (pending putchers control of offspeed) if this will be 3rd FB in a row,\nespecially if 1B is open with runners on base.',
                  '1-2':'Offspeed starts as strike, ends as ball in the dirt.',
                  '2-2':'FB in if worked away mostly, read swings in this at-bat.\nIf batter looked bad or crossed up prior, go back to that.',
                  '3-2':'FB or Offspeed (pending pitchers control of offspeed) if this will be 3rd FB in a row,\nespecially if 1B is open with runners on base.',
                  '2-0':'FB or Offspeed (pending pitchers control of offspeed.',
                  '3-0':'Whatever will result in a strike.'
                  }

print('Welcome to the Catchers Guide to Calling Pitches!\n')
print('The most important thing to remember is that FB location trumps everything else.\nKeep an eye on the batters feet, let that dictate your FB locations, otherwise safe to stay away and go in once it is set up.')

at_bat_prompt = int(input('Which at-bat is this for the hitter? '))
3

phils_atbat_values = list(phils_by_atbat.values())

if at_bat_prompt < 1:
    print('Please choose appropriate at-bat appearance')
elif at_bat_prompt == 1:
    print(phils_atbat_values[0])
elif at_bat_prompt == 2:
    print(phils_atbat_values[1])
else:
    print(phils_atbat_values[2])
    

for count in range(0, len(phils_by_count), 2):
    phils_by_count[count] = count

count_prompt = input('What is the count? Ex 0-2: ')    
    
print(phils_by_count[count_prompt])
    
    



