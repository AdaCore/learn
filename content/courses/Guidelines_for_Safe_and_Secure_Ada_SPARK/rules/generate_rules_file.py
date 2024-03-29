import argparse
import os

RULE   = "---"
HEADER = "==="

global_file = None
global_blank_lines = 0

def write ( text ):
    global global_file
    global global_blank_lines
    if len(text) > 0:
        global_blank_lines = 0
        global_file.write ( text + '\n' )
    else:
        global_blank_lines = global_blank_lines + 1
        if global_blank_lines < 2:
            global_file.write ( '\n' )

def list_text ( items ):
    retval = ""
    for item in items:
        if len(retval) > 0:
            retval = retval + ", "
        retval = retval + item
    return retval

def camel_case ( text ):
    retval = ''
    if len(text) <= 1:
        retval = text.upper()
    else:
        retval = text[0].upper()
        for i in range(1,len(text)):
            if text[i-1] == ' ':
                retval = retval + text[i].upper()
            else:
                retval = retval + text[i]
    return retval

'''
Assumes format for rule:
  :rule:`<rule>`
(role does not have to be alone on the line)
'''
def print_rule ( lines, detail, id, name ):
    rule = ""
    for line in lines:
        location = line.find(':rule:')
        if location >= 0:
            pieces = line[location:].split('`')
            if len(pieces) > 1:
                rule = pieces[1]
    if detail != 'quiet':
        write ( '-- ' + id + ' ' + name )
    if len(rule) > 0:
        write ( '+R' + rule )
    elif detail != 'quiet':
        write ( '-- (no GNATcheck rule)')

'''
Assumes format for level:
  **Level** :math:`\rightarrow` Required
(everything after the last space is the level)
'''
def print_level ( lines ):
    for line in lines:
        if '**Level**' in line:
            level = line[line.rindex(' '):].replace('*','').strip()
            write ( '-- Level: ' + level )

def print_collection ( title, values, lines ):
    to_print = []
    for value in values:
        key = ':' + value + ':'
        for line in lines:
            if (key in line) and ('checkmark') in line:
                to_print.append ( value )
    if len(to_print) > 0:
        write ('-- ' + title)
        for one in to_print:
            write ('--   ' + one)

def print_categories ( lines ):
    values = ['Safety', 'Cyber']
    print_collection ( 'Category', values, lines )

def print_goals ( lines ):
    values = [ 'Maintainability',
               'Reliability',
               'Portability',
               'Performance',
               'Security']
    print_collection ( 'Goal', values, lines )

def process_rule ( lines, detail ):
    id_start = lines[1].index('(')
    id = f = lines[1][id_start+1:lines[1].index(')')]
    name = lines[1][:id_start].strip()
    print_rule ( lines[2:], detail, id, name )
    if detail != 'quiet' and detail != 'short':
        print_level ( lines )
        print_categories ( lines )
        print_goals ( lines )
        write('')
    if detail != 'quiet':
        write('')
            
def print_header ( title, want_header ):
    if want_header and len(title) > 0:
        sep = '-'.ljust(len(title),'-')
        write ( "\n---" + sep + "---" )
        write ( "-- " + title + " --" )
        write ( "---" + sep + "---" )
        write ( '' )
    return ''

def process_one_file ( in_filename, detail, title ):

    header = title
    with open ( in_filename ) as f:
        lines = f.read().splitlines()

    start_of_rule = -1

    for i in range(1,len(lines)-1):
        line = lines[i].strip()
        if line.startswith ( RULE ):
            if start_of_rule < 0:
                start_of_rule = i
            elif i - start_of_rule > 3:
                header = print_header ( header, detail != 'quiet' )
                process_rule ( lines[start_of_rule:i], detail )
                start_of_rule = i
    if start_of_rule > 1:
        header = print_header ( header, detail != 'quiet' )
        process_rule ( lines[start_of_rule-2:len(lines)], detail )

    return header

def process_one_directory ( dir, detail ):
    dirname = camel_case ( os.path.basename ( os.path.normpath ( dir ) ).replace('_',' ') )

    for root, dirs, files in os.walk ( dir ):
        for file in files:
            if file.lower().endswith('.rst'):
                dirname = process_one_file ( os.path.join ( root, file ), detail, dirname )
        if detail != 'quiet' and len(dirname) == 0:
            write ( '' )
            write ( '' )

def process_source ( source, output, detail ):
    global global_file

    global_file = open ( args.output, 'w' )

    for something in os.listdir ( args.source ):
        full_path = os.path.join ( args.source, something )
        if os.path.isdir ( full_path ):
            process_one_directory ( full_path, detail )


if __name__== "__main__":

    parser = argparse.ArgumentParser(
        description='Use coding standards to generate a rules file for using with "gnatcheck"')

    parser.add_argument('--source',
                        help='Directory containing source files for guidelines (will be searched recursively)',
                        required=True)

    parser.add_argument('--output',
                        help='Filename for rules file',
                        required=True)

    parser.add_argument('--detail',
                        help='quiet | short | full => just rule | name and rule | name, rule, info',
                        default='full')
                        
    args = parser.parse_args()

    process_source ( args.source, args.output, args.detail.lower() )
