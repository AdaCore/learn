import argparse
import os

RULE   = "---"
HEADER = "==="

global_file = None

def write ( text ):
    global global_file
    global_file.write ( text + '\n' )

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

def find_value ( lines, key ):
    for line in lines:
        if key in line:
            return line[line.rindex(' '):].replace('*','').strip()
    return ''

def find_values ( lines, key ):
    start_looking = False
    retval = ''

    for line in lines:
        if key in line:
            start_looking = True
        elif start_looking:
            l = line.strip()
            if len(l) <= 1:
                break
            first = l.find(':')
            last = l.find(':', first+1)
            if first >= 0 and last > first:
                if len(retval) > 0:
                    retval = retval + ', '
                retval = retval + l[first+1:last]
    return retval

def process_rule ( lines, detail ):
    id_start = lines[1].index('(')
    id = f = lines[1][id_start+1:lines[1].index(')')]
    name = lines[1][:id_start].strip()
    rule = find_value ( lines[2:], 'GNATcheck Rule' )
    if detail != 'quiet':
        write ( '-- ' + id + ' ' + name )
    write ( '+R' + rule )
    if detail != 'quiet' and detail != 'short':
        level = find_value ( lines, "*Level*" )
        if len(level) > 0:
            write ( '--  Level: ' + level )
        category = find_values ( lines, '*Category*' )
        if len(category) > 0:
            write ( '--  Category: ' + category )
        goal = find_values ( lines, '*Goal*' )
        if len(category) > 0:
            write ( '--  Goal: ' + goal )
        write('')
    if detail != 'quiet':
        write('')
            
def print_header ( title, want_header ):
    if want_header and len(title) > 0:
        sep = '-'.ljust(len(title),'-')
        write ( "---" + sep + "---" )
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
            write ( '\n' )

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
