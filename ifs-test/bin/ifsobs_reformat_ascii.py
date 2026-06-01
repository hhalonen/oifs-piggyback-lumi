__author__ = "Sebastien Massart - ECMWF - 2019"

import os
import sys
import getopt
import datetime


class color:
    RED = '\033[91m'
    BOLD = '\033[1m'
    UNDERLINE = '\033[4m'
    END = '\033[0m'


class UserInterface:

    def __init__(self, script_name, description):
        """
        Initialisation of the class
        """
        #
        # Create the usage message
        #
        self.__script_name = script_name
        self.__description = description
        self.__MakeUsage__()
        #
        # Return 
        #
        return None 

    def __MakeUsage__(self):
        """
        Text for the usage of the script
        """
        #
        # Get the input parameters
        #
        usage =  "Usage: python3 %s [-h or --help] \n" % self.__script_name
        usage += "                          [--verbose=<verbose>] \n"
        usage += "\n" + color.BOLD + "NAME" + color.END + "\n"
        usage += "    %s - %s \n" % (self.__script_name, self.__description)
        usage += "\n" + color.BOLD + "DESCRIPTION" + color.END + "\n"
        usage += "  -i " + color.UNDERLINE + "string" + color.END + "\n"
        usage += "    \t input filename\n"
        usage += "  -o " + color.UNDERLINE + "string" + color.END + "\n"
        usage += "    \t output filename\n"
        usage += "\n" + color.BOLD + "OPTIONS" + color.END + "\n"
        usage += "  -h, --help\n"
        usage += "    \t for displaying this help and exist\n"
        usage += "  -v " + color.UNDERLINE + "interger" + color.END +"\n"
        usage += "    \t verbosity of the job (0=silent,1=warnings,2=info)\n" 
        #
        # Save the usage 
        #
        self.usage = usage
        #
        # Return 
        #
        return None 

    def __initialise_input__ (self) :
        #
        # Initialisation of the inputs
        #
        self.input_file = None
        self.output_file = None
        self.verbose = 2
        #
        # Return 
        #
        return None 

    def __read_user_input__(self):
        #
        # Get the input data
        #
        try:
            opts, args = getopt.getopt(sys.argv[1:],
                "i:o:h:v:",
                ["help"])
        except (getopt.GetoptError, IndexError, ValueError):
            sys.exit(self.usage)
        #
        # Read input
        #
        for opt, arg in opts:
            if opt in ["-h", "--help"]:
                sys.exit(self.usage)
            elif opt in ["-i"]:
                if not isinstance(arg, str):
                    sys.exit("STOP: the input filename should be a string")
                else: 
                    self.input_file = arg
            elif opt in ["-o"]:
                if not isinstance(arg, str):
                    sys.exit("STOP: the output filename should be a string")
                else: 
                    self.output_file = arg
            elif opt in ["-v"]:
                try: 
                    self.verbose = int(arg)
                except ValueError: 
                    sys.exit("STOP: the verbosity should be an integer")
        #
        # Return 
        #
        return None 
        
    def get_user_input(self):
        #
        # Initialisation of the inputs
        #
        self.__initialise_input__()
        #
        # Read input
        #
        self.__read_user_input__()
        #
        # Return 
        #
        return None 

def reformat_ascii_to_odb(input_file, output_file, verbosity):
    """
    """
    #
    # Open input file
    #
    if input_file is None:
        sys.exit("STOP: input file not specified")
    elif not os.path.exists(input_file):
        sys.exit("STOP: input file %s does not exist" % input_file)
    #
    fid = open(input_file, 'r')
    lines = fid.readlines()
    fid.close()
    #
    # Initialisation
    #
    header=""
    values = ""
    sumary = {'name': [], 'table': [], 'type': [], 'value': [] }
    #
    # Loop over the lines of the file
    #
    data_len = None
    for line in lines:
        if line.find('#') >= 0 : 
            my_table = line.replace('#','').strip()
        else:
            if line.find('!') < 0 : 
                l = line.split()
                if data_len is None:
                    data_len = len(l)
                    values = [''] * (data_len-2)
                if len(l) != data_len: 
                    sys.exit("Problem with line %s" % line)
                my_name, my_type = l[0:2]
                my_values = l[2:]
                header += "%s@%s:%s  " % (my_name, my_table, my_type)
                for i_data in range(data_len-2):
                    values[i_data] += "%s  " % my_values[i_data]
    #
    # Open input file
    #
    if output_file is None:
        sys.exit("STOP: output file not specified")
    else:
        fid = open(output_file, 'w')
        fid.writelines(header)
        fid.writelines("\n")
        for i_data in range(data_len-2):
            fid.writelines(values[i_data])
            fid.writelines("\n")
#    print header
#    print values
#    


if __name__ == '__main__':
    #
    # Get user input
    #
    usr = UserInterface(sys.argv[0], "Reformat ASCII file for ODB conversion")
    usr.get_user_input()
    #
    # Reformat
    #
    reformat_ascii_to_odb(usr.input_file, usr.output_file, usr.verbose)

