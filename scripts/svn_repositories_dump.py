import re
import os
import sys
import codecs
import shutil
import subprocess

def dumpRepositories(repositories_path, dump_path, debug_script=False, verbose=False, report_output=False, report_filename='report.txt'):    
    """
    DESCRIPTION:
    Dump automatically Subversion repositories and store them in current folder.

    ARGUMENTS:
        repositories_path                path to the actual repositories (it must be accessible through the file system)
        dump_path                        path to the directory to store the dump files
        debug_script = False             debug ON/OFF, in debug mode no action is done (files copy, compile)
                                         but a prompt of those actions is displayed (if verbose = True) and
                                         reported in a log file (if report_output = True)
        verbose = False                  verbose mode (print messages of current actions) ON/OFF
        report_output = False            report current actions into log file (ON/OFF)
        report_filename = 'report.txt'   report output file (is report_output = True),
    """

    report_file = 0
    if report_output:
        report_file = open(report_filename,'w')   

    try:
        _dumpRepositories(repositories_path, dump_path, debug_script, verbose, report_output, report_file)
    finally:
        if report_output:
            report_file.close()
            if verbose:
                message = '\n\n[ automatic dump report was written to file <' + report_filename + '> ]'
                print(message)

def _report(message, verbose=True, report_output=True, report_file=0):
    if verbose:
        print(message)
    if report_output:
        report_file.write(message + '\n')

def _dumpRepositories(repositories_path, dump_path, debug_script=False, verbose=False, report_output=False, report_file=0):
    
    if debug_script:
        message = '\n<<run script in debug mode>>\n\n'
        _report(message, verbose, report_output, report_file)

    from datetime import datetime
    now = datetime.now()
    date = now.strftime("%Y-%m-%d")

    #####################
    # DUMP REPOSITORIES #
    #####################
    message = """\n\t###################
    \t#DUMP REPOSITORIES#
    \t###################\n"""
    _report(message, verbose, report_output, report_file)

    repositories_names = os.listdir(repositories_path)
    _repositories_names = []
    message = ">> Detected repositories:\n"
    _report(message, verbose, report_output, report_file)
    for file in repositories_names:
        try:
            if os.path.isdir(os.path.join(repositories_path, file, "db")):
                _report(file, verbose, report_output, report_file)
                _repositories_names.append(file)
        except OSError:
            pass
    repositories_names = _repositories_names
    print(repositories_names)
 
    message = 'Repositories: '
    for rep in repositories_names:
        message = message + rep + ', '
    message = message[:-2]
    _report(message, verbose, report_output, report_file)

    for repository_name in repositories_names:

        dump_name = repository_name + '_dump_' + date + '.dmp'
        message = '\nDump [' + repository_name + '] repository to ' + dump_name + '...'
        _report(message, verbose, report_output, report_file)
        
        command  = 'svnadmin dump ' + os.path.join(repositories_path, repository_name) + ' > ' + os.path.join(dump_path, dump_name)
        message = '>> ' + command + ' ...'
        _report(message, verbose, report_output, report_file)

        if not debug_script:            
            p = os.system(command)
            message = 'Over'
            _report(message, verbose, report_output, report_file)
        else:
            message = 'Over [dummy]'
            _report(message, verbose, report_output, report_file)

def print_help():
    message =     """
    ERROR>> 2 arguments must be provided!

    Dump automatically Subversion repositories and store them in current folder.

    ARGUMENTS:
        repositories_path                path to the actual repositories (it must be accessible through the file system)
        dump_path                        path to the directory to store the dump files
"""
    print(message)

if __name__ == "__main__":
    if len(sys.argv) < 3:
        print_help()
    else:
        repositories_path = sys.argv[1]
        dump_path = sys.argv[2]
        print("repositories_path: " + repositories_path)
        print("dump_path: " + dump_path)
        dumpRepositories(repositories_path, dump_path, debug_script=False, verbose=True, report_output=True, report_filename='report.txt')
