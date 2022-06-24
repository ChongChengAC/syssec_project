from base64 import decode
import os
import re
import csv
import tlsh

currentPath = os.getcwd()
resultPath  = currentPath + "/repo_res/"
dataPath = "./data"
total_file = 0

def compressFileBody(fileName):
    string = ""
    with open(fileName, 'r', encoding='utf-8', errors='ignore') as lf:
        for line in lf:
            line = str(line).replace("\n", " ").replace("\t", " ").replace('\r', ' ').replace('{', ' ').replace('}', ' ')
            string += line
    lf.close()
    return string

def removeComment(string):
	c_regex = re.compile(
		r'(?P<comment>//.*?$|[{}]+)|(?P<multilinecomment>/\*.*?\*/)|(?P<noncomment>\'(\\.|[^\\\'])*\'|"(\\.|[^\\"])*"|.[^/\'"]*)',
		re.DOTALL | re.MULTILINE)
	return ''.join([c.group('noncomment') for c in c_regex.finditer(string) if c.group('noncomment')])

def computeTlsh(string):
	string 	= str.encode(string)
	hs 		= tlsh.hash(string)
	return hs

def doHash(projName, projPath, projResPath):
    global total_file
    csvFile = projResPath + '/' + projName + '.csv'
    print("   [*] Create a result file : %s.csv." % projName)
    fres = open(csvFile, 'w')
    csv_writer = csv.writer(fres)
    # csv_writer.writerow([f'Hash Value', 'Path'])

    fileCnt = 0
    possible = (".c", ".cc", ".cpp")
    for root, dirs, files in os.walk(projPath, topdown=False):
        for file in files:
            if file.endswith(possible):
                fileName = root + "/" + file
                s = compressFileBody(fileName)
                s = removeComment(s)
                if(len(s) < 50):
                    continue
                s = computeTlsh(s)
                if s == "TNULL": continue
                csv_writer.writerow([s, fileName])
                fileCnt += 1

    print("   [*] result file write ends! %d lines written." % fileCnt)
    total_file += fileCnt
    fres.close()

def main():
    os.chdir(currentPath)

    isExists = os.path.exists(resultPath)
    if not isExists:
        os.makedirs(resultPath)
        print("[+] Create a new path : " + resultPath)

    for projs in os.listdir(dataPath): 
        if(projs == ".DS_Store"): continue
        projPath = os.path.join(dataPath, projs) 
        for versions in os.listdir(projPath): 
            if(versions == ".DS_Store"): continue
            projResPath = resultPath + projs
            isExists = os.path.exists(projResPath)
            if not isExists:
                os.makedirs(projResPath)
            print("[+] Start to process projects : %s." % versions)
            doHash(versions, projPath+'/'+versions, projResPath) 
    
    print("[+] The amount of files : %s." % total_file)

if __name__ == "__main__":
    main()

