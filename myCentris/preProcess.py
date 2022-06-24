import os
import csv
import tlsh

# import detector

resultPath = "./repo_res"
afterEliminationPath = "./repo_pre"
DBPath = "./db"
threshold = 30
total_versions = 0
total_project = 0


def Read2Mem(path):
    hashes = []
    fp = open(path, 'r')
    csv_reader = csv.reader(fp)
    i = 0
    for row in csv_reader:
        # hashval = row[0]
        hashes.append(row[0])
        i += 1
    return hashes


def redundancyElimination():
    global total_versions
    global total_project
    for repoName in os.listdir(resultPath):
        # repoName : 当前工程名字
        total_project += 1
        print("[+] Start %s..." % repoName)
        if repoName == ".DS_Store": continue

        versionList = []
        verDict = {}
        signature = {}
        tempDateDict = {}
        idx = 1

        for eachVersion in os.listdir(os.path.join(resultPath, repoName)):
            # eachVersion   : 完整的.csv文件名
            # versionNumber : 版本号
            print("  [*] Process Version : %s" % eachVersion)
            versionNumber = eachVersion.split("-")[-1].replace(".csv", "")
            if versionNumber == "" or versionNumber == " ":
                continue
            versionList.append(versionNumber)
            versionList.sort()
            print("  [*] Get current version number : %s" % versionNumber)

        for versionNumber in versionList:
            # open each csv file
            # with open(resultPath+"/"+repoName+'/'+repoName+'-'+versionNumber+'.csv', 'r') as fp:
            hashs = []
            verDict[versionNumber] = idx
            idx += 1
            # csv_reader = csv.reader(fp)
            tmpPath = resultPath + "/" + repoName + '/' + repoName + '-' + versionNumber + '.csv'
            hashs = Read2Mem(tmpPath)
            # for row in csv_reader:
            #     hashval = row[0]
            for hashval in hashs:
                if hashval not in signature:
                    signature[hashval] = []
                    tempDateDict[hashval] = []
                if idx - 1 not in signature[hashval]:
                    signature[hashval].append(repoName + '_' + str(idx - 1))
        # print("*****************\n", signature, "*****************\n")
        print("[+] Successfully eliminate redundancy of project : %s!" % repoName)
        print("[+] Output the result of : %s" % repoName)
        csvFile = afterEliminationPath + '/' + repoName + ".csv"
        fres = open(csvFile, 'w')
        csv_writer = csv.writer(fres)
        filecnt = 0
        for hashval in signature:
            csv_writer.writerow([hashval, str(signature[hashval])])
            filecnt += 1
        print("   [*] %d lines of data are written." % filecnt)

        print("[+] Output the version list of : %s" % repoName)
        csvFile = afterEliminationPath + '/' + repoName + "_version.csv"
        fres = open(csvFile, 'w')
        csv_writer = csv.writer(fres)
        versionCnt = 0
        for eachVersion in verDict:
            csv_writer.writerow([repoName + '-' + eachVersion, repoName + '_' + str(verDict[eachVersion])])
            versionCnt += 1
        total_versions += versionCnt
        print("   [*] %d versions." % versionCnt)
        print("=========================================")


def createDB():
    possibleMembers = {}
    lineCnt = 0
    print("[+] The amount of versions are: %d" %total_versions)
    print("[+] The amount of projects are: %d" %total_project)
    print("[+] Start Create Database.")
    for eachfile in os.listdir(afterEliminationPath):
        if eachfile[-8:] == "sion.csv": continue  # skip glibc_version.csv
        if eachfile == ".DS_Store": continue 
        with open(afterEliminationPath + '/' + eachfile, 'r', encoding="UTF-8") as fs:
            csv_reader = csv.reader(fs)
            print(eachfile)
            for eachrow in csv_reader:
                hashval = eachrow[0]
                if hashval == "TNULL": continue
                flag = True

                for hashdata in possibleMembers:
                    if len(possibleMembers) < 2:
                        break
                    distance = tlsh.diffxlen(hashdata, hashval)
                    # print(distance)
                    if distance <= threshold:
                        flag = False
                        break
                if flag:
                    lst = eachrow[1].replace('\"', "").replace(' ', "").replace("\'", "").replace("[", "").replace("]","").split(',')
                    # print(f"        @@ lst is {lst}")
                    possibleMembers[hashval] = []
                    for i in lst:
                        possibleMembers[hashval].append(i)
                    lineCnt += 1
                    # print(f"        @@ possible Member: {possibleMembers}",
                    # "\n------------------------------------------------------------------\n")

    print("[+] Output the final database.")
    isExists = os.path.exists(DBPath)
    if not isExists:
        os.makedirs(DBPath)
        print("[+] Create a new path : " + DBPath)

    csvFile = DBPath + '/' + "database.csv"
    fres = open(csvFile, 'w')
    csv_writer = csv.writer(fres)
    for hashval in possibleMembers:
        csv_writer.writerow([hashval, possibleMembers[hashval]])
    print("[+] Print %d" % lineCnt)


def main():
    isExists = os.path.exists(afterEliminationPath)
    if not isExists:
        os.makedirs(afterEliminationPath)
        print("[+] Create a new path : " + afterEliminationPath)
    redundancyElimination()
    createDB()


if __name__ == "__main__":
    main()
