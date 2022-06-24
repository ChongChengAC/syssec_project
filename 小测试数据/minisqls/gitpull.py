import requests
import os

project_list = open("project-list.txt", "w")
with open("projects_url.txt") as f:
    for repo_name in f:
        owner_name = repo_name[: repo_name.index("/")]
        print("owner name: <" + owner_name + ">")
        dir_name = repo_name[repo_name.index("/") + 1 :].rstrip()
        print("dir name: <" + dir_name + ">")
        download_url = "https://codeload.github.com/" + \
            repo_name.rstrip() + "/zip/refs/heads/master"
        print("download url: <" + download_url + ">")
        raw_file = requests.get(download_url)
        zip_name = owner_name + "_" + dir_name + ".zip"
        with open(zip_name, "wb") as zip_file:
            zip_file.write(raw_file.content)

        cwd = os.getcwd()
        project_list.write(cwd + "/" + zip_name + "\n")

project_list.close()
