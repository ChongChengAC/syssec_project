 # syssec_proj

本文档非实验报告

我们的项目完成了以下工作：

- 调研SourcererCC算法，达到完全看懂、能够组会精讲的水平
- 根据我们对SourcererCC的理解，自己写一个SourcererCC玩具，并与作者提供的工具对同样的代码进行检测分析，验证正确性
  - 亮点：论文作者的tokenizer使用六百多行python代码实现，我们使用30行lex实现；论文作者的clone-detector使用无数shell脚本和大量java代码实现，我们使用100行C++实现
  - 用途：比伪代码和作者的真代码更清晰易懂，可用于理解SourcererCC算法。在某些情景应该会更高效
- 调研Centris算法，达到完全看懂、能够组会精讲的水平
- 根据我们对centris的理解，自己写一个centris玩具，……
- 使用Centris和SourcererCC算法对同样的数据集进行克隆检测。数据集：80个minisql；GNU+libc+github C project。检测结果直观表现出来（数据集尚未整理完毕）

目录组织：

- `SourcererCC_toy`——我们自己写的SourcererCC玩具
  - 通过重定向输入和重定向输出（`file1.tokens`, `file2.tokens`）来实现tokenizer的文件读写
  - 然后运行clone_detector
  - 测试数据为`k.cpp`和`codegen.cpp`
  - 测试结果分析详见内部的文件夹
- `SourcererCC小测试数据`——几个测试数据，用于论文提供的SourcererCC工具（不是我们自己写的玩具）
  - `minisqls`——运行我们写的gitpull脚本拉取80个minisql的压缩包
  - `flameshot&deepin-screenshot`
  - `linux`——自行下载linux-5.18.1源代码
- `SourcererCC`——我们修正后的SourcererCC，原仓库开源在[Github](https://github.com/Mondego/SourcererCC)
- `Centris`——我们改进后的Centris，原仓库开源在Github（待添加）

如何运行：

- 运行SourcererCC工具：观看原仓库readme即可。我们这里补充原作者readme里缺少的部分，以及我们碰到过的坑：
  - 原作者将环境打包成了vm。该vm为Lubuntu 16.10，不支持vmware的拖放文件、共享剪贴板，需要在客户机中运行`sudo dhclient`手动打开dhcp分配ip才能上网，除了跑demo、检查实验环境，几乎没有任何实用价值。vm中有些代码相对仓库更新，有些代码相对仓库更旧。我们使用的是仓库的版本（并做了必要的修改，下面会详述），在本地linux环境执行
  - clone-detector需要用到`ant`命令，原readme中没有提到这个依赖（在issue中有大量反馈）。安装方法（以ArchLinux为例）：`sudo pacman -S ant`
  - clone-detector的输入文件（`blocks.file`）不存在时不但不报错，反而报SUCCESS，尽管面对空输入什么结果也产生不出来
  - 没有顶层脚本，所有繁复的移动、重命名、执行操作都要手动完成
  - 仓库中tokenizer部分有`db-importer`文件夹，里面是将查询结果导入mysql数据库的方法。但这里的python脚本是python2，最新的8.0.28 python-mysql-connector库已经不支持python2了，所以需要安装8.0.23版本的mysql-connector库。作为参考，我们使用了`pyenv` python虚拟环境。此外，还需要修改`mysql-import.py`最后几行，把注释取消掉，并且把`import_pairs(pairs_path)`改成`import_pairs(db_object, pairs_path)`。这样这个模块才能正常工作
  - 修改好后，python代码中项目间克隆率低于50%直接过滤掉，我们认为这样是不合理的，所以在`clone-finder.py`中地113行处注释掉对应的逻辑代码
  
- 运行修改后的centris工具（待补充）：

  - 运行示例：
  - `in_disk` 控制是否在磁盘IO
  - `build_tree` 建树
  - `target` 目标文件的名字
  - `acc` 准确度， 1/2/4/8 值越大，准确度越高，但是速度越慢
  - ```
    python3 ./detector.py --in_disk=0 --build_tree 1 --target glibc-2.9 --acc 4
    ```