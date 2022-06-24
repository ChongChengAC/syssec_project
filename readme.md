本文档非实验报告

我们的项目完成了以下工作：

- 调研SourcererCC算法
- 根据我们对SourcererCC的理解，自己写了一个SourcererCC玩具，并与作者提供的工具对同样的代码进行检测分析，验证正确性
  - 亮点：论文作者的tokenizer使用六百多行python代码实现，我们使用30行lex实现；论文作者的clone-detector使用无数shell脚本和大量java代码实现，我们使用100行C++实现
  - 用途：比伪代码和作者的真代码更清晰易懂，可用于理解SourcererCC算法。在某些情景应该会更高效
- 调研Centris算法
- 根据我们对centris的理解，自己写了一个myCentris，在file粒度上复现了原作者Centris的版本冗余消除、数据库生成
  - 亮点：引进树结构优化目标重用检测速度，并且保持了较高精确度；做到了大数据集的克隆检测
  - 用途：因为对于File粒度的检测比论文作者的原版本更快，所以我们接下来的克隆检测都是使用自己的Centris版本进行的

- 使用Centris和SourcererCC算法对同样的数据集进行克隆检测，比较评估两种算法
  - 数据集：80个minisql；GNU+libc


目录组织：

- `SourcererCC_toy`——我们自己写的SourcererCC玩具
  - 通过重定向输入和重定向输出（`file1.tokens`, `file2.tokens`）来实现tokenizer的文件读写
  - 然后运行clone_detector，比较前一步生成的`file1.tokens`和`file2.tokens`
  - 测试数据为`k.cpp`和`codegen.cpp`
  - 测试结果分析详见内部的文件夹
- `小测试数据`——几个测试数据，用于论文提供的SourcererCC工具（不是我们自己写的玩具）
  - `minisqls`——运行我们写的gitpull脚本拉取80个minisql的压缩包
  - `flameshot&deepin-screenshot`
  - `linux`——自行下载linux-5.18.1源代码
- `myCentris`——我们自己写的Centris

如何运行：

- 运行SourcererCC工具：克隆原作者[Github](https://github.com/Mondego/SourcererCC)仓库，观看readme。我们这里补充原作者readme里缺少的部分，以及我们碰到过的坑：
  - 原作者将环境打包成了vm。该vm为Lubuntu 16.10，不支持vmware的拖放文件、共享剪贴板，需要在客户机中运行`sudo dhclient`手动打开dhcp分配ip才能上网，除了跑demo、检查实验环境，几乎没有任何实用价值。vm中有些代码相对仓库更新，有些代码相对仓库更旧。我们使用的是仓库的版本（并做了必要的修改，下面会详述），在本地linux环境执行
  - clone-detector需要用到`ant`命令，原readme中没有提到这个依赖（在issue中有大量反馈）。安装方法（以ArchLinux为例）：`sudo pacman -S ant`
  - clone-detector的输入文件（`blocks.file`）不存在时不但不报错，反而报SUCCESS，尽管面对空输入什么结果也产生不出来
  - 没有顶层脚本，所有繁复的移动、重命名、执行操作都要手动完成
  - 仓库中tokenizer部分有`db-importer`文件夹，里面是将查询结果导入mysql数据库的方法。但这里的python脚本是python2，最新的8.0.28 python-mysql-connector库已经不支持python2了，所以需要安装8.0.23版本的mysql-connector库。作为参考，我们使用了`pyenv` python虚拟环境。此外，还需要修改`mysql-import.py`最后几行，把注释取消掉，并且把`import_pairs(pairs_path)`改成`import_pairs(db_object, pairs_path)`。这样这个模块才能正常工作
  - 修改好后，python代码中项目间克隆率低于50%直接过滤掉，我们认为这样是不合理的，所以在`clone-finder.py`中地113行处注释掉对应的逻辑代码
  
- 运行myCentris工具：

  - 运行示例
  - `python3 OSS4file.py` 将`./data`中所有项目的C/C++文件转换成hash值，并存放在`./repo_pre`中对应文件夹下
  - `python3 preProcess.py`合并之前`./repo_pre`中的所有hash数据，并进行版本消除，建立完整的数据库
  - `python3 ./detector.py --in_disk=0 --build_tree 1 --target [target name] --acc 4`默认将`./target`下的指定项目作为待检测项目
    - `in_disk` 控制是否在磁盘IO
    - `build_tree` 建树
    - `target` 目标文件的名字
    - `acc` 准确度， 1/2/4/8 值越大，准确度越高，但是速度越慢

项目分工：

- 邱明冉——调研SourererCC算法
- 姚一语、徐巧颖——调研Centris算法
