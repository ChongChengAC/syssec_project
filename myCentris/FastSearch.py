import tlsh
import preProcess

INF = 10000000
nitems_in_leaf = 100
# AccRate = 1
# threshold = 30


# min_diff = INF

class Node:
    def __init__(self, Y="", T=0, data=[], LC=None, RC=None) -> None:
        self.split = Y
        self.threshold = T
        self.data = data
        self.LC = LC
        self.RC = RC
        if len(data):
            Y = data[0]

    def size_node(self):
        return len(self.data)


def FindThre(node: Node, split_val: int):
    """hashes[i]   --  diff_sepci[i]"""
    nitem = int(len(node.data) / 2)  # size of each son
    diff_sepci = []  # tlsh_diff from specific node to every hashval, int type
    for hashval in node.data:
        diff_sepci.append(tlsh.diff(hashval, split_val))
    mid_diff = sorted(diff_sepci)[nitem]
    # print(f"    @@DIFF_SPECI is {diff_sepci}, split = {split_val}, threshold is {mid_diff}")
    return mid_diff, diff_sepci


def Divide(node: Node, thre: int, diff_sepci: list):
    nitem = int(len(node.data) / 2)
    i = 0
    llist = []
    rlist = []
    node_sz = node.size_node()
    while i < node_sz:
        tmp_diff = diff_sepci[i]
        if tmp_diff <= thre:
            llist.append(node.data[i])
        else:
            rlist.append(node.data[i])
        i += 1
    node.LC = Node(T=thre, data=llist)
    node.RC = Node(T=thre, data=rlist)
    return node


def Split(node: Node):
    nitems = len(node.data)
    Y = node.data[0]  # select 1st element of data
    thre, diff_sepci = FindThre(node, Y)
    node.split = Y
    node.threshold = thre
    node = Divide(node, thre, diff_sepci)
    return node


def TreeBuild(node: Node):
    if node.size_node() < nitems_in_leaf:
        return node
    node = Split(node=node)
    TreeBuild(node.LC)
    TreeBuild(node.RC)
    return node


def ClosestDist(node: Node, val):
    min_diff = INF
    for hashval in node.data:
        temp_diff = tlsh.diff(hashval, val)
        if temp_diff < min_diff:
            min_diff = temp_diff
    return min_diff


def SearchVal(l, val):
    for i in l:
        if i == val:
            return True
    return False


def SearchTree(node: Node, val, acc) -> int:
    min_diff = INF
    # if node is a leaf:
    if node.size_node() <= nitems_in_leaf * acc:
        min_diff = ClosestDist(node, val)
        return min_diff
    else:
        this_dist = tlsh.diff(node.split, val)
        if this_dist <= node.threshold:
            min_diff = SearchTree(node.LC, val, acc)
        else:
            min_diff = SearchTree(node.RC, val, acc)
        return min_diff


def test():
    hashes = preProcess.Read2Mem("./db/database.csv")
    val = "T19CD02EB14D7F3152A203E2FE63828A26A28D107A03950135786F85A0B3B086CB3B33CF"
    print(tlsh.diff(val, "T19CD02EB14D7F3152A203E2FE63828A26A28D107A03950135786F85A0B3B086CB3B33C0"))
    root = Node(data=hashes)
    TreeBuild(root)
    print(SearchTree(root, val))


# test()
