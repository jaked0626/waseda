import textwrap

class kdtree(object):
    def __init__(self, point, axis=0):
        """
        Inputs: 
          point (tuple) = k次元の座標
          axis (int) = 0とkの間で，このノードで比較する座標軸を指す
        """
        self.point = point # tuple of length k
        self.k = len(self.point) # 次元
        self.axis = axis % self.k  # 比較する軸をk以下に
        self.left_child = None
        self.right_child = None
        self.children = [] # needed for visualization
    
    def insert(self, point):
        assert len(point) == self.k #　同次元でなければ比較できない
        if point[self.axis] > self.point[self.axis]:
            if self.right_child:
                self.right_child.insert(point)
            else:
                self.right_child = kdtree(point, self.axis + 1) # 子の比較の軸を変える
                self.children.insert(0, self.right_child)
        else:
            if self.left_child:
                self.left_child.insert(point)
            else:
                self.left_child = kdtree(point, self.axis + 1) # 子の比較の軸を変える
                self.children.append(self.left_child)
    
    def num_children(self):
        return len(self.children)
    
    def __print_r(self, prefix, last, kformat, vformat, maxdepth):
        """
        Recursive method to print out the tree. Should not be
        called directly. See print() method for more details.
        """

        if maxdepth is not None:
            if maxdepth == 0:
                return ""
            else:
                maxdepth -= 1    

        if len(prefix) > 0:
            if last:
                lprefix1 = prefix[:-3] + u"  └──"
            else:
                lprefix1 = prefix[:-3] + u"  ├──"
        else:
            lprefix1 = u""
    
        if len(prefix) > 0:
            lprefix2 = prefix[:-3] + u"  │"
        else:
            lprefix2 = u""

        if last:
            lprefix3 = lprefix2[:-1] + "   "
        else:
            lprefix3 = lprefix2 + "  "

        ltext = (kformat + "; " + vformat).format(self.axis, self.point)

        ltextlines = textwrap.wrap(ltext, 80, initial_indent=lprefix1, 
            subsequent_indent=lprefix3)

        print(lprefix2)
        print(u"\n".join(ltextlines))

        for i, st in enumerate(self.children):
            if i == self.num_children() - 1:
                newprefix = prefix + u"   "
                newlast = True
            else:
                newprefix = prefix + u"  │"
                newlast = False

            st.__print_r(newprefix, newlast, kformat, vformat, maxdepth)
    
    
    def print(self, kformat="axis: {}", vformat="point: {}", maxdepth=None):
        """
        Prints out the tree.
        
        Parameters:
        - kformat, vformat: Format strings for the key and value.
        - maxdepth: Maximum depth to print.
        """
        
        self.__print_r(u"", False, kformat, vformat, maxdepth)