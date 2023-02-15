from random import randint
from binarytree import Node

def insert_bst(node, pt):
    if pt >= node.value:
        if node.right:
            insert_bst(node.right, pt)
        else:
            node.right = Node(pt)
    else:
        if node.left:
            insert_bst(node.left, pt)
        else:
            node.left = Node(pt)


if __name__ == '__main__':
    points = [randint(1, 20) for _ in range(5)]
    root = Node(0)
    root2 = Node(0)
    for point in points:
        insert_bst(root, point)
    points.sort()
    for point in points:
        insert_bst(root2, point)
    root.pprint()
    root2.pprint()

