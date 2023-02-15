
def extract_suffix_array(string):
    n = len(string)
    suffixes = []
    for i in range(n):
        suffixes.append((i, string[i:]))
    sorted_suffixes = sorted(suffixes, key=lambda x:x[1])
    for i in range(n):
        print(suffixes[i], " " * i, "   --->  ", sorted_suffixes[i])
    return sorted_suffixes


def pattern_match(P, S):


if __name__=="__main__":
    string = input("\ntype in a word: ")
    print("\n")
    suffix_array = extract_suffix_array(string)
    print("\n")
    print(f"suffix array of {string} is: {[x[0] for x in suffix_array]}")