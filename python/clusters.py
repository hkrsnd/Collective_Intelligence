def readfile(filename):
    lines = [line for line in file(filename)]

    # The first line is the title of the column
    colnames = lines[0].strip().split('\t')[1:]
    rownames = []
    data = []
    for line in lines[1:]:
        p = line.strip().split('\t')
        # The first column of each lines isthe name of line
        rownames.append(p[0])
        # The last is the data of the line
        data.append([float(x) for x in p[1:]])
    return rownames, colnames, data
