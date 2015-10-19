from math import sqrt

class bicluster:
    def __init__(self, vec, left = None, right = None, distance = 0.0, id = None):
        self.left = left
        self.right = right
        self.vec = vec
        self.id = id
        self.distance = distance

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

def pearson(v1, v2):
    sum1 = sum(v1)
    sum2 = sum(v2)

    sum1Sq = sum([pow(v, 2) for v in v1])
    sum2Sq = sum([pow(v, 2) for v in v2])

    pSum = sum([v1[i] * v2[i] for i in range(len(v1))])

    num = pSum - (sum1 * sum1 / len(v1))
    den = sqrt((sum1Sq - pow(sum1, 2) / len(v1)) * (sum2Sq - pow(sum2, 2) / len(v1)))
    if den == 0: return 0

    return 1.0 - num / den

def hcluster(rows, distance = pearson):
    distances = {}
    currentclustid = -1

    # The first cluster is all of the rows
    # rows is "data" which is List[List[Int]]
    clust = [bicluster(rows[i], id = i) for i in range(len(rows))]

    while len(clust) > 1:
        lowestpair = (0, 1)
        closest = distance(clust[0].vec, clust[1].vec)

        # find the most closest pair
        for i in range(len(clust)):
            for j in range(i+1, len(clust)):
                # reuse the distance data if it's cahsed
                if(clust[i].id, clust[j].id) not in distances:
                    distances[(clust[i].id, clust[j].id)] = distance(clust[i].vec, clust[j].vec)

                d = distances[(clust[i].id, clust[j].id)]

                if d < closest:
                    closest = d
                    lowestpair = (i, j)

        # get avarage of the two clusters
        mergevec = [
                (clust[lowestpair[0]].vec[i] + clust[lowestpair[1]].vec[i]) / 2.0
                for i in range(len(clust[0].vec))
                ]
        # create new cluster
        newcluster = bicluster(mergevec, left = clust[lowestpair[0]], right = clust[lowestpair[1]], distance = closest, id = currentclustid)

        currentclustid -= 1
        del clust[lowestpair[1]]
        del clust[lowestpair[0]]
        clust.append(newcluster)

    return clust[0]
