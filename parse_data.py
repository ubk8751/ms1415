with open("data.txt") as f:
    lines = f.readlines()
dists = {}
latest_dist = ""
latest_dims = ""
for line in lines:
    if "dist" in line.lower():
        latest_dist = line.split(' ')[0]
        dists[latest_dist] = {}
        continue
    if line[0] is not "x" and "x" in line:
        latest_dims = line.rstrip('\n')
        if latest_dims in dists[latest_dist].keys():
            dists[latest_dist][latest_dims] = []
        else:
            dists[latest_dist] = {latest_dims: []}
        continue
    print(dists)
    dists[latest_dist][latest_dims].append(line)
print(dists["Gamma"])
