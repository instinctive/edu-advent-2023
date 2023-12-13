import sys

words = "one two three four five six seven eight nine".split()

sum1 = 0
sum2 = 0

for line in sys.stdin:
    part1 = []
    part2 = []

    for i in range(0,len(line)-1):
        if line[i] >= '1' and line[i] <= '9':
            part1.append(int(line[i]))
            part2.append(int(line[i]))
        else:
            for n,w in enumerate(words,1):
                if line.startswith(w,i):
                    part2.append(n)
                    break

    sum1 += 10 * part1[0] + part1[-1]
    sum2 += 10 * part2[0] + part2[-1]

print(sum1)
print(sum2)
