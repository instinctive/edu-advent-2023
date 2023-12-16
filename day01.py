import sys

words = "one two three four five six seven eight nine".split()

sum1 = 0
sum2 = 0

for line in sys.stdin:
    nums1 = []
    nums2 = []

    for i in range(0,len(line)-1):
        if line[i] >= '1' and line[i] <= '9':
            nums1.append(int(line[i]))
            nums2.append(int(line[i]))
        else:
            for n,w in enumerate(words,1):
                if line.startswith(w,i):
                    nums2.append(n)
                    break

    sum1 += 10 * nums1[0] + nums1[-1]
    sum2 += 10 * nums2[0] + nums2[-1]

print(sum1)
print(sum2)
