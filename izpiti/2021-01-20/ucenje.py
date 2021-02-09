from functools import lru_cache

test = [350,230,370,920,620,80,520,410,780,630]

test2 = [2,3,6,8,4,4,6,7,12,8,9]

def razlike_neumne(seznam):
    trenutni = 0
    for i in range(len(seznam)):
        for j in range(i,len(seznam)):
            if seznam[i] - seznam[j] < trenutni:
                trenutni = seznam[i] - seznam[j]
    return trenutni

