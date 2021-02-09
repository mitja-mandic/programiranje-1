def merge(target,list1, list2):
    if len(list1) == 0 and len(list2) != 0:
        target += [list2]
        return target
    elif len(list2) == 0 and len(list1) != 0:
        target += list1
        return target
    elif len(list1) > 0 and len(list2) > 0:
        l1 = list1[0]
        l2 = list2[0]
        if l1 >= l2:
            target += [l2]
            list2 = list2[1:]
            merge(target, list1, list2)
        else:
            target += [l1]
            list1 = list1[1:]
            merge(target, list1, list2)

list_1 = [1, 3, 5, 7, 10]
list_2 = [1, 2, 3, 4, 5, 6, 7]
target = [-1 for _ in range(len(list_1) + len(list_2))]
merge(target, list_1, list_2)


def pivot(a, start, end):
    p = a[start]
    dobri_indeksi = []
    zadnji_manjsi_indeks = 0
    for i in range(start+1, end+1):
        if p <= a[i]:
            dobri_indeksi.append(i)
        else:
            zadnji_manjsi_indeks = dobri_indeksi[0]
            manjsi = a[i]
            temp = a[dobri_indeksi[0]]
            
            a[dobri_indeksi[0]] = manjsi
            a[i] = temp

            dobri_indeksi.pop(0)
    
    mini = a[zadnji_manjsi_indeks]
    a[start] = mini
    a[zadnji_manjsi_indeks] = p
    
    return a, zadnji_manjsi_indeks

ar = [10, 4, 5, 15, 11, 2, 17, 0, 18]