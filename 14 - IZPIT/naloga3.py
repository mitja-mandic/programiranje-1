from functools import lru_cache

# =============================================================================
# Psička Nara po njivi preganja krokarje. Opazila je, da jo lastnik čaka na
# drugem koncu polja, zato hiti k njemu, pri tem pa hoče prestrašiti kar se da
# veliko ubogih ptičev.
#
# Njivo predstavimo kot matriko, ki v vsakem polju vsebuje število krokarjev,
# ki jih pasja navihanka prežene, če teče preko tega polja.
# =============================================================================

primer = [
    [2, 3, 0, 2, 9],
    [8, 3, 5, 1, 2],
    [1, 2, 7, 2, 0],
    [4, 3, 6, 5, 5],
]

# (a)
# =============================================================================
# Nara se nahaja v zgornjem levem kotu njive (polje `(0, 0)`). Ker se ji mudi
# k lastniku, se vztrajno premika desno. Na vsakem koraku se lahko premakne:
#   - desno
#   - diagonalno desno-gor
#   - diagonalno desno-dol
#
# Pregon krokarjev zaključi na poljubnem skrajno desnem polju njive. Napišite
# funkcijo, ki izračuna največje število krokarjev, ki jih lahko nagajivka
# prežene.
# =============================================================================
def krokarji(matrika):
    
    stolpci = len(matrika[0])
    vrstice = len(matrika)
    
    def pomozna(v, s):
        if v >= (vrstice-1) or s >= (stolpci-1):
            return 0
        
        #elif v == 0:
        #    desno = pomozna(v, s+1)
        #    desno_dol = pomozna(v+1,s+1)
        #    return matrika[0][s] + max(desno_dol, desno)
        #elif v >=1:
        else:
            desno_gor = pomozna(v-1,s+1)
            desno_dol = pomozna(v+1,s+1)
            desno = pomozna(v,s+1)
            return matrika[v][s] + max(desno_gor, desno_dol, desno)
    return pomozna(0,0)

# (b)
# =============================================================================
# Funkcijo iz točke (a) prilagodite tako, da ji dodatno podate indeks vrstice,
# v kateri Nara začne, in indeks vrstice, v kateri Nara konča.
#
# Funkcija naj vrne seznam VSEH optimalnih poti, kjer pot predstavimo s
# seznamom indeksov polj, preko katerih Nara teče.
# =============================================================================


