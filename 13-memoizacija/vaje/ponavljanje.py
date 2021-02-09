def dolzina_zap(seznam, zap):
    if seznam == []:
        return zap
    else:
        if zap[-1] <= seznam[0]:
            dodam = zap + [seznam[0]]
            dolzina_dodam = 1 + len(dolzina_zap(seznam[1:], dodam))
            dolzina_spustim = len(dolzina_zap(seznam[1:], zap))
            return max(dolzina_dodam, dolzina_spustim)
        else:
            return dolzina_zap(seznam[1:], zap)


test = [2, 3, 6, 8, 4, 4, 6, 7, 12, 8, 9]


def pobeg(mocvara):
    def skoki(energija, indeks):
        if indeks >= len(mocvara):
            return 0
        e = energija + mocvara[indeks]
        vsi = [skoki(e-dolzina, indeks+dolzina) for dolzina in range(1,e+1)]
        return 1 + min(vsi)
    return skoki(0,0)
