from functools import lru_cache


def stevilo_moznih(start, razlika):
    if start - razlika >= 0:
        return 2*razlika + 1
    else:
        return start + razlika + 1
