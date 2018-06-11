import sys

if not hasattr(sys, 'argv'):
    sys.argv = ['']

def mean(numbers):
    total = float(sum(numbers))
    count = len(numbers)
    return total/count
