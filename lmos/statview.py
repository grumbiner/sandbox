#viewing cProfile output from
#python3 -m cProfile -o pstats main2.py
import pstats

p = pstats.Stats('pstats')

p.sort_stats('time').print_stats(35)

p.sort_stats('call').print_stats(15)