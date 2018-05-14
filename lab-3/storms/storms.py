
def B(i, j):
    return 'B_%d_%d' % (i, j)


good_2x2_patterns = [
    '[1, 1, 1, 1]',
    '[0, 0, 0, 0]',
    '[1, 1, 0, 0]',
    '[1, 0, 1, 0]',
    '[0, 0, 1, 1]',
    '[0, 1, 0, 1]',
    '[1, 0, 0, 0]',
    '[0, 1, 0, 0]',
    '[0, 0, 1, 0]',
    '[0, 0, 0, 1]'
    ]

good_3x1_patterns = [
    '[1, 1, 1]',
    '[0, 0, 0]',
    '[1, 0, 1]',
    '[1, 1, 0]',
    '[0, 1, 1]',
    '[1, 0, 0]',
    '[0, 0, 1]'
]

good_1x3_patterns = [
    '[1, 1, 1]',
    '[0, 0, 0]',
    '[1, 0, 1]',
    '[1, 1, 0]',
    '[0, 1, 1]',
    '[1, 0, 0]',
    '[0, 0, 1]'
]


def at_least(R, C):
    hor = ['%s #==> (%s #\/ %s),' % (B(i, j), B(i, j-1), B(i, j+1))
           for i in range(R) for j in range(1, C-1)]
    ver = ['%s #==> %s #\/ %s,' % (B(i, j), B(i-1, j), B(i+1, j))
           for i in range(1, R-1) for j in range(C)]
    writeln('    %s' % (''.join(hor)),)
    writeln('    %s' % (''.join(ver)),)


def triple_h(R, C):
    return ['[%s, %s, %s]' % (B(i, j), B(i, j+1), B(i, j+2))
            for i in range(R) for j in range(C-2)]


def triple_v(R, C):
    return ['[%s, %s, %s]' % (B(i, j), B(i+1, j), B(i+2, j))
            for i in range(R-2) for j in range(C)]


def show_arr(arr):
    return '[' + ', '.join(arr) + ']'


def constants(triples):
    eqs = ['%s #= %s,' % (B(i, j), str(b)) for (i, j, b) in triples]
    writeln('    %s' % (''.join(eqs)),)


def quads(R, C):
    return ['[%s, %s, %s, %s]' % (B(i, j), B(i, j+1), B(i+1, j), B(i+1, j+1))
            for i in range(R-1) for j in range(C-1)]


def radars(rows, cols):
    R = len(rows)
    C = len(cols)
    r = [' + '.join([B(i, j) for j in range(C)]) + ' #= ' + str(rows[i]) for i in range(R)]
    for row in r:
        writeln(row + ',')
    c = [' + '.join([B(i, j) for i in range(R)]) + ' #= ' + str(cols[j]) for j in range(C)]
    for col in c:
        writeln(col + ',')


def storms(rows, cols, triples):
    writeln(':- [library(clpfd)].')

    R = len(rows)
    C = len(cols)

    bs = [B(i, j) for i in range(R) for j in range(C)]

    writeln('solve([' + ', '.join(bs) + ']) :- ')

    constants(triples)

    writeln('    tuples_in(%s, %s),' % (show_arr(quads(R, C)),
            show_arr(good_2x2_patterns)))

    writeln('    tuples_in(%s, %s),' % (show_arr(triple_h(R, C)),
            show_arr(good_3x1_patterns)))

    writeln('    tuples_in(%s, %s),' % (show_arr(triple_v(R, C)),
            show_arr(good_1x3_patterns)))

    radars(rows, cols)

    # at_least(R, C)

    # TODO: add some constraints

    # writeln('    [%s] = [1,1,0,1,1,0,1,1,0,1,1,0,0,0,0,0,0,0,1,1,1,1,1,0,1,1,1,1,1,0,1,1,1,1,1,0],' % (', '.join(bs),)) # only for test 1

    writeln('    labeling([ff], [' + ', '.join(bs) + ']).')
    writeln('')
    writeln("main(_) :- tell('prolog_result.txt'), solve(X), write(X), nl, told.")


def writeln(s):
    output.write(s + '\n')

txt = open('zad_input.txt').readlines()
output = open('zad_output.txt', 'w')

raws = [int(c) for c in txt[0].split()]
cols = [int(c) for c in txt[1].split()]
triples = []

for i in range(2, len(txt)):
    if txt[i].strip():
        triples.append([int(c) for c in txt[i].split()])

storms(raws, cols, triples)
