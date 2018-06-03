import pickle

from sklearn.neural_network import MLPClassifier


def parse_line(line):
    cls, dat = line.split()
    v = []
    for c in dat:
        if c == '0':
            v.append(-1)
        elif c == '_':
            v.append(0)
        else:
            v.append(1)
    return (v, int(cls))


def get_data(file):
    with open(file) as f:
        points = [parse_line(line) for line in f]

    X = [x for (x, y) in points]
    Y = [y for (x, y) in points]

    return X, Y


X, Y = get_data('data/bigger.dat')
net = MLPClassifier(hidden_layer_sizes=(64, 64, 32))
net.fit(X, Y)
print net.score(X, Y)

X, Y = get_data('data/smaller.dat')
print net.score(X, Y)
pickle.dump(net, open('network.dat', 'wb'))