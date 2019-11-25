# -*- coding: utf-8 -*-
import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D

def main():
    x = np.array(np.recfromcsv('body.csv'))
    print(x)
    y = [t[0] for t in x]
    print(y)
    x1 = [t[1] for t in x]
    x2 = [t[2] for t in x]
    # 3Dでプロット
    fig = plt.figure()
    ax = Axes3D(fig)
    ax.plot(x1, x2, y, "o")
    # 軸ラベル
    ax.set_xlabel('x1')
    ax.set_ylabel('x2')
    ax.set_zlabel('y')

    def f(x1, x2):
        a1 = 1.259649123
        a2 = 4.861403509
        return a1 * x1 + a2 * x2 + 19.77017544

    X1, X2 = np.mgrid[min(x1):max(x1), min(x2):max(x2)]
    Y = f(X1, X2)

    ax.plot_surface(X1, X2, Y, shade=False, edgecolor="orange", alpha=0.3)
    # 表示
    plt.show()

if __name__ == '__main__':
    main()
