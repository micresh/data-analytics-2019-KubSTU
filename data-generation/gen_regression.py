from sklearn.datasets import make_regression
import pandas as pd

for num in range(1, 11):
    x, y = make_regression(n_samples=1000, n_features=10, noise=0.9)
    df = pd.DataFrame(x, y)
    df.to_csv("variant-" + str(num) + ".csv")