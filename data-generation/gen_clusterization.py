from sklearn.datasets import make_blobs
import pandas as pd

for num in range(1, 10):
    x, y = make_blobs(n_samples=1000, n_features=10)
    df = pd.DataFrame(x, y)
    df.to_csv("variant1" + str(num) + ".csv")