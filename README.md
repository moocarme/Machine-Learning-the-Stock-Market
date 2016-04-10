# Machine Learning the Stock Market

Here I investigate a number of different algorithms in order to build predictions on the stock price of Apple using R.
I use a classification algorithm using the Kernlab package, a regression algoritmn using the Earth function, and support vector machines to build predictions to see which one gives the best results.

Classification using Kernlab gives the best results in the testing dataset after training on the training dataset.
This is then used to excute either long trades or short trades on in the market.
From analysis of the returns the long trades perform better with returns over 18%.

This machie learning process produces good results, however in order to show robust results the method could be improved via muliple training and testing runs by Monte Carlo/bootsrapping at beginning at various times, and evaluating the method.
