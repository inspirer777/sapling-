
# Statistical Analysis and Sampling Simulations

This repository contains R code for performing various statistical analyses and simulations. It includes operations on a dataset (`edit deg.csv`) and demonstrates different sampling techniques, statistical functions, and visualization methods. Below are the key operations and features of the code:

## Key Operations

1. **Data Loading and Preprocessing:**
   - Load a CSV file (`edit deg.csv`) using `readr` package.
   - View and print the dataset.

2. **Random Sampling and Variance Calculations:**
   - Randomly sample 20 rows from the dataset with replacement.
   - Calculate variance and mean for a specific column (`pish girl`).
   
3. **Statistical Calculations:**
   - Compute variance and mean of the `pish girl` column.
   - Use the `choose()` function to calculate combinations.

4. **Random Sampling and Distribution:**
   - Generate random normal samples and plot the distribution.
   - Simulate a sample distribution for the `second boy` column.
   - Calculate sample means and overlay a theoretical normal distribution.

5. **Standard Deviation Calculations:**
   - Calculate and print the standard deviation for multiple columns (`pish girl`, `pish boy`, `year`).

6. **Chi-Square Distribution:**
   - Set degrees of freedom for a Chi-Square distribution.
   - Plot the histogram of column sums of squares and overlay the theoretical chi-square density curve.

7. **Coin Toss Simulation:**
   - Simulate repeated coin tossing and calculate the cumulative sum of heads over multiple tosses.
   - Plot the share of heads as it converges to 0.5.

8. **Binomial Distribution Simulation:**
   - Simulate binomial distributions for different sample sizes.
   - Plot histograms of standardized sample means and overlay the normal distribution curve.

9. **Spatial Sampling:**
   - Generate a spatially balanced sample using inclusion probabilities and plot the population and sample.

10. **Additional Sampling and Matrix Operations:**
    - Perform additional random sampling and matrix operations on the dataset.
    - Sample values from the `pish girl` column with replacement.

## Requirements

- R (version 4.x or above)
- Required R libraries: `readr`, `ggplot2` (if not installed, use `install.packages()` to install them)

## Instructions

1. Clone the repository to your local machine.
2. Install the required R packages.
3. Load the `edit deg.csv` dataset and explore the various functions and simulations provided in the code.


## License

This project is licensed under the [MIT License](LICENSE).  
You are free to use, modify, and distribute this project under the terms of the license.

# Good luck .
