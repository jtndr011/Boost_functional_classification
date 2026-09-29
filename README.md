# Functional module classification

This directory contains the R script used to assign differentially expressed genes (DEGs) to curated functional modules for the bacterial leaf streak transcriptomic analysis.

Script:
functional_assignment_script.R

Input:
Boost_DEGs.csv

The input file should contain functional annotation information together with log2 fold-change and adjusted P-value columns for the 24, 48, and 96 HAI comparisons.

The script performs the following steps:
1. Cleans functional annotation text.
2. Assigns each DEG to a detailed functional category using manually curated regular-expression keyword rules.
3. Resolves multiple possible matches using a predefined priority order.
4. Collapses the detailed categories into seven higher-order functional modules:
   - Immune perception and signal initiation
   - Calcium–ROS immune amplification
   - Transcriptional regulation and systemic immunity
   - Cell-wall reinforcement and apoplastic defense
   - Antimicrobial metabolism and PR defense
   - Metabolic and cellular reprogramming
   - Immune feedback and homeostasis
5. Generates a single Excel workbook containing the final module assignments.

Output:
Boost_BLS_vs_Mock_DEGs_with_Fig_7modules.xlsx

The same functional classification framework was applied to the Boost and Timstein datasets to enable direct comparison of temporal defense responses.
