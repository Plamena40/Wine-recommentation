WineMatch: Personalized Wine Pairing Guide

Overview
WineMatch is an interactive Shiny app designed to help users discover personalized wine recommendations based on their preferences. The app uses machine learning to predict the best wine match by considering preferences in alcohol level, color intensity, and flavanoid levels (an antioxidant often preferred for its health benefits). It provides users with real-time recommendations along with images of the wines and their predicted probabilities of being the best match.

Features
* Wine Preferences: Users can adjust sliders and buttons to select their preferred alcohol content, color intensity, and flavanoid levels.
* Wine Recommendation: Based on the input preferences, the app suggests a wine (Barbera, Barolo, or Grignolino) along with the probability of each wine being the best match.
* Wine Images: Each recommended wine is displayed with an image of the wine.

Data
The app uses theÊWineÊDataÊSetÊfrom the UCI Machine Learning Repository. The data includes 178 measurements of various chemical properties of wines from three different classes:
* Barbera
* Barolo
* Grignolino
This dataset is preprocessed to include low and high categories for color intensity and flavanoid based on sample medians.

Model
An elastic net algorithm, a penalized regression method which implements feature selection, was trained to predict the type of wine using leave-one-out cross-validation and accuracy evaluation metric. This resulted in sensitivity=1.000, specificity=0.966, and C-statistic=0.99.

App UI Design
The app features a simple user interface built using theÊbs_themeÊfunction from theÊbs4DashÊpackage in R. The theme is customized to provide a cozy wine-themed interface with a background color of red wine and complementary fonts.
Main UI Elements:
* Wine Preferences Section:
o A slider for alcohol content selection.
o Action buttons to choose between "Low" or "High" for color intensity and flavanoids.
* Wine Recommendation Section:
o A table displaying the predicted probabilities for each wine.
o A section displaying the wine name and its image.

Conclusion
WineMatch is a fun and interactive tool to help you discover new wines based on your preferences. With its easy to navigate user interface, real-time recommendations, and engaging features, it's a great way to make informed wine choices!

