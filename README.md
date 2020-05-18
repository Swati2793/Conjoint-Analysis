# Conjoint-Analysis

Conjoint analysis is a survey-based statistical technique used in market research that helps determine how people value different attributes (feature, function, benefits) that make up an individual product or service. It works by breaking a product or service down into its components (referred to as attributes and levels) and then testing different combinations of these components to identify consumer preferences.

For the purpose of this exercise, we simulated survey responses by taking 24 cards with different combinations of attributes for Television. These attributes were - price, brand, screen size, technology value, and varied from numerical to categorical.

By replicating binary sorting algorithm, we were able to rank these 24 products from 1 to 24. 
The script has a code snippet that builds a Conjoint function taking two inputs - 
- Design matrix (binary value of four attributes against each product)
- Preference Vector (inverse rank for each product)

The function outputs the following:

- Partworth estimates
- Attribute importance (%)
- Willingness to pay for each attribute ($)
- Optimal price
- Market share at optimal price
- Maximum profit at optimal price





