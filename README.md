# Forecast_Inventory_Demand
Model developed in R to forecast inventory demand based on historical sales data

This code was created for the Data Scientist Training project at
Data Science Academy

Business Problem: Forecasting inventory demand based on sales history.

Data fields:
- Semana: Week number (From Thursday to Wednesday)
- Agencia_ID: Sales Depot ID
- Canal_ID: Sales Channel ID
- Ruta_SAK: Route ID (Several routes = Sales Depot)
- Cliente_ID: Client ID
- NombreCliente: Client name
- Producto_ID: Product ID
- NombreProducto: Product Name
- Venta_uni_hoy: Sales unit this week (integer)
- Venta_hoy: Sales this week (unit: pesos)
- Dev_uni_proxima: Returns unit next week (integer)
- Dev_proxima: Returns next week (unit: pesos)
- Demanda_uni_equil: Adjusted Demand (integer) (This is the target to predict)

Datasets were made available by Grupo Bimbo and can be
found on Kaggle
https://www.kaggle.com/c/grupo-bimbo-inventory-demand/data

