## Welcome to Tabiri App
### Forecasting Demand and Supply for SMall and Medium enterprices in Kenya

[Tabiri App](https://tabiri.shinyapps.io/tabiriApp/) is live at the ShinyApps Server

### How to use the Platform

- Step 1. Download the Dataset Template
- Step 2. Clear the Sample Data and provide your Daily Sales and Purchase Qunatities
- Step 3. Upload your Dataset to the on [Tabiri App] (https://tabiri.shinyapps.io/tabiriApp/)
- Step 4. Select your Desired Forecast Duration **Currently Limited to a choise between 2-30 days**
- Step 5. Select a Forecasting algorithm from the drop down
- Step 6. Select Desired Forecast Attribute e.g Sales, Sales Refund, Purchases, Purchase Cancelations

### Data Preparation  
Field                 | Description                                                                                         | Example Data
-----------------------------------------------------------------------------------------------------------------------------------------------
Custom_Category	      | This can be categorization of the business or products.                             | Product Names, Product Codes, Store Names, Sales person Names
Date                  |	This records when the sales, refunds, purchases and purchase occurred.              | 4-Apr-20
Sales	                | Captures complete Sales order (not individualproducts within the sales order).      | Number Value
Sales_Refund	        | An adjustment to sales that arises from actual return by a customer of merchandise. | Number Value
Purchases	            | Purchases by the SME from its supplier.                                             | Number Value
Purchase_Cancelation  |	Total returns of purchased products back to SMEs suppliers                          | Number Value
Location	            | The Location from which business operate.                                           | Rural Area, Urban Area, or Rural and Urban Area
Service	              | The type of undertaking the SME carries out.                                        | Merchandising, Service, Manufacturing and Hybrid Business
SME_size              |	Employee count.                                                                     | A Number between 1 and 100
Sector                | Global Industry Classification Standard.                                            | [GICS Classification](https://www.spglobal.com/marketintelligence/en/documents/112727-gics-mapbook_2018_v3_letter_digitalspreads.pdf)
