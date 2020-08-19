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
- Custom_Category	      
  This can be categorization of the business or products.                             
  Example: Product Names, Product Codes, Store Names, Sales person Names
  
- Date   
  This records when the sales, refunds, purchases and purchase occurred.              
  Example: 4-Apr-20
  
- Sales	               
  Captures complete Sales order (not individualproducts within the sales order).      
  Example: Number Value, 33
  
- Sales_Refund	        
  An adjustment to sales that arises from actual return by a customer of merchandise. 
  Example: Number Value, 33
  
- Purchases	            
  Purchases by the SME from its supplier.                                             
  Example: Number Value, 33
  
- Purchase_Cancelation  
  Total returns of purchased products back to SMEs suppliers                          
  Example: Number Value, 33
  
- Location	            
  The Location from which business operate.                                           
  Example: Rural Area, Urban Area, or Rural and Urban Area
  
- Service	              
  The type of undertaking the SME carries out.                                        
  Example: Merchandising, Service, Manufacturing and Hybrid Business
  
- SME_size              
  Employee count.                                                                     
  Example: A Number between 1 and 100
  
- Sector                
  Global Industry Classification Standard.                                            
  Example: Energy, Materials, Industrials, Consumer Discretionary, Consumer Staples, Health Care, Financials, Information Technology, Communications services and Utilities. [GICS Classification](https://www.spglobal.com/marketintelligence/en/documents/112727-gics-mapbook_2018_v3_letter_digitalspreads.pdf)
