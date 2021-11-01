/* Question 20 */
--20.	On average, which costs (list price) more: road tires or mountain bike tires?
SELECT Road, AVG(ListPrice) as AvgOfListPrice 
FROM Component
WHERE Road IN ('Road', 'MTB') AND Description LIKE '%tire%'
GROUP BY Road
ORDER BY AVG(ListPrice) DESC

/* Question 21 */
--21.	In May 2003, which employees sold road bikes that they also painted?
SELECT E.EmployeeID, E.LastName
FROM Employee as E
JOIN CustomerTransaction as CT ON E.EmployeeID = CT.EmployeeID
JOIN Bicycle as B ON E.EmployeeID = B.Painter
JOIN BikeParts as BP ON E.EmployeeID = BP.EmployeeID
JOIN Component as Comp ON BP.ComponentID = Comp.ComponentID
WHERE B.Painter = CT.EmployeeID
	  AND (CT.TransactionDate BETWEEN '2003-04-30' AND '2003-06-01')
	  AND (Comp.Road = 'Road')
GROUP BY E.EmployeeID, E.LastName

/* Question 22 */
--22.	In 2002, was the Old English letter style more popular with some paint jobs?
SELECT P.PaintID, P.ColorName, COUNT(LetterStyleID) as 'Number of Bikes Painted'
FROM Paint as P
JOIN Bicycle as B ON P.PaintID = B.PaintID
JOIN LetterStyle as LS ON B.LetterStyleID = LS.LetterStyle
WHERE B.OrderDate BETWEEN '2001-12-31' AND '2003-01-01'
	  AND B.LetterStyleID = 'English'
GROUP BY P.PaintID, P.ColorName
ORDER BY COUNT(LetterStyleID) DESC

/* Question 23 */
--23.	Which race bikes in 2003 sold for more than the average price of race bikes in 2002?
SELECT B1.SerialNumber, B1.ModelType, B1.OrderDate, B1.SalePrice
FROM Bicycle as B1
WHERE B1.ModelType = 'Race'
	AND (B1.OrderDate BETWEEN '2002-12-31' AND '2004-01-01')
	AND (B1.SalePrice > (SELECT AVG(B.SalePrice) as avg_sale_price_2002
					  FROM Bicycle as B
					  WHERE (B.ModelType = 'Race')
						AND (B.OrderDate BETWEEN '2001-12-31' AND '2003-01-01')))
GROUP BY B1.SerialNumber, B1.ModelType, B1.OrderDate, B1.SalePrice;

/* Question 24 */
--24.	Which component that had no sales (installations) in 2004 has the highest inventory value (cost basis)?
SELECT TOP(1) M.ManufacturerName, C.ProductNumber, C.Category, (C.EstimatedCost * C.QuantityOnHand) as 'Value', C.ComponentID
FROM Component as C 
JOIN BikeParts as BP ON C.ComponentID = BP.ComponentID
JOIN Manufacturer as M ON C.ManufacturerID = M.ManufacturerID
JOIN ManufacturerTransaction as MT ON M.ManufacturerID = MT.ManufacturerID
WHERE BP.DateInstalled IS NULL AND (MT.TransactionDate BETWEEN '2003-12-31' AND '2005-01-01')
GROUP BY M.ManufacturerName, C.ProductNumber, C.Category,(C.EstimatedCost * C.QuantityOnHand), C.ComponentID
ORDER BY (C.EstimatedCost * C.QuantityOnHand) DESC

/* Question 25 */
--25.	Create a vendor contacts list of all manufacturers and retail stores in California.Include only the columns for VendorName and Phone. The retail stores should only include stores that participated in the sale of at least one bicycle in 2004
SELECT DISTINCT RS.StoreName AS 'VendorName', RS.Phone
FROM RetailStore as RS
JOIN Bicycle as B ON RS.StoreID = B.StoreID
WHERE YEAR(B.OrderDate) = '2004'

UNION 

SELECT DISTINCT M.ManufacturerName AS 'VendorName', M.Phone
FROM Manufacturer as M
JOIN PurchaseOrder as PO ON M.ManufacturerID = PO.ManufacturerID
JOIN Employee AS E ON PO.EmployeeID = E.EmployeeID
JOIN Bicycle AS B ON E.EmployeeID = B.EmployeeID
WHERE YEAR(B.OrderDate) = '2004'

/* Question 26 */
--26.	List all of the employees who report to Venetiaan.
SELECT (SELECT E2.LastName 
		FROM Employee AS E2
		WHERE E2.LastName = 'Venetiaan') AS 'LastName', E.EmployeeID, E.LastName, E.FirstName, E.Title
FROM Employee AS E
WHERE E.CurrentManager = 15293

/* Question 27 */
--27.	List the components where the company purchased at least 25 percent more units than it used through June 30, 2000. 
SELECT C.ComponentID, M.ManufacturerName, C.ProductNumber, C.Category, 
	   SUM(PI.QuantityReceived) AS TotalReceived, SUM(PI.Quantity) AS TotalUsed, 
	   ((C.ListPrice - PI.PricePaid) * PI.Quantity) AS NetGain,
	   ((C.ListPrice - PI.PricePaid) / PI.PricePaid) AS NetPct, C.ListPrice
FROM Component AS C 
JOIN PurchaseItem AS PI ON C.ComponentID = PI.ComponentID
JOIN PurchaseOrder AS PO ON PI.PurchaseID = PO.PurchaseID
JOIN Manufacturer AS M ON PO.ManufacturerID = M.ManufacturerID
GROUP BY C.ComponentID, M.ManufacturerName, C.ProductNumber, C.Category, C.ListPrice, PO.OrderDate, PI.PricePaid, PI.Quantity
HAVING SUM(PI.Quantity) >= (SUM(PI.QuantityReceived) * .25) + SUM(PI.QuantityReceived)
	AND PO.OrderDate <= '2000-06-30'
ORDER BY ((C.ListPrice - PI.PricePaid) / PI.PricePaid)

/* Question 28 */
--28.	In which years did the average build time for the year exceed the overall average build time for all years? The build time is the difference between order date and ship date. 
SELECT YEAR(B.OrderDate) AS Year, AVG(DATEDIFF(DAY,B.OrderDate, B.ShipDate)) AS BuildTime
FROM Bicycle AS B
GROUP BY YEAR(B.OrderDate)
HAVING AVG(DATEDIFF(DAY,B.OrderDate, B.ShipDate)) > (SELECT (AVG(DATEDIFF(DAY, B1.OrderDate, B1.ShipDate)))
													 FROM Bicycle as B1)
ORDER BY Year ASC
