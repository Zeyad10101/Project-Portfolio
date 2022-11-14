SELECT *,
(a.value-a.lagged_pricing)/a.lagged_pricing as discrete_return,
sqrt(1+LN(a.value/a.lagged_pricing))-1 as continuous_return
FROM
(SELECT *, lag(value,1)OVER(
	PARTITION BY ticker
	ORDER BY date
) as lagged_pricing
FROM client_2.pricing_daily_new
WHERE price_type = 'Adjusted') a;
-----------------------
DELIMITER $$
CREATE DEFINER=`root`@`localhost` PROCEDURE `Portfolio_risk`(IN dur int)
SELECT z.ticker,
avg(z.continuous_return) as mu,
std(z.continuous_return) as sigma
FROM
(
SELECT *,
LN(a.value/a.lagged_pricing) as continuous_return
FROM
(SELECT *, lag(value,1)OVER(
PARTITION BY ticker
ORDER BY date
) as lagged_pricing
FROM client_2.pricing_daily_new
WHERE price_type = 'Adjusted') a
) z
GROUP BY ticker;
END$$
DELIMITER ;

