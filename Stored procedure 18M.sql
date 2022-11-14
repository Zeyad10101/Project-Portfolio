DELIMITER $$
CREATE DEFINER=`root`@`localhost` PROCEDURE `gtprices_18M`(IN dur int)
BEGIN
SELECT *,
(a.value-a.lagged_pricing)/a.lagged_pricing as discrete_return,
sqrt(1+LN(a.value/a.lagged_pricing))-1 as continuous_return
FROM
(SELECT *, lag(value,375)OVER(
PARTITION BY ticker
ORDER BY date desc
) as lagged_pricing
FROM client_2.pricing_daily_new
WHERE ticker in ('IXN','QQQ','IEF','GLD') AND price_type = 'Adjusted' ) a;
END$$
DELIMITER ;
--------------------------
