DELIMITER $$
CREATE DEFINER=`root`@`localhost` PROCEDURE `12M_sigma`(IN dur int)
SELECT z.ticker,
std(z.continuous_return) as sigma
FROM
(
SELECT *,
LN(a.value/a.lagged_pricing) as continuous_return
FROM
(SELECT *, lag(value,250)OVER(
PARTITION BY ticker
ORDER BY date
) as lagged_pricing
FROM client_2.pricing_daily_new
WHERE price_type = 'Adjusted') a
) z
GROUP BY ticker;
END$$
DELIMITER ;
