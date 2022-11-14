DELIMITER $$
CREATE DEFINER=`root`@`localhost` PROCEDURE `correlation_matrix`(IN dur int)
SELECT z.ticker,
avg(z.discrete_return) as mu1,
std(z.discrete_return) as sigma1,
avg(z.continuous_return) as mu2,
std(z.continuous_return) as sigma,
variance(z.discrete_return) as variance1,
variance(z.continuous_return) as variance2
FROM
(
SELECT *,
(a.value-a.lagged_pricing)/a.lagged_pricing as discrete_return,
sqrt(1+LN(a.value/a.lagged_pricing))-1 as continuous_return
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









