-- Balance sheet Year 2018 --
SELECT 
        ss.statement_section AS statement_section,
        FORMAT(SUM(IFNULL(jeli.debit, 0)),
            0) AS debit,
        FORMAT(SUM(IFNULL(jeli.credit, 0)),
            0) AS credit,
        FORMAT((SUM(jeli.debit) - SUM(jeli.credit)),
            0) AS total
    FROM
        (((journal_entry_line_item jeli
        LEFT JOIN account ac ON ((ac.account_id = jeli.account_id)))
        LEFT JOIN statement_section ss ON ((ss.statement_section_id = ac.balance_sheet_section_id)))
        LEFT JOIN journal_entry je ON ((je.journal_entry_id = jeli.journal_entry_id)))
    WHERE
        ((ss.statement_section_code IN ('CA' , 'FA', 'DA', 'CL', 'LLL', 'DL', 'EQ'))
            AND (je.debit_credit_balanced <> 0)
            AND (YEAR(je.entry_date) = 2018)
            AND (je.cancelled = 0)
            AND (ac.company_id = 1))
    GROUP BY ss.statement_section_id
    ORDER BY ss.statement_section_id

---------------
-- Balance sheet year 2019 -- 

SELECT 
        ss.statement_section AS statement_section,
        FORMAT(SUM(IFNULL(jeli.debit, 0)),
            0) AS debit,
        FORMAT(SUM(IFNULL(jeli.credit, 0)),
            0) AS credit,
        FORMAT((SUM(jeli.debit) - SUM(jeli.credit)),
            0) AS total
    FROM
        (((journal_entry_line_item jeli
        LEFT JOIN account ac ON ((ac.account_id = jeli.account_id)))
        LEFT JOIN statement_section ss ON ((ss.statement_section_id = ac.balance_sheet_section_id)))
        LEFT JOIN journal_entry je ON ((je.journal_entry_id = jeli.journal_entry_id)))
    WHERE
        ((ss.statement_section_code IN ('CA' , 'FA', 'DA', 'CL', 'LLL', 'DL', 'EQ'))
            AND (je.debit_credit_balanced <> 0)
            AND (YEAR(je.entry_date) = 2019)
            AND (je.cancelled = 0)
            AND (ac.company_id = 1))
    GROUP BY ss.statement_section_id
    ORDER BY ss.statement_section_id
    
---------------------------
-- Balance Sheet Year 2020 --
SELECT 
        ss.statement_section AS statement_section,
        FORMAT(SUM(IFNULL(jeli.debit, 0)),
            0) AS debit,
        FORMAT(SUM(IFNULL(jeli.credit, 0)),
            0) AS credit,
        FORMAT((SUM(jeli.debit) - SUM(jeli.credit)),
            0) AS total
    FROM
        (((journal_entry_line_item jeli
        LEFT JOIN account ac ON ((ac.account_id = jeli.account_id)))
        LEFT JOIN statement_section ss ON ((ss.statement_section_id = ac.balance_sheet_section_id)))
        LEFT JOIN journal_entry je ON ((je.journal_entry_id = jeli.journal_entry_id)))
    WHERE
        ((zz.statement_section_code IN ('CA' , 'FA', 'DA', 'CL', 'LLL', 'DL', 'EQ'))
            AND (je.debit_credit_balanced <> 0)
            AND (YEAR(je.entry_date) = 2020)
            AND (je.cancelled = 0)
            AND (ac.company_id = 1))
    GROUP BY ss.statement_section_id
    ORDER BY ss.statement_section_id
    ----------
    -- P&L statement for the year 2018 --
     SELECT 
        ss.statement_section_order AS statement_section_order,
        ss.statement_section_code AS statement_section_code,
        ss.statement_section AS statement_section,
        FORMAT(SUM((CASE
                WHEN
                    (YEAR(je.entry_date) = 2018)
                THEN
                    (CASE
                        WHEN (ss.debit_is_positive = 0) THEN jel.credit
                        ELSE (jel.credit * -(1))
                    END)
                ELSE 0
            END)),
            0) AS Amount
    FROM
        (((journal_entry_line_item jel
        JOIN account ac ON ((ac.account_id = jel.account_id)))
        JOIN journal_entry je ON ((je.journal_entry_id = jel.journal_entry_id)))
        JOIN statement_section ss ON ((ss.statement_section_id = ac.profit_loss_section_id)))
    WHERE
        ((ac.profit_loss_section_id <> 0)
            AND (je.debit_credit_balanced <> 0)
            AND (je.cancelled = 0)
            AND (YEAR(je.entry_date) = 2018))
    GROUP BY ss.statement_section_id
    -------------
    -- P&L statement for the year 2019 --
    SELECT 
        ss.statement_section_order AS statement_section_order,
        ss.statement_section_code AS statement_section_code,
        ss.statement_section AS statement_section,
        FORMAT(SUM((CASE
                WHEN
                    (YEAR(je.entry_date) = 2019)
                THEN
                    (CASE
                        WHEN (ss.debit_is_positive = 0) THEN jel.credit
                        ELSE (jel.credit * -(1))
                    END)
                ELSE 0
            END)),
            0) AS Amount
    FROM
        (((journal_entry_line_item jel
        JOIN account ac ON ((ac.account_id = jel.account_id)))
        JOIN journal_entry je ON ((je.journal_entry_id = jel.journal_entry_id)))
        JOIN statement_section ss ON ((ss.statement_section_id = ac.profit_loss_section_id)))
    WHERE
        ((ac.profit_loss_section_id <> 0)
            AND (je.debit_credit_balanced <> 0)
            AND (je.cancelled = 0)
            AND (YEAR(je.entry_date) = 2019))
    GROUP BY ss.statement_section_id
    -----------
    SELECT 
        ss.statement_section_order AS statement_section_order,
        ss.statement_section_code AS statement_section_code,
        ss.statement_section AS statement_section,
        FORMAT(SUM((CASE
                WHEN
                    (YEAR(je.entry_date) = 2020)
                THEN
                    (CASE
                        WHEN (ss.debit_is_positive = 0) THEN jel.credit
                        ELSE (jel.credit * -(1))
                    END)
                ELSE 0
            END)),
            0) AS Amount
    FROM
        (((journal_entry_line_item jel
        JOIN account ac ON ((ac.account_id = jel.account_id)))
        JOIN journal_entry je ON ((je.journal_entry_id = jel.journal_entry_id)))
        JOIN statement_section ss ON ((ss.statement_section_id = ac.profit_loss_section_id)))
    WHERE
        ((ac.profit_loss_section_id <> 0)
            AND (je.debit_credit_balanced <> 0)
            AND (je.cancelled = 0)
            AND (YEAR(je.entry_date) = 2020))
    GROUP BY ss.statement_section_id