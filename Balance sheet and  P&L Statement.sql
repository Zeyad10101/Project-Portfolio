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