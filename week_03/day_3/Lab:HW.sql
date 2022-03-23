-- MVP

-- SFWGHO

/* 
 * Question 1
 * How many employee records are lacking both a grade and salary?
 */

-- 

SELECT
    count(*),
    grade,
    salary
FROM employees
GROUP BY grade, salary 
HAVING grade IS NULL AND salary IS NULL ;

-- 2 employees are lacking both a grade and salary.

/*
 * Question 2
 * Produce a table with the two following fields (columns):
 * the department
 * the employees full name (first and last name)
 * Order your resulting table alphabetically by department, and then by last name
 */

SELECT 
    department,
    concat(first_name, ' ', last_name) AS full_name
FROM employees 
ORDER BY department, last_name; 

/*
 * Question 3.
 * Find the details of the top ten highest paid employees who have a 
 * last_name beginning with ‘A’.
*/

SELECT
    salary
FROM employees 
WHERE last_name ~ '^A.*'
ORDER BY salary DESC NULLS LAST 
LIMIT 10;

/*
 * Question 4.
 * Obtain a count by department of the employees who started work with 
 * the corporation in 2003.
 */

SELECT 
    department,
    count(id)
FROM employees 
WHERE start_date BETWEEN '2003-01-01' AND '2003-12-31'
GROUP BY department;

/*
 * Question 5.
 * Obtain a table showing department, fte_hours and the number of employees 
 * in each department who work each fte_hours pattern. Order the table 
 * alphabetically by department, and then in ascending order of fte_hours.
 */

SELECT 
    department,
    count(id) AS count_fte_hours,
    fte_hours 
FROM employees 
GROUP BY department, fte_hours 
ORDER BY department , fte_hours ;

/*
 * Question 6
 * Provide a breakdown of the numbers of employees enrolled, not enrolled, 
 * and with unknown enrollment status in the corporation pension scheme.
 */

SELECT 
    count(id),
    pension_enrol 
FROM employees 
GROUP BY pension_enrol

/*
 * Question 7.
 * Obtain the details for the employee with the highest salary in the ‘Accounting’ 
 * department who is not enrolled in the pension scheme?
 */

SELECT *
FROM employees
WHERE department = 'Accounting' AND pension_enrol = FALSE
ORDER BY salary DESC NULLS LAST  
LIMIT 1;

/*
 * Question 8.
 * Get a table of country, number of employees in that country, and the average 
 * salary of employees in that country for any countries in which more than 30 
 * employees are based. Order the table by average salary descending.
 */

SELECT 
    country,
    count(id) AS num_employees_per_country,
    avg(salary) AS avg_salary
FROM employees 
GROUP BY country
HAVING count(id) > 30
ORDER BY avg(salary) DESC NULLS LAST; 

/*
 * Question 9.
 * Return a table containing each employees first_name, last_name, 
 * full-time equivalent hours (fte_hours), salary, and a new column 
 * effective_yearly_salary which should contain fte_hours multiplied by salary. 
 * Return only rows where effective_yearly_salary is more than 30000.
 */

SELECT 
    first_name ,
    last_name ,
    fte_hours ,
    salary ,
    (fte_hours * salary) AS effective_yearly_salary
FROM employees 
WHERE (fte_hours * salary) > 30000
ORDER BY (fte_hours * salary) DESC NULLS LAST;

/*
 * Question 10
 * Find the details of all employees in either Data Team 1 or Data Team 2
 */

SELECT *
FROM employees  AS e 
LEFT JOIN teams AS t
ON e.team_id  = t.id 
WHERE t.name IN ('Data Team 1', 'Data Team 2');

/*
 * Question 11
 * Find the first name and last name of all employees who lack a local_tax_code.
 */

SELECT 
    first_name ,
    last_name,
    local_tax_code 
FROM employees AS e 
LEFT JOIN pay_details AS p 
ON e.pay_detail_id = p.id 
WHERE local_tax_code IS NULL  ;

/*
 * Question 12.
 * The expected_profit of an employee is defined as 
 * (48 * 35 * charge_cost - salary) * fte_hours, where charge_cost depends upon 
 * the team to which the employee belongs. Get a table showing expected_profit 
 * for each employee.
*/

SELECT
    first_name,
    LAST_name, 
    (48 * 35 * (CAST(charge_cost AS int) - salary) * fte_hours) 
    AS expected_profit
FROM employees AS e
LEFT JOIN teams AS t    
ON e.team_id = t.id
ORDER BY (48 * 35 * (CAST(charge_cost AS int) - salary) * fte_hours) 
NULLS LAST ;
    
/*
 * Question 13. [Tough]
 * Find the first_name, last_name and salary of the lowest paid employee in Japan 
 * who works the least common full-time equivalent hours across the corporation.”
*/

SELECT
  first_name,
  last_name,
  salary
FROM employees
WHERE country = 'Japan' AND fte_hours = (
    SELECT 
        fte_hours
    FROM employees
  GROUP BY fte_hours
  ORDER BY count(*) ASC NULLS LAST
  LIMIT 1
  )
ORDER BY salary ASC NULLS LAST
LIMIT 1;

























